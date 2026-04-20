#' Analyser automatiquement la structure descriptive d'un jeu de données
#'
#' Cette fonction effectue une analyse automatisée de la structure d'un jeu de
#' données en catégorisant les variables selon leur type statistique et génère
#' un rapport descriptif formaté. Elle identifie automatiquement les variables
#' quantitatives (paramétriques et non-paramétriques), qualitatives, dichotomiques
#' et de type date, puis configure les statistiques descriptives appropriées
#' pour chaque catégorie. Cette fonction constitue un outil essentiel pour
#' l'exploration préliminaire et la préparation d'analyses statistiques.
#'
#' @param data Un data.frame contenant les données à analyser. Toutes les
#'   colonnes seront examinées et catégorisées selon leur type statistique.
#' @param parametric Pattern d'expression régulière ou critère spécifiant
#'   quelles variables numériques doivent être considérées comme paramétriques.
#'   Par défaut utilise `nullfile()` pour une détection automatique.
#' @param qt_stat Liste optionnelle de statistiques personnalisées pour les
#'   variables quantitatives. Permet de surcharger les statistiques par défaut
#'   (min, Q1, médiane, Q3, max, moyenne±écart-type).
#' @param ql_stat Liste optionnelle de statistiques personnalisées pour les
#'   variables qualitatives. Permet de surcharger la statistique par défaut
#'   (effectifs et pourcentages).
#'
#' @returns Une liste structurée contenant la classification des variables
#'   et leurs statistiques associées, organisée en quatre catégories principales.
#'   Chaque catégorie inclut les noms des variables (`vars`), les statistiques
#'   configurées (`stat`) et les en-têtes de colonnes (`spanner`) appropriés.
#'
#' @section Architecture de classification automatique :
#' La fonction implémente un algorithme de classification séquentielle qui
#' examine chaque variable selon des critères statistiques précis. Les variables
#' numériques avec plus de deux valeurs uniques sont classées comme quantitatives,
#' puis subdivisées en paramétriques et non-paramétriques selon le pattern
#' spécifié. Les variables non-numériques et non-dates deviennent qualitatives.
#' Les variables numériques binaires (exactement deux valeurs) sont catégorisées
#' comme dichotomiques. Les variables de type Date sont identifiées séparément
#' pour un traitement temporel approprié.
#'
#' @section Adaptation linguistique des statistiques :
#' La fonction détecte automatiquement la configuration linguistique via
#' `getOption("OutDec")` et adapte les étiquettes statistiques en conséquence.
#' Pour une configuration française (virgule décimale), les termes "Médiane"
#' et "Moyenne" remplacent automatiquement leurs équivalents anglais. Cette
#' adaptation garantit la cohérence culturelle des productions analytiques
#' sans intervention manuelle de l'utilisateur.
#'
#' @section Génération de rapports CLI formatés :
#' La fonction produit un rapport détaillé utilisant l'interface CLI pour
#' présenter la classification des variables de manière structurée et visuelle.
#' Le rapport inclut le nombre total de variables par catégorie, la liste
#' des variables dans chaque groupe, et utilise des codes couleur pour
#' améliorer la lisibilité. Cette fonctionnalité facilite l'inspection
#' rapide de la structure des données et la validation des classifications
#' automatiques avant l'analyse statistique proprement dite.
#'
#' @section Prérequis techniques et dépendances :
#' Cette fonction nécessite un écosystème de packages spécialisés pour un
#' fonctionnement optimal. Les packages `cli` pour le formatage de sortie,
#' `stringr` pour la manipulation de chaînes, `purrr` pour les opérations
#' sur listes, et `glue` pour l'interpolation sont essentiels. Les fonctions
#' auxiliaires `nullfile()`, `str_u()`, `str_flatten_comma()` et `list_modify()`
#' doivent également être disponibles dans l'environnement d'exécution.
#'
#' @examples
#' # Configuration par défaut avec classification automatique
#' df_mtcars <- easy_descr(mtcars)
#' str(df_mtcars, max.level = 2)
#' df_mtcars
#'
#' # Appel des éléments
#' df_mtcars$qt$vars$total
#' df_mtcars$ql$vars
#'
#' # Spécification explicite de variables paramétriques
#' df_mtcars_para <- mtcars |> easy_descr(parametric = "mpg|hp|disp")
#' df_mtcars_para$qt$vars$parametric
#'
#' # Typographie par défaut (EN)
#' df_en <- easy_descr(mtcars)
#' df_en$qt$stat |> with(c(median, mean))
#'
#' # Typographie FR
#' lang_fr()
#' df_fr <- easy_descr(mtcars)
#' df_fr$qt$stat |> with(c(median, mean))
#'
#' # Personnalisation des statistiques quantitatives
#' df_stats <-
#' easy_descr(data = mtcars,
#'            qt_stat =
#'              list(median = c("Median" = "{median}"),
#'                   range = c("Range" = "{min}-{max}")))
#'
#' df_stats$qt$stat
#'
#' # Analyse avec données temporelles
#' df_storms <-
#' dplyr::storms |>
#'   dplyr::mutate(date_storm = as.Date(paste(year, month, day, sep = "-")))
#'
#' # Configuration avec toutes variables continues comme paramétriques
#' df_storms_para <- df_storms |> easy_descr(parametric = "all_continuous")
#' df_storms_para$qt$vars$parametric
#' df_storms_para$qt$vars$nonparametric
#'
#' # Application avec gtsummary (TODO)
#'
#' @family fonctions d'analyse exploratoire
#'
#' @export
easy_descr <- \(data, parametric = nullfile(), qt_stat = NULL, ql_stat = NULL) {
  cli_h1("easy_descr")
  cli_text("\n\n")

  ### QT DATA -------------------------------------------------------------------------------

  str_parametric <- glue("\\b{parametric}\\b")

  qt_vars <-
    lst(
      total = data |>
        keep(~ is.numeric(.) & length(unique(na.omit(.))) != 2) |>
        names(),
      parametric = str_subset(total, str_u(str_parametric)),
      nonparametric = data |>
        select(all_of(total), -matches(str_parametric)) |>
        names()
    )

  if (all(parametric == "all_continuous")) {
    qt_vars$parametric <- qt_vars$total
    qt_vars$nonparametric <- NULL
  }

  .qt_stat <-
    list(
      min = c("Min" = "{min}"),
      q1 = c("Q1" = "{p25}"),
      median = c("Median (IQR)" = "{median} ({p25}\u2014{p75})"),
      q3 = c("Q3" = "{p75}"),
      max = c("Max" = "{max}"),
      mean = c("Mean\u00b1SD" = "{mean}\u00b1{sd}")
    )

  if (getOption("OutDec") == ",") {
    .qt_stat <-
      .qt_stat |>
      list_modify(
        median = c("M\u00e9diane (IQR)" = "{median} ({p25}\u2014{p75})"),
        mean = c("Moyenne\u00b1SD" = "{mean}\u00b1{sd}")
      )
  }

  qt_stat <- .qt_stat |> list_modify(!!!qt_stat)

  ### QL DATA -----------------------------------------------------------------------------

  ql_vars <-
    data |>
    keep(~ !is.numeric(.) & !is.Date(.)) |>
    names()

  ql_stat <-
    list(n = c("n (%)" = "{n} ({p})")) |>
    list_modify(!!!ql_stat)

  ### BIN DATA ----------------------------------------------------------------------------

  bin_vars <-
    data |>
    select(-eval(qt_vars$total), -all_of(ql_vars), -where(is.Date)) |>
    names()

  ### DATE DATA ---------------------------------------------------------------------------

  date_vars <-
    data |>
    keep(is.Date) |>
    names()

  ### ASSIGN ------------------------------------------------------------------------------

  descr <-
    lst(
      qt = lst(vars = qt_vars, stat = qt_stat, spanner = names(list_c(stat))),
      ql = lst(vars = ql_vars, stat = ql_stat, spanner = names(list_c(stat))),
      bin = lst(vars = bin_vars, stat = ql_stat, spanner = names(list_c(stat))),
      date = lst(vars = date_vars)
    )

  ### CLI -------------------------------------------------------------------------------

  cli_qt_total_length <-
    data |>
    select(eval(descr$qt$vars$total)) |>
    length()

  cli_qt_p <- str_flatten_comma(descr$qt$vars$parametric)
  cli_qt_np <- str_flatten_comma(descr$qt$vars$nonparametric)
  cli_ql <- str_flatten_comma(descr$ql$vars)
  cli_bin <- str_flatten_comma(descr$bin$vars)
  cli_date <- str_flatten_comma(descr$date$vars)

  cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli_text("\n\n")
  cli_alert_success("{.strong Quantitative:} {cli_qt_total_length} variables")
  cli_li(c("Parametric: {cli_qt_p}", "Non-parametric: {cli_qt_np}"))
  cli_text("\n\n")
  cli_alert_success("{.strong Qualitative:} {length(descr$ql$vars)} variables")
  cli_li("{cli_ql}")
  cli_text("\n\n")
  cli_alert_success("{.strong Dichotomous:} {length(descr$bin$vars)} variables")
  cli_li("{cli_bin}")
  cli_text("\n\n")
  cli_alert_success("{.strong Date:} {length(descr$date$vars)} variables")
  cli_li("{cli_date}")
  cli_text("\n\n")
  cli_rule()

  return(descr)
}
