#' Configurer la localisation française pour R et gtsummary
#'
#' Cette fonction configure l'environnement R pour adopter les conventions
#' de formatage françaises, incluant le séparateur décimal virgule et les
#' paramètres de localisation pour les tableaux gtsummary. Elle permet également
#' la restauration des paramètres anglais par défaut. Cette fonction facilite
#' la production de rapports et analyses conformes aux standards typographiques
#' français dans un contexte professionnel ou académique.
#'
#' @param reset Valeur logique contrôlant l'action de la fonction. Si `FALSE`
#'   (par défaut), active la localisation française. Si `TRUE`, restaure
#'   les paramètres anglais par défaut.
#'
#' @returns Cette fonction est appelée pour ses effets de bord et ne retourne
#'   aucune valeur. Elle modifie les options globales de R et la configuration
#'   du package gtsummary.
#'
#' @section Configuration française :
#' Lorsque la localisation française est activée, la fonction applique plusieurs
#' modifications aux paramètres globaux. Le séparateur décimal devient la virgule
#' via `options(OutDec = ",")`, conforme aux standards typographiques français.
#' Le thème gtsummary est configuré pour utiliser la langue française avec
#' l'espace comme séparateur de milliers, garantissant une présentation cohérente
#' des tableaux statistiques selon les conventions locales.
#'
#' @section Restauration des paramètres :
#' Le mode reset restaure les paramètres par défaut de R avec le point comme
#' séparateur décimal et remet le thème gtsummary à sa configuration initiale.
#' Cette fonctionnalité s'avère particulièrement utile dans les environnements
#' de développement collaboratifs où différentes conventions de formatage
#' peuvent être nécessaires selon les projets ou les destinataires des analyses.
#'
#' @section Prérequis techniques :
#' Cette fonction nécessite les packages `gtsummary` pour la gestion des thèmes
#' de tableaux et `cli` pour l'affichage des messages informatifs. Les fonctions
#' `theme_gtsummary_language()` et `reset_gtsummary_theme()` doivent être
#' disponibles dans l'environnement pour un fonctionnement optimal.
#'
#' @examples
#' library(gtsummary)
#' 
#' # Activation de la typo française
#' lang_fr()
#' 
#' # Vérification du formatage des nombres
#' print(3.14159)
#' 
#' mean(mtcars$mpg)
#' 
#' mtcars |>
#'   select(mpg, disp, hp) |>
#'   tbl_summary()
#' 
#' # Restauration des paramètres en
#' lang_fr(reset = TRUE)
#' 
#' # Vérification du retour aux paramètres par défaut
#' print(3.14159)
#' 
#' mean(mtcars$mpg)
#' 
#' mtcars |>
#'   select(mpg, disp, hp) |>
#'   tbl_summary()
#'
#' @family fonctions de configuration
#' @seealso 
#' * [gtsummary::theme_gtsummary_language()] pour la configuration des tableaux
#'
#' @references
#' Documentation gtsummary sur les thèmes et typographies :
#' \url{https://www.danieldsjoberg.com/gtsummary/articles/themes.html}
#' 
#' @export
lang_fr <- \(reset = FALSE) {
  
  if (reset) {
    
    options(OutDec = ".")
  
    reset_gtsummary_theme()
    
    cli_alert_info("Setting language: EN")
    
  } else {
  
    options(OutDec = ",")
    
    suppressMessages(
      theme_gtsummary_language(language = "fr", big.mark = " ")
    )
    
    cli_alert_info("Setting language: FR")
    
  }
  
}

#' Title
#'
#' @param data arg
#' @param .parametric arg
#' @param ... arg
#'
#' @returns arg
#' @export
#'
#' @examples arg
#' 
use_vars <- \(data, .parametric = check_opts(parametric), ...) {
  
  assign(".vars_context", new.env(parent = emptyenv()), envir = globalenv())
  
  .vars_context$current <- easy_descr(data, .parametric, ...)
  
  return(data)

}

#' Title
#'
#' @param env arg
#'
#' @returns arg
#' @export
#'
#' @examples arg
#' 
clear_vars <- \(env = ".vars_context") {
  
  env <- enexpr(env)
  
  if (exists(env, envir = globalenv())) rm(list = env, envir = globalenv())

}

#' Configurer les options globales pour l'analyse statistique et la visualisation
#'
#' Cette fonction établit un ensemble complet de paramètres de configuration
#' pour l'analyse statistique, la visualisation de données et la production
#' de rapports. Elle génère une structure d'options centralisée adaptée aux
#' conventions linguistiques et typographiques, facilitant la standardisation
#' des analyses dans un environnement professionnel ou de recherche.
#'
#' @param .default_font Police de caractères par défaut à utiliser dans
#'   l'ensemble du système. Par défaut `"trebuchet ms"`.
#' @param .assign Valeur logique contrôlant l'assignation de l'objet `opts`
#'   dans l'environnement global. Si `TRUE` (par défaut), l'objet est
#'   automatiquement disponible pour les autres fonctions.
#' @param ... Arguments supplémentaires permettant de personnaliser ou
#'   surcharger les paramètres de configuration par défaut.
#'
#' @returns Une liste structurée contenant l'ensemble des paramètres de
#'   configuration, organisée en sous-catégories thématiques (statistiques,
#'   étiquettes, formatage, couleurs, polices). Si `.assign = TRUE`, l'objet
#'   est également assigné à la variable `opts` dans l'environnement global.
#'
#' @section Architecture de la configuration :
#' La fonction génère une structure hiérarchique organisée en modules
#' fonctionnels distincts. Le module statistique définit les tests et
#' statistiques descriptives pour variables quantitatives et qualitatives.
#' Le module étiquetage configure les libellés pour différents contextes
#' d'analyse. Les modules de formatage établissent les conventions pour
#' les séparateurs, intervalles de confiance et précision numérique.
#' Les modules visuels configurent les polices et palettes de couleurs.
#'
#' @section Adaptation linguistique automatique :
#' La fonction détecte automatiquement la configuration linguistique de
#' l'environnement R via `getOption("OutDec")` et adapte les étiquettes
#' et conventions de formatage en conséquence. Pour une configuration
#' française (virgule comme séparateur décimal), les termes statistiques
#' sont traduits et les conventions typographiques françaises sont appliquées.
#' Cette adaptation garantit la cohérence culturelle des productions analytiques.
#'
#' @section Fonctions auxiliaires intégrées :
#' La fonction intègre plusieurs utilitaires spécialisés pour le traitement
#' des configurations. La fonction `.vars()` génère des configurations
#' dynamiques basées sur l'analyse descriptive des données. La fonction
#' `.ci()` structure les paramètres d'intervalles de confiance. La fonction
#' `.fonts()` gère la détection et validation des polices système. Ces
#' composants garantissent une configuration robuste et adaptative.
#'
#' @section Prérequis techniques :
#' Cette fonction nécessite plusieurs packages spécialisés pour un
#' fonctionnement optimal. Les packages `scales`, `glue`, `purrr` et
#' `gtsummary` fournissent les fonctions de formatage et d'étiquetage.
#' Les fonctions auxiliaires `check_fonts()`, `easy_descr()`, `str_cap()`
#' et `nullfile()` doivent être disponibles dans l'environnement pour
#' une exécution complète de la configuration.
#'
#' @examples
#' # Configuration basique avec paramètres par défaut
#' config_analyse <- set_opts()
#' 
#' # Vérification des paramètres statistiques configurés
#' config_analyse$qt_stat$mean
#' config_analyse$ql_stat$n
#' 
#' # Configuration avec police personnalisée
#' set_opts(.default_font = "arial", .assign = FALSE)
#' 
#' # Personnalisation des étiquettes via arguments supplémentaires
#' config_personnalise <- set_opts(
#'   labs = list(
#'     header = "Variables analysées",
#'     overall = "Ensemble de l'échantillon"
#'   ),
#'   .assign = FALSE
#' )
#' 
#' # Utilisation avec données mtcars pour démonstration
#' set_opts()
#' 
#' # Accès aux configurations de formatage numérique
#' opts$digits
#' opts$pvalue$format
#' 
#' # Configuration des couleurs et palettes
#' opts$color$base
#' opts$palette
#' 
#' # Démonstration des configurations d'intervalles de confiance
#' opts$ci$label
#' opts$ci$data
#' 
#' # Activation de la localisation française
#' options(OutDec = ",")
#' config_fr <- set_opts(.assign = FALSE)
#' 
#' # Vérification des adaptations linguistiques
#' config_fr$labs$sex
#' config_fr$qt_stat$median
#' 
#' # Restauration de la configuration anglaise
#' options(OutDec = ".")
#' config_en <- set_opts(.assign = FALSE)
#' config_en$labs$sex
#' 
#' # Utilisation dans un contexte d'analyse avec mpg
#' set_opts()
#' 
#' # Application des configurations de test statistique
#' # (nécessite l'écosystème gtsummary)
#' \dontrun{
#' library(gtsummary)
#' mpg |>
#'   select(cty, hwy, class, drv) |>
#'   tbl_summary(
#'     statistic = opts$qt_stat,
#'     digits = opts$digits
#'   )
#' }
#' 
#' # Configuration pour production de rapports
#' set_opts(
#'   color = list(
#'     base = "#2C3E50",
#'     cold = c("#EBF5FF", "#3498DB")
#'   ),
#'   .default_font = "calibri"
#' )
#' 
#' # Vérification des paramètres de police configurés
#' opts$font$alpha
#' opts$font$digit
#' 
#' # Démonstration des séparateurs configurés
#' opts$sep$int  # Séparateur interne
#' opts$sep$ext  # Séparateur externe
#'
#' @family fonctions de configuration
#' @seealso 
#' * [gtsummary::theme_gtsummary_language()] pour la localisation des tableaux
#' * [scales::label_number()] et [scales::label_percent()] pour le formatage
#'
#' @references
#' Documentation gtsummary sur la configuration des thèmes :
#' \url{https://www.danieldsjoberg.com/gtsummary/articles/themes.html}
#' 
#' @export
set_opts <- \(.default_font = "trebuchet ms",
              .vars_envir = .vars_context$current,
              .assign = TRUE,
              .name = "opts",
              ...) {
  
  dots <- lst(...)

  .fonts <- \(x) check_fonts(.default = .default_font, .auto = x)
  
  .label <-
  list(n = 
         label_number(accuracy = .1, 
                      decimal.mark = getOption("OutDec")),
       p = 
         label_percent(accuracy = .1,
                       suffix = "",
                       decimal.mark = getOption("OutDec")))
  
  .opts_set <-
  lst(parametric = 
        nullfile(),
      qt_stat =
        list(min = c("Min" = "{min}"),
             q1 = c("Q1" = "{p25}"),
             median = c("Median (IQR)" = "{median} ({p25}\u2014{p75})"),
             q3 = c("Q3" = "{p75}"),
             max = c("Max" = "{max}"),
             mean = c("Mean\u00b1SD" = "{mean}\u00b1{sd}")),
        ql_stat = 
          list(n = c("n (%)" = "{n} ({p})")),
      labs =
        list(sex = list(m = "Male", f = "Female"),
             bin = list(no = "No", yes = "Yes"),
             missing = "Missing data",
             header = "Characteristic",
             reference = "Reference",
             overall = "Overall",
             spanner = glue("{c('Univariable', 'Multivariable')} analysis")),
      sep = 
        list(int = ": ",
             ext = "; "),
      ci =
        list(lim = "[",
             sep = "; ",
             label = "95%CI",
             data = c("conf.low", "conf.high")),
      acro = 
        acro(),
      digits =
        list(all_continuous() ~ c(1, .label$n, .label$n),
             all_categorical() ~ c(0, .label$p)),
      pvalue = 
        list(format = label_style_pvalue(digits = 2),
             seuil = 0.05),
      font =
        list(alpha = "luciole",
             digit = "luciole"),
      color = 
        list(base = "#999",
             cold = c("#F0FAFF", "#0099FF"),
             warm = c("#FFF5F5", "#FF0000")),
      palette = 
        c(color$base, 
          color$cold[2]))

  if (getOption("OutDec") == ",") {

    .opts_set <-
    .opts_set |> 
      list_modify(qt_stat =
                    list(median = c("M\u00e9diane (IQR)" = "{median} ({p25}\u2014{p75})"),
                         mean = c("Moyenne\u00b1SD" = "{mean}\u00b1{sd}")),
                  labs = 
                    list(sex = list(m = "Hommes", f = "Femmes"),
                         bin = list(no = "Non", yes = "Oui"),
                         missing = "Donn\u00e9e manquante",
                         header = "Variable",
                         reference = "R\u00e9f\u00e9rence",
                         overall = "Total",
                         spanner = glue("Analyse {c('univariable', 'multivariable')}")),
                  sep =
                    list(int = " : ",
                         ext = " ; "),
                  ci =
                    list(sep = " ; ",
                         label = "IC95%"),
                  acro = 
                    acro())

  }
  
  .vars <- \(..., envir = .vars_envir) {
  
    cap <- \(x) str_cap(tolower, names(x))
    
    list(test =
           list(envir$qt$vars$parametric ~ "quanti.test.para",
                envir$qt$vars$nonparametric ~ "quanti.test.nonpara",
                all_categorical() ~ "quali.test"),
         stat =
           list(envir$qt$vars$parametric ~ envir$qt$stat$mean,
                envir$qt$vars$nonparametric ~ envir$qt$stat$median,
                all_categorical() ~ envir$ql$stat$n),
         label =
           list(envir$qt$vars$parametric ~ cap(envir$qt$stat$mean),
                envir$qt$vars$nonparametric ~ cap(envir$qt$stat$median),
                all_categorical() ~ cap(envir$ql$stat$n)))
    
  }
  
  .ci <- \(lim, sep, label, data) {
  
    lim <- if (lim == "[") c("[", "]") else c("(", ")")
    
    lst(label = glue("{lim[1]}{label}{lim[2]}"),
        data = glue("{lim[1]}{{{data[1]}}}{sep}{{{data[2]}}}{lim[2]}"))
    
  }

  opts <- .opts_set
  
  opts <-
  opts |>
    list_modify(vars =
                  .vars(!!!with(opts, list(parametric, qt_stat, ql_stat))),
                qt_stat_wide =
                  opts$qt_stat |> 
                    list_modify(median =
                                  list2("{str_remove(names(opts$qt_stat$median), '\\\\s.+')}" :=
                                          str_remove(opts$qt_stat$median, "\\s.+")) |> 
                                    unlist()) |>
                    list_c(),
                ci = 
                  .ci(!!!opts$ci),
                font = 
                  list(alpha = .fonts(opts$font[[1]]),
                       digit = .fonts(opts$font[[2]])),
                !!!dots) |> 
    inject()

  if (identical(opts$font[[1]], opts$font[[2]])) opts$font <- opts$font[[1]]
  
  if (.assign) {
    
    assign(.name, opts, envir = .GlobalEnv)
    
  } else {
    
    return(get(.name))
    
  }

}

#' Vérifier et évaluer des options dans un environnement configuré
#'
#' Cette fonction utilitaire vérifie l'existence d'un objet de configuration
#' nommé `opts` dans l'environnement courant et évalue une expression donnée
#' dans le contexte de cet objet. Elle constitue un mécanisme de gestion
#' centralisée des paramètres et options de configuration pour les applications
#' et packages R nécessitant une personnalisation dynamique des comportements.
#'
#' @param x Expression R à évaluer dans le contexte de l'objet `opts`.
#'   L'expression est capturée sans évaluation préalable grâce au mécanisme
#'   de Non-Standard Evaluation.
#'
#' @returns La valeur résultant de l'évaluation de l'expression `x` dans
#'   le contexte de l'objet `opts`. Le type de retour dépend de la nature
#'   de l'expression évaluée et des données contenues dans `opts`.
#'
#' @section Mécanisme technique :
#' La fonction utilise `rlang::enexpr()` pour capturer l'expression fournie
#' sans l'évaluer immédiatement. Cette expression est ensuite évaluée dans
#' l'environnement de l'objet `opts` via `base::with()`. Cette approche
#' permet un accès direct aux éléments de `opts` sans qualification
#' explicite du namespace.
#'
#' @section Gestion des erreurs :
#' Si l'objet `opts` n'existe pas dans l'environnement courant, la fonction
#' génère une erreur claire via `cli::cli_abort()`. Cette approche garantit
#' un diagnostic précis des problèmes de configuration et facilite le
#' débogage dans les environnements de développement complexes.
#'
#' @section Prérequis techniques :
#' Cette fonction nécessite les packages `rlang` pour la capture d'expressions
#' et `cli` pour la gestion des messages d'erreur. L'objet `opts` doit être
#' une structure de données compatible avec `base::with()`, typiquement
#' une liste nommée ou un environnement.
#'
#' @examples
#' # Initialisation de opts dans l'environnement global
#' set_opts()
#' 
#' # Accès direct aux paramètres via `check_opts()`
#' check_opts(color)
#'
#' # Suppression de opts pour déclencher l'erreur
#' rm(opts, envir = .GlobalEnv)
#' 
#' check_opts(color)  # Génère une erreur
#'   
#' @family fonctions utilitaires
#' 
#' @export
check_opts <- \(x, .name = "opts") {
  
  if (exists(.name)) { 
    
    with(get(.name), eval(enexpr(x)))
    
  }
  
  else {
    
    cli_abort(
      c("L'objet {.strong { .name }} n'existe pas dans l'environnement global",
        i = "Créer {.strong { .name }} avec {.fun set_opts}")
    )
    
  }

}
