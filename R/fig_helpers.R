#' Créer un graphique en barres avec comptage d'effectifs et pourcentages
#'
#' Cette fonction génère un graphique en barres sophistiqué affichant les effectifs
#' et pourcentages pour une variable catégorielle. Elle combine automatiquement
#' les barres de comptage avec des étiquettes numériques et des annotations
#' de pourcentages, optimisée pour la production de visualisations professionnelles
#' dans un contexte d'analyse statistique descriptive.
#'
#' @param data Un data.frame contenant les données à visualiser.
#' @param var Nom de la variable catégorielle à analyser (en tant que chaîne
#'   de caractères). Cette variable servira de base pour le comptage des effectifs.
#' @param color Code couleur hexadécimal ou nom CSS pour le remplissage des barres.
#'   Par défaut `"#333333"` (gris foncé).
#' @param alpha Valeur de transparence des barres, comprise entre 0 (transparent)
#'   et 1 (opaque). Par défaut `0.7`.
#' @param nudge_y Facteur de décalage vertical pour le positionnement des étiquettes,
#'   exprimé comme proportion de la hauteur maximale. Par défaut `0.04`.
#' @param pct_min Seuil minimum d'effectif pour l'affichage des pourcentages,
#'   exprimé comme proportion de l'effectif total. Par défaut `0.05`.
#' @param ylab Étiquette de l'axe des ordonnées. Par défaut `"Effectif"`.
#' @param family Famille de police à utiliser pour les étiquettes de texte.
#'   Par défaut déterminée par `check_fonts(.auto = "luciole")`.
#' @param grid Valeur logique contrôlant l'affichage de la grille de fond.
#'   Par défaut `TRUE`.
#' @param ... Arguments supplémentaires transmis aux couches de texte
#'   (`geom_text`), permettant la personnalisation avancée des étiquettes.
#'
#' @return Un objet ggplot représentant le graphique en barres avec effectifs
#'   et pourcentages, prêt pour l'affichage ou l'export.
#'
#' @section Architecture de la visualisation :
#' Le graphique produit intègre trois couches principales de données. La couche
#' de base affiche les barres d'effectifs avec la couleur et transparence
#' spécifiées. La première couche de texte présente les valeurs numériques
#' d'effectifs positionnées au-dessus des barres. La seconde couche de texte
#' affiche les pourcentages correspondants sous les barres, mais uniquement
#' pour les modalités dépassant le seuil `pct_min`.
#'
#' @section Fonctions auxiliaires requises :
#' Cette fonction s'appuie sur plusieurs utilitaires spécialisés qui doivent
#' être disponibles dans l'environnement. `check_fonts()` gère la détection
#' automatique des polices système. `pct_min()` filtre les modalités selon
#' le seuil de pourcentage minimum. `label_p()` formate les pourcentages
#' selon les conventions typographiques françaises. `theme_bar()` applique
#' le thème graphique standardisé pour les graphiques en barres.
#'
#' @section Gestion automatique des étiquettes :
#' La fonction utilise `var_label()` pour extraire automatiquement l'étiquette
#' descriptive de la variable analysée. Si aucune étiquette n'est définie,
#' le nom de la variable est utilisé par défaut. Cette approche garantit
#' une cohérence dans la présentation des résultats et facilite l'interprétation
#' des graphiques dans un contexte de reporting automatisé.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Utilisation avec le dataset mpg (ggplot2)
#' ggcount(data = mpg,
#'         var = "class")
#'         
#' # Utilisation dans un pipeline d'analyse avec filtrage
#' mpg |>
#'   filter(year == 2008) |>
#'   ggcount(var = "class", 
#'           color = "#0099FF")
#'           
#' # Personnalisation avancée des étiquettes
#' mpg |> 
#'   ggcount(var = "fl",
#'           color = "#FF0000",
#'           size = 3.5,
#'           fontface = "bold",
#'           grid = FALSE)
#'           
#' # Analyse de données avec distribution déséquilibrée
#' storms |>
#'   filter(year >= 2010) |>
#'   mutate(status = forcats::fct_infreq(status)) |>
#'   ggcount(var = "status",
#'           pct_min = 0.1)
#'
#' @family fonctions de visualisation
#' @seealso 
#' * [ggplot2::geom_bar()] pour les graphiques en barres de base
#' * [ggplot2::geom_text()] pour l'ajout d'étiquettes textuelles
#' * [labelled::var_label()] pour la gestion des étiquettes de variables
#' * [scales::label_percent()] pour le formatage des pourcentages
#'
#' @export
ggcount <- \(data,
             var,
             color = "#333333",
             alpha = 0.7,
             nudge_y = 0.04,
             pct_min = 0.05,
             ylab = "Effectif",
             family = check_fonts(.auto = "luciole"),
             grid = TRUE,
             ...) {

  data |>
    ggplot(aes(x = get(var),
               fill = I(color))) +
    geom_bar(alpha = alpha) +
    geom_text(mapping =
                aes(y = after_stat(count + nudge_y * max(count)),
                    label = after_stat(count),
                    color = after_scale(fill)),
              stat = "count",
              family = family,
              ...) +
    geom_text(data = ~ pct_min(., var, pct_min),
              mapping = 
                aes(y = after_stat(count - nudge_y * max(count)),
                    label = label_p()(after_stat(count) / nrow(data)),
                    color = after_scale(fill |> lighten(0.95))),
              stat = "count",
              family = family,
              ...) +
    labs(x = var_label(data[[var]]) %||% var,
         y = ylab) +
    theme_bar(family = family,
              grid = grid)

}
