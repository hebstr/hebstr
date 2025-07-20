#' Construire une expression d'union pour expressions régulières
#'
#' Cette fonction utilitaire génère une chaîne de caractères représentant
#' une expression d'union (OR) pour les expressions régulières, en combinant
#' plusieurs motifs avec le séparateur pipe (|). Elle constitue un raccourci
#' syntaxique pour la construction efficace de patterns regex d'alternatives
#' multiples.
#'
#' @param ... Chaînes de caractères représentant les différents motifs
#'   à combiner dans l'expression d'union. Chaque argument devient un
#'   élément de l'alternative dans le pattern résultant.
#'
#' @return Une chaîne de caractères unique contenant tous les motifs
#'   séparés par le caractère pipe (|), prête à être utilisée comme
#'   expression régulière d'union.
#'
#' @section Prérequis techniques :
#' Cette fonction nécessite que le package `stringr` soit chargé dans
#' l'environnement ou que la fonction `str_c()` soit explicitement
#' importée. Utilisez `library(stringr)` ou `stringr::str_c()` selon
#' votre configuration de projet.
#'
#' @section Applications professionnelles :
#' Cette fonction s'avère particulièrement utile dans les contextes
#' d'analyse de données textuelles, de validation de formats multiples,
#' et de filtrage de grandes bases de données selon des critères
#' d'inclusion variés. Elle simplifie significativement la construction
#' de patterns regex complexes en production.
#'
#' @examples
#' # Chargement préalable requis
#' library(stringr)
#' 
#' # Construction d'un pattern pour départements français
#' depts_idf <- str_u("75", "77", "78", "91", "92", "93", "94", "95")
#' depts_idf
#' 
#' # Application pour filtrer des codes postaux
#' codes_postaux <- c("75001", "69001", "77300", "78000", "13001")
#' str_detect(codes_postaux, paste0("^(", depts_idf, ")"))
#' 
#' # Pattern pour extensions de fichiers bureautiques
#' extensions_office <- str_u("xlsx", "docx", "pptx", "pdf")
#' fichiers_projet <- c("rapport.docx", "donnees.xlsx", "image.png", "presentation.pptx")
#' str_detect(fichiers_projet, paste0("\\.(", extensions_office, ")$"))
#' 
#' # Validation de statuts administratifs
#' statuts_valides <- str_u("actif", "suspendu", "en_attente", "archivé")
#' base_utilisateurs <- data.frame(
#'   identifiant = 1:4,
#'   statut = c("actif", "invalide", "en_attente", "archivé")
#' )
#' utilisateurs_conformes <- base_utilisateurs[
#'   str_detect(base_utilisateurs$statut, paste0("^(", statuts_valides, ")$")), 
#' ]
#' 
#' # Recherche de terminologie économique
#' indicateurs_macro <- str_u("PIB", "inflation", "chômage", "croissance", "déficit")
#' titres_etudes <- c(
#'   "Analyse du PIB français 2023",
#'   "Étude démographique régionale",
#'   "Impact de l'inflation sur les ménages",
#'   "Recherche en sciences sociales"
#' )
#' etudes_economiques <- titres_etudes[
#'   str_detect(titres_etudes, regex(indicateurs_macro, ignore_case = TRUE))
#' ]
#' 
#' # Construction de patterns pour validation de données
#' formats_date <- str_u("\\d{2}/\\d{2}/\\d{4}", "\\d{4}-\\d{2}-\\d{2}", "\\d{2}-\\d{2}-\\d{4}")
#' dates_saisies <- c("01/01/2023", "2023-12-31", "31-12-2023", "invalide")
#' dates_valides <- dates_saisies[str_detect(dates_saisies, paste0("^(", formats_date, ")$"))]
#'
#' @family fonctions de formatage
#' @seealso 
#' * [stringr::str_c()] pour la concaténation de chaînes de caractères
#'
#' @references
#' Documentation stringr pour les expressions régulières :
#' \url{https://stringr.tidyverse.org/articles/regular-expressions.html}
#'
#' @export
str_u <- \(...) {
  
  str_c(c(...), collapse = "|")
  
}

#' Formater du texte avec couleurs et styles HTML intégrés
#'
#' Cette fonction génère du code HTML formaté permettant d'appliquer simultanément
#' des couleurs de texte et d'arrière-plan à du contenu textuel. Elle combine
#' automatiquement la mise en forme colorée avec l'emphase typographique en gras,
#' optimisée pour l'intégration dans des documents Markdown, des rapports HTML
#' et des interfaces web interactives.
#'
#' @param text Chaîne de caractères contenant le texte à formater.
#'   Supporte l'interpolation avec la syntaxe `glue` pour l'insertion
#'   dynamique de variables.
#' @param color Couleur du texte exprimée sous forme de nom CSS standard,
#'   code hexadécimal ou valeur RGB/RGBA. Par défaut `"red"`.
#' @param bg Couleur d'arrière-plan exprimée sous forme de code hexadécimal,
#'   nom CSS ou valeur RGB/RGBA. Par défaut `"#ffffff00"` (transparent
#'   avec canal alpha).
#'
#' @return Une chaîne de caractères contenant le code HTML formaté avec
#'   les styles CSS appliqués via attribut `style` et la syntaxe Markdown
#'   pour l'emphase en gras.
#'
#' @section Formats de couleurs supportés :
#' La fonction accepte tous les formats de spécification des couleurs
#' reconnus par les standards CSS3. Les noms CSS standard (red, blue, green,
#' navy, darkgreen), les codes hexadécimaux (#FF0000, #0000FF, #00FF00),
#' les valeurs RGB (rgb(255,0,0)) et RGBA (rgba(255,0,0,0.8)) sont
#' entièrement supportés. Le canal alpha dans les codes hexadécimaux
#' à 8 caractères permet de contrôler la transparence.
#'
#' @section Intégration dans les documents :
#' Le code HTML généré s'intègre parfaitement dans les documents R Markdown,
#' Quarto, les applications Shiny et les rapports HTML dynamiques. La syntaxe
#' Markdown combinée (`**texte**`) garantit un rendu en gras compatible
#' avec la plupart des moteurs de rendu Markdown modernes.
#'
#' @section Recommandations d'accessibilité :
#' Pour garantir l'accessibilité des documents produits, respectez les
#' directives WCAG concernant les contrastes de couleurs. Utilisez des
#' combinaisons de couleurs offrant un contraste suffisant et évitez
#' de transmettre des informations uniquement par la couleur.
#'
#' @examples
#' # Formatage basique avec couleur rouge par défaut
#' str_color("Attention importante")
#' 
#' # Utilisation avec des couleurs CSS nommées
#' str_color("Information validée", color = "green")
#' str_color("Statut en attente", color = "orange")
#' 
#' # Application de couleurs avec codes hexadécimaux
#' str_color("Données critiques", color = "#DC143C", bg = "#FFF8DC")
#' str_color("Résultats finalisés", color = "#006400", bg = "#F0FFF0")
#' 
#' # Utilisation de couleurs RGBA avec transparence
#' str_color("Alerte système", 
#'           color = "white", 
#'           bg = "rgba(220, 20, 60, 0.8)")
#' 
#' # Interpolation dynamique avec glue
#' niveau_alerte <- "élevé"
#' message_alerte <- str_color("Niveau de risque : {niveau_alerte}", 
#'                            color = "darkred")
#' 
#' # Application dans un contexte de tableau de bord
#' taux_reussite <- 95.2
#' couleur_performance <- if (taux_reussite >= 90) "green" else "red"
#' indicateur <- str_color("Taux : {round(taux_reussite, 1)}%", 
#'                        color = couleur_performance)
#' 
#' # Mise en évidence de valeurs dans un rapport
#' budget_depassement <- 15000
#' alerte_budget <- str_color(
#'   "Dépassement budgétaire : {format(budget_depassement, big.mark = ' ')} €",
#'   color = "#B22222",
#'   bg = "#FFE4E1"
#' )
#' 
#' # Utilisation pour catégoriser des résultats
#' resultats_analyses <- c("Conforme", "Non-conforme", "À vérifier")
#' couleurs_statuts <- c("green", "red", "orange")
#' statuts_formates <- map2_chr(resultats_analyses, couleurs_statuts,
#'                             ~ str_color(.x, color = .y))
#' 
#' # Application dans la communication de données scientifiques
#' significativite <- 0.03
#' seuil_alpha <- 0.05
#' resultat_test <- str_color(
#'   "p-value = {significativite} (significatif)",
#'   color = if (significativite < seuil_alpha) "green" else "red"
#' )
#'
#' @family fonctions de formatage
#'
#' @export
str_color <- \(text,
               color = "red",
               bg = "#ffffff00") {

  color <- glue("color:{color};")
  bg <- glue("background-color:{bg};")

  glue("<span style='{color}{bg}'>**{text}**</span>")

}

#' Générer un titre de figure formaté avec note et acronymes
#'
#' Cette fonction crée un titre de figure HTML formaté avec une note explicative
#' et des acronymes en sous-titre. Elle adapte automatiquement le formatage selon
#' le contexte de sortie (document standard ou Quarto Markdown).
#'
#' @param title Chaîne de caractères contenant le titre principal de la figure.
#'   Supporte l'interpolation avec la syntaxe `glue`.
#' @param note Chaîne de caractères contenant la note explicative à afficher
#'   sous le titre. Par défaut une chaîne vide. Supporte l'interpolation `glue`.
#' @param acro Chaîne de caractères contenant les acronymes ou abréviations
#'   à ajouter après la note. Par défaut une chaîne vide.
#' @param sub_size Taille de police pour la note et les acronymes, exprimée
#'   en points. Par défaut `"7.5"`.
#' @param qmd Valeur logique indiquant si la sortie est destinée à un document
#'   Quarto Markdown. Par défaut `FALSE`.
#' @param class Classe CSS à appliquer à la note dans le contexte Quarto.
#'   Par défaut `"quarto-float-subcaption"`.
#'
#' @return Une chaîne de caractères contenant le HTML formaté du titre
#'   de figure avec sa note et ses acronymes.
#'
#' @section Formatage conditionnel :
#' La fonction adapte son comportement selon la valeur du paramètre `qmd`.
#' Pour les documents standards (`qmd = FALSE`), elle utilise des styles
#' CSS inline avec la taille de police spécifiée. Pour les documents
#' Quarto (`qmd = TRUE`), elle applique une classe CSS pour s'intégrer
#' au système de styles de Quarto.
#'
#' @section Interpolation de variables :
#' Les paramètres `title` et `note` supportent l'interpolation de variables
#' via le package `glue`. Cette fonctionnalité permet d'insérer dynamiquement
#' des valeurs dans les chaînes de caractères.
#'
#' @examples
#' # Titre simple sans note
#' str_fig("Évolution du PIB français (2010-2020)")
#' 
#' # Titre avec note explicative
#' str_fig(
#'   title = "Distribution des revenus par région",
#'   note = "Source : INSEE, enquête revenus fiscaux 2021.",
#'   acro = "PIB : Produit Intérieur Brut"
#' )
#' 
#' # Adaptation pour Quarto Markdown
#' str_fig(
#'   title = "Analyse de régression linéaire",
#'   note = "Données ajustées pour la saisonnalité.",
#'   qmd = TRUE
#' )
#' 
#' # Utilisation avec interpolation de variables
#' annee_ref <- 2021
#' region <- "Île-de-France"
#' str_fig(
#'   title = "Données démographiques {region}",
#'   note = "Période de référence : {annee_ref}",
#'   acro = "INSEE : Institut National de la Statistique"
#' )
#' 
#' # Personnalisation de la taille de police
#' str_fig(
#'   title = "Graphique pour présentation",
#'   note = "Note lisible en grand format",
#'   sub_size = "10"
#' )
#' 
#' # Application dans un contexte de rapport automatisé
#' titre_figure <- str_fig(
#'   title = "Tendances macroéconomiques",
#'   note = "Données trimestrielles corrigées des variations saisonnières.",
#'   acro = "CVS : Corrigé des Variations Saisonnières"
#' )
#'
#' @family fonctions de formatage
#'
#' @references
#' Documentation Quarto sur les figures et légendes :
#' \url{https://quarto.org/docs/authoring/figures.html}
#'
#' @export
str_fig <- \(title,
             note = "",
             acro = "",
             sub_size = "7.5",
             qmd = FALSE,
             class = "quarto-float-subcaption") {
  
  title <- glue(title)
  note <- glue(note)

  if (!qmd) {
    
    .str <-
    glue("{title}<br>
         <span style='font-size:{sub_size}pt'>{note} {acro}</span>")
    
  } else {
    
    .str <-
    glue("{title}<br>
         <span class='{class}'>{note} {acro}</span>")
    
  }

  return(.str)
  
}

#' Concaténer les étiquettes de variables avec formatage grammatical
#'
#' Cette fonction extrait les étiquettes de variables spécifiées depuis un jeu de
#' données et les concatène en une chaîne de caractères avec une ponctuation
#' grammaticalement correcte (virgules et conjonction finale).
#'
#' @param data Un data.frame ou objet similaire contenant les variables
#'   à traiter.
#' @param ... <data-masking> Noms des variables dont il faut extraire
#'   les étiquettes. Accepte la sélection de variables nue (sans guillemets).
#' @param last Chaîne de caractères spécifiant la conjonction finale
#'   à utiliser avant le dernier élément. Par défaut `"and"`.
#'
#' @return Une chaîne de caractères unique contenant les étiquettes
#'   des variables concaténées avec des virgules et la conjonction finale.
#'
#' @section Dépendances :
#' Cette fonction nécessite les packages `purrr`, `stringr` et `glue`.
#' Elle utilise également `var_label()` qui doit être disponible
#' (typiquement via le package `labelled`).
#'
#' @examples
#' # Créer un jeu de données avec étiquettes
#' library(labelled)
#' 
#' donnees <- data.frame(
#'   age = c(25, 30, 35),
#'   revenu = c(30000, 45000, 60000),
#'   education = c("BAC", "Master", "Doctorat")
#' )
#' 
#' # Ajouter des étiquettes aux variables
#' var_label(donnees$age) <- "Âge du répondant"
#' var_label(donnees$revenu) <- "Revenu annuel en euros"
#' var_label(donnees$education) <- "Niveau d'éducation"
#' 
#' # Utilisation basique avec conjonction anglaise
#' str_label(donnees, age, revenu)
#' 
#' # Utilisation avec conjonction française
#' str_label(donnees, age, revenu, education, last = "et")
#' 
#' # Utilisation avec une seule variable
#' str_label(donnees, age)
#' 
#' # Application dans un contexte de rapport
#' variables_demographie <- str_label(donnees, age, education, last = "et")
#' message("Variables démographiques analysées : ", variables_demographie)
#'
#' @family fonctions d'étiquetage
#' @seealso 
#' * [labelled::var_label()] pour l'attribution d'étiquettes aux variables
#' * [stringr::str_flatten_comma()] pour la concaténation avec virgules
#' * [purrr::map_chr()] pour l'application de fonctions sur des vecteurs
#'
#' @export
str_label <- \(data, ..., last = "and") {
  
  c(...) |>
    map_chr(~ with(data, get(.)) |>
              var_label()) |>
    str_flatten_comma(glue(" {last} "))

}

#' Appliquer une fonction de transformation au premier caractère d'une chaîne
#'
#' Cette fonction utilitaire permet d'appliquer une transformation spécifique
#' au premier caractère d'une chaîne de caractères tout en préservant le reste
#' de la chaîne inchangé. Elle constitue un outil flexible pour la manipulation
#' typographique et le formatage de texte.
#'
#' @param fun Fonction à appliquer au premier caractère. Doit être une fonction
#'   qui accepte une chaîne de caractères et retourne une chaîne de caractères
#'   transformée (par exemple `toupper`, `tolower`).
#' @param str Chaîne de caractères à traiter. Le premier caractère sera
#'   transformé selon la fonction spécifiée.
#'
#' @return Une chaîne de caractères identique à l'entrée, avec le premier
#'   caractère transformé selon la fonction appliquée.
#'
#' @section Mécanisme technique :
#' La fonction utilise `stringr::str_sub()` pour extraire le premier caractère,
#' `do.call()` pour appliquer dynamiquement la fonction de transformation,
#' puis `stringr::str_replace()` pour substituer le caractère transformé
#' dans la chaîne originale.
#'
#' @section Fonctions compatibles :
#' Cette fonction fonctionne avec toute fonction qui accepte une chaîne
#' de caractères en entrée et retourne une chaîne transformée. Les fonctions
#' couramment utilisées incluent `base::toupper()`, `base::tolower()`,
#' ou des fonctions personnalisées de transformation de caractères.
#'
#' @examples
#' # Conversion du premier caractère en majuscule
#' str_cap(toupper, "france")
#' 
#' # Conversion du premier caractère en minuscule
#' str_cap(tolower, "PARIS")
#' 
#' # Application sur des phrases complètes
#' str_cap(toupper, "analyse des données économiques")
#' str_cap(tolower, "RAPPORT TRIMESTRIEL")
#' 
#' # Utilisation avec des chaînes contenant des caractères spéciaux
#' str_cap(toupper, "émission de CO₂")
#' str_cap(tolower, "ÉLÉVATION du niveau de la mer")
#' 
#' # Application dans un contexte de formatage de titres
#' titres_bruts <- c("méthodologie statistique", "RÉSULTATS PRINCIPAUX")
#' titres_formates <- sapply(titres_bruts, \(x) str_cap(toupper, x))
#' 
#' # Utilisation avec une fonction personnalisée
#' inverser_casse <- function(char) {
#'   if (char == toupper(char)) tolower(char) else toupper(char)
#' }
#' str_cap(inverser_casse, "Données")
#' str_cap(inverser_casse, "données")
#' 
#' # Application pour standardiser des noms propres
#' noms_regions <- c("île-de-france", "NORMANDIE", "provence-alpes-côte d'azur")
#' noms_standardises <- sapply(noms_regions, \(x) str_cap(toupper, x))
#'
#' @family fonctions de formatage
#' @seealso 
#' * [stringr::str_to_title()] pour la conversion en casse de titre
#'
#' @export
str_cap <- \(fun, str) {

  cap <- str_sub(str, end = 1)
  call <- do.call(fun, list(cap))

  str_replace(str, cap, call)

}
