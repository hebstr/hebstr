#' Générer un dictionnaire d'acronymes avec définitions localisées
#'
#' Cette fonction crée un dictionnaire d'acronymes statistiques et leurs
#' définitions complètes, avec adaptation automatique selon la configuration
#' linguistique de l'environnement R. Elle combine des acronymes prédéfinis
#' avec des définitions personnalisées, facilitant la production de glossaires
#' cohérents pour la documentation technique et les rapports d'analyse.
#'
#' @param ... Expressions définissant des acronymes personnalisés sous la
#'   forme `acronyme ~ "définition"`. Utilise la syntaxe de Non-Standard
#'   Evaluation pour une interface intuitive.
#' @param .sep Chaîne de caractères définissant le séparateur entre
#'   l'acronyme et sa définition. Par défaut `":"`.
#' @param .auto Valeur logique contrôlant l'inclusion des acronymes
#'   prédéfinis. Si `TRUE` (par défaut), combine les définitions
#'   personnalisées avec la base d'acronymes standards.
#' @param .tolower Valeur logique contrôlant la casse des acronymes.
#'   Si `TRUE`, convertit les acronymes en minuscules. Par défaut `FALSE`.
#'
#' @returns Une liste nommée où les noms correspondent aux acronymes
#'   et les valeurs aux définitions formatées. Le format inclut
#'   automatiquement le séparateur spécifié entre l'acronyme et
#'   sa définition complète.
#'
#' @section Architecture technique de la fonction :
#' La fonction utilise un environnement personnalisé pour définir l'opérateur
#' `~` qui gère le formatage des paires acronyme-définition. Cette approche
#' exploite les capacités de métaprogrammation de R via `rlang::enexpr()`
#' pour capturer les expressions sans évaluation préalable. Le mécanisme
#' permet une syntaxe intuitive tout en maintenant la flexibilité nécessaire
#' pour diverses configurations de formatage.
#'
#' @section Adaptation linguistique automatique :
#' La fonction détecte automatiquement la configuration linguistique via
#' `getOption("OutDec")` et adapte les acronymes prédéfinis en conséquence.
#' Pour une configuration anglaise (point décimal), les termes statistiques
#' standards sont fournis en anglais avec formatage compact. Pour une
#' configuration française (virgule décimale), les équivalents français
#' sont utilisés avec adaptation des conventions typographiques locales,
#' notamment l'espacement autour des séparateurs.
#'
#' @section Acronymes prédéfinis par localisation :
#' La base d'acronymes intégrée couvre les termes statistiques descriptifs
#' essentiels. En configuration anglaise, elle inclut SD (standard deviation),
#' IQR (interquartile range), Q1 et Q3 (quartiles), et 95%CI (confidence
#' interval). En configuration française, les équivalents comprennent
#' l'écart-type, l'intervalle interquartile, les quartiles avec ordinaux
#' français, et l'intervalle de confiance à 95%. Cette standardisation
#' facilite la cohérence terminologique dans les productions analytiques.
#'
#' @section Prérequis techniques et dépendances :
#' Cette fonction nécessite les packages `rlang` pour la métaprogrammation,
#' `glue` pour l'interpolation de chaînes, `purrr` pour les opérations
#' sur listes, et `stringr` pour la manipulation de texte. L'environnement
#' R doit également supporter les fonctions avancées de Non-Standard
#' Evaluation pour un fonctionnement optimal des mécanismes de capture
#' d'expressions.
#'
#' @examples
#' # Génération du dictionnaire par défaut (configuration anglaise)
#' options(OutDec = ".")
#' dictionnaire_base <- acro()
#' dictionnaire_base$SD
#' dictionnaire_base$IQR
#' 
#' # Ajout d'acronymes personnalisés
#' glossaire_etendu <- acro(
#'   BMI ~ "body mass index",
#'   SBP ~ "systolic blood pressure",
#'   DBP ~ "diastolic blood pressure"
#' )
#' glossaire_etendu$BMI
#' glossaire_etendu$SD
#' 
#' # Configuration française avec adaptation linguistique
#' options(OutDec = ",")
#' dictionnaire_fr <- acro()
#' dictionnaire_fr$SD
#' dictionnaire_fr$IQR
#' 
#' # Acronymes personnalisés en français
#' glossaire_medical_fr <- acro(
#'   IMC ~ "indice de masse corporelle",
#'   PAS ~ "pression artérielle systolique",
#'   PAD ~ "pression artérielle diastolique"
#' )
#' glossaire_medical_fr$IMC
#' 
#' # Personnalisation du séparateur
#' dictionnaire_tiret <- acro(
#'   HR ~ "heart rate",
#'   RR ~ "respiratory rate",
#'   .sep = " -"
#' )
#' dictionnaire_tiret$HR
#' 
#' # Utilisation sans acronymes prédéfinis
#' acronymes_specifiques <- acro(
#'   ROC ~ "receiver operating characteristic",
#'   AUC ~ "area under the curve",
#'   .auto = FALSE
#' )
#' names(acronymes_specifiques)
#' 
#' # Application avec conversion en minuscules
#' acronymes_minuscules <- acro(
#'   COPD ~ "chronic obstructive pulmonary disease",
#'   ARDS ~ "acute respiratory distress syndrome",
#'   .tolower = TRUE
#' )
#' names(acronymes_minuscules)
#' 
#' # Démonstration avec datasets officiels pour contexte médical
#' # Utilisation dans l'analyse de mtcars comme proxy de données biométriques
#' options(OutDec = ".")
#' glossaire_automobile <- acro(
#'   MPG ~ "miles per gallon",
#'   HP ~ "horsepower",
#'   WT ~ "weight in thousands of pounds"
#' )
#' 
#' # Application pour légendes de graphiques
#' paste(glossaire_automobile$MPG)
#' paste(glossaire_automobile$HP)
#' 
#' # Configuration pour rapport multilingue
#' options(OutDec = ",")
#' legende_fr <- acro(
#'   CONSO ~ "consommation moyenne",
#'   PUIS ~ "puissance nominale",
#'   .sep = " :"
#' )
#' legende_fr$CONSO
#' 
#' # Restauration configuration par défaut
#' options(OutDec = ".")
#' 
#' # Intégration dans un pipeline de production de rapports
#' glossaire_analyse <- acro(
#'   CYL ~ "number of cylinders",
#'   DISP ~ "displacement in cubic inches",
#'   GEAR ~ "number of forward gears"
#' )
#' 
#' @family fonctions de formatage
#'
#' @references
#' Documentation rlang sur la métaprogrammation tidyeval :
#' \url{https://rlang.r-lib.org/reference/topic-metaprogramming.html}
#'
#' @export
acro <- \(...,
          .sep = ":",
          .auto = TRUE,
          .tolower = FALSE) {
  
  .envir <- \(x, y) {
    
    x <- if (.tolower) tolower(enexpr(x)) else enexpr(x)
    
    glue("{x}{sep}{y}")
  
  }
  
  .fun <- \(...) {
  
    list(...) |> 
      map(eval, env("~" = .envir)) |> 
      set_names(str_extract, glue(".+(?={sep})"))
  
  }

  if (getOption("OutDec") == ".") {
  
    sep <- glue("{.sep} ")
      
    base <-
    .fun(SD ~ "standard deviation",
         IQR ~ "interquartile range",
         Q1 ~ "1st quartile",
         Q3 ~ "3rd quartile",
         `95%CI` ~ "95% confidence interval")
  
  } else {
    
    sep <- glue(" {.sep} ")
    
    base <-
    .fun(SD ~ "\u00e9cart-type",
         IQR ~ "intervalle interquartile",
         Q1 ~ "1er quartile",
         Q3 ~ "3e quartile",
         `IC95%` ~ "intervalle de confiance \u00e0 95%")
    
  }
  
  .acro <- if (.auto) list_modify(base, !!!.fun(...)) else .fun(...)
  
  return(.acro)

}

#' Extraire les acronymes présents dans un texte selon un dictionnaire de référence
#'
#' Cette fonction identifie et extrait tous les acronymes contenus dans une
#' chaîne de caractères en utilisant un dictionnaire de référence prédéfini.
#' Elle applique une recherche exhaustive pour détecter toutes les occurrences
#' des acronymes spécifiés, retournant une liste dédupliquée des termes
#' identifiés. Cette fonction facilite l'analyse terminologique automatisée
#' et la génération de glossaires contextuels pour la documentation technique.
#'
#' @param x Chaîne de caractères dans laquelle rechercher les acronymes.
#'   Accepte du texte libre, des titres, des descriptions ou tout contenu
#'   textuel nécessitant une analyse terminologique systematique.
#' @param acro_list Liste nommée d'acronymes servant de dictionnaire de
#'   référence. Les noms de cette liste constituent les termes à rechercher
#'   dans le texte d'entrée.
#'
#' @returns Vecteur de caractères contenant les acronymes uniques identifiés
#'   dans le texte d'entrée. Les résultats sont automatiquement dédupliqués
#'   pour éliminer les occurrences multiples d'un même terme.
#'
#' @section Mécanisme d'extraction :
#' La fonction utilise une approche de recherche exhaustive qui examine
#' le texte d'entrée pour identifier toutes les occurrences des acronymes
#' présents dans le dictionnaire de référence. Le processus d'extraction
#' applique automatiquement la déduplication des résultats pour garantir
#' qu'un acronyme n'apparaisse qu'une seule fois dans la liste retournée,
#' indépendamment du nombre de ses occurrences dans le texte source.
#'
#' @section Applications dans l'environnement professionnel :
#' Cette fonction répond aux besoins d'analyse documentaire dans les
#' contextes organisationnels où la cohérence terminologique constitue
#' un enjeu stratégique. Elle facilite l'identification automatique des
#' acronymes utilisés dans les rapports d'analyse, les présentations
#' techniques et la documentation projet. Cette automatisation améliore
#' l'efficacité des processus de révision documentaire et garantit
#' la conformité aux standards terminologiques établis.
#'
#' @section Considérations techniques importantes :
#' Cette implémentation utilise une approche de correspondance simple
#' qui peut détecter des acronymes même lorsqu'ils constituent une
#' partie d'un terme plus large. Les utilisateurs doivent considérer
#' cette caractéristique lors de la construction de leurs dictionnaires
#' d'acronymes pour éviter les correspondances non intentionnelles.
#' La fonction nécessite le package stringr pour les opérations de
#' manipulation de chaînes de caractères.
#'
#' @examples
#' # Création d'un dictionnaire d'acronymes statistiques
#' dictionnaire_analyse <- list(
#'   "SD" = "standard deviation",
#'   "IQR" = "interquartile range", 
#'   "CI" = "confidence interval",
#'   "OR" = "odds ratio"
#' )
#' 
#' # Extraction d'acronymes dans un titre de rapport
#' titre_rapport <- "Statistical analysis with SD and CI calculations"
#' acro_extract(titre_rapport, dictionnaire_analyse)
#' 
#' # Analyse d'une description de méthodologie
#' methodologie <- "Results include OR estimates with 95% CI bounds"
#' acro_extract(methodologie, dictionnaire_analyse)
#' 
#' # Application avec le dataset mtcars pour contexte automobile
#' dictionnaire_vehicules <- list(
#'   "MPG" = "miles per gallon",
#'   "HP" = "horsepower", 
#'   "CYL" = "cylinders",
#'   "DISP" = "displacement"
#' )
#' 
#' description_vehicule <- "Vehicle performance: MPG efficiency and HP output analysis"
#' acro_extract(description_vehicule, dictionnaire_vehicules)
#' 
#' # Utilisation avec dataset diamonds pour analyse gemmologique
#' dictionnaire_gemmes <- list(
#'   "CT" = "carat weight",
#'   "CL" = "clarity",
#'   "COL" = "color grade"
#' )
#' 
#' rapport_diamants <- "Diamond evaluation considers CT and CL parameters"
#' acro_extract(rapport_diamants, dictionnaire_gemmes)
#' 
#' # Démonstration avec texte ne contenant aucun acronyme référencé
#' texte_general <- "This comprehensive analysis examines various factors"
#' acro_extract(texte_general, dictionnaire_analyse)
#' 
#' # Application pour validation de cohérence terminologique
#' resume_executif <- "Key findings show significant OR values with narrow CI ranges"
#' termes_detectes <- acro_extract(resume_executif, dictionnaire_analyse)
#' length(termes_detectes)
#' 
#' # Utilisation dans l'analyse de documentation technique
#' specification_technique <- "System requirements include RAM and CPU specifications"
#' dictionnaire_informatique <- list(
#'   "RAM" = "random access memory",
#'   "CPU" = "central processing unit",
#'   "GPU" = "graphics processing unit"
#' )
#' acro_extract(specification_technique, dictionnaire_informatique)
#' 
#' # Analyse de légendes de graphiques avec données storms
#' legende_meteo <- "Hurricane analysis: wind speed and pressure measurements"
#' dictionnaire_meteorologie <- list(
#'   "MPH" = "miles per hour",
#'   "MB" = "millibars",
#'   "KPH" = "kilometers per hour"
#' )
#' acro_extract(legende_meteo, dictionnaire_meteorologie)
#' 
#' # Application pour audit de présentation professionnelle
#' slide_presentation <- "Market analysis reveals ROI improvements and KPI achievements"
#' dictionnaire_business <- list(
#'   "ROI" = "return on investment",
#'   "KPI" = "key performance indicator",
#'   "ROE" = "return on equity"
#' )
#' acronymes_business <- acro_extract(slide_presentation, dictionnaire_business)
#' acronymes_business
#'
#' @family fonctions de manipulation de chaînes
#'
#' @export
acro_extract <- \(x, acro_list) {

  .acro <- names(acro_list)

  x |> 
    map(str_extract, .acro) |> 
    unlist() |>
    na.omit() |> 
    unique()

}

#' Formater une chaîne d'acronymes avec séparateurs et ponctuation automatiques
#'
#' Cette fonction utilitaire concatène des éléments textuels en une chaîne
#' de caractères unique en utilisant des séparateurs standardisés et applique
#' automatiquement un point final pour respecter les conventions typographiques
#' professionnelles. Elle gère les cas de chaînes vides en retournant NULL,
#' facilitant l'intégration dans des systèmes de formatage conditionnel pour
#' la production de documentation technique et de rapports d'analyse.
#'
#' @param ... Éléments à concaténer en une chaîne unique. Accepte des
#'   vecteurs de caractères, des chaînes individuelles, ou tout objet
#'   convertible en caractère via `paste()`.
#' @param collapse Chaîne de caractères utilisée pour séparer les éléments
#'   lors de la concaténation. Par défaut `"; "` (point-virgule suivi d'un espace),
#'   conforme aux conventions typographiques pour les énumérations techniques.
#'
#' @returns Une chaîne de caractères formatée avec séparateurs appropriés et
#'   point final si le contenu n'est pas vide, ou `NULL` si la concaténation
#'   produit une chaîne vide. Le formatage garantit la conformité aux standards
#'   typographiques pour les documents professionnels.
#'
#' @section Mécanisme de formatage standardisé :
#' La fonction utilise `base::paste()` pour effectuer la concaténation initiale
#' des éléments fournis avec le séparateur spécifié, puis applique `glue::glue()`
#' pour ajouter la ponctuation finale. Cette approche en deux étapes garantit
#' un contrôle précis sur le formatage tout en maintenant la flexibilité
#' nécessaire pour diverses applications de mise en forme textuelle. Le
#' traitement conditionnel des chaînes vides évite la génération de contenu
#' superflu dans les documents automatisés.
#'
#' @section Applications dans l'environnement professionnel :
#' Cette fonction s'intègre efficacement dans les systèmes de génération
#' automatique de légendes, de notes explicatives, et d'annotations techniques.
#' Elle facilite la production de glossaires formatés et la standardisation
#' typographique dans les environnements organisationnels où la cohérence
#' de présentation constitue un enjeu de qualité documentaire. L'utilisation
#' du point-virgule comme séparateur par défaut respecte les conventions
#' éditoriales françaises pour les énumérations complexes.
#'
#' @examples
#' # Formatage basique avec séparateurs par défaut
#' acro_str("SD: standard deviation", "CI: confidence interval")
#' 
#' # Utilisation avec des définitions d'acronymes statistiques
#' acro_str("IQR: interquartile range", "OR: odds ratio", "HR: hazard ratio")
#' 
#' # Application avec le dataset mtcars pour contexte automobile
#' definitions_vehicules <- c(
#'   "MPG: miles per gallon",
#'   "HP: horsepower", 
#'   "CYL: number of cylinders"
#' )
#' acro_str(definitions_vehicules[1], definitions_vehicules[2])
#' 
#' # Personnalisation du séparateur pour différents contextes
#' acro_str("Mean: average value", "SD: standard deviation", collapse = " | ")
#' 
#' # Utilisation avec le dataset diamonds pour analyses gemmologiques
#' termes_gemmes <- c(
#'   "CT: carat weight",
#'   "CL: clarity grade",
#'   "COL: color classification"
#' )
#' acro_str(termes_gemmes[1], termes_gemmes[3], collapse = " - ")
#' 
#' # Gestion des cas de chaînes vides
#' acro_str("")  # Retourne NULL
#' acro_str()    # Retourne NULL
#' 
#' # Application dans la génération de légendes de graphiques
#' legende_analyse <- acro_str(
#'   "n: sample size",
#'   "p: probability value",
#'   "CI: confidence interval"
#' )
#' legende_analyse
#' 
#' # Utilisation avec le dataset storms pour la météorologie
#' variables_meteo <- acro_str(
#'   "mph: miles per hour",
#'   "mb: millibars pressure",
#'   "cat: category scale"
#' )
#' 
#' # Intégration dans un workflow de documentation automatisée
#' glossaire_medical <- acro_str(
#'   "BMI: body mass index",
#'   "BP: blood pressure",
#'   "HR: heart rate"
#' )
#' 
#' # Application pour notes de bas de page techniques
#' note_methodologique <- acro_str(
#'   "ANOVA: analysis of variance",
#'   "post-hoc: posterior comparison tests"
#' )
#' 
#' # Utilisation conditionnelle dans des rapports
#' acronymes_detectes <- c("ROC: receiver operating characteristic")
#' if (length(acronymes_detectes) > 0) {
#'   note_finale <- acro_str(acronymes_detectes)
#'   note_finale
#' }
#' 
#' # Démonstration avec formatage personnalisé pour publications
#' definitions_statistiques <- acro_str(
#'   "α: significance level",
#'   "β: type II error probability",
#'   "R²: coefficient of determination",
#'   collapse = "; "
#' )
#' definitions_statistiques
#'
#' @family fonctions de formatage
#'
#' @export
acro_str <- \(..., collapse = "; ") {

  acro <- paste(c(...), collapse = collapse)
  
  if (acro != "") glue("{acro}.") else NULL

}

#' Identifier et formater automatiquement les acronymes présents dans les étiquettes de variables
#'
#' Cette fonction analyse automatiquement les étiquettes descriptives des variables
#' d'un jeu de données pour identifier les acronymes présents, puis génère une
#' chaîne formatée contenant les définitions de ces acronymes selon un dictionnaire
#' de référence. Elle constitue un outil essentiel pour la génération automatique
#' de glossaires contextuels dans la production de rapports d'analyse professionnels.
#'
#' @param x Un data.frame ou objet similaire contenant les variables à analyser.
#'   Les variables doivent posséder des attributs d'étiquetage pour un
#'   fonctionnement optimal de l'analyse terminologique.
#' @param vars Vecteur de caractères spécifiant les noms des variables à examiner.
#'   Par défaut utilise `names(x)` pour analyser toutes les variables du jeu
#'   de données.
#' @param acro_list Liste nommée servant de dictionnaire de référence pour
#'   les acronymes. Les noms constituent les acronymes à rechercher et les
#'   valeurs leurs définitions complètes.
#' @param acro_sep Chaîne de caractères définissant le séparateur à utiliser
#'   entre les différentes définitions d'acronymes dans le résultat formaté.
#'
#' @returns Une chaîne de caractères formatée contenant les définitions des
#'   acronymes identifiés dans les étiquettes des variables, ou `NULL` si
#'   aucun acronyme n'est détecté. Le formatage inclut automatiquement la
#'   ponctuation finale appropriée.
#'
#' @section Architecture du processus d'analyse :
#' La fonction exécute une séquence d'opérations analytiques coordonnées pour
#' identifier les acronymes pertinents. Elle extrait d'abord les attributs
#' d'étiquetage de chaque variable spécifiée via `label_attribute()`, puis
#' consolide ces informations en un corpus textuel unifié. Ce corpus est
#' ensuite analysé par `acro_extract()` pour identifier les acronymes présents
#' selon le dictionnaire de référence fourni. Finalement, `acro_str()` formate
#' les définitions correspondantes selon les conventions typographiques
#' professionnelles établies.
#'
#' @section Applications en environnement organisationnel :
#' Cette fonction répond aux exigences de documentation automatisée dans les
#' contextes professionnels où la cohérence terminologique constitue un enjeu
#' stratégique. Elle facilite la génération de notes explicatives pour les
#' tableaux statistiques, les graphiques d'analyse et les rapports techniques.
#' L'automatisation du processus d'identification des acronymes améliore
#' significativement l'efficacité des workflows de production documentaire
#' tout en garantissant la conformité aux standards de qualité organisationnels.
#'
#' @section Prérequis techniques et dépendances :
#' Cette fonction s'appuie sur un écosystème de fonctions auxiliaires
#' spécialisées qui doivent être disponibles dans l'environnement d'exécution.
#' Les fonctions `label_attribute()`, `acro_extract()` et `acro_str()` constituent
#' les composants essentiels de cette architecture modulaire. Les packages
#' `purrr` pour les opérations sur listes et `base` pour les fonctions de
#' manipulation d'environnements sont également requis pour un fonctionnement
#' optimal de l'ensemble du système d'analyse terminologique.
#'
#' @examples
#' 
#' # Préparation d'un dataset avec étiquettes pour démonstration
#' library(dplyr)
#' library(labelled)
#' 
#' # Configuration d'un dictionnaire d'acronymes statistiques
#' acronymes <- acro()
#' 
#' # Création d'un dataset avec étiquettes contenant des acronymes
#' df <-
#' mtcars |> 
#'   select(mpg, hp, wt) |> 
#'   set_variable_labels(mpg = "Fuel efficiency with SD calculation",
#'                       hp = "Engine power and 95%CI estimation",
#'                       wt = "Vehicle weight distribution")
#' 
#' # Identification automatique des acronymes dans les étiquettes
#' acro_match(x = df,
#'            acro_list = acronymes,
#'            acro_sep = "; ")
#' 
#' # Démonstration avec dataset ne contenant aucun acronyme référencé
#' df <-
#' mtcars |> 
#'   select(mpg, hp, wt) |> 
#'   set_variable_labels(mpg = "Fuel efficiency",
#'                       hp = "Engine power",
#'                       wt = "Vehicle weight")
#' 
#' # Retourne NULL car aucun acronyme détecté
#' acro_match(x = df,
#'            acro_list = acronymes,
#'            acro_sep = "; ")
#' 
#' @family fonctions de manipulation de chaînes
#' @seealso 
#' * [labelled::var_label()] pour la gestion des étiquettes de variables
#'
#' @references
#' Documentation labelled pour la gestion des métadonnées :
#' \url{https://larmarange.github.io/labelled/}
#'
#' @export
acro_match <- \(x,
                vars = names(x),
                acro_list,
                acro_sep) {
  
  .acro <-
  vars |>
    map_chr(~ label_attribute(x[[.]])) |>
    paste(collapse = " ") |>
    acro_extract(acro_list)
  
  acro_str(with(acro_list, mget(.acro)),
           collapse = acro_sep)

}
