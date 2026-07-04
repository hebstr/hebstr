#' Generate an acronym dictionary with localized definitions
#'
#' Creates a dictionary of statistical acronyms and their full definitions,
#' adapting automatically to the language configuration of the R environment.
#' It combines predefined acronyms with custom definitions to help produce
#' consistent glossaries for technical documentation and analysis reports.
#'
#' @param ... Expressions defining custom acronyms in the form
#'   `acronym ~ "definition"`. Uses non-standard evaluation for an intuitive
#'   interface.
#' @param .sep Character string setting the separator between the acronym and
#'   its definition. Defaults to `":"`.
#' @param .auto Logical controlling inclusion of the predefined acronyms. If
#'   `TRUE` (default), combines the custom definitions with the standard
#'   acronym base.
#' @param .tolower Logical controlling acronym case. If `TRUE`, converts
#'   acronyms to lowercase. Defaults to `FALSE`.
#'
#' @returns A named list where the names are the acronyms and the values are
#'   the formatted definitions. The format includes the specified separator
#'   between each acronym and its full definition.
#'
#' @section Technical architecture:
#' The function uses a custom environment to define the `~` operator, which
#' handles the formatting of acronym-definition pairs. This approach relies on
#' R metaprogramming via `rlang::enexpr()` to capture expressions without
#' prior evaluation. The mechanism supports an intuitive syntax while
#' retaining the flexibility needed for various formatting configurations.
#'
#' @section Automatic language adaptation:
#' The function detects the language configuration via `getOption("OutDec")`
#' and adapts the predefined acronyms accordingly. For an English
#' configuration (decimal point), standard statistical terms are provided in
#' English with compact formatting. For a French configuration (decimal
#' comma), the French equivalents are used, following local typographic
#' conventions, notably the spacing around separators.
#'
#' @section Predefined acronyms by locale:
#' The built-in acronym base covers the essential descriptive statistics
#' terms. In English configuration, it includes SD (standard deviation), IQR
#' (interquartile range), Q1 and Q3 (quartiles), and 95%CI (confidence
#' interval). In French configuration, the equivalents cover the standard
#' deviation, the interquartile range, the quartiles with French ordinals,
#' and the 95% confidence interval. This standardization supports terminology
#' consistency across analytical outputs.
#'
#' @section Requirements and dependencies:
#' The function requires the `rlang` package for metaprogramming, `glue` for
#' string interpolation, `purrr` for list operations, and `stringr` for text
#' manipulation. The R environment must also support non-standard evaluation
#' for the expression-capture mechanisms to work.
#'
#' @examples
#' # Generate the default dictionary (English configuration)
#' options(OutDec = ".")
#' dictionnaire_base <- acro()
#' dictionnaire_base$SD
#' dictionnaire_base$IQR
#'
#' # Add custom acronyms
#' glossaire_etendu <- acro(
#'   BMI ~ "body mass index",
#'   SBP ~ "systolic blood pressure",
#'   DBP ~ "diastolic blood pressure"
#' )
#' glossaire_etendu$BMI
#' glossaire_etendu$SD
#'
#' # French configuration with language adaptation
#' options(OutDec = ",")
#' dictionnaire_fr <- acro()
#' dictionnaire_fr$SD
#' dictionnaire_fr$IQR
#'
#' # Custom acronyms in French
#' glossaire_medical_fr <- acro(
#'   IMC ~ "indice de masse corporelle",
#'   PAS ~ "pression artérielle systolique",
#'   PAD ~ "pression artérielle diastolique"
#' )
#' glossaire_medical_fr$IMC
#'
#' # Custom separator
#' dictionnaire_tiret <- acro(
#'   HR ~ "heart rate",
#'   RR ~ "respiratory rate",
#'   .sep = " -"
#' )
#' dictionnaire_tiret$HR
#'
#' # Without predefined acronyms
#' acronymes_specifiques <- acro(
#'   ROC ~ "receiver operating characteristic",
#'   AUC ~ "area under the curve",
#'   .auto = FALSE
#' )
#' names(acronymes_specifiques)
#'
#' # With conversion to lowercase
#' acronymes_minuscules <- acro(
#'   COPD ~ "chronic obstructive pulmonary disease",
#'   ARDS ~ "acute respiratory distress syndrome",
#'   .tolower = TRUE
#' )
#' names(acronymes_minuscules)
#'
#' # Demonstration with built-in datasets in a medical context
#' # Using mtcars as a proxy for biometric data
#' options(OutDec = ".")
#' glossaire_automobile <- acro(
#'   MPG ~ "miles per gallon",
#'   HP ~ "horsepower",
#'   WT ~ "weight in thousands of pounds"
#' )
#'
#' # For plot captions
#' paste(glossaire_automobile$MPG)
#' paste(glossaire_automobile$HP)
#'
#' # Configuration for a multilingual report
#' options(OutDec = ",")
#' legende_fr <- acro(
#'   CONSO ~ "consommation moyenne",
#'   PUIS ~ "puissance nominale",
#'   .sep = " :"
#' )
#' legende_fr$CONSO
#'
#' # Restore default configuration
#' options(OutDec = ".")
#'
#' # Integration into a report-production pipeline
#' glossaire_analyse <- acro(
#'   CYL ~ "number of cylinders",
#'   DISP ~ "displacement in cubic inches",
#'   GEAR ~ "number of forward gears"
#' )
#'
#' @family formatting functions
#'
#' @references
#' rlang documentation on tidyeval metaprogramming:
#' \url{https://rlang.r-lib.org/reference/topic-metaprogramming.html}
#'
#' @export
acro <- \(..., .sep = ":", .auto = TRUE, .tolower = FALSE) {
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
      .fun(
        SD ~ "standard deviation",
        IQR ~ "interquartile range",
        Q1 ~ "1st quartile",
        Q3 ~ "3rd quartile",
        `95%CI` ~ "95% confidence interval"
      )
  } else {
    sep <- glue(" {.sep} ")

    base <-
      .fun(
        SD ~ "\u00e9cart-type",
        IQR ~ "intervalle interquartile",
        Q1 ~ "1er quartile",
        Q3 ~ "3e quartile",
        `IC95%` ~ "intervalle de confiance \u00e0 95%"
      )
  }

  .acro <- if (.auto) list_modify(base, !!!.fun(...)) else .fun(...)

  return(.acro)
}

#' Extract the acronyms present in a text using a reference dictionary
#'
#' Identifies and extracts all acronyms contained in a character string using
#' a predefined reference dictionary. It searches the text for every
#' occurrence of the specified acronyms and returns a deduplicated list of the
#' identified terms. The function supports automated terminology analysis and
#' the generation of contextual glossaries for technical documentation.
#'
#' @param x Character string in which to search for acronyms. Accepts free
#'   text, titles, descriptions, or any textual content requiring systematic
#'   terminology analysis.
#' @param acro_list Named list of acronyms used as the reference dictionary.
#'   The names of this list are the terms to search for in the input text.
#'
#' @returns Character vector of the unique acronyms identified in the input
#'   text. Results are deduplicated to remove multiple occurrences of the same
#'   term.
#'
#' @section Extraction mechanism:
#' The function scans the input text to identify every occurrence of the
#' acronyms present in the reference dictionary. The extraction process
#' deduplicates the results so that an acronym appears only once in the
#' returned list, regardless of how many times it occurs in the source text.
#'
#' @section Use cases:
#' The function identifies the acronyms used in analysis reports, technical
#' presentations, and project documentation, supporting terminology
#' consistency across documents.
#'
#' @section Technical considerations:
#' This implementation uses a simple matching approach that can detect
#' acronyms even when they form part of a larger term. Users should account
#' for this when building their acronym dictionaries to avoid unintended
#' matches. The function requires the stringr package for string manipulation.
#'
#' @examples
#' # Create a dictionary of statistical acronyms
#' dictionnaire_analyse <- list(
#'   "SD" = "standard deviation",
#'   "IQR" = "interquartile range",
#'   "CI" = "confidence interval",
#'   "OR" = "odds ratio"
#' )
#'
#' # Extract acronyms from a report title
#' titre_rapport <- "Statistical analysis with SD and CI calculations"
#' acro_extract(titre_rapport, dictionnaire_analyse)
#'
#' # Analyze a methodology description
#' methodologie <- "Results include OR estimates with 95% CI bounds"
#' acro_extract(methodologie, dictionnaire_analyse)
#'
#' # Using the mtcars dataset in an automotive context
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
#' # Using the diamonds dataset for gemological analysis
#' dictionnaire_gemmes <- list(
#'   "CT" = "carat weight",
#'   "CL" = "clarity",
#'   "COL" = "color grade"
#' )
#'
#' rapport_diamants <- "Diamond evaluation considers CT and CL parameters"
#' acro_extract(rapport_diamants, dictionnaire_gemmes)
#'
#' # Demonstration with text containing no referenced acronym
#' texte_general <- "This comprehensive analysis examines various factors"
#' acro_extract(texte_general, dictionnaire_analyse)
#'
#' # For terminology-consistency validation
#' resume_executif <- "Key findings show significant OR values with narrow CI ranges"
#' termes_detectes <- acro_extract(resume_executif, dictionnaire_analyse)
#' length(termes_detectes)
#'
#' # Use in technical documentation analysis
#' specification_technique <- "System requirements include RAM and CPU specifications"
#' dictionnaire_informatique <- list(
#'   "RAM" = "random access memory",
#'   "CPU" = "central processing unit",
#'   "GPU" = "graphics processing unit"
#' )
#' acro_extract(specification_technique, dictionnaire_informatique)
#'
#' # Analyze plot captions with the storms data
#' legende_meteo <- "Hurricane analysis: wind speed and pressure measurements"
#' dictionnaire_meteorologie <- list(
#'   "MPH" = "miles per hour",
#'   "MB" = "millibars",
#'   "KPH" = "kilometers per hour"
#' )
#' acro_extract(legende_meteo, dictionnaire_meteorologie)
#'
#' # For auditing a business presentation
#' slide_presentation <- "Market analysis reveals ROI improvements and KPI achievements"
#' dictionnaire_business <- list(
#'   "ROI" = "return on investment",
#'   "KPI" = "key performance indicator",
#'   "ROE" = "return on equity"
#' )
#' acronymes_business <- acro_extract(slide_presentation, dictionnaire_business)
#' acronymes_business
#'
#' @family string manipulation functions
#'
#' @export
acro_extract <- \(x, acro_list) {
  .acro <- names(acro_list)
  .pattern <- str_c("\\b", str_escape(.acro), "\\b")

  x |>
    map(str_extract, .pattern) |>
    unlist() |>
    na.omit() |>
    unique()
}

#' Format an acronym string with automatic separators and punctuation
#'
#' Concatenates textual elements into a single character string using a
#' standard separator and appends a final period. Empty strings return NULL,
#' which makes the function easy to integrate into conditional formatting for
#' technical documentation and analysis reports.
#'
#' @param ... Elements to concatenate into a single string. Accepts character
#'   vectors, individual strings, or any object coercible to character via
#'   `paste()`.
#' @param collapse Character string used to separate the elements during
#'   concatenation. Defaults to `"; "` (semicolon followed by a space),
#'   following typographic conventions for technical enumerations.
#'
#' @returns A character string formatted with the separator and a final period
#'   when the content is non-empty, or `NULL` when concatenation produces an
#'   empty string.
#'
#' @section Formatting mechanism:
#' The function uses `base::paste()` to concatenate the supplied elements with
#' the specified separator, then `glue::glue()` to add the final punctuation.
#' The conditional handling of empty strings avoids generating superfluous
#' content in automated documents.
#'
#' @section Use cases:
#' The function fits into automated generation of captions, explanatory notes,
#' and technical annotations, and into the production of formatted glossaries.
#' The semicolon default separator follows French editorial conventions for
#' complex enumerations.
#'
#' @examples
#' # Basic formatting with default separators
#' acro_str("SD: standard deviation", "CI: confidence interval")
#'
#' # With statistical acronym definitions
#' acro_str("IQR: interquartile range", "OR: odds ratio", "HR: hazard ratio")
#'
#' # Using the mtcars dataset in an automotive context
#' definitions_vehicules <- c(
#'   "MPG: miles per gallon",
#'   "HP: horsepower",
#'   "CYL: number of cylinders"
#' )
#' acro_str(definitions_vehicules[1], definitions_vehicules[2])
#'
#' # Custom separator for different contexts
#' acro_str("Mean: average value", "SD: standard deviation", collapse = " | ")
#'
#' # Using the diamonds dataset for gemological analysis
#' termes_gemmes <- c(
#'   "CT: carat weight",
#'   "CL: clarity grade",
#'   "COL: color classification"
#' )
#' acro_str(termes_gemmes[1], termes_gemmes[3], collapse = " - ")
#'
#' # Handling empty strings
#' acro_str("")  # Returns NULL
#' acro_str()    # Returns NULL
#'
#' # Use in generating plot captions
#' legende_analyse <- acro_str(
#'   "n: sample size",
#'   "p: probability value",
#'   "CI: confidence interval"
#' )
#' legende_analyse
#'
#' # Using the storms dataset for meteorology
#' variables_meteo <- acro_str(
#'   "mph: miles per hour",
#'   "mb: millibars pressure",
#'   "cat: category scale"
#' )
#'
#' # Integration into an automated documentation workflow
#' glossaire_medical <- acro_str(
#'   "BMI: body mass index",
#'   "BP: blood pressure",
#'   "HR: heart rate"
#' )
#'
#' # For technical footnotes
#' note_methodologique <- acro_str(
#'   "ANOVA: analysis of variance",
#'   "post-hoc: posterior comparison tests"
#' )
#'
#' # Conditional use in reports
#' acronymes_detectes <- c("ROC: receiver operating characteristic")
#' if (length(acronymes_detectes) > 0) {
#'   note_finale <- acro_str(acronymes_detectes)
#'   note_finale
#' }
#'
#' # Demonstration with custom formatting for publications
#' definitions_statistiques <- acro_str(
#'   "α: significance level",
#'   "β: type II error probability",
#'   "R²: coefficient of determination",
#'   collapse = "; "
#' )
#' definitions_statistiques
#'
#' @family formatting functions
#'
#' @export
acro_str <- \(..., collapse = "; ") {
  acro <- paste(c(...), collapse = collapse)

  if (acro != "") glue("{acro}.") else NULL
}

#' Identify and format the acronyms present in variable labels
#'
#' Analyzes the descriptive labels of a dataset's variables to identify the
#' acronyms present, then generates a formatted string containing their
#' definitions from a reference dictionary. It supports the automatic
#' generation of contextual glossaries in analysis reports.
#'
#' @param x A data.frame or similar object containing the variables to
#'   analyze. Variables should carry label attributes for the terminology
#'   analysis to work.
#' @param vars Character vector giving the names of the variables to examine.
#'   Defaults to `names(x)`, analyzing all variables in the dataset.
#' @param acro_list Named list used as the reference dictionary for acronyms.
#'   The names are the acronyms to search for and the values their full
#'   definitions.
#' @param acro_sep Character string setting the separator between acronym
#'   definitions in the formatted result.
#'
#' @returns A formatted character string containing the definitions of the
#'   acronyms identified in the variable labels, or `NULL` if no acronym is
#'   detected. The formatting includes the final punctuation.
#'
#' @section Analysis process:
#' The function runs a sequence of steps to identify the relevant acronyms. It
#' first extracts the label attributes of each specified variable via
#' `label_attribute()`, then consolidates them into a single text corpus. This
#' corpus is analyzed by `acro_extract()` to identify the acronyms present
#' according to the supplied reference dictionary. Finally, `acro_str()`
#' formats the matching definitions.
#'
#' @section Use cases:
#' The function generates explanatory notes for statistical tables, analysis
#' plots, and technical reports. Automating acronym identification streamlines
#' documentation workflows.
#'
#' @section Requirements and dependencies:
#' The function relies on the helper functions `label_attribute()`,
#' `acro_extract()`, and `acro_str()`, which must be available in the
#' execution environment. The `purrr` package for list operations and `base`
#' for environment manipulation are also required.
#'
#' @examples
#'
#' # Prepare a labelled dataset for demonstration
#' library(dplyr)
#' library(labelled)
#'
#' # Set up a dictionary of statistical acronyms
#' acronymes <- acro()
#'
#' # Create a dataset with labels containing acronyms
#' df <-
#' mtcars |>
#'   select(mpg, hp, wt) |>
#'   set_variable_labels(mpg = "Fuel efficiency with SD calculation",
#'                       hp = "Engine power and 95%CI estimation",
#'                       wt = "Vehicle weight distribution")
#'
#' # Automatic identification of acronyms in the labels
#' acro_match(x = df,
#'            acro_list = acronymes,
#'            acro_sep = "; ")
#'
#' # Demonstration with a dataset containing no referenced acronym
#' df <-
#' mtcars |>
#'   select(mpg, hp, wt) |>
#'   set_variable_labels(mpg = "Fuel efficiency",
#'                       hp = "Engine power",
#'                       wt = "Vehicle weight")
#'
#' # Returns NULL because no acronym is detected
#' acro_match(x = df,
#'            acro_list = acronymes,
#'            acro_sep = "; ")
#'
#' @family string manipulation functions
#' @seealso
#' * [labelled::var_label()] for managing variable labels
#'
#' @references
#' labelled documentation on metadata management:
#' \url{https://larmarange.github.io/labelled/}
#'
#' @export
acro_match <- \(x, vars = names(x), acro_list, acro_sep) {
  vars_missing <- setdiff(vars, names(x))

  if (length(vars_missing) > 0) {
    cli_abort("{.field {vars_missing}} not found in {.arg x}.")
  }

  .acro <-
    vars |>
    map_chr(~ label_attribute(x[[.]]) %||% "") |>
    paste(collapse = " ") |>
    acro_extract(acro_list)

  acro_str(with(acro_list, mget(.acro)), collapse = acro_sep)
}
