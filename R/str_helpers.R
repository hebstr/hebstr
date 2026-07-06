#' Build an alternation expression for regular expressions
#'
#' Generates a string representing an alternation (OR) expression for regular
#' expressions by joining several patterns with the pipe separator (|). It is
#' a syntactic shortcut for building regex patterns with multiple alternatives.
#'
#' @param ... Strings representing the patterns to combine into the alternation
#'   expression. Each argument becomes one branch of the alternation in the
#'   resulting pattern.
#'
#' @return A single string containing all patterns separated by the pipe
#'   character (|), ready to use as an alternation regular expression.
#'
#' @section Technical requirements:
#' This function requires the `stringr` package to be loaded in the environment,
#' or the `str_c()` function to be explicitly imported. Use `library(stringr)`
#' or `stringr::str_c()` depending on your project setup.
#'
#' @section Applications:
#' Useful for text data analysis, validation against multiple formats, and
#' filtering large data sets against several inclusion criteria. It simplifies
#' the construction of complex regex patterns.
#'
#' @examples
#' # Load stringr first
#' library(stringr)
#'
#' # Build a pattern for French departments
#' depts_idf <- str_u("75", "77", "78", "91", "92", "93", "94", "95")
#' depts_idf
#'
#' # Filter postal codes
#' codes_postaux <- c("75001", "69001", "77300", "78000", "13001")
#' str_detect(codes_postaux, paste0("^(", depts_idf, ")"))
#'
#' # Pattern for office file extensions
#' extensions_office <- str_u("xlsx", "docx", "pptx", "pdf")
#' fichiers_projet <- c("rapport.docx", "donnees.xlsx", "image.png", "presentation.pptx")
#' str_detect(fichiers_projet, paste0("\\.(", extensions_office, ")$"))
#'
#' # Validate administrative statuses
#' statuts_valides <- str_u("actif", "suspendu", "en_attente", "archivé")
#' base_utilisateurs <- data.frame(
#'   identifiant = 1:4,
#'   statut = c("actif", "invalide", "en_attente", "archivé")
#' )
#' utilisateurs_conformes <- base_utilisateurs[
#'   str_detect(base_utilisateurs$statut, paste0("^(", statuts_valides, ")$")),
#' ]
#'
#' # Search for economic terminology
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
#' # Build patterns for data validation
#' formats_date <- str_u("\\d{2}/\\d{2}/\\d{4}", "\\d{4}-\\d{2}-\\d{2}", "\\d{2}-\\d{2}-\\d{4}")
#' dates_saisies <- c("01/01/2023", "2023-12-31", "31-12-2023", "invalide")
#' dates_valides <- dates_saisies[str_detect(dates_saisies, paste0("^(", formats_date, ")$"))]
#'
#' @family formatting functions
#' @seealso
#' * [stringr::str_c()] for string concatenation
#'
#' @references
#' stringr documentation on regular expressions:
#' \url{https://stringr.tidyverse.org/articles/regular-expressions.html}
#'
#' @export
str_u <- \(...) {
  str_c(c(...), collapse = "|")
}

#' Format text with inline HTML colors and styles
#'
#' Generates HTML code that applies both text and background colors to text
#' content. It combines the color styling with bold emphasis, for use in
#' Markdown documents, HTML reports, and interactive web interfaces.
#'
#' @param text String containing the text to format. Inserted verbatim into the
#'   HTML output; `glue` braces in `text` are not interpolated.
#' @param color Text color given as a standard CSS name, hexadecimal code, or
#'   RGB/RGBA value. Defaults to `"red"`.
#' @param bg Background color given as a hexadecimal code, CSS name, or RGB/RGBA
#'   value. Defaults to `"#ffffff00"` (transparent via the alpha channel).
#'
#' @return A string containing the HTML code with the CSS styles applied through
#'   the `style` attribute and the Markdown syntax for bold emphasis.
#'
#' @examples
#' str_color("Important")
#' str_color("Warning", color = "#ffffff", bg = "#cc0000")
#' str_color("Highlighted", color = "green", bg = "rgba(0, 128, 0, 0.15)")
#'
#' @export
str_color <- \(text, color = "red", bg = "#ffffff00") {
  color <- glue("color:{color};")
  bg <- glue("background-color:{bg};")

  glue("<span style='{color}{bg}'>**{text}**</span>")
}

#' Generate a formatted figure title with note and acronyms
#'
#' Builds a formatted HTML figure title with an explanatory note and acronyms as
#' a subtitle. The formatting adapts to the output context (standard document or
#' Quarto Markdown).
#'
#' @param title String containing the main figure title. Supports interpolation
#'   with the `glue` syntax.
#' @param note String containing the explanatory note shown below the title.
#'   Defaults to an empty string. Supports `glue` interpolation.
#' @param acro String containing the acronyms or abbreviations to append after
#'   the note. Defaults to an empty string.
#' @param sub_size Font size for the note and acronyms, in points. Defaults to
#'   `"7.5"`.
#' @param render Logical value indicating whether the output targets a Quarto
#'   Markdown document. Defaults to `getOption("str_fig.render", TRUE)`.
#' @param class CSS class applied to the note in the Quarto context. Defaults to
#'   `"quarto-float-subcaption"`.
#'
#' @return A string containing the formatted HTML for the figure title with its
#'   note and acronyms.
#'
#' @examples
#' str_fig("Body mass distribution")
#'
#' str_fig(
#'   "Body mass distribution",
#'   note = "Measured across three islands",
#'   acro = "BMI: body mass index"
#' )
#'
#' str_fig(
#'   "Body mass distribution",
#'   note = "Measured across three islands",
#'   sub_size = "6",
#'   render = FALSE
#' )
#'
#' @export
str_fig <- \(
  title,
  note = "",
  acro = "",
  sub_size = "7.5",
  render = getOption("str_fig.render", default = TRUE),
  class = "quarto-float-subcaption"
) {
  title <- glue(title)
  note <- glue(note)

  if (!render) {
    .str <- glue(
      "{title}<br>
      <span style='font-size:{sub_size}pt'>{note} {acro}</span>"
    )
  } else {
    .str <- glue(
      "{title}<br>
      <span class='{class}'>{note} {acro}</span>"
    )
  }

  return(.str)
}

#' Concatenate variable labels with grammatical formatting
#'
#' Extracts the labels of the specified variables from a data set and
#' concatenates them into a single string with grammatically correct
#' punctuation (commas and a final conjunction).
#'
#' @param data A data.frame or similar object containing the variables to
#'   process.
#' @param ... <data-masking> Names of the variables whose labels are extracted.
#'   Accepts bare variable selection (unquoted).
#' @param last String specifying the final conjunction to use before the last
#'   element. Defaults to `"and"`.
#'
#' @return A single string containing the variable labels concatenated with
#'   commas and the final conjunction.
#'
#' @section Dependencies:
#' This function requires the `purrr`, `stringr`, and `glue` packages. It also
#' uses `var_label()`, which must be available (typically through the `labelled`
#' package).
#'
#' @examples
#' df <- datasets::penguins[, c("bill_len", "flipper_len", "body_mass")]
#' labelled::var_label(df) <- list(
#'   bill_len = "Bill length",
#'   flipper_len = "Flipper length",
#'   body_mass = "Body mass"
#' )
#'
#' # Concatenate two labels
#' str_label(df, bill_len, flipper_len)
#'
#' # Concatenate three labels with a custom final conjunction
#' str_label(df, bill_len, flipper_len, body_mass, last = "or")
#'
#' @export
str_label <- \(data, ..., last = "and") {
  enexprs(...) |>
    map_chr(~ var_label(data[[as_string(.x)]]) %||% "") |>
    str_flatten_comma(glue(" {last} "))
}

#' Apply a transformation function to the first character of a string
#'
#' Applies a transformation to the first character of a string while leaving the
#' rest of the string unchanged. Useful for typographic manipulation and text
#' formatting.
#'
#' @param fun Function to apply to the first character. Must be a function that
#'   takes a string and returns a transformed string (for example `toupper`,
#'   `tolower`).
#' @param str String to process. Its first character is transformed by the
#'   specified function.
#'
#' @return A string identical to the input, with the first character transformed
#'   by the applied function.
#'
#' @section Mechanism:
#' The function uses `stringr::str_sub()` to extract the first character,
#' `do.call()` to apply the transformation function dynamically, then
#' `stringr::str_replace()` to substitute the transformed character back into
#' the original string.
#'
#' @section Compatible functions:
#' This function works with any function that takes a string as input and
#' returns a transformed string. Commonly used functions include
#' `base::toupper()`, `base::tolower()`, or custom character-transformation
#' functions.
#'
#' @examples
#' # Convert the first character to upper case
#' str_cap(toupper, "france")
#'
#' # Convert the first character to lower case
#' str_cap(tolower, "PARIS")
#'
#' # Apply to full sentences
#' str_cap(toupper, "analyse des données économiques")
#' str_cap(tolower, "RAPPORT TRIMESTRIEL")
#'
#' # Use with strings containing special characters
#' str_cap(toupper, "émission de CO₂")
#' str_cap(tolower, "ÉLÉVATION du niveau de la mer")
#'
#' # Apply in a title-formatting context
#' titres_bruts <- c("méthodologie statistique", "RÉSULTATS PRINCIPAUX")
#' titres_formates <- sapply(titres_bruts, \(x) str_cap(toupper, x))
#'
#' # Use with a custom function
#' inverser_casse <- function(char) {
#'   if (char == toupper(char)) tolower(char) else toupper(char)
#' }
#' str_cap(inverser_casse, "Données")
#' str_cap(inverser_casse, "données")
#'
#' # Standardize proper nouns
#' noms_regions <- c("île-de-france", "NORMANDIE", "provence-alpes-côte d'azur")
#' noms_standardises <- sapply(noms_regions, \(x) str_cap(toupper, x))
#'
#' @family formatting functions
#' @seealso
#' * [stringr::str_to_title()] for title-case conversion
#'
#' @export
str_cap <- \(fun, str) {
  str_sub(str, end = 1) <- do.call(fun, list(str_sub(str, end = 1)))

  str
}
