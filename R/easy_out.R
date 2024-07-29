#' Title
#'
#' @param x
#' @param filename
#' @param dir
#' @param suffix
#' @param width
#' @param height
#' @param size
#' @param print
#'
#' @return
#' @export
#'
#' @examples
#'
easy_out <- \(x,
              filename = NULL,
              dir = "output",
              suffix = NULL,
              width = NA,
              height = NULL,
              size = NULL,
              assign = TRUE,
              print = NULL) {

  cli_h1("easy_out")
  cli_text("\n\n")
  
  if (is.null(filename)) {
    
    filename <- enexpr(x)
    
  } else cli_progress_step("Creating {.strong {filename}}")
  
  if (!TRUE %in% str_detect(class(x), "tbl|gg")) {

    cli_abort("{.strong {filename}} must be a gt/gtsummary object or a ggplot object")

  }

  if (!is.null(suffix)) filename <- glue("{filename}_", suffix)

  if (!dir.exists(dir)) dir.create(path = dir, recursive = TRUE)

  if (assign && !as.character(filename) %in% ls()) {
    
    assign(as.character(filename), x, envir = .GlobalEnv)
    
  }
  
  path <- glue("{dir}/{filename}")
  to_html <- glue("{path}.html")
  to_svg <- glue("{path}.svg")
  to_png <- glue("{path}.png")

### TAB -------------------------------------------------------------------------

  if (TRUE %in% str_detect(class(x), "tbl")) {
    
    if (R.version$os == "linux-gnu") Sys.setenv(OPENSSL_CONF = "/dev/null")
    
    if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
    
    if (!"gt_tbl" %in% class(x)) x <- as_gt(x)

    width <-
    x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      unlist() |>
      str_extract("\\d+") |>
      as.numeric()

      if (exists(".gtsum_out")) {
        
        cli_text("\n\n")
        print(.gtsum_out, n = 100, na.print = NULL)
        
      }
      
      if (!is.null(print)) {
        
        cli_text("\n\n")
        print(print)
        
      }

    cli_text("\n\n")
    cli_progress_step("Creating HTML file")

    gtsave(x, file = to_html)

    browseURL(glue("{getwd()}/{to_html}"))

    cli_progress_step("Capturing HTML to PNG")

    to_html |> 
      webshot(file = to_png,
              vwidth = width + width / 10,
              vheight = 1,
              zoom = 3)

    cli_progress_done()

### PLOT -------------------------------------------------------------------------

  } else if ("gg" %in% class(x)) {

    if (!rlang::is_installed("rsvg")) install.packages("rsvg", quiet = TRUE)
    
    cli_progress_step("Creating SVG file")

    x |>
      capturePlot(glue("{getwd()}/{to_svg}"),
                  grDevices::svg,
                  height = size[1],
                  width = size[2]) |>
      browseURL()

    cli_progress_step("Capturing SVG to PNG")

    image_read_svg(to_svg, height = 1000) |>
      image_write(to_png, format = "png")

    cli_progress_done()

  }

  if (!is.null(print)) {
        
    cli_text("\n\n")
    print(print)
        
  }

### CLI --------------------------------------------------------------------------

  cli_text("\n\n")
  cli_alert_info("{.strong Destination}")
  cli_ul()
    cli_li("Working directory: {.path {getwd()}}")
    cli_li("Filename: {cli::col_br_red(path)}")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}


#' Title
#'
#' @param data
#' @param filename
#' @param dir
#' @param suffix
#' @param size
#'
#' @return
#' @export
#'
#' @examples
#'
easy_out_map <- \(data,
                  filename = NULL,
                  dir = "output",
                  suffix = NULL,
                  size = NULL) {

  if (is.null(filename)) filename <- enexpr(data)

  if (!is.list(data)) cli_abort("{.strong {filename}} must be a list")

  if (is.null(suffix)) suffix <- names(data) %||% seq(data)

  map2(.x = data,
       .y = suffix,
       ~ .x |>
         easy_out(filename = glue("{filename}.{.y}"),
                  dir = dir,
                  size = size))

}
