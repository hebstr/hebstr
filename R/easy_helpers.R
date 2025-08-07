#' Title
#'
#' @param ... A character vector
#' @param replace Replacement pattern. A character vector.
#'
#' @return A named character vector
#' @export
#'
#' @examples "arg"
#'
easy_replace <- \(..., replace = "</>") {

  col_replace <- cli::col_br_red(replace)
  col_replace <- glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  map(c(...),
      ~ list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
        unlist())

  replace_list <- list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))

}


#' Title
#'
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_recode <- \(...) {

  dots <- list(...)

  list(name = dots |> map(names) |> unlist(),
       label = dots |> map(unname) |> unlist())

}


#' Title
#'
#' @param x arg 
#' @param var arg 
#' @param incr arg 
#' @param drop arg 
#' @param values arg 
#' @param labels arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
easy_cut <- \(x, 
              var, 
              incr = FALSE,
              drop = FALSE,
              values = NULL, 
              labels = NULL,
              ...) {
  
  var <- enexpr(var)
  
  if (!incr) {
    
    name <- glue("{var}_cat")
    
    .min <- min(x[[var]], na.rm = TRUE)
    
    .max <- max(x[[var]], na.rm = TRUE)
    
    .values <- map_dbl(values, ~ . - 1/10000)
    
    .labels <- labels |> str_replace_all("\\.|,", getOption("OutDec"))
    
    x <-
    x |> 
      mutate(!!name :=
               cut(x = {{ var }},
                   breaks = c(.min - 1, .values, .max + 1),
                   labels = .labels,
                   right = FALSE),
             .after = all_of(var))
      
  } else {
    
    name <- glue("{var}_incr")
    
    x <-
    x |>
      mutate(!!name :=
               cut(x = {{ var }},
                   breaks = seq(...),
                   right = FALSE) |> 
               as.numeric(),
             .after = all_of(var))
    
  }
  
  if (drop) {
    
    x <-
    x |> 
      select(-!!var) |> 
      rename(!!var := all_of(name))
    
  }
  
  return(x)
  
}


#' Title
#'
#' @param x arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
pca_var_extract <- \(x) {

  lst(coord =
        x |>
          broom::tidy("rotation") |>
          tidyr::pivot_wider(names_from = "PC",
                             names_prefix = "PC",
                             values_from = "value") |>
          mutate(column = str_remove_all(column, "hamd"),
                 .keep = "all"),
      contrib =
        coord |>
          mutate(across(matches("PC"), ~ . ^ 2 / sum(. ^ 2))),
      weight =
        coord["column"] |>
          mutate(PC1 = contrib$PC1 / max(contrib$PC1)) |>
          pull(PC1))

}


#' Title
#'
#' @param data arg
#' @param times arg
#' @param method arg
#' @param fit arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_boot <- \(data,
               times = 1000,
               method,
               fit,
               ...) {

  .f <- \(.) {

    do.call(method,
            list(parsnip::fit,
                 rsample::analysis(.),
                 ...))

  }

  boot <-
  data |>
    rsample::bootstraps(times = times,
                        apparent = TRUE) |>
    mutate(model = map(splits, .f))

  boot <-
  list(estim_data = tidy,
       fitted = augment) |>
    map(~ boot |>
          mutate(coef = map(model, .)))

  boot <-
  list(estimate =
         list(data = boot$estim_data,
              int = boot$estim_data |> int_pctl(coef)),
       fitted = boot$fitted)

  assign("boot", boot, envir = .GlobalEnv)

  print(boot$estim$int)

}


#' Title
#'
#' @param data arg
#' @param model arg
#' @param y arg
#' @param vars arg
#' @param pv arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
p_picking <- \(data,
               model,
               y,
               vars,
               pv) {

  fit <- expr(list(reformulate(., y), data = data))

  vars[!vars %in% y] |>
    map(~ do.call(model, eval(fit)) |>
          tidy() |>
          mutate(variable = str_extract(term, str_u(vars)))) |>
    list_rbind() |>
    filter(variable %in% vars,
           p.value <= pv) |>
    pull(variable)

}


#' Title
#'
#' @param x arg
#' @param column arg
#' @param digits arg
#' @param seuil arg
#' @param table arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
p_shortenr <- \(x,
                column = p.value,
                digits = 3,
                seuil = 0.001,
                table = TRUE) {

  column <- enexpr(column)

  if (table) inf <- "<" else inf <- "< "
  if (table) sup <- "" else sup <- "= "

  x |>
    rowwise() |>
    mutate(!!column :=
             if_else(!!column < seuil,
                     seuil,
                     round(!!column, digits)),
           !!column :=
             if_else(!!column == seuil,
                     glue(inf, !!column),
                     glue(sup, !!column)))

}


#' Title
#'
#' @param data arg 
#' @param .var arg
#' @param .min arg
#' @param .fun arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
pct_min <- \(data,
             .var,
             .min,
             .fun = "max") {

  .count <-
  data |>
    count("{.var}" := get(.var)) |>
    mutate(p = n / do.call(.fun, list(n))) |>
    filter(p >= .min) |> 
    pull(.var)

  data |>
    filter(get(.var) %in% .count)

}


#' Title
#'
#' @param file arg
#' @param dir arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
read_png <- \(file,
              dir = "output") {

  glue("{dir}/{file}.png") |>
    readPNG() |>
    rasterGrob()

}


#' Exécuter automatiquement tous les scripts R d'un répertoire avec filtrage
#'
#' Cette fonction automatise l'exécution de tous les scripts R contenus dans
#' un répertoire spécifié, en excluant optionnellement les fichiers commençant
#' par un préfixe donné. Elle facilite l'initialisation de projets complexes
#' nécessitant le chargement de multiples modules de configuration, fonctions
#' personnalisées ou scripts de préparation de données. Cette automatisation
#' améliore l'efficacité des workflows de développement et garantit la
#' reproductibilité des environnements de travail analytiques.
#'
#' @param dir Chemin vers le répertoire contenant les scripts à exécuter.
#'   Par défaut `"scripts"`, représentant un dossier relatif au répertoire
#'   de travail courant.
#' @param prefix Caractère ou chaîne de caractères définissant le préfixe
#'   des fichiers à exclure de l'exécution automatique. Par défaut `"_"`
#'   (underscore), permettant de marquer les fichiers inactifs ou de test.
#'
#' @returns Cette fonction est appelée pour ses effets de bord (exécution
#'   des scripts) et retourne une liste contenant les résultats de chaque
#'   appel à `source()`. Cette liste peut être utilisée pour diagnostiquer
#'   l'exécution ou récupérer des valeurs retournées par les scripts.
#'
#' @section Mécanisme de filtrage et d'exécution :
#' La fonction utilise `list.files()` pour inventorier tous les fichiers
#' du répertoire spécifié, puis applique `stringr::str_subset()` avec une
#' expression régulière pour exclure les fichiers dont le nom commence
#' par le préfixe défini. Cette approche permet de maintenir des fichiers
#' de travail, de test ou de sauvegarde dans le même répertoire sans
#' risquer leur exécution accidentelle lors de l'initialisation automatique.
#'
#' @section Applications en gestion de projets tidyverse :
#' Cette fonction s'intègre parfaitement dans les workflows tidyverse en
#' permettant l'initialisation automatique de scripts de préparation de
#' données, de configuration de visualisations et de définition de fonctions
#' personnalisées utilisant dplyr, ggplot2 et autres packages de l'écosystème.
#' Elle facilite la gestion de projets d'analyse complexes où multiple
#' transformations de données et configurations graphiques doivent être
#' appliquées de manière cohérente et reproductible.
#'
#' @section Prérequis techniques et dépendances :
#' Cette fonction nécessite les packages `stringr` pour les opérations
#' de filtrage par expressions régulières, `purrr` pour l'application
#' de fonctions sur des collections, et `glue` pour la construction
#' dynamique de chemins de fichiers. L'intégration tidyverse optimise
#' l'utilisation avec les workflows modernes d'analyse de données.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(stringr)
#' library(purrr)
#' 
#' # Configuration automatique d'un projet d'analyse
#' \dontrun{
#' dir.create("analysis_setup", showWarnings = FALSE)
#' 
#' # Script de préparation des données
#' writeLines(c(
#'   "# Préparation données mtcars",
#'   "mtcars_clean <- mtcars |>",
#'   "  mutate(",
#'   "    efficiency = case_when(",
#'   "      mpg >= 20 ~ 'High',",
#'   "      mpg >= 15 ~ 'Medium',",
#'   "      TRUE ~ 'Low'",
#'   "    )",
#'   "  )",
#'   "cat('Données préparées\\n')"
#' ), "analysis_setup/prepare_data.R")
#' 
#' # Script de fonctions d'analyse
#' writeLines(c(
#'   "# Fonctions d'analyse tidyverse",
#'   "summarize_by_group <- function(data, group_var) {",
#'   "  data |>",
#'   "    group_by({{ group_var }}) |>",
#'   "    summarise(",
#'   "      count = n(),",
#'   "      avg_mpg = mean(mpg),",
#'   "      .groups = 'drop'",
#'   "    )",
#'   "}",
#'   "cat('Fonctions chargées\\n')"
#' ), "analysis_setup/functions.R")
#' 
#' # Exécution automatique
#' auto_exec(dir = "analysis_setup")
#' 
#' # Utilisation des éléments chargés
#' mtcars_clean |> summarize_by_group(efficiency)
#' }
#' 
#' # Exclusion de scripts avec préfixe personnalisé
#' \dontrun{
#' dir.create("project_scripts", showWarnings = FALSE)
#' 
#' # Script de production
#' writeLines(c(
#'   "# Configuration production",
#'   "diamonds_sample <- diamonds |>",
#'   "  slice_sample(n = 1000) |>",
#'   "  select(carat, cut, color, price)",
#'   "cat('Configuration production chargée\\n')"
#' ), "project_scripts/production.R")
#' 
#' # Script de développement (sera exclu)
#' writeLines(c(
#'   "# Script de test - ne sera pas exécuté",
#'   "test_data <- diamonds |> slice_head(n = 10)",
#'   "stop('Script de test')"
#' ), "project_scripts/dev_testing.R")
#' 
#' # Exécution en excluant les scripts dev_
#' auto_exec(dir = "project_scripts", prefix = "dev_")
#' 
#' # Seul le script de production sera exécuté
#' glimpse(diamonds_sample)
#' }
#'
#' @family fonctions d'automatisation
#'
#' @export
auto_exec <- \(dir = "scripts",
               prefix = "_") {
  
  list.files(dir) |> 
    str_subset(glue("^[^{prefix}]")) |> 
    map(~ source(glue("{dir}/{.}")))
  
}


#' Title
#'
#' @param df arg 
#' @param y arg 
#' @param x arg 
#' @param breaks arg 
#' @param color arg 
#' @param label_y arg 
#' @param label_x arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
logit_lty <- \(df,
               y,
               x,
               breaks = 30,
               color = "#0099FF",
               label_y = "Logit(P {y})",
               label_x = x) {
  
  y <- enexpr(y)
  x <- enexpr(x)
  
  lst(data =
        df |> 
          mutate("{x}_cat" := cut(!!x, breaks = 40)) |> 
          summarise("mean_{x}" := mean(!!x),
                    "prop_{y}" := mean(as.numeric(!!y) == 2),
                    logit_prop = 
                      log(get(glue("prop_{y}")) / (1 - get(glue("prop_{y}")))),
                    .by = glue("{x}_cat")) |> 
          filter(!logit_prop %in% c(-Inf, Inf)),
      model =
        glm(data = df |> mutate("{x}_quantile" := cut_number(!!x, n = 4)),
            reformulate(glue("{x}_quantile"), y),
            family = binomial) |>
          tidy(exponentiate = TRUE,
               conf.int = TRUE) |> 
          mutate(p.value = style_pvalue(p.value, digits = 1)) |>
          select(term, estimate, starts_with("conf"), p.value),
      plot = 
        data |> 
          ggplot(aes(y = logit_prop,
                     x = get(glue("mean_{x}")))) +
          geom_point(alpha = 0.4) +
          geom_smooth(method = "lm",
                      formula = "y ~ x",
                      se = FALSE,
                      color = color) +
          labs(y = glue(label_y),
               x = glue(label_x)))
  
}

#' Title
#'
#' @param model arg 
#' @param limit_inf_num arg 
#' @param limit_sup_num arg 
#' @param limit_inf_color arg 
#' @param limit_sup_color arg 
#' @param obs_color arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
cooksd <- \(model,
            limit_inf_num = 4,
            limit_sup_num = 25,
            limit_inf_color = "#0099FF",
            limit_sup_color = "#FF0000",
            obs_color = "#000") {
    
  .out <-
  list(n = "{nrow(obs$inf)} total out. for {nrow(data)} total obs.",
       p = "({label_p()(nrow(obs$inf) / nrow(data))})")
  
  .list <-
  lst(data =
        model |>
          augment() |> 
          rownames_to_column("id"),
      limit =
        c(inf = limit_inf_num, 
          sup = limit_sup_num) |> 
          map_dbl(~ . / nrow(data)),
      outliers =
        limit |> 
          map(~ data |> 
                filter(.cooksd > .) |>
                pull(id)),
      obs =
        map(outliers, ~ data[., ]),
      plot =
        data |> 
          ggplot() +
          aes(x = as.numeric(id),
              y = .cooksd) +
          geom_jitter(color = obs_color,
                      alpha = 0.4) +
          geom_hline(yintercept = limit,
                     color = 
                       c(limit_inf_color,
                         limit_sup_color),
                     linewidth = 0.8) +
          annotate(geom = "label",
                   label = glue(.out$n, .out$p),
                   y = max(data$.cooksd),
                   x = 1,
                   size = 3,
                   hjust = 0,
                   vjust = 1) +
          xlab(NULL) +
          theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()))
    
  .list[names(.list) != "outliers"]
  
}


#' Title
#'
#' @param data arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
flow_filter <- \(data, ...) {
  
  .exprs <- exprs(...)
  
  if (!is_named(.exprs)) .exprs <- set_names(.exprs)
  
  .data <- if ("tbl_conn" %in% class(data)) collect(data) else data
  
  .flow <-
  .exprs |>
    accumulate(~ filter(.x, !!.y), .init = .data) |>
    map(~ glue("{nrow(.)} ({label_p()(nrow(.) / nrow(.data))})"))
  
  list(data = .data |> filter(!!!unname(.exprs)),
       flow = .flow)
  
}


#' Title
#'
#' @param accuracy arg 
#' @param scale arg 
#' @param prefix arg 
#' @param suffix arg 
#' @param big.mark arg 
#' @param decimal.mark arg 
#' @param trim arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
label_p <- \(accuracy = 0.1, 
             scale = 100, 
             prefix = "", 
             suffix = "%", 
             big.mark = " ",
             decimal.mark = getOption("OutDec"),
             trim = TRUE,
             ...) {
  
  number_format(accuracy = accuracy, 
                scale = scale, 
                prefix = prefix,
                suffix = suffix,
                big.mark = big.mark, 
                decimal.mark = decimal.mark, 
                trim = trim, 
                ...)
  
}

#' Title
#'
#' @param x arg
#' @param to_hash arg
#' @param to_hide arg
#' @param hash_trunc arg
#' @param hide_pattern arg
#'
#' @returns arg
#' @export
#'
#' @examples arg
#' 
easy_ano <- \(x,
              to_hash = NULL,
              to_hide = NULL,
              hash_trunc = 25,
              hide_pattern = "---") {

  .ano_hash_fun <- \(x_hash, to_hash) {

    hash_trunc <- as.character(hash_trunc)

    x_hash |>
      mutate("{to_hash}" :=
               get(to_hash) |>
               rlang::hash() |>
               str_remove_all(glue(".{{{hash_trunc}}}$")),
             .by = all_of(to_hash))

  }

  .ano_hide_fun <- \(x_hide) {

    x_hide |> mutate(across(matches(to_hide), ~ hide_pattern))

  }

  if (!is.null(to_hash)) {

    .ano_data <-
    names(x) |>
      str_subset(to_hash |> paste(collapse = "|")) |>
      reduce(.ano_hash_fun, .init = x)

    if (!is.null(to_hide)) {

      .ano_data <- .ano_hide_fun(.ano_data)

    }

  } else if (!is.null(to_hide)) {

    .ano_data <- .ano_hide_fun(x)

  } else {

    .ano_data <- x

  }

  return(.ano_data)

}

#' Title
#'
#' @param x arg
#' @param sheet arg
#' @param ... arg
#' @param data arg
#' @param width arg
#' @param halign arg
#' @param font_size arg
#' @param font_color arg
#' @param concept_var arg
#' @param concept_color arg
#' @param text_var arg
#' @param text_color arg
#' @param border_color arg
#' @param border_type arg
#'
#' @returns arg
#' @export
#'
#' @examples arg
#' 
wb_add_custom <- \(x,
                   sheet,
                   ...,
                   data,
                   width = "auto",
                   halign = "center",
                   font_size = 8,
                   font_color = "#222222",
                   concept_var = "concept",
                   concept_color = NULL,
                   text_var = with(config, text),
                   text_color = NULL,
                   border_color = "#999999",
                   border_type = "thin") {

  .xlsx_output <-
  x |>
    wb_add_worksheet(sheet = sheet,
                     zoom = 105,
                     ...) |>
    wb_add_data_table(x = data,
                      na.strings = NULL) |>
    wb_add_font(dims = wb_dims(x = data, select = "col_names"),
                size = font_size + 1,
                bold = TRUE) |>
    wb_add_font(dims = wb_dims(x = data, select = "data"),
                size = font_size) |>
    wb_add_fill(dims = wb_dims(x = data, select = "col_names"),
                color = wb_color("grey90")) |>
    wb_set_col_widths(cols = 1:ncol(data), widths = width) |>
    wb_add_cell_style(dims = wb_dims(x = data),
                      horizontal = halign,
                      vertical = "center",
                      wrap_text = TRUE) |>
    wb_add_border(dims = wb_dims(x = data),
                  top_color = wb_color(border_color),
                  top_border = border_type,
                  bottom_color = wb_color(border_color),
                  bottom_border = border_type,
                  left_color = wb_color(border_color),
                  left_border = border_type,
                  right_color = wb_color(border_color),
                  right_border = border_type,
                  inner_hcolor = wb_color(border_color),
                  inner_hgrid = border_type,
                  inner_vcolor = wb_color(border_color),
                  inner_vgrid = border_type)

  .add_font <- \(wb, vars, color) {

    wb_add_font(wb = wb,
                dims =
                  wb_dims(x = data,
                          cols = vars,
                          select = "data"),
                color = wb_color(color),
                size = font_size,
                bold = TRUE)

  }

  if (!is.null(concept_color)) {

    .xlsx_output <-
    .add_font(.xlsx_output,
              concept_var,
              concept_color)

  }

  if (!is.null(text_color)) {

    .xlsx_output <-
    .add_font(.xlsx_output,
              text_var,
              text_color)

  }

  return(.xlsx_output)

}
