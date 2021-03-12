#' Forest Plots
#'
#' Forst plots for given data and rma object.
#'
#' @importFrom metafor weights.rma.mv
#' @importFrom benelib to_tibble
#' @importFrom dplyr rename left_join select arrange summarise if_else starts_with desc across
#' @importFrom broom tidy glance
#' @importFrom stringr str_glue
#' @importFrom ggtext geom_richtext
#' @importFrom ggplot2 geom_text annotate geom_errorbarh geom_segment scale_size_continuous theme_void unit element_line element_text
#' @importFrom patchwork plot_layout plot_annotation
#'
#' @param data A dataframe containing the data
#' @param rma_object An object of class `rma.mv`
#' @param plot_title A string containing the plot title
#'
#' @export
forest_plot <- function(data, rma_object, plot_title = NA) {
  where <- utils::getFromNamespace("where", "tidyselect")

  ci_lb <- ci_lower <- ci_ub <- ci_upper <- cochran.qe <- column_label <- correlation <- cr_lb <- cr_ub <-
    g <- i_2_2 <- i_2_3 <- id <- instrument <- intervention_id <- justification <- mean_1 <- mean_2 <- n <-
    n_1 <- n_2 <- n_pairs <- overall_y <- p.value <- p.value.cochran.qe <- pred <- prediction_y <- sd_1 <-
    sd_2 <- se_g <- statistic <- study_label <- study_weight <- weight_percent <- x_position <- year <- NULL

  # Process the data for plotting, i.e. join weights to original data, add n for
  # between-subjects effect sizes, calculate study confidence intervals, select
  # only relevant variables and add an id for plotting
  weights <- rma_object %>%
    weights.rma.mv() %>%
    to_tibble("comparison_id") %>%
    rename(study_weight = ".")

  preprocessed_data <- data %>%
    left_join(weights, by = "comparison_id") %>%
    mutate(
      n = if_else(is.na(n_pairs), n_1 + n_2, n_pairs),
      ci_lower = g - 1.96 * se_g,
      ci_upper = g + 1.96 * se_g,
      weight_percent = study_weight / sum(study_weight)
    ) %>%
    select(study_label, intervention_id, year, instrument, mean_1:sd_2, n , correlation, g, starts_with("ci_"), weight_percent) %>%
    arrange(desc(intervention_id), desc(year)) %>%
    mutate(id = row_number(),
           across(mean_1:ci_upper, round, 2),
           weight_percent = round(weight_percent, 4))


  # Calculate overall effect, confidence, and prediction interval and generate
  # heterogeneity statistics
  rma_glanced <- rma_object %>%
    glance()

  rma_i_squared <- rma_object %>%
    ext_i_square()

  rma_tidied <- rma_object %>%
    tidy() %>%
    mutate(statistic = round(statistic, 2),
           p.value = ifelse(p.value < .001, "< .001", round(p.value, 3)))

  overall_effect <- rma_object %>%
    predict.rma() %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    rename(g = pred) %>%
    mutate(overall_y = 0,
           prediction_y = -0.5,
           across(where(is.numeric), round, 2))

  sum_n <- preprocessed_data %>%
    summarise(n = sum(n)) %>%
    pull(n)

  overall_string <- str_glue("<b>Overall</b> (<i>t</i>({ rma_glanced$df.residual }) = { rma_tidied$statistic }, <i>p</i> = { rma_tidied$p.value })")

  het_stats <- bind_cols(rma_glanced, rma_i_squared) %>%
    mutate(
      p.value.cochran.qe = ifelse(p.value.cochran.qe < .001, "< .001", round(p.value.cochran.qe, 3)),
      across(c(cochran.qe, i_2_2, i_2_3), round, 2)
    )

  het_string <- str_glue("<i>Q</i>({ het_stats$df.residual }) = { het_stats$cochran.qe },
                         <i>p</i> = { het_stats$p.value.cochran.qe },
                         <i>I</i><sub>2</sub><sup>2</sup> = { het_stats$i_2_2},
                         <i>I</i><sub>3</sub><sup>2</sup> = { het_stats$i_2_3 }")

  prediction_string <- str_glue("<b>Prediction Interval</b> ({ het_string })")


  # Create column labels
  number_of_studies <- nrow(preprocessed_data)
  column_y_position <- number_of_studies + 0.7

  left_headers <- tibble(
    column_label = c("<b>Study</b>", "<b>Site</b>", "<b>Instrument</b>", "<b><i>M</i><sub>1</sub></b>", "<b><i>M</i><sub>2</sub></b>",
                     "<b><i>SD</i><sub>1</sub></b>", "<b><i>SD</i><sub>2</sub></b>", "<b><i>n</i></b>", "<b><i>r</i></b>"),
    x_position = c(1, 1 + 1.5, 1 + 1.5 + 1:7 * 0.6),
    justification = c(0, rep(0.5, 8))
  )

  right_headers <- tibble(
    column_label = c("<b><i>g</i></b>", "<b>95%-CI</b>", "<b>Weight</b>"),
    x_position = 1:3
  )


  # Set y limits for all plots and geom_richtext options
  y_limits <- c(-0.5, column_y_position)
  overall_y_position <- 0
  prediction_y_position <- -0.5


  # Create data table plot (on the left)
  table_left <- preprocessed_data %>%
    ggplot(aes(y = id)) +
    geom_richtext(data = left_headers, aes(label = column_label, x = x_position, hjust = justification), y = column_y_position, label.size = NA, label.padding = unit(0, "lines")) +
    geom_text(aes(label = study_label,                    x = left_headers$x_position[[1]]), hjust = left_headers$justification[[1]]) +
    geom_text(aes(label = intervention_id,                x = left_headers$x_position[[2]])) +
    geom_text(aes(label = instrument,                     x = left_headers$x_position[[3]])) +
    geom_text(aes(label = mean_1,                         x = left_headers$x_position[[4]])) +
    geom_text(aes(label = mean_2,                         x = left_headers$x_position[[5]])) +
    geom_text(aes(label = sd_1,                           x = left_headers$x_position[[6]])) +
    geom_text(aes(label = sd_2,                           x = left_headers$x_position[[7]])) +
    geom_text(aes(label = n,                              x = left_headers$x_position[[8]])) +
    geom_text(aes(label = correlation,                    x = left_headers$x_position[[9]])) +
    annotate(geom = "text", label = sum_n,                x = left_headers$x_position[[8]], y = overall_y_position) +
    geom_richtext(label = overall_string,                 x = left_headers$x_position[[1]], hjust = left_headers$justification[[1]], y = overall_y_position, label.size = NA, label.padding = unit(0, "lines")) +
    geom_richtext(label = prediction_string,              x = left_headers$x_position[[1]], hjust = left_headers$justification[[1]], y = prediction_y_position, label.size = NA, label.padding = unit(0, "lines")) +
    expand_limits(x = 1, y = y_limits) +
    # scale_y_continuous(breaks = c(-0.5, 0:number_of_studies)) +
    labs(x = NULL, y = NULL) +
    theme_void()
  # theme(axis.text.y = element_text())


  # Create SMD and weights table (on the right)
  table_right <- preprocessed_data %>%
    ggplot(aes(y = id)) +
    geom_richtext(data = right_headers, aes(label = column_label, x = x_position), y = column_y_position, label.size = NA, label.padding = unit(0, "lines")) +
    geom_text(aes(label = g,                                        x = right_headers$x_position[[1]])) +
    geom_text(aes(label = str_glue("[{ ci_lower }, { ci_upper }]"), x = right_headers$x_position[[2]])) +
    geom_text(aes(label = str_glue("{ weight_percent * 100 }%"),    x = right_headers$x_position[[3]])) +
    geom_text(data = overall_effect, aes(label = g, y = overall_y), x = right_headers$x_position[[1]]) +
    geom_text(data = overall_effect, aes(label = str_glue("[{ ci_lb }, {ci_ub}]"), y = overall_y), x = right_headers$x_position[[2]]) +
    geom_text(data = overall_effect, aes(label = str_glue("[{ cr_lb }, {cr_ub}]"), y = prediction_y), x = right_headers$x_position[[2]]) +
    expand_limits(x = c(0.8, 3.2), y = y_limits) +
    labs(x = NULL, y = NULL) +
    theme_void()


  # Create forest plot
  forest_plot <- preprocessed_data %>%
    ggplot(aes(g, id)) +
    geom_richtext(label = "<b>Hedges' <i>g</i></b>", x = overall_effect$g, y = column_y_position, label.size = NA, label.padding = unit(0, "lines")) +
    geom_point(aes(size = weight_percent), shape = 15, alpha = 0.4) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.1) +
    geom_point(data = overall_effect, y = overall_y_position, shape = 18, size = 7) +
    geom_errorbarh(data = overall_effect, aes(xmin = ci_lb, xmax = ci_ub, y = overall_y), height = 0.1) +
    geom_errorbarh(data = overall_effect, aes(xmin = cr_lb, xmax = cr_ub, y = prediction_y), height = 0, size = 1.5, color = "darkred") +
    geom_segment(x = 0, xend = 0, y = -1, yend = number_of_studies + 0.3) +
    scale_size_continuous(guide = "none") +
    labs(y = NULL, x = NULL) +
    expand_limits(x = 0, y = y_limits) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(),
          line = element_line(color = "black"),
          text = element_text(color = "black"))


  # Stich them all together
  complete_plot <- table_left + forest_plot + table_right + plot_layout(widths = c(1.5, 1, 0.5))

  if (is.na(plot_title)) {
    complete_plot
  } else {
    if (!is.character(plot_title)) stop("The plot title must be a string of characters. Please provide another title.")
    complete_plot + plot_annotation(title = plot_title)
  }
}
