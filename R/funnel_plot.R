#' Funnel Plot
#'
#' Funnel plots based on a dataframe.
#'
#' @importFrom stats qnorm
#' @importFrom ggplot2 geom_vline geom_ribbon scale_fill_grey guides guide_legend coord_cartesian ggtitle
#'
#' @param data A dataframe containing the effect sizes
#' @param plot_title A string for the plot title
#' @param refline Numeric, position of reference line (defaults to 0)
#' @param alpha Numeric, transparency value (between 0 an 1, defaults to 0.4)
#' @param percent_expansion Numeric, value for overplotting to avoid clipping
#' @param ci_resolution Numeric, number of points to plot confidence interval
#'
#' @export
funnel_plot <- function(data, plot_title = NA, refline = 0, alpha = 0.4, percent_expansion = 0.05, ci_resolution = 1000) {
  g <- inverse_se <- left_border_1 <- left_border_10 <- left_border_5 <- right_border_1 <- right_border_10 <- right_border_5 <-
    se_g <- study_label <- y_values <- NULL

  # Select only used variables
  plotting_data <- data %>%
    select(study_label, g, se_g) %>%
    mutate(inverse_se = 1 / se_g)

  # Calculate y-limits for plotting
  vec_inverse_se <- plotting_data %>% pull(inverse_se)
  vec_effect_size <- plotting_data %>% pull(g)
  y_upper_limit <- min(vec_inverse_se, 1)
  y_lower_limit <- max(vec_inverse_se)
  x_lower_limit <- min(refline - qnorm((1 - 0.99)/2, lower.tail=FALSE) * sqrt(1 / y_upper_limit^2), vec_effect_size, -5)
  x_upper_limit <- max(refline + qnorm((1 - 0.99)/2, lower.tail=FALSE) * sqrt(1 / y_upper_limit^2), vec_effect_size, 5)

  # Optimize plotting by adding a few percent to the limits
  y_limit_difference <- y_lower_limit - y_upper_limit
  x_limit_difference <- x_upper_limit - x_lower_limit
  y_upper_limit <- y_upper_limit - percent_expansion * y_limit_difference
  y_lower_limit <- y_lower_limit + percent_expansion * y_limit_difference
  x_lower_limit <- x_lower_limit - percent_expansion * x_limit_difference
  x_upper_limit <- x_upper_limit + percent_expansion * x_limit_difference

  # Calculate Confidence Interval
  ci_data <- tibble(
    y_values        = seq(y_upper_limit, y_lower_limit, length.out = ci_resolution),
    left_border_10  = refline - qnorm(0.1 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2),
    right_border_10 = refline + qnorm(0.1 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2),
    left_border_5   = refline - qnorm(0.05 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2),
    right_border_5  = refline + qnorm(0.05 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2),
    left_border_1   = refline - qnorm(0.01 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2),
    right_border_1  = refline + qnorm(0.01 / 2, lower.tail = FALSE) * sqrt(1 / y_values^2)
  )

  funnel_plot <- plotting_data %>%
    ggplot(aes(g, inverse_se)) +
    geom_vline(xintercept = refline) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = -Inf,            xmax = left_border_1,  fill = "p < .01"), alpha = alpha) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = right_border_1,  xmax = Inf,            fill = "p < .01"),show.legend = FALSE, alpha = alpha) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = left_border_1,   xmax = left_border_5,  fill = "p < .05"),show.legend = FALSE, alpha = alpha) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = right_border_1,  xmax = right_border_5, fill = "p < .05"),show.legend = FALSE, alpha = alpha) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = left_border_5,   xmax = left_border_10, fill = "p < .10"),show.legend = FALSE, alpha = alpha) +
    geom_ribbon(data = ci_data, aes(x = NA_real_, y = y_values, xmin = right_border_10, xmax = right_border_5, fill = "p < .10"),show.legend = FALSE, alpha = alpha) +
    geom_point() +
    scale_fill_grey() +
    guides(fill = guide_legend(override.aes = list(alpha = alpha))) +
    expand_limits(y = 1) +
    coord_cartesian(xlim = c(-4, 4), expand = FALSE) +
    labs(x = "Effect Size", y = "1 / SE", fill = "Region of\nSignificance")  +
    theme(panel.grid = element_blank())

  if (is.na(plot_title)) {
    funnel_plot
  } else {
    if (!is.character(plot_title)) stop("The plot title must be a string of characters. Please provide another title.")
    funnel_plot + ggtitle(label = plot_title)
  }
}
