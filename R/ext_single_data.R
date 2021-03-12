#' Extract estimates for single study comparisons
#'
#' @param data A dataframe containing the effect size with its associated SE
#'
#' @importFrom dplyr select mutate
#'
#' @export
ext_single_data <- function(data) {
  g <- se_g <- pred <- se <- NULL

  data %>%
    select(pred = g, se = se_g) %>%
    mutate(
      ci_lb = pred - 1.96 * se,
      ci_ub = pred + 1.96 * se
    )
}
