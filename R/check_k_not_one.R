#' Check If k Is Not Equal To 1 In All Cases
#'
#' This is a helper function needed to plot trajectories of outcomes. If for an
#' outcome (including all measurements) the number of studies (k) per
#' measurement is only 1, it should not be used to plot the results because this
#' plots should only include meta-analytic summary effects.
#'
#' @param data A dataframe with column `k`
#'
#' @importFrom dplyr pull
#'
#' @return Logical
#' @export
check_k_not_one <- function(data) {
  k <- NULL

  n_studies <- data %>%
    pull(k)

  any(n_studies > 1)
}
