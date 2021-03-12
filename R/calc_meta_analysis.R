#' Calculate a Three-Level Meta-Analysis
#'
#' Calculate the three-level meta-analysis with restricted maximum-likelihood and
#' Knapp-Hartung adjustement for confidence intervals. In some cases, there is
#' only one `study_id` or `intervention_id`. In this case, the function would throw a
#' message, which is suppressed as we know what is happening there.
#'
#' @importFrom metafor rma.mv
#'
#' @param data A data frame containing data for a meta-analysis
#' @param ... Additional options passed to [metafor::rma.mv()]
#'
#' @return An object of class `rma.mv`
#' @export
calc_meta_analysis <- function(data, ...) {
  g <- var_g <- comparison_id <- . <- NULL

  data %>%
    rma.mv(yi = g, V = var_g, random = ~ 1 | intervention_id/study_id, tdist = TRUE, slab = comparison_id, method = "REML", data = ., ...) %>%
    suppressWarnings()
}
