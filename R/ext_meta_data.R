#' Extract Meta-Analysis Estimates
#'
#' Extract meta-analysis estimates (I squared, CI, and PI) from an `rma.mv` object.
#'
#' @importFrom metafor `predict.rma`
#' @importFrom dplyr as_tibble bind_cols
#' @importFrom janitor clean_names
#'
#' @param rma_object An object of class `rma.mv` modelling a three-level meta-analysis
#'
#' @export
ext_meta_data <- function(rma_object) {
  estimates <- rma_object %>%
    predict.rma() %>%
    as_tibble() %>%
    clean_names()

  heterogeneity <- ext_i_square(rma_object)

  bind_cols(estimates, heterogeneity)
}
