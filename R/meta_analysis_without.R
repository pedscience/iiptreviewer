#' Meta-Analysis without a Particular Study
#'
#' Calculate a three-level meta-analysis for a given comparison without a
#' particular study.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom stats pnorm
#'
#' @param data A dataframe containing the effect sizes, CI and analysis numbers
#' @param analysis_number Numeric, the analysis number
#' @param comparison A string, the measurement against "pre" should be compared
#' @param without A string, the study's first author of who the study should be omitted
#' @param ... Additional options
#'
#' @export
meta_analysis_without <- function(data, analysis_number, comparison, without, ...) {
  where <- utils::getFromNamespace("where", "tidyselect")

  a <- analysis <- analysis_time <- i_2_total <- n <- n_1 <- n_2 <-
    n_pairs <- pred <- study_label <- text_string <- NULL

  data <- data %>%
    filter(str_detect(analysis, analysis_number) & analysis_time == comparison, !str_detect(study_label, without)) %>%
    mutate(
      comparison_id = row_number(),
      n = if_else(is.na(n_pairs), n_1 + n_2, n_pairs)
    )

  results <- data %>%
    calc_meta_analysis(...)

  n_participants <- data %>%
    summarise(n = sum(n))

  ext_meta_data(results) %>%
    mutate(
      a = pnorm(pred / sqrt(2)),
      a = round(a, 4) * 100
    ) %>%
    bind_cols(n_participants) %>%
    mutate(
      i_2_total = i_2_total * 100,
      across(where(is.numeric), round, 2),
      text_string = str_glue("g = { pred } 95%-CI [{ ci_lb }, { ci_ub }],  95%-PI [{ cr_lb }, {cr_ub}], k = { nrow(data) }, n = { n }, I2total = { i_2_total }%, A = { a }%")
    ) %>%
    pull(text_string)
}
