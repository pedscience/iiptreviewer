#' Plot Trajectory for Logan et al. Study
#'
#' In the Logan et al. (2015) Study, the follow-up ranged from 1 - 3 months, so
#' their results can not be plotted together with all other reults.
#'
#' @param data A dataframe containing the effect size and its associated CI
#' @param analysis_title A string for the plot title
#'
#' @export
plot_trajectory_logan <- function(data, analysis_title) {
  analysis_time <- ci_lb <- ci_ub <- i_2_total <- k <- pred <- NULL

  data %>%
    ggplot(aes(analysis_time, pred)) +
    geom_line(aes(group = 1), size = 1) +
    geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), width = 0.1, size = 1) +
    geom_point(aes(size = k, color = i_2_total)) +
    geom_hline(yintercept = 0) +
    scale_x_discrete(limits = c("post", "fu", "fu4", "fu6", "fu12"), labels = c("Post", "FU 1-3", "FU 4", "FU 6", "FU 12")) +
    scale_size_identity(breaks = c(1, 3, 6, 9), guide = "legend") +
    scale_color_viridis_c(option = "A", limits = c(0, 1), na.value = "grey80", labels = scales::percent) +
    expand_limits(y = c(-3, 3)) +
    labs(title = analysis_title, x = "Comparison", y = "*g* \u00b1 95%-CI", color = "<i>I</i><sup>2</sup><sub>total</sub>", size = "*k*") +
    theme(panel.grid = element_blank(),
          legend.title = element_markdown(),
          axis.title.y = element_markdown())
}
