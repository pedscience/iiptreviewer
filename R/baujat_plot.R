#' Baujat Plots
#'
#' Baujat plots to identify potential study outliers.
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats resid
#'
#' @inheritParams metafor::baujat.rma
#' @param plot_title A string, the plot title
#'
#' @export
#'
#' @source \url{https://github.com/cran/metafor/blob/master/R/baujat.rma.r}
baujat_plot <- function(x, xlim, ylim, xlab, ylab, cex, symbol, grid=TRUE, progbar=FALSE, plot_title, ...) {
  x_values <- y_values <- study_label <- NULL

  ### grid argument can either be a logical or a color

  if (is.logical(grid))
    gridcol <- "lightgray"
  if (is.character(grid)) {
    gridcol <- grid
    grid <- TRUE
  }

  #########################################################################

  ### set up vectors to store results in

  delpred  <- rep(NA_real_, x$k.f)
  vdelpred <- rep(NA_real_, x$k.f)

  ### predicted values under the full model

  pred.full <- x$X.f %*% x$beta

  ### note: skipping NA cases
  ### also: it is possible that model fitting fails, so that generates more NAs (these NAs will always be shown in output)

  if (progbar)
    pbar <- txtProgressBar(min=0, max=x$k.f, style=3)

  for (i in seq_len(x$k.f)) {

    if (progbar)
      setTxtProgressBar(pbar, i)

    if (!x$not.na[i])
      next

    if (inherits(x, "rma.uni"))
      res <- try(suppressWarnings(rma.uni(x$yi.f, x$vi.f, weights=x$weights.f, mods=x$X.f, intercept=FALSE, method=x$method, weighted=x$weighted, test=x$test, tau2=ifelse(x$tau2.fix, x$tau2, NA), control=x$control, subset=-i)), silent=TRUE)

    if (inherits(res, "try-error"))
      next

    ### removing an observation could lead to a model coefficient becoming inestimable (for 'rma.uni' objects)

    if (any(res$coef.na))
      next

    Xi          <- matrix(x$X.f[i,], nrow=1)
    delpred[i]  <- Xi %*% res$beta
    vdelpred[i] <- Xi %*% tcrossprod(res$vb,Xi)

  }

  if (progbar)
    close(pbar)

  yhati <- (delpred - pred.full)^2 / vdelpred

  #########################################################################

  ### x-axis values (use 'na.pass' to make sure we get a vector of length k.f)

  options(na.action = "na.pass")
  xhati <- 1/(x$tau2.f + x$vi.f) * resid(x)^2
  options(na.action = "na.omit")

  #########################################################################

  ### set some defaults (if not specified)
  tibble(
    study_label = x$slab,
    x_values = xhati,
    y_values = yhati[, 1]
  ) %>%
    ggplot(aes(x_values, y_values, label = study_label)) +
    geom_point() +
    geom_text_repel() +
    expand_limits(x = c(0, 8), y = c(0, 8)) +
    labs(x = "Squared Pearson Residual", y = "Influence on Overall Result", title = plot_title)
}
