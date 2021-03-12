#' Calculate I Squared
#'
#' Calculate I squared for each level of a three-level meta-analysis
#'
#' @param rma_object Object of class `rma.mv` modeling a three-level meta-analysis
#'
#' @return A [tibble][tibble::tibble-package] with 1 row and 4 columns
#' @export
#'
#' @source \url{https://github.com/MathiasHarrer/dmetar/blob/master/R/mlm.variance.distribution.R}
ext_i_square <- function(rma_object) {
  # Get variance diagonal and calculate total variance
  n                 <- rma_object$k.eff
  vector.inv.var    <- 1 / (diag(rma_object$V))
  sum.inv.var       <- sum(vector.inv.var)
  sum.sq.inv.var    <- (sum.inv.var)^2
  vector.inv.var.sq <- 1 / (diag(rma_object$V)^2)
  sum.inv.var.sq    <- sum(vector.inv.var.sq)
  num               <- (n - 1) * sum.inv.var
  den               <- sum.sq.inv.var - sum.inv.var.sq
  est.samp.var      <- num / den

  # Calculate variance proportions
  level1 <- ((est.samp.var) / (rma_object$sigma2[1] + rma_object$sigma2[2] + est.samp.var))
  level2 <- ((rma_object$sigma2[2]) / (rma_object$sigma2[1] + rma_object$sigma2[2] + est.samp.var))
  level3 <- ((rma_object$sigma2[1]) / (rma_object$sigma2[1] + rma_object$sigma2[2] + est.samp.var))

  tibble(
    i_2_1 = level1,
    i_2_2 = level2,
    i_2_3 = level3,
    i_2_total = sum(level2, level3) %>% round(4)
  )
}
