
#' resid function shows residual in linreg
#' @param data is a data frame
#' @export
#'

resid.linreg <- function(data, ...) {
  rd <- data$residuals
  return(rd)
}
