#' resid function shows residual in linreg
#' @param data is a data frame
#' @return this function returns residual
#' @export
#'

resid.linreg <- function(data, ...) {
  rd <- data$residuals
  return(rd)
}
