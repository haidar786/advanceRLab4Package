#' coef function shows regression coefficients in linreg
#' @param data is a data frame
#' @export
#'

coef.linreg <- function(data, ...) {
  return(data$regressionsCoefficients)
}
