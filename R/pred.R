#' pred function shows fitted values in linreg
#' @param data is a data frame
#' @return thios function returns fitted value
#' @export
#'

predA <- function(data, ...) {
  fitValue <- data$fittedValues
  return(fitValue)
}
