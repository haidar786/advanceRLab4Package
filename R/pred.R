#' pred function shows fitted values in linreg
#' @param data is a data frame
#' @export
#'

pred.linreg <- function(data, ...) {
  fitValue <- data$fittedValues
  return(fitValue)
}
