#' print function shows linear regression coefficients
#' @param data is a data frame
#' @export
#'

print.linreg <- function(data, ...) {
  cat("Call:\n")
  print(data$call)
  cat("\nCoefficients:\n")
  print(data$regressionsCoefficients)
}
