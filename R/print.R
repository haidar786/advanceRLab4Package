#' print function shows linear regression coefficients
#' @param data is a data frame
#' @export
#'

printA <- function(data) {
  cat("Call:\n")
  print(paste0("linreg(formula = ", format(data$call), ", data = ", data$dataA, ")"))
  cat("\nCoefficients:\n")
  print(as.data.frame(t(data$regressionsCoefficients)))
}
