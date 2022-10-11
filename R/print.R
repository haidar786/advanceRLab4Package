#' print function shows linear regression coefficients
#' @param data is a data frame
#' @export
#'

printA <- function(data) {
  cat("Call:\n")
  print(paste0("linreg(formula = ", format(data$call), ", data = ", data$dataA, ")"))
  cat("\nCoefficients:\n")
  print(data$regressionsCoefficients)
}
#
# linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
# printA(linreg_mod)
