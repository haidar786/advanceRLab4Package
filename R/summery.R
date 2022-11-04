#' summary function shows summary of linreg
#' @param data is a data frame
#' @importFrom stats median var
#' @export
#'

summaryA <- function(data) {
  stars <- function(value) {
    switch (value < 0.001, return("***"))
    switch (value < 0.01, return("**"))
    switch (value < 0.05, return("*"))
    switch (value < 0.1, return("."))
    return(" ")
  }
  e <- as.vector(data$regressionsCoefficients)
  se <- as.vector(sqrt(abs(var(data$varianceRegressionCoefficients)) / length(data$varianceRegressionCoefficients)))
  tValues <- as.vector(data$tValuesForEachCoefficient)
  pValues <- as.vector(data$pValues)
  starsValues <- sapply(data$pValues, stars)
  roundedValues <- round(c(e, e, e, se, tValues, pValues), 5)
  roundedValues <- append(roundedValues, starsValues)
  df <- matrix(roundedValues, ncol = 5)
  colnames(df) <- c("Estimate", "Std. Error", "t-values", "p-values", "")
  mat <- matrix(e)
  mat <- cbind(mat, sqrt(as.vector(diag(data$varianceRegressionCoefficients))))
  mat <- cbind(mat, diag(as.matrix(data$tValuesForEachCoefficient)))
  mat <- cbind(mat, c(c(),c(), c()))
  print(as.data.frame(mat))
  # cat("Call: \n")
  # print(data$call)
  # cat("\n")
  # cat("Coefficients: \n")
  # print(df)
  # cat("---\n")
  # cat("Residual standard error:", se,"on", data$degreesOfFreedom,"degrees of freedom")
}


data("iris")
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$summary()
