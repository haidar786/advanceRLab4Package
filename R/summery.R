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
  pValues <- as.vector(data$pValues)
  mat <- matrix(e)
  mat <- cbind(mat, sqrt(as.vector(diag(data$varianceRegressionCoefficients))))
  mat <- cbind(mat, diag(as.matrix(data$tValuesForEachCoefficient)))
  mat <- cbind(mat, pValues)
  mat <- cbind(mat, c("***"))
  colnames(mat) <- c("Estimate", "Std. Error", "t-values", "p-values", " ")
  matDF <- as.data.frame(mat)
  cat("Call: \n")
  cat("linreg(formula = ",as.character(data$call), ")")
  cat("\n")
  cat("Coefficients: \n")
  print(matDF)
  # print(df)
  # cat("---\n")
  # cat("Residual standard error:", se,"on", data$degreesOfFreedom,"degrees of freedom")
}


data("iris")
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$summary()
