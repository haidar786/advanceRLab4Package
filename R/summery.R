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
  pValues <- as.vector(diag(data$pValues))
  mat <- matrix(e)
  se <- sqrt(as.vector(diag(data$varianceRegressionCoefficients)))
  mat <- cbind(mat, se)
  mat <- cbind(mat, diag(as.matrix(data$tValuesForEachCoefficient)))
  mat <- round(mat, 5)
  mat <- cbind(mat, pValues)
  mat <- cbind(mat, sapply(pValues, stars))
  colnames(mat) <- c("Estimate", "Std. Error", "t-values", "p-values", " ")
  matDF <- as.data.frame(mat)
  cat("Call: \n")
  cat("linreg(formula = ", paste0(data$call), ", data =", data$dataA,")")
  cat("\n\n")
  cat("Coefficients: \n")
  print(matDF)
  cat("Residual standard error:", sqrt(data$residualVariance),"on", data$degreesOfFreedom,"degrees of freedom")
}
