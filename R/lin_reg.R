lm <- function(formula, data) {
  modelMatrixX <- as.matrix(model.matrix(formula, data))
  modelMatrixY <- as.matrix(data[all.vars(formula)[1]])

  regressionsCoefficients <- (solve(t(modelMatrixX) %*% modelMatrixX)) %*% (t(modelMatrixX) %*% modelMatrixY)

  fittedValues <- as.vector(modelMatrixX %*% regressionsCoefficients)

  residuals <- modelMatrixY - fittedValues

  degreesOfFreedom <- nrow(data) - ncol(modelMatrixX)

  residualVariance <- as.numeric((t(residuals) %*% residuals) / degreesOfFreedom)

  varianceRegressionCoefficients <- as.numeric(residualVariance) * (solve(t(modelMatrixX) %*% modelMatrixX))

  isPositive <- function(x) x>=0

  tValuesForEachCoefficient <- as.vector(regressionsCoefficients) / (sqrt(isPositive(varianceRegressionCoefficients)))

  pValues <- pt(-abs(tValuesForEachCoefficient), degreesOfFreedom)

  call <- match.call()

  linreg <- setRefClass("linreg", fields =  list("regressionsCoefficients" = "matrix",
                                                      "fittedValues" = "numeric",
                                                      "residuals" = "matrix",
                                                      "degreesOfFreedom" = "numeric",
                                                      "residualVariance" = "numeric",
                                                      "varianceRegressionCoefficients" = "matrix",
                                                      "tValuesForEachCoefficient" = "matrix",
                                                      "pValues" = "matrix",
                                                      "call" = "call"))
  lr <- linreg$new(regressionsCoefficients = regressionsCoefficients,
                        fittedValues = fittedValues,
                        residuals = residuals,
                        degreesOfFreedom = degreesOfFreedom,
                        residualVariance = residualVariance,
                        varianceRegressionCoefficients = varianceRegressionCoefficients,
                        tValuesForEachCoefficient = tValuesForEachCoefficient,
                        pValues = pValues,
                        call = call)
  return(lr)
}


