
linreq <- function(formula, data) {
  modelMatrixX <- model.matrix(formula, data)
  modelMatrixY <- data[all.vars(formula)[1]]
  head(modelMatrix)

  regressionsCoefficients <- (inv(t(modelMatrixX) %*% modelMatrixX)) %*% (t(modelMatrixX) %*% modelMatrixY)

  fittedValues <- modelMatrixX %*% regressionsCoefficients

  residuals <- modelMatrixY - fittedValues == modelMatrixY - (modelMatrixX / regressionsCoefficients)

  degreesOfFreedom <- nrow(data) - ncol(modelMatrixX)

  residualVariance <- (t(residuals) %*% residuals) / degreesOfFreedom

  varianceRegressionCoefficients <- residualVariance %*% (inv(t(modelMatrixX) %*% modelMatrixX))

  tValuesForEachCoefficient <- regressionsCoefficients / (sqrt(varianceRegressionCoefficients))

  pValues <- pt(tValuesForEachCoefficient, degreesOfFreedom)


  LinreqClass <- setRefClass("linreq", fields = list( "regressionsCoefficients" = regressionsCoefficients,
                                                      "fittedValues" = fittedValues,
                                                      "residuals" = residuals,
                                                      "degreesOfFreedom" = degreesOfFreedom,
                                                      "residualVariance" = residualVariance,
                                                      "varianceRegressionCoefficients" = varianceRegressionCoefficients,
                                                      "tValuesForEachCoefficient" = tValuesForEachCoefficient,
                                                      "pValues" = pValues))
  a <- LinreqClass$new(balance = 100)
  return(a)
}
