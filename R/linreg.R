linreg <- setRefClass("linreg",fields = list(regressionsCoefficients = "matrix",
                                    fittedValues = "numeric",
                                    residuals = "matrix",
                                    degreesOfFreedom = "numeric",
                                    residualVariance = "numeric",
                                    varianceRegressionCoefficients = "matrix",
                                    tValuesForEachCoefficient = "matrix",
                                    pValues = "matrix",
                                    call = "call"),
                      methods = list(
                                      initialize = function(formula, data) {
                                        modelMatrixX <- as.matrix(model.matrix(formula, data))
                                        modelMatrixY <- as.matrix(data[all.vars(formula)[1]])

                                        .self$regressionsCoefficients <- (solve(t(modelMatrixX) %*% modelMatrixX)) %*% (t(modelMatrixX) %*% modelMatrixY)

                                        .self$fittedValues <- as.vector(modelMatrixX %*% regressionsCoefficients)

                                        .self$residuals <- modelMatrixY - fittedValues

                                        .self$degreesOfFreedom <- nrow(data) - ncol(modelMatrixX)

                                        .self$residualVariance <- as.numeric((t(residuals) %*% residuals) / degreesOfFreedom)

                                        .self$varianceRegressionCoefficients <- as.numeric(residualVariance) * (solve(t(modelMatrixX) %*% modelMatrixX))

                                        isPositive <- function(x) x>=0

                                        .self$tValuesForEachCoefficient <- as.vector(regressionsCoefficients) / (sqrt(isPositive(varianceRegressionCoefficients)))

                                        .self$pValues <- pt(-abs(tValuesForEachCoefficient), degreesOfFreedom)

                                        .self$call <- match.call()
                                      },
                                      print = function() {
                                       print.linreg(.self)
                                      },
                                      pred = function() {
                                        res <- pred.linreg(.self)
                                        return(res)
                                      },
                                      plot = function() {
                                        plot.linreg(.self)
                                      },
                                      coef = function() {
                                        coef.linreg(.self)
                                      },
                                      resid = function() {
                                        resid.linreg(.self)
                                      },
                                      summary = function() {
                                        summary.linreg(.self)
                                      }
                                    ))

