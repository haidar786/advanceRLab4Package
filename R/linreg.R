#' Advance r lab 4 using rc class related to linear regression
#' @field regressionsCoefficients a linear regression coefficient of linreg
#' @field fittedValues a fitted value of linreg
#' @field residuals a residual of linreg
#' @field degreesOfFreedom a degree of freedom of linreg
#' @field residualVariance a residual variance of linreg
#' @field varianceRegressionCoefficients a variance Regression Coefficients of linreg
#' @field tValuesForEachCoefficient a t Values For Each Coefficient
#' @field pValues a p Values of linreg
#' @field call is a call of function name
#' @description Linear regressions functions to calculate some functions
#' @return class
#' @import methods
#' @export
#'

linreg <- setRefClass("linreg",fields = list(regressionsCoefficients = "matrix",
                                    fittedValues = "numeric",
                                    residuals = "matrix",
                                    degreesOfFreedom = "numeric",
                                    residualVariance = "numeric",
                                    varianceRegressionCoefficients = "matrix",
                                    tValuesForEachCoefficient = "matrix",
                                    pValues = "matrix",
                                    call = "formula",
                                    dataA = "character"),
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

                                        .self$tValuesForEachCoefficient <- as.vector(regressionsCoefficients) / (sqrt(varianceRegressionCoefficients))

                                        .self$pValues <- pt(-abs(tValuesForEachCoefficient), degreesOfFreedom)

                                        .self$call <- formula

                                        .self$dataA <- deparse(substitute(data))
                                      },

                                      print = function() {
                                       printA(.self)
                                      },

                                      pred = function() {
                                        res <- predA(.self)
                                        return(res)
                                      },

                                      plot = function() {
                                        plotA(.self)
                                      },

                                      coef = function() {
                                        coefA(.self)
                                      },

                                      resid = function() {
                                        residA(.self)
                                      },

                                      summary = function() {
                                        summaryA(.self)
                                      }
                                    ))

