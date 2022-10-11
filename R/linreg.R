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
                                    call = "call"),
                      methods = list(
                                      #' initialize function initialize values
                                      #' @param formula is a formula given as an input
                                      #' @param data is a data frame
                                      #' @export
                                      #'
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
                                      #' print function shows linear regression coefficients
                                      #' @export
                                      #'
                                      print = function() {
                                       printA(.self)
                                      },
                                      #' pred function shows fitted values in linear regression
                                      #' @export
                                      #'
                                      pred = function() {
                                        res <- predA(.self)
                                        return(res)
                                      },
                                      #' plot function shows graphical representation of data
                                      #' @export
                                      #'
                                      plot = function() {
                                        plotA(.self)
                                      },
                                      #' coef function return regression coefficient in linear regression coefficients
                                      #' @export
                                      #'
                                      coef = function() {
                                        coefA(.self)
                                      },
                                      #' resid function returns residual
                                      #' @export
                                      #'
                                      resid = function() {
                                        residA(.self)
                                      },
                                      #' summary function shows summary of linear regression
                                      #' @export
                                      #'
                                      summary = function() {
                                        summaryA(.self)
                                      }
                                    ))

