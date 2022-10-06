printA <- function(data, ...) {
  cat("Call:\n")
  print(data$call)
  cat("\nCoefficients:\n")
  print(data$regressionsCoefficients)
}

data(iris)
mod_object <- linreg(Petal.Length~Species, data = iris)
printA(mod_object)
