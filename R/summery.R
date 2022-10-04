

summary <- function(data, ...) {
  stars <- function(value) {
    if (value > 0.1) return("*")
    if (value > 0.01) return("**")
    if (value > 0.001) return("***")
    return(" ")
  }
  df <- data.frame(matrix(nrow = 0, ncol = 5))
  names(df) <- c("Estimate", "Std-Error", "t-values", "p-values", " ")
  df$Estimate <- round(as.vector(data$regressionsCoefficients), 5)
  df$Std-Error <- round(as.vector(sqrt(data$varianceRegressionCoefficients)), 5)
  df$t-values <- round(as.vector(data$tValuesForEachCoefficient), 5)
  df$p-values <- round(as.vector(data$pValues))
  df$
  print(df)
}

data(iris)
mod_object <- lm(Petal.Length~Species, data = iris)
summary(mod_object)
