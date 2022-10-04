library(ggplot2)

plot <- function(data, ...) {
  res <- data$residuals
  fittedV <- data$fittedValues
  print(res)
  print(fittedV)
}

a <-lm(formula = Petal.Length ~ Species, data = iris)
plot(a)
