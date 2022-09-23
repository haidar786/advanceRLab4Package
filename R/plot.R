library(ggplot2)

plot <- function(data, ...) {
  df <- data.frame(data$residuals, data$fittedValues)
  ggplot(df, aes(x = df[,1], y = df[,2]))
}

lm<-linreg(formula = Petal.Length ~ Species, data = iris)
plot(lm)
