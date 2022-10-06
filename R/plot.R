library(ggplot2)

plot.linreg <- function(data, ...) {
  stR <- sqrt(abs(data$residuals / sqrt(abs(data$residualVariance))))
  df <- data.frame(data$residuals, data$fittedValues, stR)
  plot1 <- ggplot(df, aes(x = data.fittedValues, y = Petal.Length)) +
    geom_point(shape = 1) +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values", y = "Residuals") +
    stat_summary(fun = median, color = "red", geom = "line")

  plot2 <- ggplot(df, aes(x = data.fittedValues, y = Petal.Length.1)) +     geom_point(shape = 1) +
    labs(title = "Scale-Location",
         x = "Fitted Values",
         y = expression(sqrt(Standardized~residuals))) +
    stat_summary(fun = mean, color = "red", geom = "line")

  cowplot::plot_grid(plot1, plot2, ncol = 1, nrow = 2)
}
