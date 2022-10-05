summary <- function(data, ...) {
  stars <- function(value) {
    if (value > 0.1) return("*")
    if (value > 0.01) return("**")
    if (value > 0.001) return("***")
    return(" ")
  }
  estimate <- round(as.numeric(data$regressionsCoefficients), 5)
  se <- round(as.numeric(sqrt(abs(var(data$varianceRegressionCoefficients)) / length(data$varianceRegressionCoefficients))), 5)
  tValues <- round(as.numeric(data$tValuesForEachCoefficient), 5)
  pValues <- round(as.numeric(data$pValues), 5)
  stars <- sapply(data$pValues,
                  function(x) if(x<0.001) {"***"}
                  else if (x<0.01) {"**"}
                  else if (x<0.05) {"*"}
                  else if (x<0.1) {"."}
                  else {""})

  df <- as.data.frame(data$regressionsCoefficients)
  df[,1] <- estimate
  df[,2] <- se
  df[,3] <- tValues
  df[,4] <- pValues
  df[,5] <- stars

  return(df)
}

summary(lm(Petal.Length~Species, data = iris))
