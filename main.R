library(ggplot2)
library("psych")
library("DescTools")
library("e1071")

myData <- read.csv('walmart_cleaned.csv')

Weekly_Sales <- myData$Weekly_Sales
Weekly_Sales_Name <- "Weekly_Sales"
Unemployment <- myData$Unemployment
Unemployment_Name <- "Unemployment"
Fuel_Price <- myData$Fuel_Price
Fuel_Price_Name <- "Fuel_Price"

printDelimiterWithNewLines <- function() {
  cat("\n\n=========================================================================\n\n")
}

printEmptyLine <- function() {
  cat("\n")
}

buildFrequencyPolygons <- function() {
  chart1 <- qplot(x = Weekly_Sales, geom = 'freqpoly')
  chart2 <- qplot(x = Unemployment, geom = 'freqpoly')
  chart3 <- qplot(x = Fuel_Price, geom = 'freqpoly')
  return(list(
    chart1=chart1,
    chart2=chart2,
    chart3=chart3
  ))
}

printSummary <- function (vector, name) {
  summary <- summary(vector)
  print(paste("Summary of '", name, "': "))
  print(summary)
}

printDeciles <- function (vector, name) {
  deciles <- quantile(
    vector,
    probs = seq(.1, .9, by = .1)
  )
  print(paste("Deciles of '", name, "': "))
  print(deciles)
}

printGeometricalMeanWithoutZeroes <- function (vector, name) {
  print(
    paste(
      "Geometric Mean of '", name, "': ", exp(mean(log(vector[vector>0])))
    )
  )
}

printHarmonicMeanWithoutZeroes <- function (vector, name) {
  print(
    paste(
      "Harmonic Mean of '", name, "': ", harmonic.mean(vector, zero = FALSE)
    )
  )
}

printMode <- function (vector, name) {
  vectorMode <- Mode(vector)
  print(
    paste(
      "Mode of '", name, "': ", toString(vectorMode)
    )
  )
}

printDispersion <- function (vector, name) {
  dispersion <- var(vector)
  print(
    paste(
      "Dispersion of '", name, "': ", dispersion
    )
  )
}

printStandardDeviation <- function (vector, name) {
  sd <- sd(vector)
  print(
    paste(
      "Standard Deviation of '", name, "': ", sd
    )
  )
}

printCoefficientOfVariation <- function (vector, name) {
  cv <- sd(vector) / mean(vector) * 100
  print(
    paste(
      "Coefficient of Variation of '", name, "': ", cv
    )
  )
}

printProbabilisticDeviation <- function (vector, name) {
  pd <- IQR(vector) / 2
  print(
    paste(
      "Probabilistic Deviation of '", name, "': ", pd
    )
  )
}

printSamplingSpan <- function (vector, name) {
  max <- max(vector)
  min <- min(vector)
  print(
    paste(
      "Sampling Span of '", name, "': ", max - min
    )
  )
}

printConcentrationInterval <- function (vector, name) {
  mean <- mean(vector)
  sd <- sd(vector)
  print(
    paste(
      "Concentration Interval of '", name, "': (", mean - 3 * sd, ", ", mean + 3 * sd, ")"
    )
  )
}

printKurtosis <- function (vector, name) {
  print(
    paste(
      "Kurtosis of '", name, "': ", kurtosis(vector)
    )
  )
}

printSkewness <- function (vector, name) {
  print(
    paste(
      "Skewness of '", name, "': ", skewness(vector)
    )
  )
}

printCorellationCoefficient <- function(vector1, vector2, name1, name2) {
  correlation <- cor(vector1, vector2)
  print(
    paste("Coefficient of correlation between ", name1, " and ", name2, " is: ", correlation)
  )
}

printPValue <- function(vector1, vector2, name1, name2) {
  res <- cor.test(vector1, vector2)
  pValue <- res$p.value
  print(
    paste("P-value of ", name1, " and ", name2, " is: ", pValue)
  )
}

printDeterminationCoefficient <- function(vector1, vector2, name1, name2) {
  linModel <- lm(vector1 ~ vector2)
  print(
    paste(
      "Coefficient of determination for '", name1, "' and '", name2, "': ",
      summary(linModel)$r.squared
    )
  )
}

printMultipleCorrelationCoefficient <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  vect <- model$model[, dependentVectorName]
  print(
    paste(
      "Multiple correlation coefficient with '", dependentVectorName, "' as dependent variable: ",
      cor(vect, model$fitted.values)
    )
  )
}

printMultipleDeterminationCoefficient <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  print(
    paste(
      "Coefficient of determination for '", dependentVectorName, "' as dependent variable: ",
      summary(model)$r.squared
    )
  )
}

printMultiplePValue <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  vect <- model$model[, dependentVectorName]

  mulCorTest <- cor.test(vect, model$fitted.values)
  mulPValue <- mulCorTest$p.value
  print(
    paste(
      "P-value for '", dependentVectorName, "' as dependent variable: ",
      mulPValue
    )
  )
}

anayzeOneVector <- function (vector, vectorName) {
  printDelimiterWithNewLines()
  printSummary(vector, vectorName)
  printEmptyLine()
  printDeciles(vector, vectorName)
  printEmptyLine()
  printGeometricalMeanWithoutZeroes(vector, vectorName)
  printEmptyLine()
  printHarmonicMeanWithoutZeroes(vector, vectorName)
  printEmptyLine()
  printMode(vector, vectorName)
  printEmptyLine()
  printDispersion(vector, vectorName)
  printEmptyLine()
  printStandardDeviation(vector, vectorName)
  printEmptyLine()
  printCoefficientOfVariation(vector, vectorName)
  printEmptyLine()
  printProbabilisticDeviation(vector, vectorName)
  printEmptyLine()
  printSamplingSpan(vector, vectorName)
  printEmptyLine()
  printConcentrationInterval(vector, vectorName)
  printEmptyLine()
  printKurtosis(vector, vectorName)
  printEmptyLine()
  printSkewness(vector, vectorName)
  printDelimiterWithNewLines()
}

analyzeCorrelation <- function() {
  printDelimiterWithNewLines()
  printCorellationCoefficient(Weekly_Sales, Unemployment, Weekly_Sales_Name, Unemployment_Name)
  printCorellationCoefficient(Weekly_Sales, Fuel_Price, Weekly_Sales_Name, Fuel_Price_Name)
  printCorellationCoefficient(Unemployment, Fuel_Price, Unemployment_Name, Fuel_Price_Name)
  printDelimiterWithNewLines()
  printPValue(Weekly_Sales, Unemployment, Weekly_Sales_Name, Unemployment_Name)
  printPValue(Weekly_Sales, Fuel_Price, Weekly_Sales_Name, Fuel_Price_Name)
  printPValue(Unemployment, Fuel_Price, Unemployment_Name, Fuel_Price_Name)
  printDelimiterWithNewLines()
  printDeterminationCoefficient(Weekly_Sales, Unemployment, Weekly_Sales_Name, Unemployment_Name)
  printDeterminationCoefficient(Weekly_Sales, Fuel_Price, Weekly_Sales_Name, Fuel_Price_Name)
  printDeterminationCoefficient(Unemployment, Fuel_Price, Unemployment_Name, Fuel_Price_Name)
  printDelimiterWithNewLines()

  printMultipleCorrelationCoefficient(Weekly_Sales_Name, c(Unemployment_Name, Fuel_Price_Name))
  printMultipleCorrelationCoefficient(Unemployment_Name, c(Weekly_Sales_Name, Fuel_Price_Name))
  printMultipleCorrelationCoefficient(Fuel_Price_Name, c(Weekly_Sales_Name, Unemployment_Name))
  printDelimiterWithNewLines()
  printMultiplePValue(Weekly_Sales_Name, c(Unemployment_Name, Fuel_Price_Name))
  printMultiplePValue(Unemployment_Name, c(Weekly_Sales_Name, Fuel_Price_Name))
  printMultiplePValue(Fuel_Price_Name, c(Weekly_Sales_Name, Unemployment_Name))
  printDelimiterWithNewLines()
  printMultipleDeterminationCoefficient(Weekly_Sales_Name, c(Unemployment_Name, Fuel_Price_Name))
  printMultipleDeterminationCoefficient(Unemployment_Name, c(Weekly_Sales_Name, Fuel_Price_Name))
  printMultipleDeterminationCoefficient(Fuel_Price_Name, c(Weekly_Sales_Name, Unemployment_Name))
  printDelimiterWithNewLines()
}

buildFrequencyPolygons()

anayzeOneVector(Weekly_Sales, Weekly_Sales_Name)
anayzeOneVector(Fuel_Price, Fuel_Price_Name)
anayzeOneVector(Unemployment, Unemployment_Name)

analyzeCorrelation()
