library(ggplot2)
library("psych")
library("DescTools")
library("e1071")

myData <- read.csv('output.csv')

Happiness_Score <- myData$Happiness_Score
Happiness_Score_Name <- "Happiness_Score"
Family <- myData$Family
Family_Name <- "Family"
Freedom <- myData$Freedom
Freedom_Name <- "Freedom"

printDelimiterWithNewLines <- function() {
  cat("\n\n=========================================================================\n\n")
}

printEmptyLine <- function() {
  cat("\n")
}

buildFrequencyPolygons <- function() {
  chart1 <- qplot(x = Happiness_Score, geom = 'freqpoly')
  chart2 <- qplot(x = Family, geom = 'freqpoly')
  chart3 <- qplot(x = Freedom, geom = 'freqpoly')
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
  printCorellationCoefficient(Happiness_Score, Family, Happiness_Score_Name, Family_Name)
  printCorellationCoefficient(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printCorellationCoefficient(Family, Freedom, Family_Name, Freedom_Name)
  printDelimiterWithNewLines()
  printPValue(Happiness_Score, Family, Happiness_Score_Name, Family_Name)
  printPValue(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printPValue(Family, Freedom, Family_Name, Freedom_Name)
  printDelimiterWithNewLines()
  printDeterminationCoefficient(Happiness_Score, Family, Happiness_Score_Name, Family_Name)
  printDeterminationCoefficient(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printDeterminationCoefficient(Family, Freedom, Family_Name, Freedom_Name)
  printDelimiterWithNewLines()

  printMultipleCorrelationCoefficient(Happiness_Score_Name, c(Family_Name, Freedom_Name))
  printMultipleCorrelationCoefficient(Family_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultipleCorrelationCoefficient(Freedom_Name, c(Happiness_Score_Name, Family_Name))
  printDelimiterWithNewLines()
  printMultiplePValue(Happiness_Score_Name, c(Family_Name, Freedom_Name))
  printMultiplePValue(Family_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultiplePValue(Freedom_Name, c(Happiness_Score_Name, Family_Name))
  printDelimiterWithNewLines()
  printMultipleDeterminationCoefficient(Happiness_Score_Name, c(Family_Name, Freedom_Name))
  printMultipleDeterminationCoefficient(Family_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultipleDeterminationCoefficient(Freedom_Name, c(Happiness_Score_Name, Family_Name))
  printDelimiterWithNewLines()
}

buildFrequencyPolygons()

anayzeOneVector(Happiness_Score, Happiness_Score_Name)
anayzeOneVector(Freedom, Freedom_Name)
anayzeOneVector(Family, Family_Name)

analyzeCorrelation()
