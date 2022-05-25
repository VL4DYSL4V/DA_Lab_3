library(ggplot2)
library("psych")
library("DescTools")
library("e1071")
# library(ggplot2)
# library(GGally)
library(car)
library(lmtest)

myData <- read.csv('output.csv')

Happiness_Score <- myData$Happiness_Score
Happiness_Score_Name <- "Happiness_Score"
Life_Expectancy <- myData$Life_Expectancy
Life_Expectancy_Name <- "Life_Expectancy"
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
  chart2 <- qplot(x = Life_Expectancy, geom = 'freqpoly')
  chart3 <- qplot(x = Freedom, geom = 'freqpoly')
  return(list(
    chart1 = chart1,
    chart2 = chart2,
    chart3 = chart3
  ))
}

printSummary <- function(vector, name) {
  summary <- summary(vector)
  print(paste("Summary of '", name, "': "))
  print(summary)
}

printDeciles <- function(vector, name) {
  deciles <- quantile(
    vector,
    probs = seq(.1, .9, by = .1)
  )
  print(paste("Deciles of '", name, "': "))
  print(deciles)
}

printGeometricalMeanWithoutZeroes <- function(vector, name) {
  print(
    paste(
      "Geometric Mean of '", name, "': ", exp(mean(log(vector[vector > 0])))
    )
  )
}

printHarmonicMeanWithoutZeroes <- function(vector, name) {
  print(
    paste(
      "Harmonic Mean of '", name, "': ", harmonic.mean(vector, zero = FALSE)
    )
  )
}

printMode <- function(vector, name) {
  vectorMode <- Mode(vector)
  print(
    paste(
      "Mode of '", name, "': ", toString(vectorMode)
    )
  )
}

printDispersion <- function(vector, name) {
  dispersion <- var(vector)
  print(
    paste(
      "Dispersion of '", name, "': ", dispersion
    )
  )
}

printStandardDeviation <- function(vector, name) {
  sd <- sd(vector)
  print(
    paste(
      "Standard Deviation of '", name, "': ", sd
    )
  )
}

printCoefficientOfVariation <- function(vector, name) {
  cv <- sd(vector) / mean(vector) * 100
  print(
    paste(
      "Coefficient of Variation of '", name, "': ", cv
    )
  )
}

printProbabilisticDeviation <- function(vector, name) {
  pd <- IQR(vector) / 2
  print(
    paste(
      "Probabilistic Deviation of '", name, "': ", pd
    )
  )
}

printSamplingSpan <- function(vector, name) {
  max <- max(vector)
  min <- min(vector)
  print(
    paste(
      "Sampling Span of '", name, "': ", max - min
    )
  )
}

printConcentrationInterval <- function(vector, name) {
  mean <- mean(vector)
  sd <- sd(vector)
  print(
    paste(
      "Concentration Interval of '", name, "': (", mean - 3 * sd, ", ", mean + 3 * sd, ")"
    )
  )
}

printKurtosis <- function(vector, name) {
  print(
    paste(
      "Kurtosis of '", name, "': ", kurtosis(vector)
    )
  )
}

printSkewness <- function(vector, name) {
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

anayzeOneVector <- function(vector, vectorName) {
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
  printCorellationCoefficient(Happiness_Score, Life_Expectancy, Happiness_Score_Name, Life_Expectancy_Name)
  printCorellationCoefficient(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printCorellationCoefficient(Life_Expectancy, Freedom, Life_Expectancy_Name, Freedom_Name)
  printDelimiterWithNewLines()
  printPValue(Happiness_Score, Life_Expectancy, Happiness_Score_Name, Life_Expectancy_Name)
  printPValue(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printPValue(Life_Expectancy, Freedom, Life_Expectancy_Name, Freedom_Name)
  printDelimiterWithNewLines()
  printDeterminationCoefficient(Happiness_Score, Life_Expectancy, Happiness_Score_Name, Life_Expectancy_Name)
  printDeterminationCoefficient(Happiness_Score, Freedom, Happiness_Score_Name, Freedom_Name)
  printDeterminationCoefficient(Life_Expectancy, Freedom, Life_Expectancy_Name, Freedom_Name)
  printDelimiterWithNewLines()

  printMultipleCorrelationCoefficient(Happiness_Score_Name, c(Life_Expectancy_Name, Freedom_Name))
  printMultipleCorrelationCoefficient(Life_Expectancy_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultipleCorrelationCoefficient(Freedom_Name, c(Happiness_Score_Name, Life_Expectancy_Name))
  printDelimiterWithNewLines()
  printMultiplePValue(Happiness_Score_Name, c(Life_Expectancy_Name, Freedom_Name))
  printMultiplePValue(Life_Expectancy_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultiplePValue(Freedom_Name, c(Happiness_Score_Name, Life_Expectancy_Name))
  printDelimiterWithNewLines()
  printMultipleDeterminationCoefficient(Happiness_Score_Name, c(Life_Expectancy_Name, Freedom_Name))
  printMultipleDeterminationCoefficient(Life_Expectancy_Name, c(Happiness_Score_Name, Freedom_Name))
  printMultipleDeterminationCoefficient(Freedom_Name, c(Happiness_Score_Name, Life_Expectancy_Name))
  printDelimiterWithNewLines()
}

printCorrelationMatrix <- function(data) {
  print("Correlation matrix:")
  print(cor(data))
}

buildVariableRatiocharts <- function(data) {
  # chart <- ggpairs(dataSlice)
  scatterplotMatrix(
    data, spread = FALSE, lty.smooth = 2, main = 'Variables Ratio'
  )
}

printSummaryForModel <- function(model) {
  print("Summary for model:")
  print(summary(model))
}

printConfidenceIntervals <- function(model) {
  print("Confidence intervals:")
  print(confint(model))
}

printBreushPaganTestResult <- function(model) {
  print("Breush-Pagan Test:")
  print(bptest(model))
}

printDurbinWatsonTestResult <- function(model) {
  print("Durbin-Watson test:")
  print(durbinWatsonTest(model))
}

buildResidualsPlot <- function(model, data) {
  ggplot(data = data, aes(x = model$residuals)) +
    geom_histogram(fill = 'steelblue', color = 'black') +
    labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
}

buildModel <- function() {
  myDataSlice <- myData[, c(Happiness_Score_Name, Life_Expectancy_Name, Freedom_Name)]
  buildVariableRatiocharts(myDataSlice)
  printDelimiterWithNewLines()
  printCorrelationMatrix(myDataSlice)
  formula <- reformulate(c(Life_Expectancy_Name, Freedom_Name), Happiness_Score_Name)
  model <- lm(formula, data = myDataSlice)
  printDelimiterWithNewLines()
  printSummaryForModel(model)
  printDelimiterWithNewLines()
  printConfidenceIntervals(model)
  printDelimiterWithNewLines()
  printBreushPaganTestResult(model)
  printDelimiterWithNewLines()
  printDurbinWatsonTestResult(model)
  printDelimiterWithNewLines()
  buildResidualsPlot(model, myDataSlice)
}

# buildFrequencyPolygons()
#
# anayzeOneVector(Happiness_Score, Happiness_Score_Name)
# anayzeOneVector(Freedom, Freedom_Name)
# anayzeOneVector(Life_Expectancy, Life_Expectancy_Name)
#
# analyzeCorrelation()

buildModel()
