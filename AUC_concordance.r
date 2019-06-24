# R Code for ROC, Concordant / Discordant :
df = read.csv("D:/Projects/Credit Risk Score in R/binary.csv")
  
  # Factor Variables
  df$admit = as.factor(df$admit)
  df$rank = as.factor(df$rank)
  
  # Logistic Model
  df$rank <- relevel(df$rank, ref='4')
  mylogistic <- glm(admit ~ ., data = df, family = "binomial")
  summary(mylogistic)$coefficient
  
  # Predict
  pred = predict(mylogistic, type = "response")
  finaldata = cbind(df, pred)
  
  
    AUC <- function (actuals, predictedScores){
    fitted <- data.frame (Actuals=actuals, PredictedScores=predictedScores)
    colnames(fitted) <- c('Actuals','PredictedScores')
    ones <- fitted[fitted$Actuals==1, ] # Subset ones
    zeros <- fitted[fitted$Actuals==0, ] # Subsetzeros
    totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
    conc <- sum (c(vapply(ones$PredictedScores, function(x) {((x > zeros$PredictedScores))}, FUN.VALUE=logical(nrow(zeros)))), na.rm=T)
    disc <- sum(c(vapply(ones$PredictedScores, function(x) {((x < zeros$PredictedScores))}, FUN.VALUE = logical(nrow(zeros)))), na.rm = T)
    concordance <- conc/totalPairs
    discordance <- disc/totalPairs
    tiesPercent <- (1-concordance-discordance)
    AUC = concordance + 0.5*tiesPercent
    Gini = 2*AUC - 1
    return(list("Concordance"=concordance, "Discordance"=discordance,
                "Tied"=tiesPercent, "AUC"=AUC, "Gini or Somers D"=Gini))
  }
  
  AUC(finaldata$admit, finaldata$pred)
  