setwd("C:/Users/idhodapkar1/OneDrive - Babson College/AQM 2000/Data")
rm(list = ls())          
gc()                    
cat("\f")

# LOAD THE DATA

source('BabsonAnalytics.R')
library(e1071)
library(scales)
LoanDF = read.csv('UniversalBank.csv')

# MANAGE DATA

LoanDF$Age = NULL
LoanDF$ID = NULL
LoanDF$ZIP.Code = NULL
LoanDF$Experience = NULL
LoanDF$Income= NULL
LoanDF$Family = NULL
LoanDF$CCAvg = NULL
LoanDF$Mortgage = NULL
LoanDF = data.frame(lapply(LoanDF, as.factor))

# PARTITION DATA

set.seed(4178)
Split = 0.7
N = nrow(LoanDF)
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = LoanDF[TrainingCases, ]
Test = LoanDF[- TrainingCases, ]

# BUILD THE MODEL
 
Bayes = naiveBayes(Personal.Loan  ~ ., data = Training)
Predictions = predict(Bayes, Test)
Actuals = Test$Personal.Loan
Benchmark = Training$Personal.Loan

# Calculate error rate, Bench error rate and Kpis

Error_Rate = percent(sum(Predictions != Actuals) / length(Actuals), accuracy = 0.1)
Error_Bench = percent(benchmarkErrorRate(Benchmark, Actuals), accuracy = 0.1)

# CONFUSION MATRIX

table(Predictions, Actuals)
Conf_matrix = table(Predictions, Actuals)
True_Pos = Conf_matrix[2, 2]
True_Neg = Conf_matrix[1, 1]
False_Pos = Conf_matrix[2, 1]
False_Neg = Conf_matrix[1, 2]


# SENSITIVITY AND SPECIFICITY

Sensitivity = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1)
Specificity = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1)


# MAKE PREDICTIONS (after introduction of threshold)

Predictions = predict(Bayes, Test, type = "raw")
Pred = Predictions[ , 2]
PredTF = (Pred > 0.3)
Actuals = Test$Personal.Loan
ActualsTF = (Test$Personal.Loan == 1)


#THIS PART OF THE CODE IS ONLY IF WE NEED TO MODIFY PREDICTION THRESHOLD

# CONFUSION MATRIX

table(PredTF,ActualsTF)
Conf_matrix = table(PredTF,ActualsTF)
True_Pos = Conf_matrix[2, 2]
True_Neg = Conf_matrix[1, 1]
False_Neg = Conf_matrix[1, 2]
False_Pos = Conf_matrix[2, 1]

# SENSITIVITY AND SPECIFICITY

SensitivityTF = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1)
SpecificityTF = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1)


# ROC AND LIFT CHART

ROCChart(ActualsTF, Pred)
liftChart(ActualsTF, Pred)


# JOINT AND CONDITIONAL PROBABILITY

Bayes$tables$Education
Bayes$tables$CD.Account

LocateDF = Training[ , c("Personal.Loan", "CD.Account")]
Took_loan = LocateDF[LocateDF[ , 2] == 1, ]
Word_prob = sum(Took_loan$Personal.Loan == "1") / nrow(Took_loan)
Word_prob = percent(Word_prob, accuracy = 0.1)

cat ("If a person with CD.Account accepts loan, the probability is", Word_prob)


#PROBABILITY FOR EXACT WORD

# CONDITIONAL PROBABILITIES 
Bayes$tables$awesome

Word = "good"
word_df = Training[ , c("PositiveTweet", Word)] 
Has_Word = word_df[word_df[ , 2] == 1, ]
Word_prob = sum(Has_Word$PositiveTweet == "1") / nrow(Has_Word)
Word_prob = percent(Word_prob, accuracy = 0.1)


cat("If a tweet has the word\"", Word, "\", the probability it is positive is", Word_prob)

