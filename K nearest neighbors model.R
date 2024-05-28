
setwd("C:/Users/idhodapkar1/OneDrive - Babson College/AQM 2000/Data")
rm(list = ls())          
gc()                    
cat("\f")

# LOAD THE DATA

Universaldf = read.csv('UniversalBank.csv')
library(caret)
library(scales)
source('BabsonAnalytics.R')

# MANAGE THE DATA

Universaldf$CreditCard = NULL
Universaldf$ZIP.Code = NULL
Universaldf$CD.Account = NULL
Universaldf$Online = NULL
Universaldf$Securities.Account = NULL
Universaldf$ID = NULL
Universaldf$ZIP.Code = NULL
Universaldf$Personal.Loan = as.factor(Universaldf$Personal.Loan)
Universaldf$Education = NULL

# STANDARDIZE THE VARIABLES

Processor = preProcess(Universaldf, method = c("center","scale"))
Universaldf = predict(Processor, Universaldf)

# PARTITION THE DATA

set.seed(8421)
Split = 0.6
N = nrow(Universaldf)
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = Universaldf[TrainingCases, ]
Test = Universaldf[- TrainingCases, ]

# BUILD THE MODEL

KNN_Class = knn3(Personal.Loan ~ ., Training, k = 7)

# MAKE PREDICTIONS

Predictions = predict(KNN_Class, Test, type = 'class')
Actuals = Test$Personal.Loan
table(Predictions, Actuals)

# EVALUATE THE MODEL

Errors = sum(Predictions != Actuals)
Error_rate = percent(sum(Errors)/length(Actuals), accuracy = 0.1)

# BENCHMARKING

Benchmark = Training$Personal.Loan
Error_bench = benchmarkErrorRate(Benchmark, Actuals)
Error_bench = percent(Error_bench, accuracy = 0.1)
  
