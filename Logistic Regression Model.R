

# LOAD THE DATA
Auctiondf = read.csv('ebayAuctions.csv')
library(scales)
source('BabsonAnalytics.R')

# MANAGE THE DATA
Auctiondf$Competitive = as.logical(Auctiondf$Competitive)
Auctiondf$EndDay = as.factor(Auctiondf$EndDay)
Auctiondf$Currency = as.factor(Auctiondf$Currency)
Auctiondf$ClosePrice = NULL

# PARITION THE DATA
set.seed(747)
Split = 0.65
N = nrow(Auctiondf)
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = Auctiondf[TrainingCases, ]
Test = Auctiondf[- TrainingCases, ]

# BUILD THE MODEL
LogReg = glm(Competitive ~., data = Training, family = 'binomial')
summary(LogReg)
LogReg = step(LogReg, direction = 'backward')
LogReg$coefficients 


# MAKE PREDICTIONS
Predictions = predict (LogReg, Test, type = 'response')
Actuals = Test$Competitive
PredTF = (Predictions > 0.6)


# EVALUATE THE MODEL
Errors = sum(PredTF != Actuals)
Error_rate = percent(sum(Errors)/length(Actuals), accuracy = 0.1)


# BENCHMARKING
Benchmark = Training$Competitive
Error_bench = benchmarkErrorRate(Benchmark, Actuals)
Error_bench = percent(Error_bench, accuracy = 0.1)


# CONFUSION MATRIX

table(PredTF, Actuals)

True_Pos = sum(PredTF == TRUE & Actuals == TRUE)
True_Neg = sum(PredTF == FALSE & Actuals == FALSE)
False_Neg = sum(PredTF == FALSE & Actuals == TRUE)
False_Pos = sum(PredTF == TRUE & Actuals == FALSE)

Sensitivity = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1)
Specificity = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1)
