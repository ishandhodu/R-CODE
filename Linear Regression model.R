
#LOAD THE DATA 
Housingdf = read.csv('BostonHousing.csv')
options(scipen = 3)
library(scales)


# MANAGE THE DATA 
Housingdf$CHAS = factor(Housingdf$CHAS)
Housingdf$RAD = factor(Housingdf$RAD)
Housingdf$ISHIGHVAL = NULL


#PARTITION THE DATA 
set.seed(6543)
split = 0.6
N = nrow(Housingdf)
TrainingSize = round(N * split)
TrainingCases = sample(N, TrainingSize)
Training = Housingdf[TrainingCases, ]
Test = Housingdf[- TrainingCases, ]

# BUILD THE MODEL 
LinReg = lm(MEDV~., data = Training)
summary(LinReg)


# REFINE THE MODEL 
Backward = step(LinReg, direction = 'backward')
Backward$coefficients
Backward$anova

# MAKE PREDICTIONS 
Predictions = predict(Backward, Test)
Actuals = Test$MEDV
Error = Actuals - Predictions
Predictions[1]
Error[6]
RMSE = sqrt(mean(Error^2))
MAPE = mean(abs(Error / Actuals))
MAPE = percent(MAPE, accuracy = 0.1)



# BENCHMARK
Benchmark = mean(Training$MEDV)
Error_Bench = Actuals - Benchmark
RMSE_Bench = sqrt(mean(Error_Bench^2))
MAPE_Bench = mean(abs(Error_Bench/Actuals))
MAPE_Bench = percent(MAPE_Bench , accuracy = 0.1)
  


