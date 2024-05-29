
# LOAD THE DATA
library(scales)
library(caret)
source('BabsonAnalytics.R')
Housingdf = read.csv('BostonHousing.csv')

# MANAGE THE DATA
Housingdf$CHAS = NULL
Housingdf$RAD = NULL
Housingdf$MEDV = NULL
Housingdf$ISHIGHVAL = as.factor(Housingdf$ISHIGHVAL)

# STANDARDIZE THE VARIABLES 
Processor = preProcess(Housingdf, method = c('center','scale'))
Housingdf = predict(Processor, Housingdf)

# PARTITION THE DATA 
set.seed(1225)
split = 0.6
N = nrow(Housingdf)
TrainingSize = round(N * split)
TrainingCases = sample(N, TrainingSize)
Training = Housingdf[TrainingCases, ]
Test = Housingdf[- TrainingCases, ]

# BUILD THE MODEL
KNN_class = knn3(ISHIGHVAL ~., data = Training, k = 5)

# MAKE PREDICTIONS 
Predictions = predict(KNN_class, Test, type = 'class')
Actuals = Test$ISHIGHVAL
table(Predictions, Actuals)

# EVALUTE THE MODEL 
Errors = Predictions != Actuals 
Error_rate = percent(sum(Errors) / length(Actuals), accuracy = 0.1)

# BENCHMARKS 
Benchmark = Training$ISHIGHVAL
Error_bench = benchmarkErrorRate(Benchmark, Actuals)
Error_bench = percent(Error_bench, accuracy = 0.1)

knnCrossVal(ISHIGHVAL ~., data = Training)




















