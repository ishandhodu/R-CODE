source("BabsonAnalytics.R")
library(scales)
library(rpart)
library(rattle)
Hittersdf = read.csv("Hitters.csv")

# Remove all categorical variables.
Hittersdf$Name = NULL
Hittersdf$League = NULL
Hittersdf$Division = NULL
Hittersdf$NewLeague = NULL

# Remove the players that do not have a salary. These are indicated by “NA”.
Hittersdf = Hittersdf[!is.na(Hittersdf$Salary), ]

# Create the partition using a seed of 471 and a 70:30 split.
set.seed(471)
Split = 0.7
N = nrow(Hittersdf)
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = Hittersdf[TrainingCases, ]
Test = Hittersdf[- TrainingCases, ]

# Start with the Benchmarking section, followed by the three trees: Default, Full and Pruned. Remember that
Actuals = Test$Salary
Benchmark = mean(Training$Salary)
Errors_bench = Actuals	-	Benchmark
RMSE_bench = sqrt(mean(Errors_bench^2))
MAPE_bench = percent(mean(abs(Errors_bench/Actuals)), accuracy = 0.1)

# Start with the Default tree, plot it using fancyRpartPlot and create the KPIs.
Default_tree = rpart(Salary ~ ., Training)
fancyRpartPlot(Default_tree, main = "Default Tree")
Predictions_def = predict(Default_tree, Test)
Errors_def = Actuals	-	Predictions_def
RMSE_def = sqrt(mean(Errors_def^2))
MAPE_def = percent(mean(abs(Errors_def/Actuals)), accuracy = 0.1)

# Now the grow the Full tree.
StoppingRules = rpart.control(minsplit = 2, minbucket = 1, cp =	-1)
Full_tree = rpart(Salary ~ ., Training, control = StoppingRules)
fancyRpartPlot(Full_tree, main = "Full Tree")
Predictions_full = predict(Full_tree, Test)
Errors_full = Actuals	-	Predictions_full
RMSE_full = sqrt(mean(Errors_full^2))
MAPE_full = percent(mean(abs(Errors_full/Actuals)), accuracy = 0.1)

# Now prune the tree.
Pruned_tree = easyPrune(Full_tree)
fancyRpartPlot(Pruned_tree, main = "Pruned Tree")
Predictions_pruned = predict(Pruned_tree, Test)
Errors_pruned = Actuals	-	Predictions_pruned
RMSE_pruned = sqrt(mean(Errors_pruned^2))
MAPE_pruned = percent(mean(abs(Errors_pruned/Actuals)), accuracy = 0.1)

