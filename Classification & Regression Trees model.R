

# LOAD THE DATA

Loandf = read.csv('UniversalBank.csv')
source('BabsonAnalytics.R')
library(scales)
library(rpart)
library(rpart.plot)

# MANAGE THE DATA

Loandf$Personal.Loan[Loandf$Personal.Loan == 0] = 'Decline'
Loandf$Personal.Loan[Loandf$Personal.Loan == 1] = 'Accept'

Loandf$Education[Loandf$Education == 1] = 'UGrad'
Loandf$Education[Loandf$Education == 2] = 'Grad' 
Loandf$Education[Loandf$Education == 3] = 'Prof'

Loandf$ZIP.Code = NULL
Loandf$ID = NULL

Loandf$Securities.Account[Loandf$Securities.Account == 0] = 'No'
Loandf$Securities.Account[Loandf$Securities.Account == 1] = 'Yes'
Loandf$CD.Account[Loandf$CD.Account == 0] = 'No'
Loandf$CD.Account[Loandf$CD.Account == 1] = 'Yes'
Loandf$Online[Loandf$Online == 0] = 'No'
Loandf$Online[Loandf$Online == 1] = 'Yes'
Loandf$CreditCard[Loandf$CreditCard == 0] = 'No'
Loandf$CreditCard[Loandf$CreditCard == 1] = 'Yes'
Loandf$Securities.Account = as.factor(Loandf$Securities.Account)
Loandf$CD.Account = as.factor(Loandf$CD.Account)
Loandf$Online = as.factor(Loandf$Online)
Loandf$Education = as.factor(Loandf$Education)
Loandf$Personal.Loan = as.factor(Loandf$Personal.Loan)
Loandf$CreditCard = as.factor(Loandf$CreditCard)

#PARTITION THE DATA

set.seed(123456)
Split = 0.6
N = nrow(Loandf)
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = Loandf[TrainingCases, ]
Test = Loandf[- TrainingCases, ]

#BENCHMARKING

Actuals = Test$Personal.Loan
Benchmark = Training$Personal.Loan
ErrorBench = percent(benchmarkErrorRate(Benchmark, Actuals), accuracy = 0.1)

#FULL TREE

StoppingRules = rpart.control(minsplit = 2, minbucket = 1, cp = -1)
Full_tree = rpart(Personal.Loan ~ . , Training, control = StoppingRules)
rpart.plot(Full_tree, main = "Full Tree", digits = 3)

#DEFAULT TREE

Default_Tree = rpart(Personal.Loan ~ . , Training)
rpart.plot(Default_Tree, main = "Default Tree", digits = 3)

#PRUNED TREE

Pruned_Tree = easyPrune(Full_tree)
rpart.plot(Pruned_Tree, main = "Pruned Tree", digits = 3)

#ERROR RATE FOR DEFAULT TREE

Predictions_def = predict(Default_Tree, Test, type = "class")
Errors_def = sum(Predictions_def != Actuals)
ErrorRate_def = percent(Errors_def/length(Actuals), accuracy = 0.1)

#ERROR RATE FOR FULL TREE

Predictions_full = predict(Full_tree, Test, type = "class")
Errors_full = sum(Predictions_full != Actuals)
ErrorRate_full = percent(Errors_full/length(Actuals), accuracy = 0.1)

#ERROR RATE FOR PRUNED TREE

Predictions_pruned = predict(Pruned_Tree, Test, type = "class")
Errors_pruned = percent(sum(Predictions_pruned != Actuals)/length(Actuals), accuracy = 0.1)
