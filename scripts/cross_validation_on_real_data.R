#################### REAL DATA: CV + HELD OUT TEST SET ################

# Data
data <- read.csv(paste0(path,"data/adult.csv"))
head(data); dim(data)

# Check levels
data_new <- data.frame(cbind(
  Income = as.numeric(data$income)-1,
  Age = as.numeric(as.factor(cut(data$age, seq(min(data$age),max(data$age),25)))),
  WorkClass = as.numeric(data$workclass),
  FnlWgt = as.numeric(data$fnlwgt),
  Edu = as.numeric(data$education),
  EduNum = as.numeric(data$educational.num),
  MaritalStatus = as.numeric(data$marital.status),
  Occup = as.numeric(data$occupation),
  Relationship = as.numeric(data$relationship),
  Race = as.numeric(data$race),
  Gender = as.numeric(data$gender),
  CapitalGain = as.numeric(data$capital.gain),
  CapitalLoss = as.numeric(data$capital.loss),
  HoursPerWeek = as.numeric(data$hours.per.week),
  NativeCountry = as.numeric(data$native.country)
)); data_new <- data.frame(na.omit(data_new)); head(data_new); dim(data_new)
apply(data_new, 2, is.numeric)

###################### K-FOLD CV: REG & CLASS ########################

# READ ME:
# This script loops through k folds.
# Each fold the algorithm fits a selected machine learning technique.
# The algorithm outputs k-fold accuracy (or other selected results). 

# Two classes:
# This is for classification only
all <- data_new
all.A <- all[all[,1] == 0, ]; dim(all.A)
all.B <- all[all[,1] == 1, ]; dim(all.B)

# Null Result:
result <- NULL

# CV:
# Write a k-fold CV loop:
how.many.folds = 4
print(paste0("This is ", how.many.folds-1, "-fold CV."))
print(paste0("We leave ", how.many.folds, "th fold untouched as held-out test."))
print(paste0("Here let us do fold 1 up to fold ", how.many.folds-1, " as CV procedure."))
for (folds.i in c(1:(how.many.folds-1))){
  # Create k-fold training data sets for CV:
  
  # Create:
  # folds: a list of numbers with different index;
  # testIndexes: the index that equals to each index in folds;
  
  # For classification
  # Then we can create test and train data sets:
  folds.A <- cut(seq(1,nrow(all.A)),breaks=how.many.folds,labels=FALSE)
  folds.B <- cut(seq(1,nrow(all.B)),breaks=how.many.folds,labels=FALSE)
  #folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
  
  # For regression
  
  # For classifiction: 
  # Set:
  #folds.i <- 1
  testIndexes.A <- which(folds.A==folds.i, arr.ind = TRUE)
  testIndexes.B <- which(folds.B==folds.i, arr.ind = TRUE)
  testData.A <- all.A[testIndexes.A, ]; trainData.A <- all.A[-testIndexes.A, ]
  testData.B <- all.B[testIndexes.B, ]; trainData.B <- all.B[-testIndexes.B, ]
  all <- data.frame(rbind(
    trainData.A, trainData.B,
    testData.A, testData.B 
  ))
  
  # MODEL FITTING / MACHINE LEARNING:
  # One can change to use Regression or Classification:
  cutoff <- round((nrow(trainData.A)+nrow(trainData.B))/nrow(all), 1) # Check this ratio out
  # (I) NONPARAMETRIC
  # PC
  PC_VS_Result <- PC.VS(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    select = 1:(3*folds.i)
  )
  
  # KMEANS
  KMEANS_VS_Result <- KMEANS.VS(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    k = 3*folds.i
  )
  
  # (II) MACHINE LEARNING
  # NONE + LOGISTIC / RF / SVM / NN
  Algo_NONE_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_NONE_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    num.tree = 10*folds.i,
    num.try = sqrt(ncol(all))*folds.i,
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_NONE_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*folds.i,
    g = 1.3*folds.i)
  Algo_NONE_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    epochs = 10*folds.i)
  
  # PC + LOGISTIC / RF / SVM /  NN
  all <- data.frame(PC_VS_Result$all)
  Algo_PC_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_PC_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff, 
    num.tree = 10*folds.i,
    num.try = sqrt(ncol(all))**folds.i,
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_PC_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*folds.i,
    g = 1.3*folds.i)
  Algo_PC_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1], 
    y = all[, 1], 
    cutoff = cutoff, 
    epochs = 10*folds.i)
  
  # KMEANS + LOGISTIC / RF / SVM / NN
  all <- data.frame(KMEANS_VS_Result$all)
  Algo_KMEANS_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_KMEANS_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff, 
    num.tree = 10*folds.i,
    num.try = sqrt(ncol(all))*folds.i,
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_KMEANS_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*folds.i,
    g = 1.3*folds.i)
  Algo_KMEANS_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1], 
    y = all[, 1], 
    cutoff = cutoff, 
    epochs = 10*folds.i)
  
  # Print result
  result <- c(
    result, 
    paste("Fold", folds.i),
    c(round(Algo_NONE_LOGISTIC$AUC,3),
      round(Algo_NONE_RF$AUC,3),
      round(Algo_NONE_SVM$AUC,3),
      round(Algo_NONE_NN$Testing.Accuracy,3)),
    c(round(Algo_PC_LOGISTIC$AUC,3),
      round(Algo_PC_RF$AUC,3),
      round(Algo_PC_SVM$AUC,3),
      round(Algo_PC_NN$Testing.Accuracy,3)),
    c(round(Algo_KMEANS_LOGISTIC$AUC,3),
      round(Algo_KMEANS_RF$AUC,3),
      round(Algo_KMEANS_SVM$AUC,3),
      round(Algo_KMEANS_NN$Testing.Accuracy,3))
    )
  print(paste("Done with fold", folds.i))
} # End of CV

# Result
print(paste0("Cross Validation Result: "))
common_procedure <- c("Logistic", "RF", "SVM", "NN")
Result <- data.frame(t(matrix(result,13)))
colnames(Result) <- c(
  "kth_Fold",
  common_procedure,
  paste0("PC+",common_procedure),
  paste0("KMEANS+",common_procedure)
); DT::datatable(Result)

######################## BEST PARAMETERS #############################

# Record best parameter from CV results above:
best_param <- NULL
for (j in 2:ncol(Result)) { best_param <- c(best_param, which.max(Result[,j])) }

# Comment:
# By recording the best parameters from CV results above,
# we can directly apply this parameter to each algorithm
# because we believe in the fairness and robustness from 
# the results of cross validation. 

######################### HELD OUT TEST SET #############################

# CV:
# Write a k-fold CV loop:
how.many.folds = 4
print(paste0("With the above CV results, we can test performance out held-out test test."))
for (folds.i in how.many.folds){
  # Create k-fold training data sets for CV:
  
  # Create:
  # folds: a list of numbers with different index;
  # testIndexes: the index that equals to each index in folds;
  
  # For classification
  # Then we can create test and train data sets:
  folds.A <- cut(seq(1,nrow(all.A)),breaks=how.many.folds,labels=FALSE)
  folds.B <- cut(seq(1,nrow(all.B)),breaks=how.many.folds,labels=FALSE)
  #folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
  
  # For regression
  
  # For classifiction: 
  # Set:
  #folds.i <- 1
  testIndexes.A <- which(folds.A==folds.i, arr.ind = TRUE)
  testIndexes.B <- which(folds.B==folds.i, arr.ind = TRUE)
  testData.A <- all.A[testIndexes.A, ]; trainData.A <- all.A[-testIndexes.A, ]
  testData.B <- all.B[testIndexes.B, ]; trainData.B <- all.B[-testIndexes.B, ]
  all <- data.frame(rbind(
    trainData.A, trainData.B,
    testData.A, testData.B 
  ))
  
  # MODEL FITTING / MACHINE LEARNING:
  # One can change to use Regression or Classification:
  cutoff <- round((nrow(trainData.A)+nrow(trainData.B))/nrow(all), 1) # Check this ratio out
  # (I) NONPARAMETRIC
  # PC
  PC_VS_Result <- PC.VS(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    select = 1:(3*folds.i)
  )
  
  # KMEANS
  KMEANS_VS_Result <- KMEANS.VS(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    k = 2*folds.i
  )
  
  # (II) MACHINE LEARNING
  # NONE + LOGISTIC / RF / SVM / NN
  Algo_NONE_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_NONE_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    num.tree = 10*best_param[2],
    num.try = sqrt(ncol(all))*best_param[2],
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_NONE_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*best_param[3],
    g = 1.3*best_param[3])
  Algo_NONE_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    epochs = 10*best_param[4])
  
  # PC + LOGISTIC / RF / SVM /  NN
  all <- data.frame(PC_VS_Result$all)
  Algo_PC_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_PC_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff, 
    num.tree = 10*best_param[6],
    num.try = sqrt(ncol(all))*best_param[6],
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_PC_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*best_param[7],
    g = 1.3*best_param[7])
  Algo_PC_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1], 
    y = all[, 1], 
    cutoff = cutoff, 
    epochs = 10*best_param[8])
  
  # KMEANS + LOGISTIC / RF / SVM / NN
  all <- data.frame(KMEANS_VS_Result$all)
  Algo_KMEANS_LOGISTIC <- logistic(
    x = all[, -1],
    y = all[ 1],
    cutoff = cutoff,
    fam = binomial,
    cutoff.coefficient = 1)
  Algo_KMEANS_RF <- Random.Forest(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff, 
    num.tree = 10*best_param[9],
    num.try = sqrt(ncol(all))*best_param[9],
    cutoff.coefficient = 1,
    SV.cutoff = 1:10)
  Algo_KMEANS_SVM <- Support_Vector_Machine(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    c = 1.5*best_param[10],
    g = 1.3*best_param[11])
  Algo_KMEANS_NN <- YinsKerasNN::YinsKerasNN(
    x = all[, -1], 
    y = all[, 1], 
    cutoff = cutoff, 
    epochs = 10*best_param[12])
  
  # Print result
  result <- c(
    result, 
    paste("Fold", folds.i),
    c(round(Algo_NONE_LOGISTIC$AUC,3),
      round(Algo_NONE_RF$AUC,3),
      round(Algo_NONE_SVM$AUC,3),
      round(Algo_NONE_NN$Testing.Accuracy,3)),
    c(round(Algo_PC_LOGISTIC$AUC,3),
      round(Algo_PC_RF$AUC,3),
      round(Algo_PC_SVM$AUC,3),
      round(Algo_PC_NN$Testing.Accuracy,3)),
    c(round(Algo_KMEANS_LOGISTIC$AUC,3),
      round(Algo_KMEANS_RF$AUC,3),
      round(Algo_KMEANS_SVM$AUC,3),
      round(Algo_KMEANS_NN$Testing.Accuracy,3))
  )
  print(paste("Done with fold", folds.i))
} # End of CV

# Result
print(paste0("Fold ", how.many.folds, " as Test Set Result: "))
common_procedure <- c("Logistic", "RF", "SVM", "NN")
Test.Result <- data.frame(t(matrix(result,13)))
colnames(Test.Result) <- c(
  "kth_Fold",
  common_procedure,
  paste0("PC+",common_procedure),
  paste0("KMEANS+",common_procedure)
); DT::datatable(Test.Result)

######################### END OF SCRIPT #################################