#CLEARING ENVIRONMENT
rm(list = ls())

# LOAD KERNLAB PACKAGE
library(kernlab)
library(caret)

# DATA LOADING 
data <- read.table("/Users/pc/Desktop/working/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

# QUICK LOOK FOR LOADED DATA  
#head(data)
#str(data)
#plot(data)

#CONVERTING DATA INTO MATRIX FORMAT
data<-data.matrix(data)

#SETTING RANDOM NUM GENERATOR SEED FOR REPRODUCIBILITY
set.seed(1)


#SVM MODEL (C-Classification, Simple Linear Kernel, Scaled)


model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="polydot", C=100, scaled=T,)

# COEFFICIENTS FOR EACH PREDICTOR
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])

# INTERCEPT
a0 <- model@b

# MODEL PREDICTION RESULTS
pred <- predict(model,data[,1:10])

#CONFUSION MATRIX AND DETAILED STATISTICAL RESULTS
confusionMatrix(factor(data[,11], levels= c(0,1)), pred)

# MODEL ACCURACY (PERCENT OF MODEL OBSERVATIONS THAT ARE CORRECTLY CLASSIFIED)

accuracy <- sum(pred == data[,11]) / nrow(data)

accuracy

