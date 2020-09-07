#CLEARING ENVIRONMENT
rm(list = ls())

# LOAD KERNLAB PACKAGE
library(kernlab)

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

#STORE 5 DIFFERENT C VALUES TO TEST ACCURACY RESULTS
accuracy <- rep(0,5)
coptions <- c(0.0001,1,50,90.35,100)
x <- 1

#LOOPING SVM MODEL FOR DIFFERENT C ABOVE (C-Classification, Simple Linear Kernel, Scaled)
for (i in coptions)
{

model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="vanilladot", C=i, scaled=T,)

# COEFFICIENTS FOR EACH PREDICTOR
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])


# INTERCEPT
a0 <- model@b


# MODEL PREDICTION RESULTS
pred <- predict(model,data[,1:10])


# MODEL ACCURACY (PERCENT OF MODEL OBSERVATIONS THAT ARE CORRECTLY CLASSIFIED)

accuracy[x] <- sum(pred == data[,11]) / nrow(data)
x <- x+1

}

accuracy
a
a0
