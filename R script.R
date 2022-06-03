#################
#  PROJECT II   #                   
#################

library('nnet')
library('class')
library('e1071')	#first install class and then this one
library('penalizedLDA')
library('MASS')
library('heplots')
library('tree')
library('mclust')
library('randomForest')
library('caret')

setwd("C:/Users/marios/Desktop/project 2 2019-2020 FT")

library("readxl")

data <- as.data.frame(read_excel("data.xls"))

str(data)




########################################
# Cleaning the data & Data Exploration #
########################################


## Convert chrs to factors ##

# Find the chrs #
chr_columns <- colnames(data[, sapply(data, class) == 'character'])

# Convert them to factors #
data[chr_columns] <-  lapply(data[chr_columns],as.factor)

str(data)


## Find Nas ##

NAs <- as.matrix(c(rep(NA, ncol(data))))
for (i in 1:ncol(data)) {
  NAs[i] <- sum(is.na(data[,i]))
}
colnames(NAs) <- "NA count"
rownames(NAs) <- colnames(data)

print(NAs)

# No NAs #

## SUMMARY for all variables ##
# Categorical #
summary(data[,-which(sapply(data,is.numeric))])
# Numeric #
# find numeric variables #
data_num <- data[,which(sapply(data,is.numeric))]
summary(data_num)

# Histograms #



for (i in 1:ncol(data_num)) {
  nam <- paste("hist", i, sep = "")
  assign(nam, hist(data_num[,i], col = i + 1, main = paste("histogram of ", colnames(data_num)[i], sep = "")))
}


par(mfrow=c(4,3))
plot(hist1, col = 2, main = paste("histogram of ", colnames(data_num)[1], sep = ""), xlab = colnames(data_num)[1])
plot(hist2, col = 3, main = paste("histogram of ", colnames(data_num)[2], sep = ""), xlab = colnames(data_num)[2])
plot(hist3, col = 4, main = paste("histogram of ", colnames(data_num)[3], sep = ""), xlab = colnames(data_num)[3])
plot(hist4, col = 5, main = paste("histogram of ", colnames(data_num)[4], sep = ""), xlab = colnames(data_num)[4])
plot(hist5, col = 6, main = paste("histogram of ", colnames(data_num)[5], sep = ""), xlab = colnames(data_num)[5])
plot(hist6, col = 7, main = paste("histogram of ", colnames(data_num)[6], sep = ""), xlab = colnames(data_num)[6])
plot(hist7, col = 8, main = paste("histogram of ", colnames(data_num)[7], sep = ""), xlab = colnames(data_num)[7])
plot(hist8, col = 10, main = paste("histogram of ", colnames(data_num)[8], sep = ""), xlab = colnames(data_num)[8])
plot(hist9, col = 11, main = paste("histogram of ", colnames(data_num)[9], sep = ""), xlab = colnames(data_num)[9])
plot(hist10, col = 12, main = paste("histogram of ", colnames(data_num)[10], sep = ""), xlab = colnames(data_num)[10])
dev.off()



# Barplots #

# find categorical variables #
data_cat <- data[,which(sapply(data,is.factor))]

library(ggplot2)

colnames(data_cat)


bar1 <- ggplot(data, aes(x= job) )+ geom_bar(fill= 2) + coord_flip()
bar2 <- ggplot(data, aes(x= marital) )+ geom_bar(fill= 3) 
bar3 <- ggplot(data, aes(x= education) )+ geom_bar(fill= 4) + coord_flip()
bar4 <- ggplot(data, aes(x= default) )+ geom_bar(fill= 5) 
bar5 <- ggplot(data, aes(x= housing) )+ geom_bar(fill= 6) 
bar6 <- ggplot(data, aes(x= loan) )+ geom_bar(fill= 7)  
bar7 <- ggplot(data, aes(x= contact) )+ geom_bar(fill= 8) 
bar8 <- ggplot(data, aes(x= month) )+ geom_bar(fill= 9) + coord_flip()
bar9 <- ggplot(data, aes(x= day_of_week) )+ geom_bar(fill= 10) 
bar10 <- ggplot(data, aes(x= poutcome) )+ geom_bar(fill= 11) 
bar11<- ggplot(data, aes(x= SUBSCRIBED) )+ geom_bar(fill= c("red","blue"))

library("ggpubr")
ggarrange(bar1, bar2, bar3, bar4, bar5, bar6,bar7, bar8, bar9, bar10, bar11, ncol = 4, nrow = 3)
dev.off()

## Class imbalance?  ##
summary(data$SUBSCRIBED)
prop.table(table(data$SUBSCRIBED))
# Class is totally imbalanced. We must be careful with our accuracy score interpretation. #

## Find correlation between numeric variables. Check for multicollinearity in numeric data ##


# corrplot #
library(corrplot)
cor <- cor(data_num)
corrplot(cor, method = "number", type = "upper")




## Which variables are important ? ##
# We will apply stepwise procedure & vif #

# full logistic model without default (it offers no information) #
full <- glm(SUBSCRIBED ~.-default, data = data, family = "binomial")



# Stepwise #
m1 <- step(full,trace=TRUE, direction = 'both')
logit1 <- glm(m1 ,data = data, family = "binomial")

# vif #
library(DAAG)
vif(logit1)

sort(vif(logit1), decreasing = TRUE)

# emp.var.rate !! #
logit2 <- glm(SUBSCRIBED ~ job + marital + contact + 
                month + day_of_week + duration + campaign + pdays + poutcome + cons.price.idx + cons.conf.idx + euribor3m ,data = data, family = "binomial")



sort(vif(logit2), decreasing = TRUE)


# pdays !! #
logit3 <- glm(SUBSCRIBED ~ job + marital + contact + 
                month + day_of_week + duration + campaign + poutcome + cons.price.idx + cons.conf.idx + euribor3m ,data = data, family = "binomial")



sort(vif(logit3), decreasing = TRUE)


# euribor3m !! #
logit4 <- glm(SUBSCRIBED ~ job + marital + contact + 
                month + day_of_week + duration + campaign + poutcome + cons.price.idx + cons.conf.idx, data = data, family = "binomial")



sort(vif(logit4), decreasing = TRUE)


summary(logit4)


varnames <- c("job", "marital", "contact", "month", "day_of_week", "duration", "campaign", "poutcome", "cons.price.idx", "cons.conf.idx")

formula = SUBSCRIBED ~ job + marital + default + contact + month + day_of_week + duration + campaign + poutcome + cons.price.idx + cons.conf.idx


##################
# Classification #
##################

set.seed(101) # Set Seed so that same sample can be reproduced in future also

# Now Selecting 80% of data as sample from total 'n' rows of the data
sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
tr_data <- data[sample, ]
val_data  <- data[-sample, ] # 20 % 




# around 1 hour #

## OVERSAMPLING ##
n <- table(tr_data$SUBSCRIBED)[1] * 1.5 
library(ROSE)
tr_data <- ovun.sample(SUBSCRIBED~., data = tr_data, method = "over", N = n)$data
prop.table(table(tr_data$SUBSCRIBED))

n <- dim(tr_data)[1]
# k=6-fold cross-validation
k <- 6
set.seed(1)
deiktes<-sample(1:n)	#random permutation of the rows
methods <- c('naiveBayes','svm', 'LDA', 'Random Forest')
accuracy <- matrix(data=NA, ncol= k, nrow = length(methods))
sensitivity <- matrix(data=NA, ncol= k, nrow = length(methods))
specificity  <- matrix(data=NA, ncol= k, nrow = length(methods))
ari <- matrix(data=NA, ncol= k, nrow = length(methods))

rownames(accuracy) <- rownames(ari) <- rownames(sensitivity) <- rownames(specificity) <- methods

for (i in 1:k){
  te <- deiktes[ ((i-1)*(n/k)+1):(i*(n/k))]	
  train <- tr_data[-te, ] 
  
  test <- tr_data[te, -21]
  
  #	naive Bayes
  z <- naiveBayes(formula, data = train)
  nvb_pr <- predict(z, test)
  
  conf_matrix<- confusionMatrix(nvb_pr,tr_data[te,'SUBSCRIBED'], positive='yes')
  accuracy['naiveBayes',i] <- round(conf_matrix$overall,5)['Accuracy'] 
  sensitivity['naiveBayes',i] <- conf_matrix$byClass["Sensitivity"]
  specificity['naiveBayes',i] <- conf_matrix$byClass["Specificity"] 
  ari['naiveBayes',i] <- adjustedRandIndex(as.matrix(nvb_pr), as.matrix(tr_data[te,'SUBSCRIBED']))
  
  #	svm
  fit1 <- svm(formula, data=train)
  svm_pr <- predict(fit1, newdata=test)
  
  conf_matrix<- confusionMatrix(svm_pr,tr_data[te,'SUBSCRIBED'], positive='yes')
  accuracy['svm',i] <- round(conf_matrix$overall,5)['Accuracy'] 
  sensitivity['svm',i] <- conf_matrix$byClass["Sensitivity"]
  specificity['svm',i] <- conf_matrix$byClass["Specificity"] 
  ari['svm',i] <- adjustedRandIndex(as.matrix(svm_pr), as.matrix(tr_data[te,'SUBSCRIBED']))
  
  #	LDA
  lda <- lda(formula, data = train)
  lda_pr <- predict(lda, test)$class
  
  conf_matrix<- confusionMatrix(lda_pr,tr_data[te,'SUBSCRIBED'], positive='yes')
  accuracy['LDA',i] <- round(conf_matrix$overall,5)['Accuracy'] 
  sensitivity['LDA',i] <- conf_matrix$byClass["Sensitivity"]
  specificity['LDA',i] <- conf_matrix$byClass["Specificity"] 
  ari['LDA',i] <- adjustedRandIndex(as.matrix(lda_pr), as.matrix(tr_data[te,'SUBSCRIBED']))
  
  
  # RANDOM FOREST MODEL
  # Create a Random Forest model with default parameters
  rfmodel <- randomForest(formula, data = train, mtry = floor(sqrt(11)), ntree = 600, importance = TRUE)
  rf_pr <- predict(rfmodel, newdata=test)
  
  conf_matrix<- confusionMatrix(rf_pr,tr_data[te,'SUBSCRIBED'], positive='yes')
  accuracy['Random Forest',i] <- round(conf_matrix$overall,5)['Accuracy'] 
  sensitivity['Random Forest',i] <- conf_matrix$byClass["Sensitivity"]
  specificity['Random Forest',i] <- conf_matrix$byClass["Specificity"] 
  ari['Random Forest',i] <- adjustedRandIndex(as.matrix(rf_pr), as.matrix(tr_data[te,'SUBSCRIBED']))

  

  
}


accuracy
ari


boxplot1 = boxplot(t(accuracy), ylab='predictive accuracy', xlab='method')

boxplot2 = boxplot(t(sensitivity), ylab='predictive sensitivity', xlab='method')

## barplots to visualize the results for each model ##

avg.accuracy <- as.numeric(apply(accuracy,1, mean))
average_accuracy <- as.data.frame(cbind(avg.accuracy, methods))
colnames(average_accuracy) <- c("value","classifier")

avg.sensitivity <- as.numeric(apply(sensitivity,1, mean))
average_sensitivity <- as.data.frame(cbind(avg.sensitivity, methods))
colnames(average_sensitivity) <- c("value","classifier")

avg.specificity <- as.numeric(apply(specificity,1, mean))
average_specificity <- as.data.frame(cbind(avg.specificity, methods))
colnames(average_specificity) <- c("value","classifier")

avg.ari <- as.numeric(apply(ari,1, mean))
average_ari <- as.data.frame(cbind(avg.ari, methods))
colnames(average_ari) <- c("value","classifier")

par(mfrow=c(2,2))
barplot(avg.accuracy, xlab = 'Average Accuracy', names.arg = methods, col = 30:length(methods), width=c(0.8,0.8,0.8,0.8))
barplot(avg.sensitivity, xlab = 'Average Sensitivity', names.arg = methods, col = 30:length(methods), width=c(0.8,0.8,0.8,0.8))
barplot(avg.specificity, xlab = 'Average Specificity', names.arg = methods, col = 30:length(methods), width=c(0.8,0.8,0.8,0.8))
barplot(avg.ari, xlab = 'Average Adjusted Rand Index', names.arg = methods, col = 30:length(methods), width=c(0.8,0.8,0.8,0.8))


## Variable importance on random forest model ##
library(randomForest)
varImpPlot(rfmodel, main="Variable Importance on Random Forest", col="blue")


## Evaluation of random forest ##
# confussion matrix #
rf_predictions <- predict(rfmodel, newdata=val_data)

conf_matrix<- confusionMatrix(rf_predictions,val_data$SUBSCRIBED, positive='yes')
conf_matrix


# ROC curve #

library(ROSE)
rf_pred<- predict(rfmodel, newdata=val_data,type = "prob")[,2] 
ROC_rf <- roc.curve(val_data$SUBSCRIBED, rf_pred)
ROC_rf

