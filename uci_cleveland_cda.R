# download and install libraries if needed

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(scales)) install.packages("scales", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(corrplot)) install.packages("corrplot", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(cran)) install.packages("cran", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(readr)) install.packages("readr", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(psych)) install.packages("psych", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(rpart)) install.packages("rpart", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(gbm)) install.packages("gbm", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(randomForest)) install.packages("randomForest", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(e1071)) install.packages("e1071", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(neuralnet)) install.packages("neuralnet", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(glmnet)) install.packages("glmnet", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(pROC)) install.packages("pROC", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(PRROC)) install.packages("PRROC", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(RWeka)) install.packages("RWeka", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(performanceEstimation)) install.packages("performanceEstimation", repos = "https://cran.rstudio.com/bin/windows/Rtools/")
if(!require(performanceEstimation)) install.packages("NeuralNetTools", repos = "https://cran.rstudio.com/bin/windows/Rtools/")

# install needed libraries
library("tidyverse")
library("dplyr")
library("data.table")
library("ggplot2")
library("scales")
library("corrplot")
library("caret")
# library("cran")
library("readr")
library("psych")
library("PerformanceAnalytics")
library("rpart")
library("rpart.plot")
library("gbm")
library("randomForest")
library("neuralnet")
library("e1071")
library("glmnet")
library("pROC")
library("PRROC")
require(gridExtra)
library("RWeka")
library("performanceEstimation")
library("NeuralNetTools")



# load dataset
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease
# https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data
dl <- tempfile()
dl_1 <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", dl)
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat", dl_1)
cleveland_data <- read.table(dl, sep = ",", col.names = c("age","sex","cp","testbps","chol","fbs","restecg","thalach","exang"
                                                          ,"oldpeak","slope","ca","thal","num"))
head(cleveland_data)
str(cleveland_data)                 

######################
#  Data Pre-processing
######################
set.seed(111)
options(digits=4)

# check rows for deletion or value substitution 
cd_to_delete <- cleveland_data %>% 
  filter(cleveland_data$ca == "?" | cleveland_data$thal == "?")
cd_to_delete
paste("Number of rows to delete: ", nrow(cd_to_delete))
paste("Number of rows before droping rows: ", nrow(cleveland_data))
# only 6 rows out of 303 !!
# we can simply delete these instead of going to the imputation process
cleveland_data <- cleveland_data %>% 
  filter(cleveland_data$ca != "?" & cleveland_data$thal != "?")
paste("Number of rows after droping rows: ", nrow(cleveland_data))

# convert the ca and thal columns to number
indx <- sapply(cleveland_data, is.factor)
cleveland_data[indx] <- lapply(cleveland_data[indx], function(x) as.numeric(as.character(x)))

# rename num col to hd to make it more meaningful and make values 0 and 1 (above 1 are 1s)
names(cleveland_data)[names(cleveland_data) == "num"] <- "hd"
# make hd (num) column binary, 0 = no disease, 1 = disease
cleveland_data <- cleveland_data %>%
  mutate(hd = ifelse(hd >0,1, hd))

# normalize data
data_testing_scaling <- scale((cleveland_data) %>%
                                select(c("testbps", "chol", "thalach", "oldpeak")))
data_testing_scaling <- data.frame(data_testing_scaling)
cleveland_data$testbps<-data_testing_scaling$testbps
cleveland_data$chol<-data_testing_scaling$chol
cleveland_data$thalach<-data_testing_scaling$thalach
cleveland_data$oldpeak<-data_testing_scaling$oldpeak

paste("Number of rows in a cleveland data: AFTER NORMALISING ", nrow(cleveland_data))

str(cleveland_data)

###################
# Data Analysis
###################
# Plot geneder vs disease (present or not)
# create a dataframe for plotting, plot objects, and combine for display
df1 <- cleveland_data %>%
  mutate(Disease = ifelse(hd == 0, "No", "Yes"), 
         Gender = ifelse(sex == 0, "Female", "Male"))
plot1 <- ggplot(data = df1, aes(x = Gender)) +
  geom_bar(aes(fill = Disease), 
           position = position_stack(reverse = F)) +
  ggtitle("Gender distribution") +
  xlab("Gender") + ylab("Counts")+
  scale_x_discrete(limits=c("Female","Male")) +
  theme(legend.position = "none")
plot2 <- ggplot(data = df1, aes(x = Gender)) +
  geom_bar(aes(fill = Disease), position = "fill") +
  ggtitle("Gender distribution in %") +
  xlab("Gender") + ylab("Percent")+
  scale_x_discrete(limits=c("Female","Male")) +
  scale_y_continuous(labels = percent)

grid.arrange(plot1, plot2, ncol=2, widths = c(1, 1.38))

# Plot age group versus disease (present or not)
# create a dataframe for plotting, plot objects, and combine for display
df2 <- cleveland_data %>%
  mutate(AgeGroup = ifelse(age %in% 20:40, "20-40",
                           ifelse(age %in% 41:60, "41-60", "61-70")))  %>%
  mutate(Disease = ifelse(hd < 1, "No", "Yes"))
plot21 <- ggplot(data = df2, aes(x = AgeGroup)) +
  geom_bar(aes(fill = Disease), position = (position_stack(reverse = F)), width=0.8) +
  ggtitle("Age group distribution") +
  xlab("Age group") + ylab("Counts")+
  theme(aspect.ratio = 2/1, legend.position="bottom")
plot22 <- ggplot(data = df2, aes(x = AgeGroup)) +
  geom_bar(aes(fill = Disease), position = "fill", width=0.8) +
  ggtitle("Age group distribution in %") +
  xlab("Age group") + ylab("Percent")+
  theme(aspect.ratio = 2/1, legend.position="bottom") +
  scale_y_continuous(labels = percent)
grid.arrange(plot21, plot22, ncol=2)

# check correlations bewweten all the attributes
corrplot(cor(cleveland_data), method="number", type = "upper", 
         number.cex = .7 )
# plot cp_type vs disease (present or not)
# create a dataframe for plotting, plot objects, and combine for display
df3 <- cleveland_data %>%
  mutate(ChestPainType = ifelse(cp <= 1, "typ.",
                                ifelse(cp <= 2, "atyp.", 
                                       ifelse(cp <= 3, "n-a.", 
                                              "asym."))))  %>%
  mutate(Disease = ifelse(hd < 1, "No", "Yes"))
plot31 <- ggplot(data = df3, aes(x = ChestPainType)) +
  geom_bar(aes(fill = Disease), position = (position_stack(reverse = F)), width=0.8) +
  xlab("Chest pain type") + ylab("Counts")+
  theme(axis.text.x = element_text(angle = 30, size = 12, hjust = 1)) 
plot32 <- ggplot(data = df3, aes(x = ChestPainType)) +
  geom_bar(aes(fill = Disease), position = "fill", width=0.8) +
  xlab("Chest pain type") + ylab("Percent") +
  theme(axis.text.x = element_text(angle = 30, size = 12, hjust = 1))
grid.arrange(plot31, plot32, ncol=2, nrow=1)

# plot excercise induced angina vs disease (present or not)
# create a dataframe for plotting, plot objects, and combine for display
df4 <- cleveland_data %>%
  mutate(ChestPainType = ifelse(exang == 1, "yes", "no")) %>%
  mutate(Disease = ifelse(hd < 1, "No", "Yes"))
plot41 <- ggplot(data = df4, aes(x = ChestPainType)) +
  geom_bar(aes(fill = Disease), position = (position_stack(reverse = F)), width=0.8) +
  xlab("Excercise Induced Angina") + ylab("Counts")+
  theme(aspect.ratio = 2/1, legend.position="none")
plot42 <- ggplot(data = df4, aes(x = ChestPainType)) +
  geom_bar(aes(fill = Disease), position = "fill", width=0.8) +
  xlab("Excercise Induced Angina") + ylab("Percent") +
  scale_y_continuous(labels = percent)

grid.arrange(plot41, plot42, ncol=2, nrow=2)

############
# SMOTE
############
#uncomment the below four lines of code if SMOTE is desired
#library(performanceEstimation)
#cleveland_data <- smote(hd ~ ., cleveland_data, perc.over = 2,perc.under=3, k=5)
#cleveland_data <- na.omit(cleveland_data)
#cleveland_data$hd <- as.numeric(cleveland_data$hd)

############################
# Prepare for Model Building
###########################
# split the dataset into train and test sets
paste("No. of rows after SMOTE set: ", nrow(cleveland_data))
trainInd <- createDataPartition(cleveland_data$hd, p=0.7, list=FALSE, times=1)
train_set <- cleveland_data[trainInd,]
test_set <- cleveland_data[-trainInd,]

paste("No. of rows in the Training set: ", nrow(train_set))
paste("No. of rows in the Test set: ", nrow(test_set))
paste("Test to Train set ratio is: ", nrow(test_set)/(nrow(train_set)+nrow(test_set)))

###########################
# Logistic Regression (GLM)
###########################
# fit the logistic regression model with glm function: generalized linear model 
glm_fit <- train(as.factor(hd) ~ ., data=train_set, method = "glm", family = "binomial")
# test the model using the predict function
glm_pred <- predict(glm_fit, newdata = test_set)
# build the confusion matrix
glm_cMatrix <- confusionMatrix(table(test_set$hd, glm_pred))
# extract parameters from the confusion matrix
accuracy_glm <- glm_cMatrix$overall["Accuracy"]
sensitivity_glm <- glm_cMatrix$byClass["Sensitivity"]
specificity_glm <- glm_cMatrix$byClass["Specificity"]
f1_glm <- 2*sensitivity_glm*specificity_glm/(sensitivity_glm+specificity_glm)
# get AUC
glm_AUC <- roc(test_set$hd, as.numeric(glm_pred))["auc"]
glm_AUC <- as.numeric(substr(glm_AUC, -7, 5))
# store the model outcome and view
model_accs <- tibble(Method = "Generalized Linear Model",
                     Accuracy = accuracy_glm,
                     "Sensitivity/Recall" = sensitivity_glm,
                     "F1 score" = f1_glm,
                     AUC = glm_AUC)
model_accs %>% knitr::kable()

#################
# Regression Tree
#################
# fit the regression tree model with rpart function
rt_fit <- rpart(as.factor(hd)~., data = train_set, method = "class")
# plot the regression tree
rpart.plot(rt_fit, cex=0.75, main = "Regression Tree for Heart Disease")
# test the model using the predict function
rt_pred <- predict(rt_fit, test_set, type = "class")
# build the confusion matrix
rt_cMatrix <- confusionMatrix(table(test_set$hd, rt_pred))
# extract the Matrix parameters
accuracy_rt <- rt_cMatrix$overall["Accuracy"]
sensitivity_rt <- rt_cMatrix$byClass["Sensitivity"]
specificity_rt <- rt_cMatrix$byClass["Specificity"]
f1_rt <- 2*sensitivity_rt*specificity_rt/(sensitivity_rt+specificity_rt)
# get AUC
rt_AUC <- roc(test_set$hd,as.numeric(rt_pred))["auc"]
rt_AUC <- as.numeric(substr(rt_AUC, -7, 5))
# store the model outcome and view
model_accs <- bind_rows(model_accs, 
                        tibble(Method = "Regression Tree Model", 
                               Accuracy = accuracy_rt,
                               "Sensitivity/Recall" = sensitivity_rt,
                               "F1 score" = f1_rt,
                               AUC = rt_AUC)
)
model_accs %>% knitr::kable()

###################
# Random Forest
###################
# fit the random forest model with the randomForest function
rf_fit <- randomForest(as.factor(hd) ~ ., data = train_set, importance=TRUE, ntree=500)
# test the model using the predict function
rf_pred <- predict(rf_fit, test_set)
# build the confusion matrix
rf_cMatrix <- confusionMatrix(table(test_set$hd, rf_pred))
# extract the Matrix parameters
accuracy_rf <- rf_cMatrix$overall["Accuracy"]
sensitivity_rf <- rf_cMatrix$byClass["Sensitivity"]
specificity_rf <- rf_cMatrix$byClass["Specificity"]
f1_rf <- 2*sensitivity_rf*specificity_rf/(sensitivity_rf+specificity_rf)
# get AUC
rf_AUC <- roc(test_set$hd,as.numeric(rf_pred))["auc"]
rf_AUC <- as.numeric(substr(rf_AUC, -7, 5))
# store the model outcome and view
model_accs <- bind_rows(model_accs, 
                        tibble(Method = "Random Forest Model", 
                               Accuracy = accuracy_rf,
                               "Sensitivity/Recall" = sensitivity_rf,
                               "F1 score" = f1_rf,
                               AUC = rf_AUC)
)
model_accs %>% knitr::kable()

##############################
# Support Vector Machine (SVM)
##############################
# fit the SVM model with the svm function
svm_fit <- svm(as.factor(hd) ~ ., data=train_set)
# test the model using the predict function
svm_pred <- predict(svm_fit, test_set)
# build the confusion matrix
svm_cMatrix <- confusionMatrix(table(test_set$hd, svm_pred))
# extract the Matrix parameters
accuracy_svm <- svm_cMatrix$overall["Accuracy"]
sensitivity_svm <- svm_cMatrix$byClass["Sensitivity"]
specificity_svm <- svm_cMatrix$byClass["Specificity"]
f1_svm <- 2*sensitivity_svm*specificity_svm/(sensitivity_svm+specificity_svm)
# get AUC
svm_AUC <- roc(test_set$hd,as.numeric(svm_pred))["auc"]
svm_AUC <- as.numeric(substr(svm_AUC, -7, 5))
# store the model outcome and view
model_accs <- bind_rows(model_accs, 
                        tibble(Method = "Support-Vector Machine", 
                               Accuracy = accuracy_svm,
                               "Sensitivity/Recall" = sensitivity_svm,
                               "F1 score" = f1_svm,
                               AUC = svm_AUC)
)
model_accs %>% knitr::kable()

#################
# Neural Networks
#################
# fit the neural networks model with the neuralnet function
nn_fit <- neuralnet(hd ~ ., data=train_set,  hidden = 3, act.fct="logistic", threshold=0.05) #changed thresh from 0.01, to 0.05
# plot the fit
plot(nn_fit)
# test the model using the predict function
nn_pred <- neuralnet::compute(nn_fit, test_set)
nn_prob <- nn_pred$net.result
nn_pred <- ifelse(nn_prob>0.6, 1, 0)

# build the confusion matrix
nn_cMatrix <- confusionMatrix(table(test_set$hd, nn_pred[,2]))
nn_cMatrix
# extract the Matrix parameters
accuracy_nn <- nn_cMatrix$overall["Accuracy"]
sensitivity_nn <- nn_cMatrix$byClass["Sensitivity"]
specificity_nn <- nn_cMatrix$byClass["Specificity"]
f1_nn <- 2*sensitivity_nn*specificity_nn/(sensitivity_nn+specificity_nn)
# get AUC
nn_AUC <- roc(test_set$hd,as.numeric(nn_pred[,2]))["auc"]
nn_AUC <- as.numeric(substr(nn_AUC, -7, 5))
# store the model outcome and view
model_accs <- bind_rows(model_accs, 
                        tibble(Method = "Neural Network", 
                               Accuracy = accuracy_nn,
                               "Sensitivity/Recall" = sensitivity_nn,
                               "F1 score" = f1_nn,
                               AUC = nn_AUC)
)
model_accs %>% knitr::kable()

library(NeuralNetTools) 
varImp_nn <- olden(nn_fit, "hd", bar_plot=FALSE)
varImp_nn
typeof(varImp_nn)
plot(varImp_nn)
####################
# K Nearest Neighbor
####################
# crete the control parameter to pass to the fit function
tr_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# fit the kNN model with the train function and knn method
knn_fit <- train(hd ~ ., data=train_set, method="knn", metric='Rsquared',
                  tuneGrid=expand.grid(k=1:70), 
                  trControl=tr_control, preProc=c("center", "scale"))
#knn_pred <- knn(train_set,test_set,cl=train_set$hd,k=10)
# test the model using the predict function
knn_pred <- predict(knn_fit, newdata=test_set)
knn_pred <- ifelse(knn_pred>0.4, 1, 0)
# build the confusion matrix
knn_cMatrix <- confusionMatrix(table(test_set$hd, knn_pred))
# extract the Matrix parameters
accuracy_knn <- knn_cMatrix$overall["Accuracy"]
sensitivity_knn <- knn_cMatrix$byClass["Sensitivity"]
specificity_knn <- knn_cMatrix$byClass["Specificity"]
f1_knn <- 2*sensitivity_knn*specificity_knn/(sensitivity_knn+specificity_knn)
# get AUC
knn_AUC <- roc(test_set$hd,as.numeric(knn_pred))["auc"]
knn_AUC <- as.numeric(substr(knn_AUC, -7, 5))
# store the model outcome and view
model_accs <- bind_rows(model_accs, 
                        tibble(Method = "k-Nearest Neighbors", 
                               Accuracy = accuracy_knn,
                               "Sensitivity/Recall" = sensitivity_knn,
                               "F1 score" = f1_knn,
                               AUC = knn_AUC)
)
model_accs %>% knitr::kable()

##################################
# Model Performance Tradeoffs
# Plot AUC-ROCs for all the models
##################################
# create object to combine plots
#par(mfrow=c(2,2))
par(mfrow=c(3,2))
par(pty = "s")
par(oma = c(3, 3, 3, 3), mar=c(1, 1, 1, 1), mgp=c(2, 0.8, 0), las=0)

# plot AUC-ROC for the Logistic Regression model
glm_obj <- roc(test_set$hd, as.numeric(glm_pred), ci=TRUE, ci.alpha=0.9, 
               plot=TRUE, legacy.axes = TRUE, percent=TRUE,
               #xlab="False Positive Percentage", ylab="True Positive Percentage",
               xlab="FPP", ylab="TPP",
               print.auc.x=80, print.auc.y=60,
               print.auc=TRUE, show.thres=TRUE)
sens.ci_glm <- ci.se(glm_obj)
plot(sens.ci_glm, type = "shape", col="lightblue", 
     title(main="GLM - ROC", line=.25))
     #title(main="Generalized Linear Model - ROC", line=3))


# plot AUC-ROC for Regression Tree model
rt_obj <- roc(test_set$hd, as.numeric(rt_pred), ci=TRUE, ci.alpha=0.9,
              plot=TRUE, legacy.axes = TRUE, percent=TRUE,
              #xlab="False Positive Percentage", ylab="True Positive Percentage",
              xlab="FPP", ylab="TPP",
              print.auc.x=75, print.auc.y=60,
              print.auc=TRUE, show.thres=TRUE)
sens.ci_rt <- ci.se(rt_obj)
plot(sens.ci_rt, type = "shape", col="lightblue", 
     title(main="Regression Tree - ROC", line=.25))

# plot AUC-ROC for the Random Forest model
rf_obj <- roc(test_set$hd, as.numeric(rf_pred), ci=TRUE, ci.alpha=0.9, 
              plot=TRUE, legacy.axes = TRUE, percent=TRUE,
              #xlab="False Positive Percentage", ylab="True Positive Percentage",
              xlab="FPP", ylab="TPP",
              print.auc.x=75, print.auc.y=60,
              print.auc=TRUE, show.thres=TRUE)
sens.ci_rf <- ci.se(rf_obj)
plot(sens.ci_rf, type = "shape", col="lightblue", 
     title(main="Random Forest - ROC", line=.25))

# plot AUC-ROC for the SVM model
svm_obj <- roc(test_set$hd, as.numeric(svm_pred), ci=TRUE, ci.alpha=0.9, 
               plot=TRUE, legacy.axes = TRUE, percent=TRUE,
               #xlab="False Positive Percentage", ylab="True Positive Percentage",
               xlab="FPP", ylab="TPP",
               print.auc.x=75, print.auc.y=60,
               print.auc=TRUE, show.thres=TRUE)
sens.ci_svm <- ci.se(svm_obj)
plot(sens.ci_svm, type = "shape", col="lightblue", 
     title(main="SVM - ROC", line=.25))

# plot AUC-ROC for the Neural Networks model
nn_obj <- roc(test_set$hd, as.numeric(nn_pred),ci=TRUE, ci.alpha=0.9, 
              plot=TRUE, legacy.axes = TRUE, percent=TRUE,
              #xlab="False Positive Percentage", ylab="True Positive Percentage",
              xlab="FPP", ylab="TPP",
              print.auc.x=75, print.auc.y=60,
              print.auc=TRUE, show.thres=TRUE)
sens.ci_nn <- ci.se(nn_obj)
plot(sens.ci_nn, type = "shape", col="lightblue", 
     title(main="Neural Network - ROC", line=.25))

# plot AUC-ROC for k-Nearest Neighbors model
knn_obj <- roc(test_set$hd, as.numeric(knn_pred), ci=TRUE, ci.alpha=0.9, 
               plot=TRUE, legacy.axes = TRUE, percent=TRUE,
               #xlab="False Positive Percentage", ylab="True Positive Percentage",
               xlab="FPP", ylab="TPP",
               print.auc.x=75, print.auc.y=60,
               print.auc=TRUE, show.thres=TRUE)
sens.ci_knn <- ci.se(knn_obj)
plot(sens.ci_knn, type = "shape", col="lightblue", 
     title(main="KNN - ROC", line=.25))

par(mfrow=c(1,1))
# Make a composite plot of ROC-AUCs
plot(glm_obj, col="darkred")
plot(rt_obj, col="darkblue", add=TRUE)
plot(rf_obj, col="darkorange1", add=TRUE)
plot(svm_obj, col="darkgreen", add=TRUE)
plot(nn_obj, col="darkmagenta", add=TRUE)
plot(knn_obj, col="chocolate3", add=TRUE)
legend("bottomright", c("GLM","RT","RF","SVM","NN","kNN"), lty=1, lwd=2, cex=0.7,
col=c("darkred","darkblue","darkorange1","darkgreen","darkmagenta","chocolate3"))


###########################################
# Variables of Importance in the Best Model 
# (Neural Network)
##########################################

# varImp_nn <- olden(nn_fit, "hd", bar_plot=FALSE)
var_imp <- data.frame( olden(nn_fit, "hd", bar_plot=FALSE))
var_imp <- cbind(var_names = rownames(var_imp), var_imp)
var_imp <- subset(var_imp, select = -c(X1) )
rownames(var_imp) <- NULL
var_imp <- var_imp %>% 
  rename(
    values = X0
  )
var_imp <- var_imp[order(-var_imp$values),]

plot(var_imp$values, var_imp$var_names,            # plot the variables 
     xlab= "values",        # x−axis label 
     ylab="var names")  

# dev.off()
# Random Forest models outperformed other models based on the accuracy, F-1 score
###########################################
# Variables of Importance in the Best Model 
# (Random Forset)
##########################################

var_imp <- data.frame(varImp(rf_fit))
var_imp <- cbind(var_names = rownames(var_imp), var_imp)
var_imp <- subset(var_imp, select = -c(X1) )
rownames(var_imp) <- NULL
var_imp <- var_imp %>% 
  rename(
    values = X0
  )
var_imp <- var_imp[order(-var_imp$values),]

plot(var_imp$values, var_imp$var_names,            # plot the variables 
     xlab= "values",        # x−axis label 
     ylab="var names")    
varImpPlot(rf_fit, type=2, main="Variables of Importance", 
           pch = 16, col="red", col.lab="black",
           n.var=min(20, nrow(rf_fit$importance)))
var_imp
# Plot varimp plot for glm_fit.
varImp(glm_fit)


