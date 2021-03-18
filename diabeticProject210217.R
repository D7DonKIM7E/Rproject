install.packages("tidyverse")
library(tidyverse)


diabetic_data <- read.csv("C:/Users/puppy/Downloads/diabetic_data.csv")
diabetic_data <- data.frame(diabetic_data[1:5], diabetic_data[7:10],diabetic_data[13:50])
diabetic_data[diabetic_data=='?'] = NA
diabetic_data <- na.omit(diabetic_data)


diabetic_data_race <- diabetic_data[3]
diabetic_data_race[diabetic_data_race == "Caucasian"] = 1
diabetic_data_race[diabetic_data_race == "AfricanAmerican"] = 2
diabetic_data_race[diabetic_data_race == "Asian"] = 3
diabetic_data_race[diabetic_data_race == "Hispanic"] = 4
diabetic_data_race[diabetic_data_race == "Other"] = 5



diabetic_data_gender <- diabetic_data[4]
diabetic_data_gender[diabetic_data_gender == "Female"] = 1
diabetic_data_gender[diabetic_data_gender == "Male"] = 2
diabetic_data_gender[diabetic_data_gender == "Unknown/Invalid"] = 3

diabetic_data_age <- diabetic_data[5]
diabetic_data_age[diabetic_data_age == "[0-10)"] = 1
diabetic_data_age[diabetic_data_age == "[10-20)"] = 10
diabetic_data_age[diabetic_data_age == "[20-30)"] = 20
diabetic_data_age[diabetic_data_age == "[30-40)"] = 30
diabetic_data_age[diabetic_data_age == "[40-50)"] = 40
diabetic_data_age[diabetic_data_age == "[50-60)"] = 50
diabetic_data_age[diabetic_data_age == "[60-70)"] = 60
diabetic_data_age[diabetic_data_age == "[70-80)"] = 70
diabetic_data_age[diabetic_data_age == "[80-90)"] = 80
diabetic_data_age[diabetic_data_age == "[90-100)"] = 90

diabetic_data_max_glu_serum <- diabetic_data[20]
diabetic_data_max_glu_serum[diabetic_data_max_glu_serum == "None"] = 1
diabetic_data_max_glu_serum[diabetic_data_max_glu_serum == "Norm"] = 2
diabetic_data_max_glu_serum[diabetic_data_max_glu_serum == ">200"] = 3
diabetic_data_max_glu_serum[diabetic_data_max_glu_serum == ">300"] = 4

diabetic_data_A1CResult <- diabetic_data[21]
diabetic_data_A1CResult[diabetic_data_A1CResult == "None"] = 1
diabetic_data_A1CResult[diabetic_data_A1CResult == "Norm"] = 2
diabetic_data_A1CResult[diabetic_data_A1CResult == ">7"] = 3
diabetic_data_A1CResult[diabetic_data_A1CResult == ">8"] = 4

diabetic_data_metformin <- diabetic_data[22]
diabetic_data_metformin[diabetic_data_metformin == "No"] = 1
diabetic_data_metformin[diabetic_data_metformin == "Down"] = 2
diabetic_data_metformin[diabetic_data_metformin == "Steady"] = 3
diabetic_data_metformin[diabetic_data_metformin == "Up"] = 4


diabetic_data_repaglinide <- diabetic_data[23]
diabetic_data_repaglinide[diabetic_data_repaglinide == "No"] = 1
diabetic_data_repaglinide[diabetic_data_repaglinide == "Down"] = 2
diabetic_data_repaglinide[diabetic_data_repaglinide == "Steady"] = 3
diabetic_data_repaglinide[diabetic_data_repaglinide == "Up"] = 4

diabetic_data_nateglinide <- diabetic_data[24]
diabetic_data_nateglinide[diabetic_data_nateglinide == "No"] = 1
diabetic_data_nateglinide[diabetic_data_nateglinide == "Down"] = 2
diabetic_data_nateglinide[diabetic_data_nateglinide == "Steady"] = 3
diabetic_data_nateglinide[diabetic_data_nateglinide == "Up"] = 4

diabetic_data_chorpropamide <- diabetic_data[25]
diabetic_data_chorpropamide[diabetic_data_chorpropamide == "No"] = 1
diabetic_data_chorpropamide[diabetic_data_chorpropamide == "Down"] = 2
diabetic_data_chorpropamide[diabetic_data_chorpropamide == "Steady"] = 3
diabetic_data_chorpropamide[diabetic_data_chorpropamide == "Up"] = 4


diabetic_data_glimepiride <- diabetic_data[26]
diabetic_data_glimepiride[diabetic_data_glimepiride == "No"] = 1
diabetic_data_glimepiride[diabetic_data_glimepiride == "Down"] = 2
diabetic_data_glimepiride[diabetic_data_glimepiride == "Steady"] = 3
diabetic_data_glimepiride[diabetic_data_glimepiride == "Up"] = 4

diabetic_data_acetohexamide <- diabetic_data[27]
diabetic_data_acetohexamide[diabetic_data_acetohexamide == "No"] = 1
diabetic_data_acetohexamide[diabetic_data_acetohexamide == "Down"] = 2
diabetic_data_acetohexamide[diabetic_data_acetohexamide == "Steady"] = 3
diabetic_data_acetohexamide[diabetic_data_acetohexamide == "Up"] = 4

diabetic_data_glipizide <- diabetic_data[28]
diabetic_data_glipizide[diabetic_data_glipizide == "No"] = 1
diabetic_data_glipizide[diabetic_data_glipizide == "Down"] = 2
diabetic_data_glipizide[diabetic_data_glipizide == "Steady"] = 3
diabetic_data_glipizide[diabetic_data_glipizide == "Up"] = 4

diabetic_data_glyburide <- diabetic_data[29]
diabetic_data_glyburide[diabetic_data_glyburide == "No"] = 1
diabetic_data_glyburide[diabetic_data_glyburide == "Down"] = 2
diabetic_data_glyburide[diabetic_data_glyburide == "Steady"] = 3
diabetic_data_glyburide[diabetic_data_glyburide == "Up"] = 4

diabetic_data_tolbutamide <- diabetic_data[30]
diabetic_data_tolbutamide[diabetic_data_tolbutamide == "No"] = 1
diabetic_data_tolbutamide[diabetic_data_tolbutamide == "Down"] = 2
diabetic_data_tolbutamide[diabetic_data_tolbutamide == "Steady"] = 3
diabetic_data_tolbutamide[diabetic_data_tolbutamide == "Up"] = 4

diabetic_data_pioglitazone <- diabetic_data[31]
diabetic_data_pioglitazone[diabetic_data_pioglitazone == "No"] = 1
diabetic_data_pioglitazone[diabetic_data_pioglitazone == "Down"] = 2
diabetic_data_pioglitazone[diabetic_data_pioglitazone == "Steady"] = 3
diabetic_data_pioglitazone[diabetic_data_pioglitazone == "Up"] = 4

diabetic_data_rosiglitazone <- diabetic_data[32]
diabetic_data_rosiglitazone[diabetic_data_rosiglitazone == "No"] = 1
diabetic_data_rosiglitazone[diabetic_data_rosiglitazone == "Down"] = 2
diabetic_data_rosiglitazone[diabetic_data_rosiglitazone == "Steady"] = 3
diabetic_data_rosiglitazone[diabetic_data_rosiglitazone == "Up"] = 4

diabetic_data_acarbose <- diabetic_data[33]
diabetic_data_acarbose[diabetic_data_acarbose == "No"] = 1
diabetic_data_acarbose[diabetic_data_acarbose == "Down"] = 2
diabetic_data_acarbose[diabetic_data_acarbose == "Steady"] = 3
diabetic_data_acarbose[diabetic_data_acarbose == "Up"] = 4

diabetic_data_miglitol <- diabetic_data[34]
diabetic_data_miglitol[diabetic_data_miglitol == "No"] = 1
diabetic_data_miglitol[diabetic_data_miglitol == "Down"] = 2
diabetic_data_miglitol[diabetic_data_miglitol == "Steady"] = 3
diabetic_data_miglitol[diabetic_data_miglitol == "Up"] = 4

diabetic_data_troglitazone <- diabetic_data[35]
diabetic_data_troglitazone[diabetic_data_troglitazone == "No"] = 1
diabetic_data_troglitazone[diabetic_data_troglitazone == "Down"] = 2
diabetic_data_troglitazone[diabetic_data_troglitazone == "Steady"] = 3
diabetic_data_troglitazone[diabetic_data_troglitazone == "Up"] = 4

diabetic_data_tolazamide <- diabetic_data[36]
diabetic_data_tolazamide[diabetic_data_tolazamide == "No"] = 1
diabetic_data_tolazamide[diabetic_data_tolazamide == "Down"] = 2
diabetic_data_tolazamide[diabetic_data_tolazamide == "Steady"] = 3
diabetic_data_tolazamide[diabetic_data_tolazamide == "Up"] = 4

diabetic_data_examide <- diabetic_data[37]
diabetic_data_examide[diabetic_data_examide == "No"] = 1
diabetic_data_examide[diabetic_data_examide == "Down"] = 2
diabetic_data_examide[diabetic_data_examide == "Steady"] = 3
diabetic_data_examide[diabetic_data_examide == "Up"] = 4

diabetic_data_citoglipton <- diabetic_data[38]
diabetic_data_citoglipton[diabetic_data_citoglipton == "No"] = 1
diabetic_data_citoglipton[diabetic_data_citoglipton == "Down"] = 2
diabetic_data_citoglipton[diabetic_data_citoglipton == "Steady"] = 3
diabetic_data_citoglipton[diabetic_data_citoglipton == "Up"] = 4

diabetic_data_insulin <- diabetic_data[39]
diabetic_data_insulin[diabetic_data_insulin == "No"] = 1
diabetic_data_insulin[diabetic_data_insulin == "Down"] = 2
diabetic_data_insulin[diabetic_data_insulin == "Steady"] = 3
diabetic_data_insulin[diabetic_data_insulin == "Up"] = 4


diabetic_data_glyburide_metformin <- diabetic_data[40]
diabetic_data_glyburide_metformin[diabetic_data_glyburide_metformin == "No"] = 1
diabetic_data_glyburide_metformin[diabetic_data_glyburide_metformin == "Down"] = 2
diabetic_data_glyburide_metformin[diabetic_data_glyburide_metformin == "Steady"] = 3
diabetic_data_glyburide_metformin[diabetic_data_glyburide_metformin == "Up"] = 4

diabetic_data_glipizide_metformin <- diabetic_data[41]
diabetic_data_glipizide_metformin[diabetic_data_glipizide_metformin == "No"] = 1
diabetic_data_glipizide_metformin[diabetic_data_glipizide_metformin == "Down"] = 2
diabetic_data_glipizide_metformin[diabetic_data_glipizide_metformin == "Steady"] = 3
diabetic_data_glipizide_metformin[diabetic_data_glipizide_metformin == "Up"] = 4

diabetic_data_glimepiride_pioglitazone <- diabetic_data[42]
diabetic_data_glimepiride_pioglitazone[diabetic_data_glimepiride_pioglitazone == "No"] = 1
diabetic_data_glimepiride_pioglitazone[diabetic_data_glimepiride_pioglitazone == "Down"] = 2
diabetic_data_glimepiride_pioglitazone[diabetic_data_glimepiride_pioglitazone == "Steady"] = 3
diabetic_data_glimepiride_pioglitazone[diabetic_data_glimepiride_pioglitazone == "Up"] = 4

diabetic_data_metformin_rosiglitazone <- diabetic_data[43]
diabetic_data_metformin_rosiglitazone[diabetic_data_metformin_rosiglitazone == "No"] = 1
diabetic_data_metformin_rosiglitazone[diabetic_data_metformin_rosiglitazone == "Down"] = 2
diabetic_data_metformin_rosiglitazone[diabetic_data_metformin_rosiglitazone == "Steady"] = 3
diabetic_data_metformin_rosiglitazone[diabetic_data_metformin_rosiglitazone == "Up"] = 4

diabetic_data_metformin_pioglitazone <- diabetic_data[44]
diabetic_data_metformin_pioglitazone[diabetic_data_metformin_pioglitazone == "No"] = 1
diabetic_data_metformin_pioglitazone[diabetic_data_metformin_pioglitazone == "Down"] = 2
diabetic_data_metformin_pioglitazone[diabetic_data_metformin_pioglitazone == "Steady"] = 3
diabetic_data_metformin_pioglitazone[diabetic_data_metformin_pioglitazone == "Up"] = 4

diabetic_data_change <- diabetic_data[45]
diabetic_data_change[diabetic_data_change == "No"] = 1
diabetic_data_change[diabetic_data_change == "Ch"] = 2

diabetic_data_diabetesMed <- diabetic_data[46]
diabetic_data_diabetesMed[diabetic_data_diabetesMed == "No"] = 1
diabetic_data_diabetesMed[diabetic_data_diabetesMed == "Yes"] = 2

#model 1 data label
diabetic_data_readmitted <- diabetic_data[47]
diabetic_data_readmitted[diabetic_data_readmitted == "NO"] = 0
diabetic_data_readmitted[diabetic_data_readmitted == "<30"] = 1
diabetic_data_readmitted[diabetic_data_readmitted == ">30"] = 0

#model 2 data label
diabetic_data_readmitted <- diabetic_data[47]
diabetic_data_readmitted[diabetic_data_readmitted == "NO"] = NA
diabetic_data_readmitted[diabetic_data_readmitted == "<30"] = 1
diabetic_data_readmitted[diabetic_data_readmitted == ">30"] = 0

##creating data frame
diabetic_data3 <- data.frame( diabetic_data_race, diabetic_data_gender, diabetic_data_age, diabetic_data[6:15],diabetic_data[19], diabetic_data_max_glu_serum, diabetic_data_A1CResult,  diabetic_data_metformin, diabetic_data_repaglinide, diabetic_data_nateglinide, diabetic_data_chorpropamide, diabetic_data_glimepiride, diabetic_data_acetohexamide, diabetic_data_glipizide, diabetic_data_glyburide, diabetic_data_tolbutamide, diabetic_data_pioglitazone, diabetic_data_rosiglitazone,diabetic_data_acarbose, diabetic_data_miglitol, diabetic_data_troglitazone,diabetic_data_tolazamide, diabetic_data_examide, diabetic_data_citoglipton, diabetic_data_insulin, diabetic_data_glyburide_metformin, diabetic_data_glipizide_metformin, diabetic_data_glimepiride_pioglitazone, diabetic_data_metformin_rosiglitazone, diabetic_data_metformin_pioglitazone, diabetic_data_change, diabetic_data_diabetesMed, diabetic_data_readmitted)

# To convert data set to integer
for (j in 1:42) {
  diabetic_data3[,j] <- as.integer(diabetic_data3[,j])
}

str(diabetic_data3)
diabetic_data3 <- na.omit(diabetic_data3)

cor(diabetic_data3[,42],diabetic_data3[,1:41], method = "spearman") 

train <- diabetic_data3[1:35000,]
train <- as.data.frame(train)
summary(train)
test <- diabetic_data3[35001:50000,]
test <- as.data.frame((test))

install.packages("plyr")
library(plyr)
count(train$readmitted)
count(test$readmitted)

glm.fit <- glm(formula = readmitted ~., family=binomial(link='logit'),data=train)
glm.fit <- glm(formula = readmitted ~ age + discharge_disposition_id + time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + A1Cresult + metformin + repaglinide + diabetesMed, family=binomial(link='logit'),data=train)

summary(glm.fit)
glm_response_scores <- predict(glm.fit, test, type="response")


# To plot the ROC curve as typically use ROCR

install.packages("ROCR")  #needs to be installed first. This needs to be run only once.

library(ROCR)
pred <- prediction(glm_response_scores, test$readmitted)
perf <- performance(pred,"tpr","fpr")
plot(perf)

glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:15000]
install.packages("InformationValue")
library(InformationValue)

optCutOff <- optimalCutoff(train$readmitted, glm.probs) #CutOff point
optCutOff

install.packages("cutpointr")
library(cutpointr)
opt_cut <- cutpointr(train, readmitted, train, direction = ">=", pos_class = "yes",
                     neg_class = "no", method = maximize_metric, metric = youden)

install.packages("OptimalCutpoints")
library(OptimalCutpoints)
optimal.cutpoint.Youden <- optimal.cutpoints(X = readmitted ~., tag.healthy = 0, 
                                             methods = "Youden", data = train, pop.prev = NULL, categorical.cov = "readmitted", 
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

glm.pred <- ifelse(glm.probs > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")

table(glm.pred[1:6855],test$readmitted)

sensitivity(train$readmitted, glm.pred, threshold = optCutOff)
specificity(train$readmitted, glm.pred, threshold = optCutOff)

glm_link_scores <- predict(glm.fit, test, type="link")
glm_response_scores <- predict(glm.fit, test, type="response")



install.packages("pROC")
library(pROC)
plot(roc(test$readmitted, glm_response_scores, direction="<"), col="yellow", lwd=3, main="The turtle finds its way")
pROC_obj <- roc(test$readmitted,glm_link_scores,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


install.packages("PRROC")
library(PRROC)

PRROC_obj <- roc.curve(scores.class0 = rf_response_scores, weights.class0=train$readmitted,
                       curve=TRUE)
plot(PRROC_obj)


## Random Forest

install.packages("randomForest")

library(randomForest)

dim(train)

rf <- randomForest(
  readmitted ~ age + discharge_disposition_id + time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + A1Cresult + metformin + repaglinide + diabetesMed,
  data=train
)
summary(rf)

rf1 <-randomForest(
  readmitted ~ .,
  data=train,
  importance = TRUE
)

summary(rf)

predRF2 <- predict(rf, test[,-42])
optCutOff <- optimalCutoff(train$readmitted, predRF2) #CutOff point
optCutOff
predRF2 <- ifelse(predRF2 > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")
table(predRF2, test$readmitted)
perfRF <- performance(predRF2,"tpr","fpr")
plot(perfRF)


rf_response_scores <- predict(rf, test, type="response")
predRf <- prediction(rf_response_scores, test$readmitted)
perfRf <- performance(predRf,"tpr","fpr")
plot(perfRf)

sensitivity(train$readmitted, predRF2, threshold = optCutOff)
specificity(train$readmitted, predRF2, threshold = optCutOff)


mean(rf.pred == diabetic_data3$readmitted)

rf_link_scores <- predict(rf, test, type="link")

rf_response_scores <- predict(rf, test, type="response")

chisq.test(rf_response_scores)
chisq.test(predRF2, test$readmitted, correct=FALSE)


install.packages("pROC")
library(pROC)
plot(roc(test$readmitted, rf_response_scores, direction="<"), col="yellow", lwd=3, main="The turtle finds its way")
pROC_obj <- roc(test$readmitted,rf_response_scores,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


## random forest variable ranking
importance(rf1)
varImp(rf)


## Support Vector Machine

install.packages("caret")
library(caret)


install.packages("e1071")
library(e1071)

svmfit = svm(readmitted ~., data = train, kernel = "linear", cost = 1, scale = FALSE)


svmfit = svm(readmitted ~ age + discharge_disposition_id + time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + A1Cresult + metformin + repaglinide + diabetesMed, data = train, kernel = "linear", cost = 10, scale = FALSE)


summary(svmfit)

svmTrain <- train(train$readmitted ~., data = train, method = "svmLinear",
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_response_scores <- predict(svmfit, test, type="response")
svm_probs <- predict(svmfit, test, type="prob")

predSvm <- prediction(svm_response_scores, test$readmitted)
predSvm2 <- predict(svmfit, test[,-42])
table(predSvm2, test$readmitted)
perfSvm <- performance(predSvm,"tpr","fpr")
plot(perfSvm)

optCutOff <- optimalCutoff(train$readmitted, predSvm2) #CutOff point
optCutOff
predSvm2 <- ifelse(predSvm2 > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")
table(predSvm2, test$readmitted)

sensitivity(train$readmitted, predSvm2, threshold = optCutOff)
specificity(train$readmitted, predSvm2, threshold = optCutOff)

confusionMatrix(predSvm2,train$readmitted)

table(test$readmitted, predSvm2)

mean(predSvm == train$readmitted)

svm_link_scores <- predict(svmfit, test, type="link")

svm_response_scores <- predict(svmfit, test, type="response")

chisq.test(svm_response_scores)
chisq.test(predSvm2, test$readmitted, correct=FALSE)

install.packages("pROC")
library(pROC)
plot(roc(test$readmitted, svm_response_scores, direction="<"), col="yellow", lwd=3, main="ROC curve")
pROC_obj <- roc(test$readmitted,svm_response_scores,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

install.packages("tensorflow")
library(tensorflow)


install.packages("keras")
library(keras)

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")



#############################
#SVM

install.packages("caret")
install.packages("kernlab")
install.packages("ROCR")

library(caret)
library(kernlab)
library(ROCR)

dim(diabetic_data3)
table(diabetic_data3$readmitted)

table(diabetic_data3$readmitted) / length(diabetic_data3$readmitted)

Index <- createDataPartition(diabetic_data3$readmitted, p=.5,list=FALSE)

svm.train_test <- diabetic_data3[Index,]
svm.validation <- diabetic_data3[-Index,]
Index_tt <- createDataPartition(svm.train_test$readmitted, p=.7,list=FALSE)

svm.train <- svm.train_test[Index_tt,]
svm.test <- svm.train_test[-Index_tt,]
trainX <- svm.train[,3]

set.seed(123)
ctrl <- trainControl(method = "cv", number=2, classProbs =  TRUE)
grid <- expand.grid(sigma= c(.01, .015, 0.2), C = c(0.75,0.9,1,1.1,1.25))
svm.tune <- train(svm.train[age],svm.train[readmitted], method="svmRadial",metric="ROC", tuneGrid = grid, trControl=ctrl)

svm_prob <- predict(svmfit, svm.validation, type="prob")
pred_var <- prediction(svm_prob,svm.validation$readmitted)
pred_var
perf_val <- performance(pred_var, "auc")
perf_val 
perf_val2 <- performance(pred_var, "tpr", "fpr")
plot(perf_val2, col ="green")


# Random Forest
require(randomForest)
system.time(modForest <- randomForest(readmitted~., data=train, importance = TRUE, proximity = TRUE))
modForest <- randomForest(readmitted~num_procedures, data=train, importance = TRUE, proximity = TRUE)

### Validation_model1

valid <- diabetic_data3[50001:98053,]
valid <- as.data.frame(valid)

valid1 <- diabetic_data3[50001:59510,]
valid1 <- as.data.frame(valid1)

valid2 <- diabetic_data3[59511:69020,]
valid2 <- as.data.frame(valid2)

valid3 <- diabetic_data3[69021:78530,]
valid3 <- as.data.frame(valid3)

valid4 <- diabetic_data3[78531:88040,]
valid4 <- as.data.frame(valid4)

valid5 <- diabetic_data3[88041:98053,]
valid5 <- as.data.frame(valid5)


### Validation_model2

valid <- diabetic_data3[22851:45715,]
valid <- as.data.frame(valid)

valid1 <- diabetic_data3[22851:27424,]
valid1 <- as.data.frame(valid1)

valid2 <- diabetic_data3[27425:31997,]
valid2 <- as.data.frame(valid2)

valid3 <- diabetic_data3[31998:36570,]
valid3 <- as.data.frame(valid3)

valid4 <- diabetic_data3[36571:41143,]
valid4 <- as.data.frame(valid4)

valid5 <- diabetic_data3[41144:45715,]
valid5 <- as.data.frame(valid5)


## Prevalence of data
install.packages("plyr")
library(plyr)
count(valid$readmitted)
count(valid1$readmitted)
count(valid2$readmitted)
count(valid3$readmitted)
count(valid4$readmitted)
count(valid5$readmitted)


#logistic regression

summary(glm.fit)
glm_response_scores <- predict(glm.fit, valid, type="response")


# To plot the ROC curve as typically use ROCR

install.packages("ROCR")  #needs to be installed first. This needs to be run only once.

library(ROCR)
pred <- prediction(glm_response_scores, valid$readmitted)
perf <- performance(pred,"tpr","fpr")
plot(perf)

glm.probs <- predict(glm.fit,type = "response")


install.packages("cutpointr")
library(cutpointr)
opt_cut <- cutpointr(train, readmitted, valid, direction = ">=", pos_class = "yes",
                     neg_class = "no", method = maximize_metric, metric = youden)

install.packages("OptimalCutpoints")
library(OptimalCutpoints)
optimal.cutpoint.Youden <- optimal.cutpoints(X = readmitted ~., tag.healthy = 0, 
                                             methods = "Youden", data = train, pop.prev = NULL, categorical.cov = "readmitted", 
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

glm.pred <- ifelse(glm.probs > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")

table(glm.pred[1:48053],valid$readmitted)

sensitivity(valid$readmitted, glm.pred, threshold = optCutOff)
specificity(valid$readmitted, glm.pred, threshold = optCutOff)

glm_link_scores <- predict(glm.fit, valid, type="link")
glm_response_scores <- predict(glm.fit, valid, type="response")



install.packages("pROC")
library(pROC)
plot(roc(valid$readmitted, glm_response_scores, direction="<"), col="yellow", lwd=3, main="The turtle finds its way")
pROC_obj <- roc(valid$readmitted,glm_link_scores,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)



## Support Vector Machine

install.packages("caret")
library(caret)


install.packages("e1071")
library(e1071)

svm_response_scores <- predict(svmfit, valid, type="response")
svm_probs <- predict(svmfit, valid, type="prob")

predSvm <- prediction(svm_response_scores, valid$readmitted)
predSvm2 <- predict(svmfit, valid[,-42])
table(predSvm2, valid$readmitted)
perfSvm <- performance(predSvm,"tpr","fpr")
plot(perfSvm)

optCutOff <- optimalCutoff(valid$readmitted, predSvm2) #CutOff point
optCutOff
predSvm2 <- ifelse(predSvm2 > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")
table(predSvm2, valid$readmitted)

sensitivity(valid$readmitted, predSvm2, threshold = optCutOff)
specificity(valid$readmitted, predSvm2, threshold = optCutOff)

confusionMatrix(predSvm2,valid$readmitted)

table(valid$readmitted, predSvm2)

mean(predSvm == valid$readmitted)

svm_link_scores <- predict(svmfit, valid, type="link")

svm_response_scores <- predict(svmfit, valid, type="response")

chisq.test(svm_response_scores)
chisq.test(predSvm2, valid$readmitted, correct=FALSE)

install.packages("pROC")
library(pROC)
plot(roc(valid$readmitted, svm_response_scores, direction="<"), col="yellow", lwd=3, main="ROC curve")
pROC_obj <- roc(valid$readmitted,svm_response_scores,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Random Forest
require(randomForest)

summary(rf)

predRF2 <- predict(rf, valid[,-42])
optCutOff <- optimalCutoff(valid$readmitted, predRF2) #CutOff point
optCutOff
predRF2 <- ifelse(predRF2 > optCutOff, "Admission in <30 day", "Admission>30 days or no re-admission")
table(predRF2, valid$readmitted)
perfRF <- performance(predRF2,"tpr","fpr")
plot(perfRF)


rf_response_scores <- predict(rf, valid, type="response")
predRf <- prediction(rf_response_scores, valid$readmitted)
perfRf <- performance(predRf,"tpr","fpr")
plot(perfRf)

sensitivity(valid$readmitted, predRF2, threshold = optCutOff)
specificity(valid$readmitted, predRF2, threshold = optCutOff)


mean(rf.pred == diabetic_data3$readmitted)

rf_link_scores <- predict(rf, valid, type="link")

rf_response_scores <- predict(rf, valid, type="response")

chisq.test(rf_response_scores)
chisq.test(predRF2, valid$readmitted, correct=FALSE)
