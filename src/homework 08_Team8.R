#############################Homework8_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린
PRSA_data <- read.csv("PRSA_data.csv")
library(dplyr)
glimpse(PRSA_data)

# Question 1
#  미세먼지가 매우 나쁨을 의미하는 bad_air column을 추가하기
summary(PRSA_data)
PRSA_data$bad_air<-ifelse(PRSA_data$pm2.5>75, TRUE, FALSE)
PRSA_data<-na.omit(PRSA_data)
PRSA_train <- subset(PRSA_data, year != 2014)
PRSA_test <- subset(PRSA_data, year == 2014)


# Question 2
# 학습데이터를 사용하여 미세먼지 나쁨여부(bad_air)를 예측하는 decision tree model(Best Model)을 만들기
# 학습데이터에서 사용가능한 모든 변수를 입력 변수로 사용하여 미세먼지 나쁨여부(bad_air) 예측 decision tree 를 학습하기

PRSA_model <- rpart(bad_air ~ . - pm2.5 - No - year, data= PRSA_train, method="class",control = rpart.control(cp = 0))
#미세먼지 농도는 직접적으로 목적변수와 연관되어있기 때문에 제외해야함
#No는 순서만 알려주기 때문에 목적변수와 관련이 없기 때문에 제외해야함
#year는 test 와 train을 나누는 용도로만 사용되지, 목적변수와 연관이 없기 때문에 제외해야함.

#Question 3
# 문제 2에서 학습한 모델의 Accuracy, Precision, Recall, FI 값을 계산하기
# train data와 Test data 둘 다에 대해서 계산한 후 비교하기
# 이 모델은 과적합인가 이유와 함께 설명하여라.


PRSA_train$pred <- predict(PRSA_model, PRSA_train, type = 'class')

# train
# Compute the accuracy on the training data
# Examine the confusion matrix
train_conf <- table(PRSA_train$bad_air, PRSA_train$pred)
train_accuracy <- mean(PRSA_train$bad_air == PRSA_train$pred)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

# test
# Make predictions on the test dataset
PRSA_test$pred <- predict(PRSA_model, PRSA_test, type = 'class')
test_conf <- table(PRSA_test$bad_air, PRSA_test$pred)
test_accuracy <- mean(PRSA_test$bad_air == PRSA_test$pred)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)

# 예측값 리셋
PRSA_train <- subset(PRSA_train, select=-pred)
PRSA_test <- subset(PRSA_test, select=-pred)

paste0("accuracy for train dataset: ",round(train_accuracy,3)) 
paste0("accuracy for test dataset: ",round(test_accuracy,3))
paste0("precision for train dataset: ",round(train_precision,3))
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))
paste0("F1 for test dataset: ",round(test_F1,3))
#performance on train data >> perf. on test data => high likely to be over-fitted
#훈련 데이터에 비해 테스트 데이터에서 모든 주요 성능 지표가 낮음
#train data의 성능이 너무 좋기 때문에 over-fitting 할 확률이 높음

# Question 4
# pre-pruning 방식을 사용하여 과적합을 해소한 모델을 학습하기
# 새롭게 학습한 모델의 Accuracy, Precision, Recall, FI 값을 계산

# pre-pruning 방식에는 maxdepth와 minsplit 두 가지가 있기 때문에 두 가지를 다 사용하였다.
# Grow a tree with maxdepth of 6, minsplit = 250
# train 데이터에 대하여 계산
PRSA_model2 <- rpart(bad_air ~ . - pm2.5 - No - year, data= PRSA_train, method="class",control = rpart.control(cp = 0, maxdepth = 6 ,minsplit=250))
PRSA_train$pred <- predict(PRSA_model2, PRSA_train, type = 'class')
train_conf <- table(PRSA_train$bad_air, PRSA_train$pred)
train_accuracy <- mean(PRSA_train$bad_air == PRSA_train$pred)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

#test데이터에 대하여 계산
PRSA_test$pred <- predict(PRSA_model2, PRSA_test, type = 'class')
test_conf <- table(PRSA_test$bad_air, PRSA_test$pred)
test_accuracy <- mean(PRSA_test$bad_air == PRSA_test$pred)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)

# 예측값 리셋
PRSA_train <- subset(PRSA_train, select=-pred)
PRSA_test <- subset(PRSA_test, select=-pred)

paste0("accuracy for train dataset: ",round(train_accuracy,3))
paste0("accuracy for test dataset: ",round(test_accuracy,3))
paste0("precision for train dataset: ",round(train_precision,3))
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))
paste0("F1 for test dataset: ",round(test_F1,3))
#overfitting은 다소 해소되었으나, model의 성능 또한 감소된것으로 보인다


#### Question 5
# post-pruning 방식을 사용하여 과적합을 해소한 모델을 학습하기
# 새롭게 학습한 모델의 Accuracy, Precision Reca11, FI 값을 계산하기
# 과적합이 얼마나 해소되었는지, 성능은 어떻게 변화하였는지 설명하여라

# Grow an overly complex tree
PRSA_model3 <- rpart(bad_air ~ . - pm2.5 - No - year, data= PRSA_train, method="class",control = rpart.control(cp = 0))
plotcp(PRSA_model3) # 엔트로피 확인

# post-pruning model
# train
PRSA_model_pruned <- prune(PRSA_model3, cp = 0.001)
PRSA_train$pred <- predict(PRSA_model_pruned, PRSA_train, type = 'class')
train_conf <- table(PRSA_train$bad_air, PRSA_train$pred)
train_accuracy <- mean(PRSA_train$bad_air == PRSA_train$pred)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

# test
PRSA_test$pred <- predict(PRSA_model_pruned, PRSA_test, type = 'class')
test_conf <- table(PRSA_test$bad_air, PRSA_test$pred)
test_accuracy <- mean(PRSA_test$bad_air == PRSA_test$pred)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)



# 예측값 리셋
PRSA_train <- subset(PRSA_train, select=-pred)
PRSA_test <- subset(PRSA_test, select=-pred)


paste0("accuracy for train dataset: ",round(train_accuracy,3))
paste0("accuracy for test dataset: ",round(test_accuracy,3))
paste0("precision for train dataset: ",round(train_precision,3))
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))
paste0("F1 for test dataset: ",round(test_F1,3))
#prepruning에 비해 더욱 많이 overfitting이 해소됨과 동시에 성능의 감소는 더욱 적은것을 확인할 수 있었다. 또한 recall 수치를 통해 성능을 비교하여, post pruning의 성능이 더 좋음을 확인함.
# pre-pruning 일 때, recall for test data : 0.626
# post-pruning 일 때, recall for test data : 0.714

#### Question6
# 미세먼지에 영향을 줄 수 있는 month, day, DEWP, cbwd, Ir, Is 변수를 사용함.
# 위의 문제를 풀었을 때 pre-pruning을 사용했을 때 보다 post-pruning을 사용했을 때 성능이 더 좋았음을 확인했기에, 이번 문제에도 변수를 변경하여 post-pruning을 사용함. 

PRSA_model4 <- rpart(bad_air ~ month + day + cbwd + Ir + Is + DEWP, data= PRSA_train, method="class",control = rpart.control(cp = 0))
plotcp(PRSA_model4)

PRSA_model_pruned <- prune(PRSA_model4, cp = 0.003)

# train
PRSA_train$pred <- predict(PRSA_model_pruned, PRSA_train, type = 'class')
train_conf <- table(PRSA_train$bad_air, PRSA_train$pred)
train_accuracy <- mean(PRSA_train$bad_air == PRSA_train$pred)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

# test
PRSA_test$pred <- predict(PRSA_model_pruned, PRSA_test, type = 'class')
test_conf <- table(PRSA_test$bad_air, PRSA_test$pred)
test_accuracy <- mean(PRSA_test$bad_air == PRSA_test$pred)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)


# 예측값 리셋
PRSA_train <- subset(PRSA_train, select=-pred)
PRSA_test <- subset(PRSA_test, select=-pred)

paste0("accuracy for train dataset: ",round(train_accuracy,3))
paste0("accuracy for test dataset: ",round(test_accuracy,3))
paste0("precision for train dataset: ",round(train_precision,3))
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))
paste0("F1 for test dataset: ",round(test_F1,3))



#Question7
#2번 문제에서 사용한 첫번째 모델을 사용하기로 결정. recall 수치를 통해 비교한 결과 성능이 첫번째 모델이 가장 좋음을 확인함.
#ROC커브 및 AUC 계산


"
library(ROCR)
pred_prob_train <- predict(PRSA_model, PRSA_train, type = 'prob')[,2]
#predict 함수에서 두 번째 열만 선택하는 이유는 그 열이 양성 클래스(여기서는 bad_air가 TRUE 또는 1인 경우)에 속할 확률을 나타내기 때문입니다
pred_train <- prediction(pred_prob_train, PRSA_train$bad_air)
plot(performance(pred_train, "tpr", "fpr"),col="red",main="ROC Curves for Training and Testing Data")

pred_prob_test <- predict(PRSA_model, PRSA_test, type = 'prob')[,2]
pred_test <- prediction(pred_prob_test, PRSA_test$bad_air)
plot(performance(pred_test, "tpr", "fpr"),add=TRUE,col="blue")

perf_auc <- performance(pred_train, measure = "auc")
perf_auc2 <- performance(pred_test, measure = "auc")
auc_train <- as.numeric(perf_auc@y.values)
auc_test <- as.numeric(perf_auc2@y.values)

paste0("auc for train :",round(auc_train,3))
paste0("auc for test :",round(auc_test,3))
#auc for train :0.952
#auc for test :0.797

"
# Qustion New 7
#다시 문제 7 해봄
library(ROCR)
pred_prob_train <- predict(PRSA_model4, PRSA_train, type = 'prob')[,2]
#predict 함수에서 두 번째 열만 선택하는 이유는 그 열이 양성 클래스(여기서는 bad_air가 TRUE 또는 1인 경우)에 속할 확률을 나타내기 때문입니다
pred_train <- prediction(pred_prob_train, PRSA_train$bad_air)
plot(performance(pred_train, "tpr", "fpr"),col="red",main="ROC Curves for Training and Testing Data")

pred_prob_test <- predict(PRSA_model, PRSA_test, type = 'prob')[,2]
pred_test <- prediction(pred_prob_test, PRSA_test$bad_air)
plot(performance(pred_test, "tpr", "fpr"),add=TRUE,col="blue")

perf_auc <- performance(pred_train, measure = "auc")
perf_auc2 <- performance(pred_test, measure = "auc")
auc_train <- as.numeric(perf_auc@y.values)
auc_test <- as.numeric(perf_auc2@y.values)

paste0("auc for train :",round(auc_train,3))
paste0("auc for test :",round(auc_test,3))
#"auc for train :0.926"
#"auc for test :0.797"

# Question 8
# 문제 7에서 선정한 모델에서 threshold를 0에서 1까지 변경할 때, Accuracy, Precision, Recall, FI 값의 변화를 확인하기

# train data, test data threshold 바꾸면서 Accuracy, Precision, Recall, FI 값 구하는 함수
cal <- function(threshold){
  pred_train_new <- ifelse(pred_prob_train > threshold, "very bad", "not very bad")
  pred_test_new <- ifelse(pred_prob_test > threshold, "very bad", "not very bad")
  cmat_train <- table(PRSA_train$bad_air, pred_train_new)
  cmat_test <- table(PRSA_test$bad_air, pred_test_new)
  #cmat_train
  #cmat_test
  train_accuracy <- round((cmat_train[1,1]+cmat_train[2,2]) / sum(cmat_train),3)
  train_precision <- round(cmat_train[2,2] / sum(cmat_train[2,]),3)
  train_recall <- round(cmat_train[2,2]/sum(cmat_train[,2]),3)
  train_F1 <- round(2*(train_precision*train_recall)/(train_precision+train_recall),3)
  
  test_accuracy <- round((cmat_test[1,1]+cmat_test[2,2]) / sum(cmat_test),3)
  test_precision <- round(cmat_test[2,2] / sum(cmat_test[2,]),3)
  test_recall <- round(cmat_test[2,2]/sum(cmat_test[,2]),3)
  test_F1 <- round(2*(test_precision*test_recall)/(test_precision+test_recall),3)
  
  # 결과 출력
  cat("Threshold:", threshold, "\n")
  cat("Train Accuracy:", train_accuracy, "\n")
  cat("Train Precision:", train_precision, "\n")
  cat("Train Recall:", train_recall, "\n")
  cat("Train F1 Score:", train_F1, "\n")
  cat("Test Accuracy:", test_accuracy, "\n")
  cat("Test Precision:", test_precision, "\n")
  cat("Test Recall:", test_recall, "\n")
  cat("Test F1 Score:", test_F1, "\n")
  cat("\n")
  #return(c(train_accuracy, train_precision, train_recall, train_F1, test_accuracy, test_precision, test_recall, test_F1))
  
}


threshold <- 0.5
cal(threshold)


#cal함수의 출력형식 : train_accuracy, train_precision, train_recall, train_F1, test_accuracy, test_precision, test_recall, test_F1

# False Positive -> 미세먼지가 매우나쁨인데 매우 나쁘지 않다고 하는 것  
# Fasle Negative -> 미세먼지가 매우나쁨이 아닌데 매우 나쁘다고 하는 것 

# 미세먼지가 실제로 매우 나쁨인 상황에서 매우 나쁘지 않다고 판단하는 것보다, 미세먼지가 매우 나쁨이 아닌 상황에서 매우 나쁘다고 판단하는 것이 더 바람직하다. 
# 이는 마스크를 착용하거나 외부 활동을 자제하는 등 예방 조치를 취할 수 있기 때문에 recall이 높은 값을 우선순위로 두었다.

#train에서 확인해보았을 때, Accuracy, PRecision, Recall, F1 값이 0.5에서 균형이 잘 이뤄진 것을 확인 할 수 있었다.
#test에서 0부터 0.1단위로 1까지 threshold를 확인해보았을 때, recall 값은 계속 상승하는 추세를 보였고,
# 나머지 값들은 0.5에서부터 최고점을 찍고 하락하는 추세를 보였기에 
# threshold를 0.5로 설정하였다.

