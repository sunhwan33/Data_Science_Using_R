#############################Homework6_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린


# loading data into R
PRSA_data <- load("PRSA_data.RData")


library(dplyr)
train_data

# Q1 
# Q1-1 미세먼지 농도를 예측하는 Single Varible 모델 만들기
# 데이터 타입 확인
glimpse(train_data) 
glimpse(test_data)
# 6가지 변수와 Month, time, pm2.5 data type이 Factor인 것을 확인할 수 있다.

# Q1-2 결측치 살펴보기
# train 결측치
colSums(is.na(train_data))
colSums(is.na(test_data))

# Q1-3
# month 결측치 확인
tapply(train_data$pm2.5, train_data$Month,function(x) sum(is.na(x)))
tapply(test_data$pm2.5, test_data$Month,function(x) sum(is.na(x)))

# time 결측치 확인
tapply(train_data$pm2.5, train_data$time,function(x) sum(is.na(x)))
tapply(test_data$pm2.5, test_data$time,function(x) sum(is.na(x)))

# 결측치가 주로 몰려있는 월은 Aug이고 시간은 night이 date보다 더 많다는 것을 확인할 수 있다.

# Q1-4
#pm2.5는 목적변수이므로 NA가 허용되지 않는다. 이를 삭제해보자
# 목적변수 = 종속변수
# 설명변수 = 독립변수

train.data <- na.omit(train_data)
test.data <- na.omit(test_data)

colSums(is.na(train.data))
colSums(is.na((test.data)))

# Q2-1
# Month 변수 활용하여 pm2.5 예측하는 Single variable 모델 만들기
# Threshold는 0.5로 설정
table(train.data$pm2.5)
prop.table(table(train.data$pm2.5))
table(test.data$pm2.5)
prop.table(table(test.data$pm2.5))

# 미세먼지가 나쁨인 pm2.5 = HIGH인 경우 positive(TRUE) sample로 보고 문제 풀기
train.data$pm2.5 <- ifelse(train.data$pm2.5 == "HIGH", "TRUE", 
                           "FALSE")
test.data$pm2.5 <- ifelse(test.data$pm2.5 == "HIGH", "TRUE", 
                          "FALSE")
# target 값이 분포가 비슷하기 때문에 single variable 모델을 만들어도 될 것이라고 판단함.

### Building a Single Variable Model
# variable choose "Month"
# 두 개의 변수 변량에 따른 2차원 크로스 테이블로 만들려면 table()에 두 개의 변수를 전달해준다. 
# 첫 번째 변수는 행으로 표현되고 두 번째 변수는 열로 표현되어 사례수가 산출된다.
tble <- table(train.data$Month, train.data$pm2.5)
tble

# ‘margin’ 매개변수를 1로 설정하면 행 단위의 합계를 산출하고 2로 설정하면 열단위의 합계를 산출해준다. 
prop.table(tble, margin = 1)

sv_model_mon <- prop.table(tble, margin = 1)[, 2]
sort(sv_model_mon, decreasing = T)

# 6월의 62%가 미세먼지가 나쁨 수준이다.

### Prediction on Training Dataset
train.data$est_prob <- sv_model_mon[train.data$Month]
head(train.data[, c("Month", "est_prob", "pm2.5")], 10)

### Making a Decision based on Prob.
threshold <- 0.5
train.data$prediction <- train.data$est_prob > threshold
head(train.data[, c("Month", "est_prob", "prediction","pm2.5")])

# Accuracy
train_conf.table <- table(pred = train.data$prediction, actual = train.data$pm2.5)
train_conf.table


train_accuracy <- sum(diag(train_conf.table)) / sum(train_conf.table)
train_accuracy


### Prediction on Test Dataset
test.data$est_prob <- sv_model_mon[test.data$Month]
head(test.data[, c("Month", "est_prob", "pm2.5")], 10)

### Making a Decision based on Prob.
threshold <- 0.5
test.data$prediction <- test.data$est_prob > threshold
head(test.data[, c("Month", "est_prob", "prediction","pm2.5")])

# Accuracy
test_conf.table <- table(pred = test.data$prediction, actual = test.data$pm2.5)
test_conf.table

test_accuracy <- sum(diag(test_conf.table)) / sum(test_conf.table)
test_accuracy

paste("accuracy for train data", round(train_accuracy, digits = 2))
paste("accuracy for test data", round(test_accuracy, digits = 2))

# Q2-2
library(ROCR)
# AUC를 계산하는 함수 정의 
calAUC <- function(predCol, targetCol){
  #prediction 객체를 생성하고 AUC를 계산 
  perf <- performance(prediction(predCol, targetCol), 'auc') 
  as.numeric(perf@y.values)
}

#train 데이터의 AUC 계산 및 출력 
train_auc <- calAUC(train.data$est_prob, train.data$pm2.5)
# AUC 값을 문자열로 변환하여 출력 
paste(("AUC value for train data"), round(train_auc, digits = 2))
#train 데이터의 ROC 곡선 그리기
plot(performance(prediction(train.data$est_prob, train.data$pm2.5), 'tpr', 'fpr'))

# 테스트 데이터의 AUC 계산 및 출력 
test_auc <- calAUC(test.data$est_prob, test.data$pm2.5)
# AUC 값을 문자열로 변환하여 출력 
paste(("AUC value for test data"), round(test_auc, digits = 2))
#test 데이터의 ROC 곡선 그리기 
plot(performance(prediction(test.data$est_prob, test.data$pm2.5), 'tpr', 'fpr'))

# Q2-3
# Overfitting은 학습 데이터(training set)에 대해 과하게 학습되어 test set에서는 모델이 잘 동작하지 못하는 상황을 말한다. 
# train set에서 높은 accuracy와 AUC값을 가지고 있지만 test set에서 낮은 값을 가진다면 overfitting일 가능성이 있지만,
#Train 과 test 의 정확도가 유사하며 (test가 너무 높지 않음), AUC value 또한 큰 차이가 없는 것으로 보았을 때 
#이 모델은 과적합(Overfitting)하지 않다고 할 수 있다.
# 2-2에서 수행한 plot() function을 통한 시각화에서도, 기울기 차이나 outlier들이 발생하지 않는 것을 볼 수 있었고, 이에 해당 모델은 Overfitting이 발생하지 않았다고 최종적으로 판단하였다.





# Q2-4
#threshold를 0.45, 0.47, 0.49, 0.51, 0.53, 0.55로 바꿔가면서 precision과 recall 값의 변화를 확인한다. 
thresholds <- seq(0.45, 0.55, by = 0.02)

# precision과 recall을 저장할 빈 벡터 생성
precisions <- numeric(length(thresholds))
recalls <- numeric(length(thresholds))

#각 threshold 값에 대해 precision과 recall 계산
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  train.data$est_prob <- sv_model_mon[train.data$Month]
  train.data$prediction <- train.data$est_prob > threshold
  
  conf.table <- table(pred = train.data$prediction, actual = train.data$pm2.5)
  
  TP <- conf.table[2, 2]
  FP <- conf.table[2, 1]
  FN <- conf.table[1, 2]
  precisions[i] <- TP / (TP + FP) 
  recalls[i] <- TP / (TP + FN) 
}

results_df <- data.frame(thresholds, precisions, recalls)
colnames(results_df) <- c('threshold', 'precision', 'recall')
results_df



# Q2-5
# precision과 recall 값이 모두 상대적으로 높은 threshold 0.47
# 미세먼지를 예측하는 상황에서는 미세먼지 농도가 낮더라도 실제로 높게 예측하는 것이 중요하다고 보았다. 그 이유는 미세먼지 농도가 높은 경우 실제로 높은 것을 낮게 예측하는 것은 건강에 치명적일 수 있기 때문이다. 따라서 recall(재현율)이 최대한 높은 모델을 선택하는 것이 바람직하다. 따라서 너무 Precision이 높지 않으면서 Recall이 낮지 않은 threshold가 0.47인 경우가 제일 효율적이라고 생각한다.

#Q2-6
# f1 score 함수
calculate_f1_score <- function(precision, recall) {
  F1_score <- 2 * (precision * recall) / (precision + recall)
  return(F1_score)
}

threshold <- 0.47

test.data$prediction <- test.data$est_prob > threshold
conf.table <- table(pred = test.data$prediction,
                    actual = test.data$pm2.5) #test.data에서 분류 모델의 예측 결과와 실제 값 사이의 confusion matrix를 만든다
conf.table

precision <- conf.table[2, 2] / sum(conf.table[2,]) #conf.table precision 구하기
recall <- conf.table[2, 2] / sum(conf.table[, 2]) #conf.table recall 구하기기

paste("F1 score for test data", round(calculate_f1_score(precision, recall), digits = 2))




#3
# 온도 데이터를 5도 간격으로 구간화
tble <- table(train.data$TEMP, train.data$pm2.5)
summary(train.data$TEMP)
# 등간격으로 온도 데이터를 그룹으로 나누기 위한 함수 정의
create_temp_groups <- function(temp_data, interval) {
  # 온도 데이터의 최솟값과 최댓값 확인
  min_temp <- min(temp_data)
  max_temp <- max(temp_data)
  
  # 등간격으로 구간을 나타내는 레이블 생성
  temp_labels <- cut(temp_data, breaks = seq(min_temp, max_temp, by = interval),include.lowest = TRUE)
  
  return(temp_labels)
}

temp_interval <- 5
temp_groups <- create_temp_groups(train.data$TEMP, temp_interval)

# 각 구간별 데이터 수 확인
table(temp_groups)
train.data$TEMP_groups <- temp_groups
tble <- table(train.data$TEMP_groups, train.data$pm2.5)
sv_model_temp <- prop.table(tble, margin = 1)[, 2]
sort(sv_model_temp, decreasing = T)

# Month기준으로 6월의 62%가 미세먼지가 나쁨 수준이였는데 TEMP를 기준으로 보아도 21도 초과 31도 이하인 것을 보아 여름에 미세먼지가 나쁘다는 것을 볼 수 있다.

### Prediction on Training Dataset
train.data$est_prob <- sv_model_temp[train.data$TEMP_groups]

### Making a Decision based on Prob.
threshold <- 0.5
train.data$prediction <- train.data$est_prob > threshold

# Accuracy
train_conf.table <- table(pred = train.data$prediction, actual = train.data$pm2.5)
train_accuracy <- sum(diag(train_conf.table)) / sum(train_conf.table)

### Prediction on Test Dataset
tble <- table(test.data$TEMP, test.data$pm2.5)
summary(test.data$TEMP)

# 등간격으로 온도 데이터를 그룹으로 나누기 위한 함수 정의
create_temp_groups <- function(temp_data, interval) {
  # 온도 데이터의 최솟값과 최댓값 확인
  min_temp <- min(temp_data)
  max_temp <- max(temp_data)
  
  # 원하는 구간을 포함한 등간격으로 구간을 나타내는 레이블 생성. 구간이 딱 떨어지지 않는 문제로 인해 max_temp 값에 하나 추가.
  temp_labels <- cut(temp_data, breaks = c(seq(min_temp, max_temp, by = interval), max_temp + 1), include.lowest = TRUE)
  
  return(temp_labels)
}
# 온도 데이터를 5도 간격으로 구간화 (37 이상 42 이하를 포함하는 구간 추가)
temp_groups <- create_temp_groups(test.data$TEMP, temp_interval)
# 각 구간별 데이터 수 확인
table(temp_groups)

# test.data에 TEMP_groups 변수 추가
test.data$TEMP_groups <- temp_groups
tble <- table(test.data$TEMP_groups, test.data$pm2.5)

sv_model_temp <- prop.table(tble, margin = 1)[, 2]
sort(sv_model_temp, decreasing = T)
test.data$est_prob <- sv_model_temp[test.data$TEMP_groups]

### Making a Decision based on Prob.
test.data$prediction <- test.data$est_prob > threshold

# Accuracy
test_conf.table <- table(pred = test.data$prediction, actual = test.data$pm2.5)
test_accuracy <- sum(diag(test_conf.table)) / sum(test_conf.table)

paste("accuracy for train data", round(train_accuracy, digits = 2))
paste("accuracy for test data", round(test_accuracy, digits = 2))

# Q3-2
# 문제 3-1에서 구한 모델의 AUC를 train과 test 데이터 각각에 대해서 계산해보고, ROC커브를 그려보기
library(ROCR)
calAUC(train.data$est_prob, train.data$pm2.5)
paste("AUC value for train data", round(calAUC(train.data$est_prob, train.data$pm2.5), digits = 2))
plot(performance(prediction(train.data$est_prob, train.data$pm2.5), 'tpr', 'fpr'), main = "train.data ROC curve")

paste("AUC value for train data", round(calAUC(test.data$est_prob, test.data$pm2.5), digits = 2))
plot(performance(prediction(test.data$est_prob, test.data$pm2.5), 'tpr', 'fpr'), main = "test.data ROC curve")

# Q3-3
# Overfitting은 학습 데이터(training set)에 대해 과하게 학습되어 test set에서는 모델이 잘 동작하지 못하는 상황을 말한다. 
# train set에서 높은 accuracy와 AUC값을 가지고 있지만 test set에서 낮은 값을 가진다면 overfitting일 가능성이 있지만,
#Train 과 test 의 정확도가 유사하며 (test가 너무 높지 않음), AUC value 또한 큰 차이가 없는 것으로 보았을 때 
#이 모델은 과적합(Overfitting)하지 않다고 할 수 있다.
# 3-2에서 수행한 plot() function을 통한 시각화에서도, 기울기 차이나 outlier들이 발생하지 않는 것을 볼 수 있었고, 이에 해당 모델은 Overfitting이 발생하지 않았다고 최종적으로 판단하였다.

# Q3-4

#threshold를 0.45, 0.47, 0.49, 0.51, 0.53, 0.55로 바꿔가면서 precision과 recall 값의 변화를 확인한다. 
thresholds <- seq(0.45, 0.55, by = 0.02)

precisions <- numeric(length(thresholds))
recalls <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  train.data$est_prob <- sv_model_temp[train.data$TEMP_groups]
  train.data$prediction <- train.data$est_prob > threshold
  
  conf.table <- table(pred = train.data$prediction, actual = train.data$pm2.5)
  
  TP <- conf.table[2, 2]
  FP <- conf.table[2, 1]
  FN <- conf.table[1, 2]
  precisions[i] <- TP / (TP + FP) 
  recalls[i] <- TP / (TP + FN) 
}

results_df <- data.frame(thresholds, precisions, recalls)
colnames(results_df) <- c('threshold', 'precision', 'recall')
results_df


# Q3-5
#0.45 와 0.47의 precision과 recall 값이 같게 나왔으나 그 중에서 threshold가 더욱 높은 0.47이 더욱 정확할 것이라 판단하였다.

#temp_invertal를 15를 기준으로 그룹화 하였을 때는 threshold일 때 f1_score가 0.62로 가장 높은 수치가 나왔다. 그리하여 이것이 가장 좋은 그룹화 기준일 것 같으나, 온도를 15도씩으로 나누는 것은 현실적으로 좋은 분할이 아니라고 판단하였다. 그리하여 interval 기준을 10으로 하였다.

# Q3-6
threshold <- 0.45

test.data$prediction <- test.data$est_prob > threshold
conf.table <- table(pred = test.data$prediction,
                    actual = test.data$pm2.5) #test.data에서 분류 모델의 예측 결과와 실제 값 사이의 confusion matrix를 만든다
conf.table

precision <- conf.table[2, 2] / sum(conf.table[2,]) #conf.table precision 구하기
recall <- conf.table[2, 2] / sum(conf.table[, 2]) #conf.table recall 구하기기

paste("F1 score for train data", round(calculate_f1_score(results_df[2, 2], results_df[2,3]), digits = 2))

paste("F1 score for test data", round(calculate_f1_score(precision, recall), digits = 2))

# "F1 score for train data 0.56"
# "F1 score for test data 0.65" 
