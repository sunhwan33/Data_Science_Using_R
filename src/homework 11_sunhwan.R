"독립 변수 (Independent Variables)
인구 통계적 변수 (Demographic Variables)
LIMIT_BAL:
설명: 주어진 신용 한도 (단위: NT 달러).
값: 개인 및 보충 카드의 총 신용 한도를 나타냅니다.

SEX:
설명: 성별을 나타내는 변수.
값: 1은 남성, 2는 여성을 의미합니다.

EDUCATION:
설명: 교육 수준을 나타내는 변수.
값: 1은 대학원, 2는 대학, 3은 고등학교, 4는 기타를 의미합니다.

MARRIAGE:
설명: 결혼 상태를 나타내는 변수.
값: 1은 기혼, 2는 미혼, 3은 기타를 의미합니다.

AGE:
설명: 나이 (단위: 년).
값: 고객의 나이를 나타냅니다.


과거 상환 기록 (History of Past Payment)
PAY_0 ~ PAY_6:
설명: 과거 6개월 동안의 상환 상태를 나타내는 변수 (0은 현재 월, 1은 이전 월).
값:
-1: 상환됨 (pay duly)
1: 한 달 연체 (payment delay for one month)
2: 두 달 연체 (payment delay for two months)
...
9: 9개월 이상 연체 (payment delay for nine months and above)


청구 금액 (Bill Statement Amounts)
BILL_AMT1 ~ BILL_AMT6:
설명: 청구서 금액 (단위: NT 달러).
값: 각 월의 청구서 금액을 나타냅니다 (BILL_AMT1은 최근 월, BILL_AMT6은 가장 이전 월).


상환 금액 (Payment Amounts)
PAY_AMT1 ~ PAY_AMT6:
설명: 상환 금액 (단위: NT 달러).
값: 각 월에 실제 상환된 금액을 나타냅니다 (PAY_AMT1은 최근 월, PAY_AMT6은 가장 이전 월)."

train_df <- read.csv("train_df.csv")
test_df <- read.csv("test_df.csv")
dim(train_df)
dim(test_df)

str(train_df)
complications <- c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE")
pay <- c("PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6")
Bill <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")
pay_amt <- c("PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")
y <- "default.payment.next.month"
x <- c(complications, pay, Bill, pay_amt)
fmla <- paste(y, paste(x, collapse = '+'), sep='~')
print(fmla)

model <- glm(fmla, data = train_df, family = binomial(link="logit"))
summary(model)
#오즈비가 exp(estimate)만큼의 차이가 나는 것.
#선형 회귀와 달리 로지스틱 회귀의 계수는 로그 오즈 비율(log odds ratio)을 기반으로 해석됨
#그래서 예를들어 AGE의 오즈비는 6.894이므로, AGE 한살 많아질때마다 default.payment.next.month에 대한 오즈(채무불이행 오즈)가 exp(6.894)배 만큼 증가함  
train_df$pred <- predict(model, newdata = train_df, type = "response")
test_df$pred <- predict(model, newdata = test_df, type = "response")

test_df[,c("pred", "default.payment.next.month")]

aggregate(pred ~ default.payment.next.month, train_df, mean)
aggregate(pred ~ default.payment.next.month, test_df, mean)


#Q2
library(ROCR)
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc') 
  as.numeric(perf@y.values)
}
calAUC(train_df$pred, train_df$default.payment.next.month)
calAUC(test_df$pred, test_df$default.payment.next.month)

#Q3
summary(model)
#긍정적 영향 변수 : 채무불이행 확률 높이는
  # AGE (6.894), PAY_0(5.742), PAY_2(8.652), PAY_3(5.471)
#부정적 영향 변수 : 채무불이행 확률 낮추는
  #LIMIT_BAL(-7.609), SEX(-1.018), EDUCATION(-1.055), MARRIAGE(-1.53), BILL_AMT1(-5.011), PAY_AMT1(-1.306), PAY_AMT2(-1.048), PAY_AMT4$(-6.444)

#예를들어, AGE값이 하나씩 올라갈때마다 채무불이행의 로그오즈도 6.894만큼 증가함. 즉 AGE 하나씩 증가할때마다 채무불이행의 오즈는 exp(6.894) = 986의 배만큼 증가함
#exp(6.894) = 986.3389


#Q4
thresholds <- 0.5
train_df$prediction <- train_df$pred > thresholds
test_df$prediction <- test_df$pred > thresholds

train_conf <- table(train_df$prediction, train_df$default.payment.next.month)
train_accuracy <- mean(train_df$default.payment.next.month == train_df$prediction)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

# test
test_conf <- table(test_df$prediction, test_df$default.payment.next.month)
test_accuracy <- mean(test_df$default.payment.next.month == test_df$prediction)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)


paste0("accuracy for train dataset: ",round(train_accuracy,3))
paste0("accuracy for test dataset: ",round(test_accuracy,3))
paste0("precision for train dataset: ",round(train_precision,3))
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))
paste0("F1 for test dataset: ",round(test_F1,3))

###### thresholds를 0.1~0.9 다 계산하여 최적의 값 찾아보기

thresholds <- seq(0.1,0.9,0.1)
accuracy_values <- numeric(length(thresholds))
precision_values <- numeric(length(thresholds))
recall_values <- numeric(length(thresholds))
f1_values <- numeric(length(thresholds))

for (i in seq_along(thresholds)){
threshold <- thresholds[i]
train_df$prediction <- train_df$pred >= threshold

train_conf <- table(train_df$prediction, train_df$default.payment.next.month)
accuracy_values[i] <- mean(train_df$default.payment.next.month == train_df$prediction)
precision_values[i] <- train_conf[2,2]/sum(train_conf[2,])
recall_values[i] <- train_conf[2,2]/sum(train_conf[,2])
f1_values[i] <- 2*(precision_values[i]*recall_values[i])/(precision_values[i]+recall_values[i])
}
#threshold가 0.5일때
accuracy_values[5] # 0.80984
precision_values[5] #0.7135334
recall_values[5] # 0.2434861
f1_values[5] #0.3630761


plot(thresholds, f1_values , type = "b",xlab="threshold",ylab="f1")
plot(thresholds, recall_values , type = "b",xlab="threshold",ylab="recall")
plot(thresholds, precision_values , type = "b",xlab="threshold",ylab="precision")
optimal_threshold <- 0.2

train_df$prediction <- train_df$pred >= optimal_threshold
train_conf <- table(train_df$prediction, train_df$default.payment.next.month)
train_accuracy <- mean(train_df$default.payment.next.month == train_df$prediction)
train_precision <- train_conf[2,2]/sum(train_conf[2,])
train_recall <- train_conf[2,2]/sum(train_conf[,2])
train_F1 <- 2*(train_precision*train_recall)/(train_precision+train_recall)

paste0("accuracy for train dataset: ",round(train_accuracy,3)) #0.612
paste0("precision for train dataset: ",round(train_precision,3))
paste0("recall for train dataset: ",round(train_recall,3))
paste0("F1 for train dataset: ",round(train_F1,3))


#Q5
test_df$prediction <- test_df$pred >= optimal_threshold
test_conf <- table(test_df$prediction, test_df$default.payment.next.month)
test_accuracy <- mean(test_df$default.payment.next.month == test_df$prediction)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)

paste0("accuracy for test dataset: ",round(test_accuracy,3)) 
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for test dataset: ",round(test_F1,3))


#Q6
calAUC(train_df$pred, train_df$default.payment.next.month) # 0.7235756
calAUC(test_df$pred, test_df$default.payment.next.month) #0.7255029


train_df <- subset(train_df, select = -pred)
train_df <- subset(train_df, select = -prediction)
test_df <- subset(test_df, select = -pred)
test_df <- subset(test_df, select = -prediction)

#AGE (6.894), PAY_0(5.742), PAY_2(8.652), PAY_3(5.471)
#부정적 영향 변수 : 채무불이행 확률 낮추는
#LIMIT_BAL(-7.609), SEX(-1.018), EDUCATION(-1.055), MARRIAGE(-1.53), BILL_AMT1(-5.011), PAY_AMT1(-1.306), PAY_AMT2(-1.048), PAY_AMT$(-6.444)
model1 <- glm(default.payment.next.month ~ AGE + PAY_0 + PAY_2 + PAY_3, data = train_df, family = binomial(link="logit"))

train_df$pred <- predict(model1, newdata = train_df, type = "response")
test_df$pred <- predict(model1, newdata = test_df, type = "response")
calAUC(train_df$pred, train_df$default.payment.next.month)  #0.7028456
calAUC(test_df$pred, test_df$default.payment.next.month) #0.7067566

train_df <- subset(train_df, select = -pred)
test_df <- subset(test_df, select = -pred)

model2 <- glm(default.payment.next.month ~ AGE + PAY_0 + PAY_2 + PAY_3 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4, data = train_df, family = binomial(link="logit"))
train_df$pred <- predict(model2, newdata = train_df, type = "response")
test_df$pred <- predict(model2, newdata = test_df, type = "response")
calAUC(train_df$pred, train_df$default.payment.next.month) #0.7223518
calAUC(test_df$pred, test_df$default.payment.next.month) #0.7258815
train_df <- subset(train_df, select = -pred)
test_df <- subset(test_df, select = -pred)

#PAY_0 * PAY_2 * PAY_3
model3 <- glm(default.payment.next.month ~ AGE + PAY_0 * PAY_2 * PAY_3 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4, data = train_df, family = binomial(link="logit"))
train_df$pred <- predict(model3, newdata = train_df, type = "response")
test_df$pred <- predict(model3, newdata = test_df, type = "response")
calAUC(train_df$pred, train_df$default.payment.next.month) #0.7261044
calAUC(test_df$pred, test_df$default.payment.next.month)  #0.7308278
train_df <- subset(train_df, select = -pred)
test_df <- subset(test_df, select = -pred)

#AGE 범주화

train_df$AGE_range <- cut(train_df$AGE, breaks = c(seq(21,80,10), Inf), labels = c('20s', '30s', '40s', '50s', '60s', '70s'), right=F)
test_df$AGE_range <- cut(test_df$AGE, breaks = c(seq(21,80,10), Inf), labels = c('20s', '30s', '40s', '50s', '60s', '70s'), right=F)
model4 <- glm(default.payment.next.month ~ AGE_range + PAY_0 * PAY_2 * PAY_3 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4, data = train_df, family = binomial(link="logit"))
train_df$pred <- predict(model4, newdata = train_df, type = "response")
test_df$pred <- predict(model4, newdata = test_df, type = "response")
calAUC(train_df$pred, train_df$default.payment.next.month) #0.7261958
calAUC(test_df$pred, test_df$default.payment.next.month)  #0.7307172
train_df <- subset(train_df, select = -pred)
test_df <- subset(test_df, select = -pred)


#model3가 가장 성능 좊음

model3 <- glm(default.payment.next.month ~ AGE + PAY_0 * PAY_2 * PAY_3 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4, data = train_df, family = binomial(link="logit"))
train_df$pred <- predict(model3, newdata = train_df, type = "response")
thresholds <- seq(0.1,0.9,0.1)
accuracy_values <- numeric(length(thresholds))
precision_values <- numeric(length(thresholds))
recall_values <- numeric(length(thresholds))
f1_values <- numeric(length(thresholds))
auc_values <- numeric(length(thresholds))

for (i in seq_along(thresholds)){
  threshold <- thresholds[i]
  train_df$prediction <- train_df$pred >= threshold
  
  train_conf <- table(train_df$prediction, train_df$default.payment.next.month)
  accuracy_values[i] <- mean(train_df$default.payment.next.month == train_df$prediction)
  precision_values[i] <- train_conf[2,2]/sum(train_conf[2,])
  recall_values[i] <- train_conf[2,2]/sum(train_conf[,2])
  f1_values[i] <- 2*(precision_values[i]*recall_values[i])/(precision_values[i]+recall_values[i])
  auc_values[i] <- calAUC(train_df$pred, train_df$default.payment.next.month)
}

plot(thresholds, f1_values , type = "b",xlab="threshold",ylab="f1")
plot(thresholds, recall_values , type = "b",xlab="threshold",ylab="recall")
plot(thresholds, precision_values , type = "b",xlab="threshold",ylab="precision")
plot(thresholds, auc_values, type="b", xlab="threshold", ylab="auc")

#0.2로 설정
optimal_threshold <- 0.2
test_df$pred <- predict(model3, newdata = test_df, type = "response")
test_df$prediction <- test_df$pred >= optimal_threshold
test_conf <- table(test_df$prediction, test_df$default.payment.next.month)
test_accuracy <- mean(test_df$default.payment.next.month == test_df$prediction)
test_precision <- test_conf[2,2]/sum(test_conf[2,])
test_recall <- test_conf[2,2]/sum(test_conf[,2])
test_F1 <- 2*(test_precision*test_recall)/(test_precision+test_recall)
test_auc <- calAUC(test_df$pred, test_df$default.payment.next.month)

paste0("accuracy for test dataset: ",round(test_accuracy,3)) 
paste0("precision for test dataset: ",round(test_precision,3))
paste0("recall for test dataset: ",round(test_recall,3))
paste0("F1 for test dataset: ",round(test_F1,3))
paste0("AUC for test dataset: ",round(test_auc,3))


##Q7
train_df$pred <- predict(model3, newdata=train_df, type="response")
train_df$prediction <- train_df$pred>=optimal_threshold
train_conf <- table(train_df$prediction, train_df$default.payment.next.month)
train_fp <- train_conf[2,1]
train_fn <- train_conf[1,2]

false_positive <- train_df[train_df$prediction == 1 & train_df$default.payment.next.month == 0, ]
false_negative <- train_df[train_df$prediction == 0 & train_df$default.payment.next.month == 1, ]

paste("Number of False Positives: ", nrow(false_positive))
paste("Number of False Negatives: ", nrow(false_negative))
# 주요 변수의 분포 시각화 (예: AGE, LIMIT_BAL, PAY_0)
library(ggplot2)

# False Positive 샘플의 AGE 분포
ggplot(false_positive, aes(x = AGE)) + geom_histogram(binwidth = 5) + ggtitle("False Positives - AGE Distribution")

# False Negative 샘플의 AGE 분포
ggplot(false_negative, aes(x = AGE)) + geom_histogram(binwidth = 5) + ggtitle("False Negatives - AGE Distribution")

# False Positive 샘플의 LIMIT_BAL 분포
ggplot(false_positive, aes(x = LIMIT_BAL)) + geom_histogram(binwidth = 50000) + ggtitle("False Positives - LIMIT_BAL Distribution")

# False Negative 샘플의 LIMIT_BAL 분포
ggplot(false_negative, aes(x = LIMIT_BAL)) + geom_histogram(binwidth = 50000) + ggtitle("False Negatives - LIMIT_BAL Distribution")

# False Positive 샘플의 PAY_0 분포
ggplot(false_positive, aes(x = PAY_0)) + geom_bar() + ggtitle("False Positives - PAY_0 Distribution")

# False Negative 샘플의 PAY_0 분포
ggplot(false_negative, aes(x = PAY_0)) + geom_bar() + ggtitle("False Negatives - PAY_0 Distribution")

#젊은 연령대의 예측 오류: 모델이 젊은 연령대의 고객에 대해 오판하는 경향이 있습니다. 이는 데이터 내에서 젊은 연령대의 샘플 수가 많거나, 젊은 연령대의 상환 패턴이 다른 변수들과 복잡하게 상호작용하기 때문일 수 있습니다.
#신용 한도와 예측 오류: 모델이 낮은 신용 한도에 대해 과대평가하거나 과소평가하는 경향이 있습니다. 낮은 신용 한도에서 FP가 많이 발생하고, FN도 일부 발생하는 것을 볼 때, 신용 한도 외에 다른 변수를 추가로 고려할 필요가 있습니다.
#PAY_0 값과 예측 오류: 모델이 PAY_0 값이 0과 1인 경우에 대해 과대평가하거나 과소평가하는 경향이 있습니다. 이는 고객들이 최근에 연체가 없거나 한 달 연체한 경우 모델이 정확하게 예측하지 못하는 경우가 많음을 나타냅니다.



