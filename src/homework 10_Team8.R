
############################Homework10_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린

student<- read.csv("regression_student.csv")
dim(student)
str(student)

# Question 1 학생의 최종 성적을 예측하는 선형 회귀 모델을 만들기 (주어진 모든 변수를 사용). 
#모델을 만드는 과정을 설명하고, 필요하다면 전처리도 수행하고 전처리 과정도 설명하기
set.seed(2024)
nstudent <- nrow(student)
rgroup <- runif(nstudent)

# 결측치 있는지 확인
sum(is.na(student))

# train, test data 나누기
student_train <- subset(student, rgroup <= 0.8)
student_test <- subset(student, rgroup > 0.8)
str(student_train)
str(student_test)


summary(student_train$G3)
summary(student_test$G3)
boxplot(student_train$G3, student_test$G3,
        names = c("Train set","Test set"), ylab = "G3")
plot(density(student_train$G3), col = "blue", 
     main = "G3 Density Plot Comparison", xlab = "G3")
lines(density(student_test$G3), col = "red")

colnames(student_train)

# 선형 회귀 모델 생성
grade_model <- lm(G3 ~ ., student_train)
grade_model

# 예측값 계산
student_train$pred <- predict(grade_model, newdata = student_train)
student_test$pred <- predict(grade_model, newdata = student_test)


#2	
# RMSE 계산 함수 생성
calcRMSE <- function(label, estimation){
  return(sqrt(mean((label - estimation) ** 2)))
}
# R2 계산 함수 생성
calcR2 <- function(label, estimation){
  RSS = sum((label - estimation) ** 2)
  SStot = sum((label - mean(label)) ** 2)
  return(1-RSS/SStot)
}

# train의 RMSE, R2 값 계산
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)
# test의 RMSE, R2 값 계산
calcRMSE(student_test$G3,student_test$pred)
calcR2(student_test$G3, student_test$pred)

#학습 데이터의 R² 값이 0.2984, 테스트 데이터의 R² 값이 0.2410으로, 모델이 학습 데이터의 약 29.84%, 테스트 데이터의 약 24.10%의 변동을 설명할 수 있음을 의미합니다.
#RMSE 값이 적절한 수준이며, 학습 데이터와 테스트 데이터에서 비슷한 값을 보이므로 모델의 과적합 문제가 크지 않습니다.
#그러나, R² 값이 낮기 때문에 모델이 데이터를 충분히 설명하지 못하고 있습니다. 이는 모델의 예측력이 제한적임을 나타냅니다.


#3
summary(grade_model)
# 모든 변수들을 독립변수로 넣었을 때, 통계적으로 유의미한 변수는 failures, higher, classport, studytime, schoolsup, romantic, health, goout 등이다.
# 모델이 데이터의 약 29.84%의 변동성을 설명하지만, 변수의 수를 고려하면 실제 설명력은 약 25.48%임
# Residual standard error: 3.271 on 651 degrees of freedom
# Multiple R-squared:  0.2978,	Adjusted R-squared:  0.2546 
# F-statistic: 6.901 on 40 and 651 DF,  p-value: < 2.2e-16


# 긍정적 변수
# higher: 추정치 1.69207: 학생이 고등 교육을 원한다고 응답한 경우, 평균적으로 성적(G3)이 1.69207 점 더 높을 것으로 예상된다. 즉, 고등 교육을 희망하는 학생들이 그렇지 않은 학생들에 비해 성적이 더 높은 경향이 있다(higheryes). => 고등 교육을 희망하는 학생들은 학업에 대한 동기와 목표가 명확하기 때문에 성적이 좋을 가능성이 있다고 생각

# classport: 추정치 2.03866: 학생이 수강한 과목이 수학인지 포르투갈어인지 구분하는 변수: 포르투갈어 수업을 듣는 학생이 수학 수업을 듣는 학생보다 성적이 더 높다. => 특정 과목에 대한 학업 성취도가 다를 수 있기 때문에 이 변수는 두 과목간의 성적 차이를 반영해주고 있다. 

# studytime: 추정치: 0.50658: 주간 학습 시간을 나타내는 변수: 학습 시간이 많을수록 당연히 학업 성취도가 높아질 가능성이 높다고 판단.

# 부정적 변수
# failures: 추정치 -1.50136: 학업 실패 횟수가 증가할수록 최종 성적(G3)은 낮아진다. => 실패 횟수가 많을수록 학업 성취도에 부정적인 영향을 미칠것이라고 생각, 학업에서의 실패 경험은 학생의 학업 성취도에 직접적인 영향을 미칠 것이다.

# schoolsup: 추정치 -1.05979: 학교에서 제공하는 추가 교육 지원을 받는지 여부를 나타내는 변수: 학교 지원을 받는 학생들이 평균적으로 성적(G3)이 약 1.06점 낮다는 것을 보여줌 => 추가 교육 지원을 받는 학생의 성적이 낮다.이것은 추가 교육 지원이 필요한 학생들은 이미 학업에서 어려움을 겪고 있을 가능성이 있을 수 있다.

# romantic: 추정치: -0.68659: 학생이 현재 연애중인지 여부=> 연애 관계는 학생의 학업  성취도에 영향을 미칠 수 있다고 판단. 중요한 것들이 더 많이 생기기 때문에 부정적 영향을 미칠 것이라고 판단.

# health: 추정치: -0.2084: 학생의 현재 건강 상태를 나타내는 변수 => 건강 상태가 좋을 수록 전체 학업 성취도는 낮아짐.. 일반적인 기대와는 다소 반대되는 결과이다. 다른 요인들에 의해 이런 결과가 나타났을 것이라고 예상. ex. 건강상태가 좋은 학생들이 다른 활동에 더 많은 시간을 할애할 수 있기 때문에 이런 결과가 나왔을 수도 있다고 생각.

# goout: 추정치: -0.28986: 친구와 외출하는 빈도를 나타냄 => 친구와의 외출 빈도가 많을수록 스스로 혼자 공부하는 시간이 줄어들기 때문에 부정적 영향을 미칠 것이다.


#4
# 유의미한 독립변수들로 선형 회귀 모델 생성 (school, famsize, Medu, studytime, failures, schoolsup, higher, internet, gogout, health, class)
grade_model1 <- lm(G3 ~ school+famsize+Medu + studytime + failures + schoolsup + higher +internet+ goout +health+ class, data = student_train)

# 예측 및 성능 평가
student_train$pred  <- predict(grade_model1, newdata = student_train)
student_test$pred  <- predict(grade_model1, newdata = student_test)

# performance on student_train
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)

# performance on student_test
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)

# train RMSE : 3.341015
# test RMSE  : 3.200559
# train R2 : 0.2604254
# test R2 : 0.2971999
# test의 R2의 값이 모든 독립변수들을 넣었을 때보다 성능이 올라갔다.





# 모델 성능 높이기 
grade_model2 <- lm(G3 ~ school + Medu + Fedu + Mjob + Fjob + reason + studytime + failures + higher + romantic + goout + class, student_train)
grade_model2

# Prediction and Testing
student_train$pred <- predict(grade_model2, newdata = student_train)
student_test$pred <- predict(grade_model2, newdata = student_test)

# performance on student_train
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)

# performance on student_test
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)

summary(grade_model2)
# Residual standard error: 3.285 on 671 degrees of freedom
# Multiple R-squared:  0.2702,	Adjusted R-squared:  0.2485 
# F-statistic: 12.42 on 20 and 671 DF,  p-value: < 2.2e-16

# [정예화. "초·중·고등학생의 학업성취도에 영향을 미치는 요인 분석." 국내석사학위논문 이화여자대학교 교육대학원, 2016. 서울]
# => 부모의 학력과 교육 기대는 모든 학교급의 학업성취도에 긍정적인 영향을 미치며, 월평균 가구 소득도 초등학교와 고등학교의 국어 성적을 제외한 모든 학업성취도에 긍정적인 영향을 미치는 것으로 나타났다는 결과를 통해 첫 번째 모델(모든 변수를 독립변수로)에서 영향이 있었던 것과 부모의 학력, 월평균 가구 소득도 영향을 미칠것으로 보아 Medu, Fedu, Mjob, Fjob을 넣어주었다.
# => school + Medu + Fedu + Mjob + Fjob + reason + studytime + failures + higher + romantic + goout + class 독립변수들을 수정했지만 .... 설명력이 더 낮아졌다.
# Residual standard error: 3.285 on 671 degrees of freedom
# Multiple R-squared:  0.2702,	Adjusted R-squared:  0.2485 
# F-statistic: 12.42 on 20 and 671 DF,  p-value: < 2.2e-16



#age를 20세 미만여부로 구분(성인여부)
#성인이냐 아니냐의 차이로, 나이가 들면서 공부하는 습관이나 지식 등이 더 성장할 것이라 생각함. 그리하여 성적과 나이도 연관있어서 성인이 되는 나이인 20살을 기준으로 나누었음
student_train$age20 <- ifelse(student_train$age <20, 0, 1)
student_test$age20 <- ifelse(student_test$age <20, 0, 1)

#age20과 age는 공상성이 있는 변수이기에 age와 pred를 선형회귀 만들때 제외함.
#부모의 교육정도와 job은 서로 연관이 있다 판단하여 Medu * Mjob과 Fedu*Fjob을 추가하였다.
grade_model3 <- lm(G3 ~ . -age - pred + Medu*Mjob + Fedu*Fjob, student_train)
grade_model3
summary(grade_model3)

student_train$pred <- predict(grade_model3, newdata = student_train)
student_test$pred <- predict(grade_model3, newdata = student_test)
# performance on train.df
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)
# performance on test.df
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)


##하나 더 만들어보기
#-age -pred +Medu*Mjob + Fedu*Fjob + 긍정적,부정적 영향 변수들
grade_model4 <- lm(G3 ~ age20 + Medu*Mjob + Fedu*Fjob + higher + class + studytime + failures + school + romantic + health + goout, student_train)
grade_model4
summary(grade_model4)

student_train$pred <- predict(grade_model4, newdata = student_train)
student_test$pred <- predict(grade_model4, newdata = student_test)
# performance on train.df
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)
# performance on test.df
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)



#estimate 작은것 제외
grade_model5 <- lm(G3 ~ . - (sex + age + Medu + reason + activities + absences + pred), data = student_train)
student_test$pred <- predict(grade_model5, student_test)
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)
#미세하게 R2값이 하락하는 것을 확인하였다
summary(grade_model4)



# log10로 변환해보기
student_test_log <- student_test
student_test_log[] <- lapply(student_test, function(x) {
  if (is.integer(x)) {
    # Apply log10
    log_x <- log10(as.numeric(x))
    # Replace -Inf with 0
    log_x[log_x == -Inf] <- 0
    return(log_x)
  } else {
    return(x)
  }
})

student_train_log <- student_train
student_train_log[] <- lapply(student_train, function(x) {
  if (is.integer(x)) {
    # Apply log10
    log_x <- log10(as.numeric(x))
    # Replace -Inf with 0
    log_x[log_x == -Inf] <- 0
    return(log_x)
  } else {
    return(x)
  }
})

grade_model6  <- lm(G3 ~ . - pred, student_train_log)

student_train_log$pred <- predict(grade_model6 , student_train_log)
student_test_log$pred <- predict(grade_model6 , student_test_log)

calcRMSE(student_train_log$G3, student_train_log$pred)
calcR2(student_train_log$G3, student_train_log$pred)
calcRMSE(student_test_log$G3, student_test_log$pred)
calcR2(student_test_log$G3, student_test_log$pred)
summary(grade_model5)


#Adjusted R-squared값 한번에 확인하기 위함.
summary(grade_model1)
summary(grade_model2)
summary(grade_model3)
summary(grade_model4)
summary(grade_model5)
summary(grade_model6)

#5
#grade-model 1~5번 모델의 Adjusted R-squared값을 확인한 결과, grade_model3가 2.662로 가장 높았다.
#Converting Num. var. into a binary indicator 및 Adding Interaction의 작업을 한 것이 좋은 결과를 만들어냈다고 생각한다.

#age20과 age는 공상성이 있는 변수이기에 age와 pred를 선형회귀 만들때 제외함.
#부모의 교육정도와 job은 서로 연관이 있다 판단하여 Medu * Mjob과 Fedu*Fjob을 추가하였다.
grade_model3 <- lm(G3 ~ . -age - pred + Medu*Mjob + Fedu*Fjob, student_train)
grade_model3
summary(grade_model3)

student_train$pred <- predict(grade_model3, newdata = student_train)
student_test$pred <- predict(grade_model3, newdata = student_test)
# performance on train.df
calcRMSE(student_train$G3, student_train$pred)
calcR2(student_train$G3, student_train$pred)
# performance on test.df
calcRMSE(student_test$G3, student_test$pred)
calcR2(student_test$G3, student_test$pred)


# Residual standard error: 3.33 on 635 degrees of freedom
# Multiple R-squared:  0.3177,	Adjusted R-squared:  0.2662 
# F-statistic: 6.161 on 48 and 635 DF,  p-value: < 2.2e-16

