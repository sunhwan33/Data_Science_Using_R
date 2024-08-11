############################Homework12_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린


##Q1
SeoulBike <- read.csv("SeoulBikeData-2.csv")
str(SeoulBike)
dim(SeoulBike)

##Q2
SeoulBike$RBC <- SeoulBike$Rented.Bike.Count
SeoulBike$TEMP <- SeoulBike$Temperature
SeoulBike$HumP <- SeoulBike$Humidity.percent.
SeoulBike$Wind <- SeoulBike$Wind.speed
SeoulBike$vis <- SeoulBike$Visibility
SeoulBike$Dew <- SeoulBike$Dew.point.temperature
SeoulBike$Solar <- SeoulBike$Solar.Radiation..MJ.m2.
SeoulBike$Rain <- SeoulBike$Rainfall.mm.
SeoulBike$Snow <- SeoulBike$Snowfall..cm.
SeoulBike$F.Day <- SeoulBike$Functioning.Day

SeoulBike <- subset(SeoulBike, select = -Rented.Bike.Count)
SeoulBike <- subset(SeoulBike, select = -Temperature)
SeoulBike <- subset(SeoulBike, select = -Humidity.percent.)
SeoulBike <- subset(SeoulBike, select = -Wind.speed)
SeoulBike <- subset(SeoulBike, select = -Visibility)
SeoulBike <- subset(SeoulBike, select = -Dew.point.temperature)
SeoulBike <- subset(SeoulBike, select = -Solar.Radiation..MJ.m2.)
SeoulBike <- subset(SeoulBike, select = -Rainfall.mm.)
SeoulBike <- subset(SeoulBike, select = -Snowfall..cm.)
SeoulBike <- subset(SeoulBike, select = -Functioning.Day)
colnames(SeoulBike)

#Q3
SeoulBike$Date <- as.Date(SeoulBike$Date,"%d/%m/%Y")
SeoulBike$Holiday <- ifelse(SeoulBike$Holiday=='Holiday',1,0)

#4. 
# Date 제거하고 대신에 Seasons 쓰기
# 모델링을 진행할 때 변수의 수가 많다면 모델의 성능은 향상될 수 있다. 그러나 너무 많은 양의 데이터를 다루게 된다면 연산에 지장이 될 것으로 판단했다.
# 이에 우리 조는 Date와 유의성이 높은 season 변수를 사용하고 date변수를 제거하였다.

# 이슬점 제거(온도와 습도와 연관성을 봐서 이슬점을 제거함)
cor.test(SeoulBike$TEMP, SeoulBike$Dew) # 상관 관계 확인
plot(density(SeoulBike$TEMP))
lines(density(SeoulBike$Dew))
# 온도와 이슬점 간에는 큰 상관관계(0.9127982)이 있다고 판단하여 이슬점을 제거해주었다. 

#Q5
SeoulBike <- subset(SeoulBike, select = -c(Date, Dew))

#6.
#Hour - 오전10시~저녁 10시 사이에만 자전거를 보통 빌릴것 같기 때문에 +
#Temp - 온도가 증가할수록 자전거를 더 잘 빌릴 것 같기 때문에 +
#HumP - 습도가 높으면 비 올 확률도 높고, 불쾌지수도 올라 야외 활동을 줄일 것 같기 빌리지 때문에 -
#Wind - 바람이 높을수록 빌리지 않을 것 같기 때문에 -
#Vis - 가시성이 낮을수록 위험하다고 느낄 것 같기 때문에 자전거 대여량이 낮을 것이다. +
#Solar - 자외선이 높아지면 사람들이 야외 활동을 줄일 것 같기 때문에 -
#Rain - 비가 오면 자전거를 타기 어렵기 때문에 -
#Snow - 눈이 오면 길이 미끄러워서 위험하기 때문에 자전거 대여량이 낮을 것이다. -
#Seasons - 봄과 가을은 날씨가 쾌적하여 + 일 것으로 예상하며, 여름과 겨울은 날씨가 자전거를 타기에 좋지 않은 날이 많기 때문에 - 일 것 같다. 
#Holiday - 따릉이를 이용하는 사람들 중에는 휴일에 놀러가기 위해 이용하는 사람들도 있을 것이다. +
#F.Day - 개장한 날에 당연히 사람들이 자전거를 빌리기 때문에 +

# Q7
# Q6에서 세운 가설 확인
#Wind를 바람이 높을 때 대여량이 낮을 것인 -관계로 가설을 세웠음, 실제 Estimate는 1.864e+01로 +의 관계로, 영향이 끼쳤음을 알 수 있었다.
# Sonw를 눈이 올 때 대여량이 낮을 것인 -관계 로 가설을 세웠음, 실제 Estimate가 3.023e+01로 + 관계로 영향을 끼친다는 것을 알 수 있었다. 
#Season은 봄과 가을은 +관계, 여름과 겨울은 -관계로 가설을 세웠음. 실제 Estimate 확인 결과, 가을에 비하여 봄은 -1.366e+02, 여름은 -1.501e+02, 겨울은 -3.670e+02배 대여량이 낮음을 확인하였다. 그렇다면 봄과 여름의 영향은 서로 비슷함을 확인 할 수 있었다. 즉 가설로는 봄,가을 / 여름,겨울의 영향이 서로 다를 것이라 예상하였으나, 실제로는 봄,여름,가을은 비슷한 영향을 끼침을 알 수 있었다.
# Holiday에 대여량이 높을 것인 +로 가설을 세웠음, 실제 Estimate 확인 결과 -1.170e+02로 -의 관계로 영향이 끼쳤음을 알 수 있었다. 휴일에 대여량이 더 적다고 해석할 수 있다.

model <- lm(RBC~.,SeoulBike)
summary(model)




# Q8
#다중공선성 확인
# VIF 값이 10을 넘어가는 값이 없음을 확인 하였다.
# 독립변수들 간에 다중공선성 문제가 없다고 해석할 수 있다.
# install.packages("car")
library(car)
vif(model)


## Q9
# 예측값 계산
SeoulBike$pred <- predict(model, newdata = SeoulBike)

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

# SeoulBike의의 RMSE, R2 값 계산
calcRMSE(SeoulBike$RBC, SeoulBike$pred) # 432.6505
calcR2(SeoulBike$RBC, SeoulBike$pred) #0.5500047

# 예측값(pred)과 실제 대여 수 (RBC)를 비교하는 산점도 생성
library(ggplot2)
ggplot(SeoulBike, aes(x = pred, y = RBC)) +
  geom_point(alpha = 0.2, col = "black") +
  geom_smooth() +
  geom_line(aes(x = RBC, y = RBC), col = "blue", linetype = 2)

## Q10
# 회귀 계수 (Coefficients)
# (Intercept): -211.34698
# 모델의 모든 독립 변수 값이 0일 때의 종속 변수RBCC의 예상 값
# Hour: 27.45872 - 시간당 자전거 대여 수는 평균적으로 27.46만큼 증가한다.
# TEMP: 26.42775 - 온도가 1도 상승할 때 자전거 대여 수는 평균적으로 26.43만큼 증가한다.
# HumP: -8.00962 - 습도가 1% 증가할 때 자전거 대여 수는 평균적으로 8.01만큼 감소한다.
# wind: 18.64081 - 바람 속도가 증가할 때 자전거 대여 수는 평균적으로 18.64만큼 증가한다. (예상 밖의 결과)
# vis: 0.01194 가시거리가 증가할 때 자전거 대여 수는 평균적으로 0.012만큼 증가한다.
# Solar: -81.90868 - 태양 광선이 증가할 때 자전거 대여 수는 평균적으로 81.91만큼 감소한다.
# Rain: -59.91700 - 비가 올 때 자전거 대여 수는 평균적으로 59.92만큼 감소한다.
# Snow: 30.22739 - 눈이 올 때 자전거 대여 수는 평균적으로 30.23만큼 증가한다. (예상 밖의 결과..)
# SeasonsSpring: -136.63393 - 가을과 비교하여 봄에 자전거 대여 수는 평균적으로 136.63만큼 감소한다.
# SeasonsSummer: -150.12231 - 가을과 비교하여 여름에 자전거 대여 수는 평균적으로 150.12만큼 감소한다.
#  SeasonsWinter: -366.97905 - 가을과 비교하여 겨울에 자전거 대여 수는 평균적으로 366.98만큼 감소한다.
# Holiday1: -117.02447 - 공휴일일 때 자전거 대여 수는 공휴일이 아닐 때보다 평균적으로 117.02만큼 감소한다.(가설은 틀렸지만 공휴일이 아닌날이 더 많고 공휴일이 아니면 놀러가는 사람보다 자신의 일을 하러 다니는 사람이 많다. 출퇴근이나 이동할 때 사람들이 자전거를 탈 것 같아서 ... 감소하는 것 같음)
# F.DayYes: 929.86738 - 운영하는 날(F.DayYes)에는 자전거 대여 수가 운영하지 않는 날(F.DayNo)보다 평균적으로 929.87만큼 증가한다.

# 모델 성능
# Residual standard error (RSE): 433
# 실제 값과 예측 값 사이의 평균적인 차이 => 모델의 예측 오차
# Multiple R-squared: 0.55
# 전체 데이터 변동성 중 모델이 설명하는 비율로, 이 경우 모델은 전체 변동성의 55%를 설명하고 있다.
# Adjusted R-squared: 0.5493
#  설명 변수의 수를 고려한 R-squared 값으로, 모델이 54.93%의 변동성을 설명하고 있다.

# VIF
# VIF값이 10을 넘지 않으므로 다중공선성 문제는 크지 않은 것으로 보인다.

# 모델은 자전거 대여 수를 예측하는 데 있어서 중간정도의 설명력을 가지고 있는 것 같다. 일부 변수들이 자전거 대여 수에 유의한 영향을 미치고 있다는 것을 알 수 있다. 
# 공휴일에는 자전거 대여 수가 줄어드는 경향이 있으며, 운영하는 날(F.DayYes)에는 자전거 대여 수가 크게 증가하는 것을 알 수 있다.

## Q11
library(dplyr)
# 습도를 직접 고려하는 것보다 습도로 인한 기분 상태가 고려된다고 생각해서 불쾌지수 열을 넣고 습도는 빼주고자한다.

# 불쾌지수 계산 함수 정의
discomfort_index <- function(temp_C, relative_humidity) {
  discomfort <- (5/9 * temp_C) - 0.55 * (1 - relative_humidity/100) * ((5/9 * temp_C) - 26) + 32
  return(discomfort)
}
# 불쾌지수 = 5/9 T-0.55(1-RH)(5/9 T-26)+32
# (T:기온(℃), RH:상대습도(%))
# 1957년 미국의 E. C. Thom에 의해 고안됨
# 이 계산식은 인터넷을 통해 가져왔다.

# 불쾌지수 계산하여 새로운 열 추가
SeoulBike$Discomfort <- discomfort_index(SeoulBike$TEMP, SeoulBike$HumP)

# 결과 확인
head(SeoulBike)

#### new1 ####
# 새로운 선형 회귀 모델 생성
model2 <- lm(RBC ~ . -TEMP -HumP -pred, SeoulBike)
model2
summary(model2)

vif(model2)
# 확인한 결과 그렇게 큰 수준은 아니다.
#VIF값이 10을 넘지 않으므로 다중공선성 문제는 크지 않은 것으로 보인다

## 
# 예측값 계산
SeoulBike$pred <- predict(model2, newdata = SeoulBike)


# SeoulBike의의 RMSE, R2 값 계산
calcRMSE(SeoulBike$RBC, SeoulBike$pred) #440.1383
calcR2(SeoulBike$RBC, SeoulBike$pred) # 0.534294

library(ggplot2)
ggplot(SeoulBike, aes(x = pred, y = RBC)) +
  geom_point(alpha = 0.2, col = "black") +
  geom_smooth() +
  geom_line(aes(x = RBC, y = RBC), col = "blue", linetype = 2)

# 성능이 조금 떨어짐..

#### new2 ####
#RBC의 분산이 한쪽으로 치우쳐있기에, 이를 log를 통해 정규화하면 성능이 개선될 지 않을까 해서 이를 적용하여 계산. 
colnames(SeoulBike)

# 종속 변수 로그 변환 적용
SeoulBike$log_RBC <- log10(SeoulBike$RBC)
sum(SeoulBike$log_RBC == -Inf)
SeoulBike$log_RBC <- ifelse(SeoulBike$log_RBC == -Inf, 0, SeoulBike$log_RBC)
sum(SeoulBike$log_RBC == 0)

colnames(SeoulBike)
model3 <- lm(log_RBC~. -RBC -TEMP -HumP -pred, SeoulBike)
summary(model3)

#다중공산성 확인

# install.packages("car")
library(car)
vif(model3)
#VIF값이 10을 넘지 않으므로 다중공선성 문제는 크지 않은 것으로 보인다

SeoulBike$pred <- predict(model3, newdata=SeoulBike)

#log가 된 예측이기 때문에, 예측값과 실제 값을 다시 exp하여 실제 오차의 합을 확인 할 수 있었다. 하지만 R2와 같은 경우에는 모델 자체가 log된 것에 train됐기 때문에 성능을 확인하기 위해서는 그대로 사용하여야 한다.
calcRMSE(exp(SeoulBike$log_RBC),exp(SeoulBike$pred))# 4.142851
calcR2(SeoulBike$log_RBC,SeoulBike$pred ) #0.7803466

ggplot(SeoulBike, aes(x = pred, y = log_RBC)) +
  geom_point(alpha = 0.2, col = "black") +
  geom_smooth() +
  geom_line(aes(x = log_RBC, y = log_RBC), col = "blue", linetype = 2)


#### new3 ####
#discomfort말고 원래대로 temp랑 hump 사용
model4 <- lm(log_RBC~. -RBC -Discomfort -pred, SeoulBike)
summary(model4)

#다중공산성 확인

# install.packages("car")
library(car)
vif(model4)
#VIF값이 10을 넘지 않으므로 다중공선성 문제는 크지 않은 것으로 보인다

SeoulBike$pred <- predict(model4, newdata=SeoulBike)

#log가 된 예측이기 때문에, 예측값과 실제 값을 다시 exp하여 실제 오차의 합을 확인 할 수 있었다. 하지만 R2와 같은 경우에는 모델 자체가 log된 것에 train됐기 때문에 성능을 확인하기 위해서는 그대로 사용하여야 한다.
calcRMSE(exp(SeoulBike$log_RBC),exp(SeoulBike$pred)) #4.143497
calcR2(SeoulBike$log_RBC,SeoulBike$pred ) #0.7852449

ggplot(SeoulBike, aes(x = pred, y = log_RBC)) +
  geom_point(alpha = 0.2, col = "black") +
  geom_smooth() +
  geom_line(aes(x = log_RBC, y = log_RBC), col = "blue", linetype = 2)

