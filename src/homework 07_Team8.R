
#############################Homework7_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린

# Loading data into R
PRSA_data <- read.csv("PRSA_data.csv")

library(dplyr)
glimpse(PRSA_data)
# PRSA dataset: 2010년 1월1일부터 2014년 12월 31일까지의 중국 베이징의 미세먼지 농도 및 날씨 정보

##Q1 미세먼지 농도(pm2.5)를 예측하는 Single variable Regression 모델을 만들기
# train test data set 만들기
train.data <- subset(PRSA_data, year <= 2013)
test.data <- subset(PRSA_data, year > 2013)

# is.na를 통해서 각 열의 결측치 합 확인  -> pm2.5변수에 NA값 존재 확인
colSums(is.na(train.data))
colSums(is.na(test.data))

#데이터에서 pm2.5변수에 결측치 있는 행 확인
na.train <- train.data[is.na(train.data$pm2.5), ]
na.test <- test.data[is.na(test.data$pm2.5), ]

# na.omit을 통해서 결측치 제거 
train.data <- na.omit(train.data)
test.data <- na.omit(test.data)

#is.na를 통해서 각 열의 결측치 합 확인 (결측치 처리 되었는지 확인)
colSums(is.na(train.data))
colSums(is.na((test.data)))

# 행, 열 개수 확인
dim(train.data)
dim(test.data)
# 샘플 수는 train data는 33096개, test data는 8661개로 나누어졌다.
# 비율은?
nrow(train.data)/nrow(test.data)
# 3.821268

# summary의 값들과 분산 값을 한 번에 도출하기 위한 새로운 벡터 생성 
train_distribution <- summary(train.data$pm2.5)
train_distribution <- c(train_distribution, var = var(train.data$pm2.5))
train_distribution

test_distribution <- summary(test.data$pm2.5)
test_distribution <- c(test_distribution, var = var(test.data$pm2.5))
test_distribution

# train data와 test data 그래프 그리기
plot(density(train.data$pm2.5), col = "black", 
     main = "distribution of pm2.5 of train(black) and test(red) data", xlab = "N = 33096 Bandwidth = 9.133") 

lines(density(test.data$pm2.5), col = "red")
##lines(density(train.data$pm2.5), col="blue")
#plot으로 틀을 먼저 만들어야. 그 후에 lines로 그래프 추가 가능

#train_data의 분포와 test data의 분포가 서로 비슷함을 확인함


##Q2-1 month를 사용하여 pm2.5를 예측하는 단일 변수 모델(single variable model)을 만들기 

# 단일 변수 모델 생성 및 예측(독립변수 : month)
sv_reg_month <- tapply(train.data$pm2.5, train.data$month, mean)
sv_reg_month

# train dataset으로 예측값 만들기 
train.data$pred <- sv_reg_month[train.data$month]
# Error 구해서 error 열 생성 및 추가
train.data$error <- train.data$pm2.5 - train.data$pred
head(train.data[, c('month', 'pm2.5', 'pred', 'error')], 10)

# test dataset으로 예측값 만들기 
test.data$pred <- sv_reg_month[test.data$month]
# Error 구해서 error 열 생성
test.data$error <- test.data$pm2.5 - test.data$pred
head(test.data[, c('month', 'pm2.5', 'pred', 'error')], 10)

##Q2-2 Question 2-1에서 구한 모델의 MSE와 RMSE 구하기
MSE_train <- mean(train.data$error ** 2)
MSE_train

RMSE_train <- sqrt(MSE_train)
RMSE_train

MSE_test <- mean(test.data$error ** 2)
MSE_test

RMSE_test <- sqrt(MSE_test)
RMSE_test

paste0("train data:  ", "(MSE ", round(MSE_train, digits = 3), ")", " ", "(RMSE ", round(RMSE_train, digits = 3), ")")

paste0("test data:  ", "(MSE ", round(MSE_test, digits = 3), ")", " ", "(RMSE ", round(RMSE_test, digits = 3), ")")


##Q2-3 Question 2-1에서 구한 모델을 적용하여 train data와 test data의 R^2값 구하기

RSS_train <- sum(train.data$error ** 2)
TSS_train <- sum((train.data$pm2.5 - mean(train.data$pm2.5))**2)
R2_train <- 1 - (RSS_train/TSS_train)

RSS_test <- sum(test.data$error ** 2)
TSS_test <- sum((test.data$pm2.5 - mean(test.data$pm2.5))**2)
R2_test <- 1 - (RSS_test/TSS_test)

paste("R2 for train data: ", round(R2_train, digits = 3))
paste("R2 for test data: ", round(R2_test, digits = 3))
# train과 test model 둘 다 R2 값이 너무 낮은 것을 볼 수 있다. # train 데이터에서는 R2값이 0.017로 pm2.5의 변동을 대부분 설명하지 못하고 있다. 이것은 모델이 훈련 데이터에서 과적합 되었거나, 사용된 단일 변수가 종속 변수와의 관계를 잘 반영하지 못하고 있는 것으로 보인다. 
# test 데이터 역시 R2값이 0.040으로 pm2.5의 변동을 제대로 예측하지 못하고 있다. 모델의 일반화 능력이 낮고, 새로운 데이터에 대한 예측 정확도가 낮을 수 있다. 
# 결과적으로 주어진 단일 변수 모델은 pm2.5의 변동을 제대로 설명하지 못하며, train data 및 test data 모두 좋은 성능을 보여주지 못한다.


##Q3 
summary(train.data$hour)
summary(test.data$hour)
table(train.data$hour) #고루 분포되어있음을 확인

# 간격 설정
interval <- 2
counts <- length(levels(as.factor(train.data$hour)))
hour_labels <- cut(train.data$hour, breaks = c(seq(min(train.data$hour), max(train.data$hour), by = interval), counts), include.lowest = TRUE)
train.data$hour_labels <- hour_labels

# sv model 생성 
sv_pm_hour <- tapply(train.data$pm2.5, train.data$hour_labels, mean)
sv_pm_hour
train.data$pred_hour <- sv_pm_hour[train.data$hour_labels]
train.data$error_hour <- train.data$pm2.5 - train.data$pred_hour
head(train.data[,c('hour_labels', 'pm2.5', 'pred_hour', 'error_hour')],10)

table(test.data$hour) #고루 분포되어있음을 확인
hour_labels <- cut(test.data$hour, breaks = c(seq(min(train.data$hour), max(train.data$hour), by = interval), counts), include.lowest = TRUE)

test.data$hour_labels <- hour_labels
test.data$pred_hour <- sv_pm_hour[test.data$hour_labels]
test.data$error_hour <- test.data$pm2.5 - test.data$pred_hour
head(test.data[,c('hour_labels', 'pm2.5', 'pred_hour', 'error_hour')],10)

##Q3-2
MSE_train_hour <- mean(train.data$error_hour ** 2)
RMSE_train_hour <- sqrt(MSE_train_hour)
MSE_test_hour <- mean(test.data$error_hour ** 2)
RMSE_test_hour <- sqrt(MSE_test_hour)

paste0("train data: (MSE ",round(MSE_train_hour,3),")  (RMSE ",round(RMSE_train_hour,3),")")
paste0("test data: (MSE ",round(MSE_test_hour,3),")  (RMSE ",round(RMSE_test_hour,3),")")
#interval : 1 -> train data: (MSE 8311.055)  (RMSE 91.165) | test data: (MSE 8662.482)  (RMSE 93.072)
#interval : 2 -> train data: (MSE 8313.696)  (RMSE 91.179) | test data: (MSE 8664.292)  (RMSE 93.082)
#interval : 3 -> train data: (MSE 8317.668)  (RMSE 91.201) | test data: (MSE 8666.813)  (RMSE 93.096)
#interval : 5 -> train data: (MSE 8329.622)  (RMSE 91.267) | test data: (MSE 8674.747)  (RMSE 93.138)"
#interval : 12 -> train data: (MSE 8397.699)  (RMSE 91.639) | test data: (MSE 8739.382)  (RMSE 93.485)

"
왜 간격을 2시간으로 했느냐?
1. 분석의 정확성:
각 간격에 대한 MSE와 RMSE 값을 비교했을 때, 2시간 간격으로 설정했을 때의 값이 다른 간격과 비교해 큰 차이가 없으면서도, 예측 모델의 오차를 상대적으로 잘 설명합니다.
구체적으로, 2시간 간격일 때의 MSE와 RMSE 값이 다른 간격에 비해 적정 수준에서 균형을 이루고 있습니다. 너무 세분화하면 데이터가 과도하게 분할되어 노이즈가 증가할 수 있고, 너무 크게 설정하면 시간대별 패턴을 놓칠 수 있습니다.

2. 해석의 용이성:
  2시간 간격은 하루 24시간을 나누기에 적합한 주기로, 하루를 12개의 구간으로 나누어 해석하기 쉬운 범위를 제공합니다.
이는 실생활에서 시간대를 구분할 때 흔히 사용되는 단위로, 결과를 해석하고 전달할 때 직관적입니다.

3. 데이터의 분포와 패턴:
  2시간 간격은 하루 동안의 미세먼지 농도의 변화를 관찰하기에 충분히 세분화되어 있어, 시간대별로 발생하는 패턴을 효과적으로 포착할 수 있습니다.
특히, 하루 중 미세먼지 농도가 급격히 변할 수 있는 특정 시간대(예: 아침 출근 시간, 저녁 퇴근 시간 등)를 효과적으로 분석할 수 있습니다.

4. 모델의 안정성:
  2시간 간격은 데이터의 분포가 고르게 분포되어 있는지를 확인하고, 각 시간대의 평균값을 계산할 때 충분한 데이터를 포함할 수 있어 모델의 예측값이 안정적입니다.
다른 간격(예: 1시간, 3시간 등)과 비교했을 때, 2시간 간격은 모델이 과적합(overfitting)되지 않으면서도 적절한 일반화(generalization)를 가능하게 합니다.

- 결론
2시간 간격을 선택한 이유는 모델의 예측 성능(MSE와 RMSE)을 고려할 때, 해석의 용이성, 데이터의 분포와 패턴을 효과적으로 포착할 수 있는 균형점을 제공하기 때문입니다. 이는 데이터 분석에서 중요한 시간대별 패턴을 놓치지 않으면서도, 과도한 세분화로 인한 노이즈를 줄여주는 적절한 선택입니다.
"
##Q3-3
RSS_train_hour <- sum(train.data$error_hour **2)
TSS_train_hour <- sum((train.data$pm2.5 - mean(train.data$pm2.5))**2)
R2_train_hour = 1 - (RSS_train_hour / TSS_train_hour )
RSS_test_hour <- sum(test.data$error_hour **2)
TSS_test_hour <- sum((test.data$pm2.5 - mean(test.data$pm2.5))**2)
R2_test_hour = 1 - (RSS_test_hour / TSS_test_hour)

paste("R2 for train data: :", round(R2_train_hour ,3))
paste("R2 for test data: :", round(R2_test_hour ,3))
#interval : 1 -> 0.011 | 0.01
#interval : 2 -> 0.01 | 0.009
#interval : 3 -> 0.01 | 0.009
#interval : 5 -> 0.009 | 0.008
#interval : 6 -> 0.009 | 0.008
#interval : 12 -> 0 | 0.001

##Q4

summary(train.data$DEWP)
table(train.data$DEWP)
summary(test.data$DEWP)
table(test.data$DEWP)
plot(train.data$DEWP)
plot(density(train.data$DEWP))
#간격 설정 
#DEWP데이터를 맨 양쪽의 특정 부분은 Inf로 설정하고, 또한 interval 간격만큼 구분하여 나누었다. 
interval <- 3
counts <- length(levels(as.factor(train.data$DEWP)))
DEWP_labels <- cut(train.data$DEWP, breaks = c(-Inf, seq(min(train.data$DEWP), max(train.data$DEWP), by = interval), counts, Inf), include.lowest = TRUE)
train.data$DEWP_labels <- DEWP_labels
#sv 모델 생성
sv_reg_dewp <- tapply(train.data$pm2.5, train.data$DEWP_labels, mean)
sv_reg_dewp
train.data$pred_DEWP <- sv_reg_dewp[train.data$DEWP_labels]
train.data$error_DEWP <- train.data$pm2.5 - train.data$pred_DEWP
head(train.data[,c('DEWP_labels', 'pm2.5', 'pred_DEWP', 'error_DEWP')],10)

#test data
DEWP_labels <- cut(test.data$DEWP, breaks = c(-Inf, seq(min(train.data$DEWP), max(train.data$DEWP), by = interval), counts, Inf), include.lowest = TRUE)
test.data$DEWP_labels <- DEWP_labels
test.data$pred_DEWP <- sv_reg_dewp[test.data$DEWP_labels]
test.data$error_DEWP <- test.data$pm2.5 - test.data$pred_DEWP
head(test.data[,c('DEWP_labels', 'pm2.5', 'pred_DEWP', 'error_DEWP')],10)

##Q4-2
MSE_train_dewp <- mean(train.data$error_DEWP ** 2)
RMSE_train_dewp <- sqrt(MSE_train_dewp)
MSE_test_dewp <- mean(test.data$error_DEWP ** 2)
RMSE_test_dewp <- sqrt(MSE_test_dewp)

paste0("train data: (MSE ",round(MSE_train,3),")  (RMSE ",round(RMSE_train,3),")")
paste0("test data: (MSE ",round(MSE_test,3),")  (RMSE ",round(RMSE_test,3),")")

#DEWP데이터를 구분할 때, Inf, -Inf 를 사용하지 않고 interval 간격만큼 구분하여 나누었을 때는 MSE와 RMSE 값이 다음과 나타났다. 이렇게 하였을 때의 추후 계산한 R2값이 test data에 한하여 NA 혹은 음수값이 존재하여, 맨처음 데이터를 구분할 때의 과정이 잘못되었음을 추측하였다.
#interval : 1 -> train data: (MSE 7297.323)  (RMSE 85.424) | test data: (MSE NA)  (RMSE NA)
#interval : 2 -> train data: (MSE 7336.821)  (RMSE 85.655) | test data: (MSE NA)  (RMSE NA)
#interval : 3 -> train data: (MSE 7363.883)  (RMSE 85.813) | test data: (MSE NA)  (RMSE NA)
#interval : 5 -> train data: (MSE 7438.724)  (RMSE 86.248) | test data: (MSE 10004.576)  (RMSE 100.023)
#interval : 10 -> train data: (MSE 7599.642)  (RMSE 87.176) | test data: (MSE 9666.631)  (RMSE 98.319)
#interval : 12 -> train data: (MSE 7658.183)  (RMSE 87.511) | test data: (MSE 9242.06)  (RMSE 96.136)

#-----------
#DEWP데이터를 Inf와 -Inf를  설정하고, 또한 interval 간격만큼 구분하여 나누었을 때의 R2의 결과이다.  test dataset에서는 train dataset에 없는 DEWP 값이 존재하였기 때문에 Inf와 -Inf를 사용하였다.
#interval : 1 -> train data: (MSE 7297.323)  (RMSE 85.424) | test data: (MSE 7806.128)  (RMSE 88.352)
#interval : 2 -> train data: (MSE 7336.821)  (RMSE 85.655) | test data: (MSE 7822.223)  (RMSE 88.443)
#interval : 3 -> train data: (MSE 7363.883)  (RMSE 85.813) | test data: (MSE 7814.02)  (RMSE 88.397)
#interval : 5 -> train data: (MSE 7438.722)  (RMSE 86.248) | test data: (MSE 7887.903)  (RMSE 88.814)
#interval : 10 -> train data: (MSE 7599.623)  (RMSE 87.176) | test data: (MSE 8012.977)  (RMSE 89.515)
#interval : 12 -> train data: (MSE 7658.183)  (RMSE 87.511) | test data: (MSE 8130.675)  (RMSE 90.17)
#interval : 30 -> train data: (MSE 8274.868)  (RMSE 90.966) | test data: (MSE 8857.809)  (RMSE 94.116)

##Q4-3
RSS_train_dewp <- sum(train.data$error_DEWP **2)
TSS_train_dewp <- sum((train.data$pm2.5 - mean(train.data$pm2.5))**2)
R2_train_dewp = 1 - (RSS_train_dewp / TSS_train_dewp)
RSS_test_dewp <- sum(test.data$error_DEWP **2)
TSS_test_dewp <- sum((test.data$pm2.5 - mean(test.data$pm2.5))**2)
R2_test_dewp = 1 - (RSS_test_dewp / TSS_test_dewp)

paste("R2 for train data: :", round(R2_train_dewp,3))
paste("R2 for test data: :", round(R2_test_dewp,3))

#DEWP데이터를 구분할 때, Inf와 -Inf를 사용하지 않고 interval 간격만큼 구분하여 나누었을 때는 R2값이 NA와 음수값이 나타났다. 이렇게 하였을 때의 R2값은 train data와 test data가 다음과 같이 나타났다. 
#            train data | test data
#interval 1 : 0.131 | NA
#interval 2 : 0.127 | NA
#interval 3 : 0.123 | NA
#interval 5 : 0.115 | -0.144
#interval 10 : 0.095 | -0.105
#interval 12 : 0.088 | -0.057
#interval 30 : 0.015 | -0.089
#-------
#DEWP데이터를 Inf와 -Inf를  설정하고, 또한 interval 간격만큼 구분하여 나누었을 때의 R2의 결과이다.  test dataset에서는 train dataset에 없는 DEWP 값이 존재하였기 때문에 Inf와 -Inf를 사용하였다.
#interval 1 : 0.131 | 0.108
#interval 2 : 0.127 | 0.106
#interval 3 : 0.123 | 0.107
#interval 5 : 0.115 | 0.098
#interval 10 : 0.095 | 0.084
#interval 12 : 0.088 | 0.07
#interval 30 : 0.015 | -0.013

#interval을 3으로 설정하였을 때 test data set에서 R^2값이 2로 설정하였을 때 보다 높게 나왔기 때문에 3으로 설정하는 것이 더욱 효과적일것이라 예측하였다. 또한, test dataset에서는 train dataset에 없는 DEWP 값이 존재하였기 때문에, 이러한 온도값들을 해결하기 위해 -Inf와 Inf로 해결하고자 하였다.
#너무 세분화된 간격(예: interval 1)은 데이터의 노이즈를 증가시키고, 너무 큰 간격(예: interval 30)은 중요한 패턴을 놓칠 수 있으므로, interval 3은 이러한 극단을 피하고 균형을 맞추기에 적합합니다.
#Q5 
#R^2값을 바탕으로 예측 모델의 성능을 비교하였을 때 이슬점(DEWP)을 변수로 둔 모델이 가장 예측 성능이 뛰어났다. hour를 변수와 미세먼지 농도의 막대그래프를 생성해보았을 때, 그래프가 특정한 경향을 보이지않는것을 확인할 수 있었다. 이에 따라 시간에 따라 미세먼지 농도를 예측하는 것은 다소 어려움이 있을것이라 판단하였다. 또한 각 시간대에 미세먼지농도가 너무 넓게 분포되어있어 더욱 어려울 것이라 판단하였다.
#월(Month)와 같은 경우에는 이상치가 너무 많을 뿐더러, 동일하게 각 월마다 미세먼지 농도가 너무 넓게 분포되어있어 예측이 어려울 것이라 생각한다.
plot(train.data$hour,train.data$pm2.5)
plot(train.data$month,train.data$pm2.5)
