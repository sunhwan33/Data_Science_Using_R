############################Homework9_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린

bike_rental_df <- read.csv("SeoulBikeData-1.csv")
str(bike_rental_df)

#1-1
# 학습 데이터와 검증데이터로 분할
train_bike <- bike_rental_df[as.Date(bike_rental_df$Date, format = "%d/%m/%Y") <= "2018-09-30", ]
dim(train_bike)
test_bike <- bike_rental_df[as.Date(bike_rental_df$Date, format = "%d/%m/%Y") >= "2018-10-01", ]
dim(test_bike)

#1-2
# 학습 데이터와 검증 데이터에 대해 각 변수들의 분포와 범위 비교하기
colnames(bike_rental_df)

# Rented.Bike.Count
summary(train_bike$Rented.Bike.Count)
summary(test_bike$Rented.Bike.Count)
boxplot(train_bike$Rented.Bike.Count, test_bike$Rented.Bike.Count,
        names = c("Train set","Test set"), ylab = "Rented.Bike.Count")
plot(density(train_bike$Rented.Bike.Count), col = "blue", 
     main = "Rented.Bike.Count Density Plot Comparison", xlab = "Rented.Bike.Count") 
lines(density(test_bike$Rented.Bike.Count), col = "red")


# Hour
summary(train_bike$Hour)
summary(test_bike$Hour)
boxplot(train_bike$Hour, test_bike$Hour,
        names = c("Train set","Test set"), ylab = "Hour")
plot(density(train_bike$Hour), col = "blue", 
     main = "Hour Density Plot Comparison", xlab = "Hour")
lines(density(test_bike$Hour), col = "red")


# Temperature
summary(train_bike$Temperature)
summary(test_bike$Temperature)
boxplot(train_bike$Temperature, test_bike$Temperature,
        names = c("Train set","Test set"), ylab = "Temperature")
plot(density(train_bike$Temperature), col = "blue", 
     main = "Temperature Density Plot Comparison", xlab = "Temperature")
lines(density(test_bike$Temperature), col = "red")


# Humidity
summary(train_bike$Humidity)
summary(test_bike$Humidity)
boxplot(train_bike$Humidity, test_bike$Humidity,
        names = c("Train set","Test set"), ylab = "Humidity")
plot(density(train_bike$Humidity), col = "blue", 
     main = "Humidity Density Plot Comparison", xlab = "Humidity")
lines(density(test_bike$Humidity), col = "red")


# Wind.speed
summary(train_bike$Wind.speed)
summary(test_bike$Wind.speed)
boxplot(train_bike$Wind.speed, test_bike$Wind.speed,
        names = c("Train set","Test set"), ylab = "Wind.speed")
plot(density(train_bike$Wind.speed), col = "blue", 
     main = "Wind.speed Density Plot Comparison", xlab = "Wind.speed")
lines(density(test_bike$Wind.speed), col = "red")


# Visibility
summary(train_bike$Visibility)
summary(test_bike$Visibility)
boxplot(train_bike$Visibility, test_bike$Visibility,
        names = c("Train set","Test set"), ylab = "Visibility")
plot(density(train_bike$Visibility), col = "blue", 
     main = "Visibility Density Plot Comparison", xlab = "Visibility")
lines(density(test_bike$Visibility), col = "red")


# Dew.point.temperature
summary(train_bike$Dew.point.temperature)
summary(test_bike$Dew.point.temperature)
boxplot(train_bike$Dew.point.temperature, test_bike$Dew.point.temperature,
        names = c("Train set","Test set"), ylab = "Dew.point.temperature")
plot(density(train_bike$Dew.point.temperature), col = "blue", 
     main = "Dew.point.temperature Density Plot Comparison", xlab = "Dew.point.temperature")
lines(density(test_bike$Dew.point.temperature), col = "red")


# Solar.Radiation
summary(train_bike$Solar.Radiation)
summary(test_bike$Solar.Radiation)
boxplot(train_bike$Solar.Radiation, test_bike$Solar.Radiation,
        names = c("Train set","Test set"), ylab = "Solar.Radiation")
plot(density(train_bike$Solar.Radiation), col = "blue", 
     main = "Solar.Radiation Density Plot Comparison", xlab = "Solar.Radiation")
lines(density(test_bike$Solar.Radiation), col = "red")


# Rainfall
summary(train_bike$Rainfall)
summary(test_bike$Rainfall)
boxplot(train_bike$Rainfall, test_bike$Rainfall,
        names = c("Train set","Test set"), ylab = "Rainfall")
plot(density(train_bike$Rainfall), col = "blue", 
     main = "Rainfall Density Plot Comparison", xlab = "Rainfall")
lines(density(test_bike$Rainfall), col = "red")


# Snowfall
summary(train_bike$Snowfall)
summary(test_bike$Snowfall)
boxplot(train_bike$Snowfall, test_bike$Snowfall,
        names = c("Train set","Test set"), ylab = "Snowfall")
plot(density(train_bike$Snowfall), col = "blue", 
     main = "Snowfall Density Plot Comparison", xlab = "Snowfall")
lines(density(test_bike$Snowfall), col = "red")

#유사한 그래프: , Hour, Humidity, Solar.Radiation, Rainfall -> train과 test 데이터들의 분포가 거의 비슷한 것을 확인할 수 있다.

#유사하지 않은 그래프 : Temperature(0~20도 사이는 test의 값이 많이 몰려있는 것을 확인), Dew.point.temperature(train과 test 데이터의 최대 최소 값 차이도 나는 것을 볼 수 있다.) 

#기타: Rented.Bike.Count(1000이하의 값에서 train의 값이 많이 몰려있는 것을 확인), Wind.speed (Test에 2 이하값이 더욱 많이 몰려있음을 확인, Train이 더욱 높은 값들이 많이 있음), Visibiity( Train에서 2000이상의 값이 더욱 많이 몰려있음을 확인), Snowfall(1이하의 값에서 train의 값이 test보다 몰려있음을 확인)

#1-3
#더미코딩 목적 : 
#KNN 알고리즘은 수치형 데이터를 사용하여 거리 계산을 수행합니다.
#범주형 데이터를 직접 사용할 수 없기 때문에, 이를 수치형 데이터로 변환하기 위해 더미 코딩을 사용합니다.
#더미 코딩을 통해 범주형 변수를 여러 개의 이진 변수로 변환하여, KNN 알고리즘이 적절히 거리 계산을 할 수 있도록 합니다.

train_bike$Holiday <- ifelse(train_bike$Holiday=='Holiday',1,0)
train_bike$Functioning.Day <- ifelse(train_bike$Functioning.Day=='Yes',1,0)

test_bike$Holiday <- ifelse(test_bike$Holiday=='Holiday',1,0)
test_bike$Functioning.Day <- ifelse(test_bike$Functioning.Day=='Yes',1,0)

##휴일, Functioning.Day : character 이기 때문에 dummy coding 으로 임시값 부여

train_bike_label <- train_bike$Rented.Bike.Count
test_bike_label <- test_bike$Rented.Bike.Count

colnames(train_bike)
train_bike<-train_bike[,3:14]
test_bike<-test_bike[,3:14]
##날짜 제거 > 날짜에 대한 수요보다 그 외의 여러 요인들이 더 클 것이라 예상
#종속변수 제거

train_bike<-train_bike[,-10]
test_bike<-test_bike[,-10]
# Seasons > 날짜와 민접한 관계가 있기 때문에 제거

summary(train_bike)
summary(test_bike)

minmax_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
train_bike_norm <- sapply(train_bike, minmax_norm)
test_bike_norm <- sapply(test_bike, minmax_norm)

#각 컬럼마다 범주가 다 다르기 때문에 모든 값을 normalize하였다.

# 2
library(class)
k_val <- sqrt(nrow(train_bike))
bike_test_pred <- knn(train = train_bike_norm, test = test_bike_norm, cl = train_bike_label, k = k_val)

error <- test_bike_label - as.numeric(bike_test_pred)
RMSE <- sqrt(mean(error ** 2))
RSS <- sum(error ** 2)
SStot <- sum((test_bike_label  - mean(test_bike_label ))^2)
SStot

Rsq <- (1-(RSS/SStot))
Rsq
#R^2값이 0.2 이하로 나온것으로 미뤄보아 k 값이 너무 높다는 가정을 할 수 있을 것 같다.

# 3
#k를 1부터 10까지 시도항, 가장 자전거 대여 수요를 잘 예측하는 k값 찾기
#가장 높은 값의 R_square값을 갖는 k값 찾기
library(class)

k_values <- 1:10
Rsq_values <- numeric(length(k_values))

for (i in k_values) {
  bike_test_pred <- knn(train = train_bike_norm, test = test_bike_norm, cl = train_bike_label, k = i)
  
  error <- test_bike_label - as.numeric(as.character(bike_test_pred))
  RSS <- sum(error^2)
  SStot <- sum((test_bike_label - mean(test_bike_label))^2)
  
  Rsq <- 1 - (RSS / SStot)
  Rsq_values[i] <- Rsq
}

plot(k_values, Rsq_values , type = "b",xlab="k",ylab="Rsq")
# K가 1부터 10일때, R^2가 점점 감소하는 것을 확인. 
# set seed를 안했기 때문에 k값은 매번 실행할 때마다 달라짐. 그렇지만 여러번 테스트 해보았을 때, 5이하일 때 최적의 값이라 판단하였다.


# 최적의 k 값 찾기
optimal_k <- k_values[which.max(Rsq_values)]
cat("Optimal k:", optimal_k, "\n")
cat("Optimal R²:", max(Rsq_values), "\n")
#4
# 최적의 k 값으로 최종 모델 적용
knn_predictions <- knn(train = train_bike_norm, test = test_bike_norm, cl = train_bike_label, k = optimal_k)
knn_predictions <- as.numeric(as.character(knn_predictions))

# 산점도 그리기
plot(knn_predictions, test_bike_label, xlab = "Predicted Bike Rentals", ylab = "Actual Bike Rentals",
     main = paste0("When k is optimal k : ",optimal_k))
abline(0, 1, col = "red", lwd = 2) #y=x선, lwd = 선의 두께(line width)

# 그래프 해석

# 선보다 위에 나타난 점들: 실제값이 예측값보다 높은 경우, 모델이 자전거 대여 수요를 과소평가한 경우
#선보다 아래에 나타난 점들: 실제값이 예측값보다 낮은 경우, 모델이 자전거 대여 수요를 과대평가한 경우
#선에 가깝거나 겹치는 점들: 예측값이 실제값과 매우 가까운 경우, 모델이 자전거 대여 수요를 정확하게 예측한 경우

"모델이 전반적으로 자전거 대여 수요를 예측하는 데 어려움을 겪고 있음을 알 수 있습니다.
많은 점들이 빨간색 선에서 벗어나 있어, 모델의 예측과 실제 수요 간에 차이가 있음을 보여줍니다.
특히 고수요 상황에서 과소평가(underestimation)하는 경향이 있으며, 저수요 상황에서 과대평가(overestimation)하는 경향이 보입니다.
모델을 개선하기 위해 추가적인 특성(예: 날씨, 요일 등)을 고려하거나, 모델의 복잡도를 조절하는 등의 접근이 필요할 수 있습니다.
"