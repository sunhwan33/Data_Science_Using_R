#############################Homework5_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333 박하린

# loading data into R
weather_df <- readRDS("weather.rds")
str(weather_df)
summary(weather_df)

# Q1 Weather_df가 tidy가 아닌 이유
# 각 변수는 하나의 열에 있어야 한다.
#그러나 Weather_df에서는 날짜별 측정값(예: 온도, 습도 수준)이 여러 열(X1, X2, ..., X31)에 분산되어 있다.
#변수에 대한 각각의 다른 관측값은 다른 행에 있어야 한다. Weather_df에서 관측치는 변수와 일일 관측치를 혼합하여 여러 열로 분할되어 있다.

# Q2 불필요한 column을 제거
weather_df <- weather_df[, -1]
# weather_df 데이터 프레임의 x column은 단순 순서만을 보여주는 의미 없는 열이기에 불필요하다고 판단하여 제거하였다.

# Q3 Dataset을 tidyr라이브러리를 사용하여 tidy한 형태로 변환하기.
library(tidyr)

# gather 함수를 사용하여 wide 형식의 데이터를 long 형식으로 변환해주었다.
# values ‘X1’부터 ‘X31’까지 각 열에 저장된 값들이 모일 새로운 열의 이름이다.
gather_weather <- gather(weather_df, dayOfMonth, values, X1:X31)

#spread 함수를 사용하여 long 형식의 데이터를 다시 wide 형식으로 변환해주었다.
weather_tidy <- spread(gather_weather, measure, values)

head(weather_tidy, 10)
str(weather_tidy)

# Q4 dayOfMonth 변수를 수치형 변수로 변환하기
library(stringr)
# X를 포함하고 있으면 빈 공백으로 바꿔주기
weather_tidy$dayOfMonth <- str_replace(weather_tidy$dayOfMonth, "X", "")
weather_tidy$dayOfMonth <- as.numeric(weather_tidy$dayOfMonth)
head(weather_tidy, 10)
str(weather_tidy)

# Q5  데이터에 year month dayOfMonth 세 column이 있는데 이를 하나로 합쳐서 date column을 추가하기(“-”으로 연결)
library(lubridate)
weather_tidy <- unite(weather_tidy,date,year,month,dayOfMonth,sep="-")
weather_tidy$date <- ymd(weather_tidy$date) # Warning message: 7 failed to parse.
#weather_tidy$date <- as.Date(weather_tidy$date) 사용시 경고문이 나오지않음
#존재할 수 없는 날짜를 날짜값으로 변경할 시 ymd함수에서 경고문을 출력한다.
head(weather_tidy,10)


# Q6
weather_tidy$PrecipitationIn
weather_tidy$PrecipitationIn <- str_replace(weather_tidy$PrecipitationIn, "T", "0")
str(weather_tidy)

# Q7 각 변수의 data type을 적절한 것으로 변환하고 타당한 이유 적기
glimpse(weather_tidy)
# date는 날짜이기 때문에 Date로 그대로 두었다.
# # 각 변수의 데이터 타입이 character로 되어있다. Events의 경우, 특정 카테고리로 바꿀 수 있기에 Factor로 변환 하였다.
# character로 되어있는 데이터 타입은 숫자 연산이 불가하기에 이를 numeric으로 변환시켜준다.
weather_tidy$Events <- as.factor(weather_tidy$Events)
library(dplyr)
weather_tidy[,-c(1,3)] <- sapply(weather_tidy[, -c(1,3)], as.double)
glimpse(weather_tidy)


# Q8 데이터셋에 missing values 확인하기 
#총 결측치는 827개입니다.
sum(is.na(weather_tidy))
colSums(is.na(weather_tidy))
#날짜에는 존재할 수 없는 날짜(예: 2015.02.30)의 NA가 7개, 다른 열에는 37개의 NA(2015.05.18~2015-12-09)가 있으며, Max.Gust.SpeedMPH에는 데이터가 관측되지 않은 날짜의 NA가 6개 더 있어 총 43개입니다.
# 해결방법: 존재할 수 없는 날짜 row는 삭제 시켜주고, 날짜는 존재하지만 관측값들이 없는 row들은 그대로 둔다 -> 측정이 되지 않은 것이지 오류가 아니기 때문에

# Q9 Max.Humidity(최대 습도) 변수 확인하기.
#박스 plot을 사용하면 쉽게 이상치를 확인 할 수 있다.
boxplot(weather_tidy$Max.Humidity, horizontal = T)
summary(weather_tidy$Max.Humidity)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 39.00   73.25   86.00   85.69   93.00 1000.00      37 

# outlier 값이 실수로 0이 하나 더 붙어 나온 값이라고 합시다. 해당 outlier를 적절한 값으로 고치기
weather_tidy$Max.Humidity[weather_tidy$Max.Humidity==1000] <- 100
summary(weather_tidy$Max.Humidity)
boxplot(weather_tidy$Max.Humidity, horizontal = T)
# 약 400개 값의 분포는 최소값 39부터 최대값 100까지의 범위에서 86을 중간값으로 가진다는 것을 확인할 수 있다.

# Q10
#박스 plot을 사용하면 쉽게 이상치를 확인 할 수 있다.

boxplot(weather_tidy$Mean.VisibilityMiles, horizontal = T)
summary(weather_tidy$Mean.VisibilityMiles)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -1.000   8.000  10.000   8.861  10.000  10.000      37 
# 거리는 -1일 수가 없다. 거리가 -1이라면 현실적이지 않은 극단적인 값
weather_tidy$Mean.VisibilityMiles[weather_tidy$Mean.VisibilityMiles < 0] <- NA
# 만약 데이터를 0이나 다른 값으로 바꿔준다면, 이 값이 변수의 평균이나 중앙값과 같은 통계값에도 영향을 미칠 수 있기 때문에 NA값으로 바꿔주었다.
# summary로 확인해보았을 때, NA 값이 늘어남을 확인하였다.
summary(weather_tidy$Mean.VisibilityMiles)

# Q11
#level에 공백이 있음을 확인
#ifelse구문을 통하여 level이 공백인 경우 level을 “None”으로 변경
#weather_tidy$Events와 table(weather_tidy$Events)로 변경 잘 되었는지 확인
weather_tidy$Events 
levels(weather_tidy$Events) <- ifelse(levels(weather_tidy$Events) =="", "None", levels(weather_tidy$Events))
weather_tidy$Events
table(weather_tidy$Events)

# Q12
#tolower을 사용하여 모두 소문자로 변환
names(weather_tidy) <- tolower(names(weather_tidy))
names(weather_tidy)
