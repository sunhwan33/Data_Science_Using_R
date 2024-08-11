#22100511 이선환

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#Q1
weather <- readRDS('weather.rds')
str(weather)
head(weather, 10)
summary(weather)
# 위 데이터는 Weather_df에서는 날짜별 측정값(예: 온도, 습도 수준)고과 날짜별 션수가 드러나있다.
#그러므로 각 연도, 월, 일별로 각 측정값이 어떠한지 볼 수 있는 데이터이다.
#이를 확인하ㅣㄱ 위해 str()과 head(), summary()를 사용해서 구조, 행과 열 갯수, 그리고 각 데이터의 배치구성을 확인하였따.

#Q2
weather <- weather[-1]
#각 변수는 하나의 열에 있어야 한다.
#그러나 Weather_df에서는 날짜별 측정값(예: 온도, 습도 수준)이 여러 열(X1, X2, ..., X31)에 분산되어 있다.
#변수에 대한 각각의 다른 관측값은 다른 행에 있어야 한다. Weather_df에서 관측치는 변수와 일일 관측치를 혼합하여 여러 열로 분할되어 있다.
#그러므로 이는 tidy 데이터가 아니라고 볼 수 있다.
#또한 weather 데이터 프레임의 x column은 단순 순서만을 보여주는 의미 없는 열이기에 불필요하다고 판단하여 제거하였다.

#Q3
gather_weather <- gather(weather, dayOfMonth, values, X1:X31)
weather_tidy <- spread(gather_weather, measure, values)
head(weather_tidy, 10)
str(weather_tidy)
# gather 함수를 사용하여 wide 형식의 데이터를 long 형식으로 변환해주었다.
# values ‘X1’부터 ‘X31’까지 각 열에 저장된 값들이 모일 새로운 열의 이름이다.
#spread 함수를 사용하여 long 형식의 데이터를 다시 wide 형식으로 변환해주었다.

#Q4
bmi_clean <- read.csv("bmi_clean.csv")
head(bmi_clean,10)
#tidy하지 않다. 왜냐하면 나라별로의 bmi 평균값이 여러 열(Y1980~Y2008)에 분산되어있다.
#변수에 대한 각각의 다른 관측값은 다른 행에 있어야 한다. bmi_clean에서 관측치는 변수와 연도별 데이터가 혼합하여 여러 열로 분할되어있다.
#그러므로 이는 tidy 데이터가 아니라고 볼 수 있다.
#국가명의 variable name으로 적합하므로 이를 column으로, 그리고 연도별 데이터를 그 column에 해당하는 데이터로 적합하므로 연도를 row에 있도록 변환해야한다
bmi_clean <- bmi_clean [-1]
#x column은 단순 순서만을 보여주는 의미 없는 열이기에 불필요하다고 판단하여 제거하였다.
bmi_clean <- gather(bmi_clean, year, bmi_val, -Country)
head(bmi_clean,10)
# gather 함수를 사용하여 wide 형식의 데이터를 long 형식으로 변환해주었다. 
#국가명의 variable name으로 적합하므로 이를 column으로, 그리고 연도별 데이터를 그 column에 해당하는 데이터로 적합하므로 연도를 row에 있도록 변환하였다.


#Q5
students2 <- read.csv('students2.csv', header=T, stringsAsFactors = F )
str(students2)
#Q6
students2$dob <- ymd(students2$dob)
students2$nurse_visit <- ymd_hms(students2$nurse_visit)
#Q7
str(students2)
#dob 행의 데이터가 Date 형으로 변환되었다. "2000-06-05", "1999-11-25"  형태로 변환되었다.
#nurse_visit 행의 데이터가 연-월-일-시-분-초 를 나타내는 형태로 변화되었다. "2014-04-10 14:59:54" "2015-09-21 14:59:54"

#Q8
winequal <- read.csv('winequality-red.csv', sep=';')
head(winequal)
#Q9
str(winequal)
#1599개의 observation(관측지), 12개의 variable(변수)가 있음을 확인하였다.

#Q10
winequal$total.acidity <- winequal$volatile.acidity + winequal$fixed.acidity
head(winequal)

#Q11
summary(winequal)
colSums(is.na(winequal))
#결측치 없다

#Q12
hist(winequal$quality)
#Q13
summary(winequal$quality)
plot(winequal$quality)
boxplot(winequal$quality)
#summary, plot, boxplot을 통해 확인해본 결과, 최솟값, 최댓값, 평균, 중간값 등 전체적으로 큰 차이가 없는 것으로 보아, outlier는 없다고 판단하였다.

#Q14
winequal[order(winequal$quality, decreasing=T), ]
#Q15
set.seed(2020)
sam_idx <- sample(nrow(winequal), 30)
wine30 <- winequal[sample(1:nrow(winequal), 30),]
str(wine30)
mean(wine30$quality)
mean(wine30$density)
mean(wine30$pH)
mean(wine30$alcohol)

#Q16

plot(winequal$pH, winequal$alcohol)
plot(wine30$pH,wine30$alcohol )
#sample데이터가 아닌, original데이터의 경우 선형적으로분산되어 있는 정도가 많이 겹쳐보인다.  즉 ph값이 늘어날 수록, alcohol의 값도 증가한다. 이는 선형적인 관계를 보여준다고 할 수 있을 것 같다.
#그러나 sample로 추려낸 데이터의 경우, original 데이터를 분석한 것처럼 선형적인 관계를 보여준다고 하기에는 어렵다.

#Q17
load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))
set.seed(2020)
n_sample <- nrow(adult)
rgroup <- runif(n_sample) #각 observation을 섞기

adult.train <- subset(adult, rgroup <=0.7)
adult.test <- subset(adult, rgroup > 0.7)

dim(adult.train)
dim(adult.test)

#Q18
table(adult.train$income_mt_50k) #-> true인 애들의 수가 수입이 50k 넘은 것
prop.table(table(adult.train$income_mt_50k)) #proportion 비율 계산
prop.table(table(adult.test$income_mt_50k))

tble <- table(adult.train$education, adult.train$income_mt_50k)
tble
prop.table(tble, margin=1) #1 -> row별로

sv_model_edu <- prop.table(tble, margin=1)[,2]
sort(sv_model_edu, decreasing=T)

adult.train$est_prob <- sv_model_edu[adult.train$education]
head(adult.train[, c('education', 'est_prob', 'income_mt_50k')], 10)

threshold <- 0.4
adult.train$prediction <- adult.train$est_prob > threshold
head(adult.train[, c('education', 'est_prob','prediction', 'income_mt_50k')], 10)

conf.table <- table(pred = adult.train$prediction, actual=adult.train$income_mt_50k)
conf.table

train_accuracy <- sum(diag(conf.table)) / sum(conf.table)
train_accuracy

#test data
adult.test$est_prob <- sv_model_edu[adult.test$education]
adult.test$prediction <- adult.test$est_prob > threshold
head(adult.test[, c('education', 'est_prob','prediction', 'income_mt_50k')], 10)
conf.table_test <- table(pred = adult.test$prediction, actual = adult.test$income_mt_50k)
conf.table_test

test_accuracy <- sum(diag(conf.table_test)) / sum(conf.table_test)
test_accuracy


paste(("accuracy for train data"), train_accuracy)
paste(("accuracy for test data"), test_accuracy)
#Q19
#베이즈모델은 조건부 확률을 구하는 것이다.
#나이브 베이즈 모델은 특히 대용량 데이터, 혹은 여러 독립변수를 사용해야 할때 사용하기에 적합하다. 또한 categorical한 범주형 자료에서 적합하다
#그러나 정확한 베이즈 모델은 여러 독립변수를 사용할 때에는 부적합하다. 
#데이터마다의 상황에 따라 나이브 베이즈 모델을 사요할지, 일반적인 베이즈 모델을 사용할지를 결정한다.

#Q20
#single variable은 단일 변수에 따른 관계성을 판단하는 것이다. 또한 multiple variable은 단일 변수가 아닌 여러 변수를 통한 관계성을 판단하고자 하는 모델이다
#그리하여 단 하나의 변수에 의한 영향을 보고 싶을 때는 single variable 모델을, 여러 변수에 의한 영향을 보기 위해서는 multiple variable 모델을 사용하는 것이 일반적이다
#그리고 classification은 분류하는 것으로, 이산적인 상황을 구분하기 위함으로 볼 수 있다. 예를들어 이메일이 스팸메일인지 아닌지 등 이분법적으로 나뉘는 사건들에 대하여 판단하고 분석하고자 할때는 classification 모델을 사용해야 한다.
#하지만 regression 모델은 선형적인 데이터, 즉 연속적인 데이터를 판단하고자 할 때 사용한다. 예를들어 그 사람의 사람의 학점(GPA)는 연속적인 데이터라고 볼 수 있다. 이러한 연속적인 사건들에 대하여 분석하고자 할 때는 regression모델이 적합하다고 할 수 있다.

#Q21
#train데이터와 test데이터를 나누는 이유는 학습데이터의 효과성과 실제 테스트 데이터의 효과성을 확인하고 싶은 것이다.
#train데이터는 학습데이터라 할 수 있는데, 일반적으로 학습을 잘 했다고 해서 그 분류방법이 정말 효과적이라 할 수는 없기 때문이다.
#각각의 정확도를 판단하며, 학습데이터로 학습된 model을 실제 test data에도 적용을 하였을 때 정확성이 얼마나 나오는지를 봐야하는 것이다.
#예를들어 Q18번 문제에서도 sv_model_edu라는 모델을 train_data 즉 학습데이터를 통해 만들었다. 그리고 test_data의 정확성을 판단하고자 할 때도, 동일한 sv_model_edu라는 모델을 사용하였다.
#이는 학습데이터를 통해 학습된 것이 실제 test 데이터에도 얼마나 효과가 있는지를 확인할 수 있다.
#만일 train데이터에서는 정확도가 높았는데, test데이터에서는 정확도가 더 낮았으면, 이는 그 독립션수를 통한 데이터분류가 효과가 없음을 의미한다.

#Q22
#accuracy를 높이기 위해서는 임계값을 적절히 조절하는 방법이 있다. 
#또한 독립변수를 적절한 것으로 교체하는 것도 효과가 있을 수 있다.

#Q23
#overfitting은 과대적합을 의미하여, 학습데이터를 너무 잘 제대로 숙지하였을 때에 발생하는 문제이다.
#예를들어 한 학생이 학습지를 열심히 숙지하였다고 하자. 시험이 학습지와 비슷한 문제로 출제가 된다면, 그 학생은 높은 점수를 맞을 수 있을 것이다. 그러나 만일 시험이 많이 변형이 되어 출제가 된다면, 그 학생은 해당 학습지만을 열심히 숙지한 것만으로는 좋은 성적을 낼 수 없을 것이다.
#이것이 바로 overfitting이다. 학습데이터를 제대로 학습하였다는 것은 오히려 실제 test데이터가 다르게 들어오면 쉽게 적용할 수 없음을 의미한다.
#그래서 학습데이터에 대한 정확도가 엄청 높은 것이 꼭 좋은 것만을 의미한다고 할 수는 없을 것이다.