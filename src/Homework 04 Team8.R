#############################Homework4_8조###############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333박하린

# Loading data into R
GDP <- read.csv("GDP.csv")
POP <- read.csv("population.csv")
LIFE_EXP <- read.csv("Life Expectancy.csv")

str(GDP)
str(POP)
str(LIFE_EXP)

#1
# GDP와 인구수 데이터프레임의 열의 이름 바꾸기
colnames(GDP) <- c("Country", "GDP")
colnames(POP) <- c("Country", "POP")
str(GDP)
str(POP)

#2.
#GDP와 POP를 merge를 사용하여 상응되는 값만 데이터프레임 형성 
GP <- merge(x = GDP, y = POP, by='Country')
head(GP)

#3.
#GDP_POP과 Life_exp를  merge를 사용하여 상응되는 값만 데이터프레임 형성 
GPL <- merge(x=GP, y= LIFE_EXP, by = 'Country')
head(GPL)

#4-1.
# subset - GDP가 대한민국보다 높은 나라들만 추출하기
Kor_G <- GPL[GPL$Country == "South Korea", "GDP"] # GPL에서 South Korea의 GDP 값을 Kor_G에 할당해주기
G_hk <- subset(GPL, GPL$GDP > Kor_G) # GDP가 South Korea보다 높은 나라들 추출해서 G_hk에 할당해주기
head(G_hk[, "Country"]) # 국가명만 출력해주기

#.4-2
# GDP가 대한민국보다 높으면서 인구가 대한민국보다 적은 국가 찾기
Kor_P <- GPL[GPL$Country == "South Korea", "POP"] # GPL에서 South Korea의 POP 값을 Kor_P에 할당해주기
GP_k <- subset(G_hk, GPL$POP < Kor_P) #4-1에서 GDP가 South Korea보다 높은 나라들만 추출했기 때문에 조건 하나만 추가하여서 결과 도출
head(GP_k[, c("Country", "GDP", "POP")]) # 국가명, GDP, POP이 보이도록 출력하기

#4-3
# 기존 데이터프레임에 Country_GDP(=GDP * POP / 1000)(단위 100USD)라는 변수를 새롭게 추가하고 Country_GDP가 미국보다는 낮고, 대한민국보다는 높은 국가들 출력하기
GPL$Country_GDP <- GPL$GDP*(GPL$POP/1000)
K_CG <- GPL[GPL$Country =="South Korea", "Country_GDP"] # 한국의 Country_GDP값 K_CG에 할당해주기 
#k_CG <- GPL$Country_GDP[which(GPL$Country =="South Korea")]
U_CG <- GPL[GPL$Country =="United States", "Country_GDP"] # 미국의 Country_GDP값 U_CG에 할당해주기
# 조건에 맞게 찾기
subset(GPL, GPL$Country_GDP > K_CG & GPL$Country_GDP < U_CG)


#5-1
# sample 함수 사용해서 임의로 20개의 국가 추출하기
# 추출한 국가들의 GDP, POP, Life_exp의 평균 계산하기
options(scipen = 999) # 값이 커서 지수로 표현되는 것을 숫자로 표현하기 위해 options(scipen = 999) 사용
sam_idx <- sample(nrow(GPL), 20)
GPL20 <- GPL[sample(1:nrow(GPL), 20), ]
GPL20 <- GPL[sam_idx,]
sapply(GPL20[, -1], mean) #same as colMeans(GPL20[, -1])

#5-2
# 10번 반복하고 그 결과 새로운 matrix로 저장하기
options(digits = 4) #자릿수 맞추기
mat <- NULL #새로운 matrix(“mat”)을 NULL로 지정
for(i in 1:10) { #10번 반복하는 동안 평균값 결과들을 mat에 추가
  mat <- rbind(mat, sapply(GPL[sample(1:nrow(GPL), 20),][, -1], mean))
}
mat

#5-3
options(digits = 5)#자릿수 맞추기
# random으로 추출한 평균들의 값, mat의 열들의 평균(1은 행, 2는 열을 의미)
#colMeans(mat)
apply(mat, 2, mean) 

# 전체값들의 평균
options(digits = 2) #자릿수 맞추기
sapply(GPL[, -1], mean) 
# 1회 추출한 값보다 10회 추출한 값이 전체 값들의 평균에 더 가까워진 것을 볼 수 있다. -> 추출 횟수가 증가할 수록 전체 값들의 평균에 가까워진다고 예상된다. 

#5-4
set.seed(2024) 

#5-2번 코드 반복
options(digits = 5)#자릿수 맞추기 
mat <- NULL 
for(i in 1:10) {
  mat <- rbind(mat, sapply(GPL[sample(1:nrow(GPL), 20),][, -1], mean))
}

options(digits = 5)#자릿수 맞추기 
apply(mat, 2, mean)
options(digits = 2)#자릿수 맞추기 
sapply(GPL[, -1], mean)

#각 컴퓨터에서는 해당 seed로 랜덤으로 선택할 때는 계속해서 같은 결과가 나왔지만, 서로의 컴퓨터에서는 다른 결과를 확인할 수 있었다. 이는 같은 시드값이여도 각 컴퓨터에서 랜덤 숫자를 생성하는 알고리즘이 다르기 때문에 이러한 결과를 확인할 수 있었다.
#set.seed 함수는 R 언어에서 난수 생성의 초기값(시드)을 설정하는 데 사용됩니다. 이 함수는 난수 생성 과정을 재현가능하게 만들어 연구와 분석에서 결과의 일관성을 보장하는 역할을 합니다. set.seed는 난수 생성기에 특정 숫자를 전달함으로써, 그 후에 실행되는 모든 난수 관련 함수가 동일한 난수 시퀀스를 생성하도록 합니다. 이는 코드의 결과를 다른 시스템에서도 동일하게 재현할 수 있게 하여, 분석의 신뢰성과 검증 가능성을 향상시킵니다.
#6.
val <- round(GPL$Country_GDP / 1000000, digits = 2) #나눗셈의 몫을 소수점 2자리에서 반올림
GPL$Country_GDP <- paste0(val, "B") #윗 결과에 ‘B’ 붙이기
head(GPL[, c("Country", "Country_GDP")], 10)

#7
#mean을 사용하여 평균값을 구하였고, which를 사용하여 평균보다 높은 국가의 Index 도출하기 
GDP_avg <- mean(GPL$GDP)
GDP_index <- which(GPL$GDP > GDP_avg)

POP_avg <- mean(GPL$POP)
POP_index <- which(GPL$POP > POP_avg)

LIFE_EXP_avg <- mean(GPL$Life_exp)
LIFE_EXP_index <- which(GPL$Life_exp > LIFE_EXP_avg)

inter <- intersect(intersect(GDP_index, POP_index), LIFE_EXP_index)
GPL$Country[inter]

# 여기서 주의할 점: intersect 함수의 인자에는 한 번에 두 개 까지밖에 사용하지 못한다. 따라서 세 개의 인자를 사용해야 할 때는 intersect 함수를 두 번 사용해서 해결해야 한다.

#8
# GDP 데이터의 4분위 수 계산
options(digits = 5)
GDP_quantiles <- quantile(GPL$GDP, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# GDP_group 변수 생성
#breaks : 데이터를 나눌 경계값 정의 
#include.lowest= TRUE : 가장 낮은 GDP 값을 가진 국가도 포함 
GPL$GDP_group <- cut(GPL$GDP,
                     breaks = GDP_quantiles,
                     labels = c("Very Low", "Low", "High", "Very High"),
                     include.lowest = TRUE)

# 각 그룹에 속하는 국가 수 확인
GDP_group_counts <- table(GPL$GDP_group)

GDP_quantiles
GDP_group_counts

#9
options(digits = 7)
aggregate(cbind(POP, Life_exp) ~ GDP_group, data = GPL, mean)
# 빈곤국일수록 많은 인구수(POP)을 가질 것으로 예상했지만 그렇지 않아서 두 요소간 상관관계는 없음을 알 수 있었다.
#  “Very Low” 그룹과 “Very High” 그룹에서는 인구수가 경제수준이나 기대수명에 어느 정도 유의미한 상관성을 보인다고 할 수 있겠지만, 경제 수준이 중간인 “Low”와 “High” 그룹에서는 인구수가 영향을 미치지 않는 것 같다는 생각이 들었다. 
#또한, 인구수의 증가와 기대수명은 큰 연관성이 없음을 보였는데 GDP와 기대수명은 점차 증가하는 경향을 보였지만, 인구수는 선형적으로 증가하는 바를 보이지 않기 때문에 연관성이 없다고 할 수 있을 것 같다.
