##############################Homework2_8조############################
### 과목명 / 분반:  데이터 과학 / 01
### 작성자: 8조
### 21900072 김기연, 22100511 이선환, 22100731 최서영, 22200333박하린

# 데이터 불러오기
load(url("https://github.com/hbchoi/SampleData/raw/master/country.RData"))


#Q1
#head의 기본 출력은 6개이므로 5로 설정하여 상위 5개만 뽑음
head(country, 5) 
#tail의 기본 출력은 6개이므로 5로 설정하여 하위 5개만 뽑음
tail(country, 5) 

#Q2
#각 변수들의 데이터 타입 확인 과정
sapply(country, class)

#unique 함수는 vector 에서 중복되지 않는 항목들을 출력해주며, 이를 length를 통해 중복되지 않는 항목들(level)의 개수를 확인 할 수 있었고, 각 열에서 이를 확인하기 위해 vector operation인 sapply를 사용하였다.
sapply(country,function(x) {length(unique(x))}) 

#위에서 continent가 6개 이하의 level로 이루어진 것을 확인하였기에, factor화 하였다
country$continent<-factor(country$continent) 

#각 vector의 타입을 class를 이용하여 확인하고자 하였고, 이를 vector operation인 sapply를 이용하여 실행하였다
sapply(country,class)

#Q3
# 변수 요약값 출력
summary(country$continent)

#Q4
# continent 변수의 levels 바꾸기
levels(country$continent) <- c("AC", "AS", "EU", "NA", "OC", "SA")

#변수 요약값 출력
summary(country$continent)

#Q5
# 국가의 GDP가 평균 GDP보다 작으면 'LOW', 평균 이상이면 'HIGH'인 GDP_group 열 생성
country$GDP_group <- ifelse(country$GDP < mean(country$GDP), "LOW", "HIGH")

# 생성된 GDP_group 열의 요약값 출력
table(country$GDP_group)


#Q6
#Find_continent라는 함수 정의
#continent열의 값이 입력값과 같은지 판단한 결과를 리턴 (return 생략)
Find_continent <- function(n){ 
  country$continent == n 
}
#테스트
Find_continent("AS")
Find_continent("EU")

