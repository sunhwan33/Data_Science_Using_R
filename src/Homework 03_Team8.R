load(url('https://github.com/hbchoi/SampleData/raw/master/country.RData'))
str(country)


##Q1
#sapply 함수를 이용하여 모든 열에 tip 으로 주어진 함수를 적용하였고, 이 결과에서 TRUE가 나온 값들을 가지는 열의 이름값만 출력할 수 있도록 하였다.
numCol <- sapply(country, function(x) {typeof(x) %in% c("integer", "double")})
result <- names(country[numCol])
result

##Q2
#desc() 함수를 사용하기 위해 dplyr 라이브러리를 설치하였고, 이를 이용하여 Q1에서 구한 결과를 이용하여 모든 열을 정렬하였다.
library(dplyr) #library 불러왔다.
lapply(country[numCol], function(x) (rank(desc(x))))
country[numCol] <- lapply(country[numCol], function(x) (rank(desc(x))))
head(country,10)

##Q3
# South Korea 행만 찾아서 가져왔다.
country[country$country_name=="South Korea",].  # which(country$country_name=="South Korea")
# south_korea는 GDP, 평균 수명, 인구, 이산화탄소 배출량에 비해 상대적으로 높은 순위를 가지고 있다. -> 경제적으로 발전된 국가임을 알 수 있다.
# 전투 사망자 수 순위는 다소 높고(전쟁 등의 이유), 여성 1명당 아이의 수는 낮은 편이다.(저출산)

##Q4
# 각 국가별로 지표들의 순위의 평균값을 계산하였다.
# country에서 지표들의 순위 평균값 계산 후 avg_rank 열 추가하였다.
country$avg_rank <- rowMeans(country[numCol])

# country에서 code, country_name, avg_rank열만 가져와서 rank_avg 변수에 저장해 주었다.
rank_avg <- country[, c('code', 'country_name', 'avg_rank')]
sort_rank <- rank_avg[order(rank_avg$avg_rank), ]

rownames(sort_rank) <- NULL  # 로우명 리셋해주기
head(sort_rank, 30)

##Q5
getwd() #현재 경로 확인
app <- read.csv('apps_delimiter.csv', sep = "^", stringsAsFactors = F, header = T)
app <- app[, -1] # 첫 columns 'x' 삭제하였다.
str(app)

#sapply를 통해 unique한 값들의 개수를 확인한 후 분류하기 적당한 변수들을 table을 통해 변수들의 분포를 확인해보았다.
sapply(app, function(x) {length(unique(x))}) 

#table(app$Category)
#table(app$Installs)
#table(app$Type)
#table(app$Content.Rating)
#table(app$Genres)
#table(app$Price)
#table(app$Rating)
#table(app$Android.Ver)'''

lapply(subset(app, select=c('Category', 'Installs', 'Type', 'Content.Rating', 'Genres', 'Price', 'Rating', 'Android.Ver')), table)
# factor로 바꿔줄 변수들 저장하였다.
to_Factor <- c('Category', 'Installs', 'Type', 'Content.Rating', 'Genres')
app[,to_Factor] <- lapply(app[ , to_Factor] , factor)
str(app)

# 확실한 분류 기준이 있는지, 유의미한 분류가 가능한 것인지, 수를 가진 데이터는 해당 숫자가 숫자 자체로 의미를 지니는지 혹은 다른 의미를 표현하기위한 수단인지를 바탕으로 factor로 바꿔줄 변수를 정해보았다. 
# Category, Installs, Type, Content.Rating, Genres를 factor로 바꿔주기
# Category, Type, Genres 는 앱을 분류해주는 것이기 때문에 하나의 범주로 볼 수 있으므로 factor로 바꾸어 주었다.
# Installs, Content.Rating 도 범위로 나눠져있어서 유의미한 분류가 가능하다고 생각하여 factor로 바꾸어 주었다.
# 세 가지 변수들(Price, Rating, Android.Ver)도 factor로 고려해보았지만 Price를 분류했을 때 유의미한 의미가 없다고 생각하였으며, Rating과 Android.Ver도 나누었을 때 특정한 한 값으로 편향되어 있으며, 다른 값들에는 소수의 수치만 분포되어 있기 때문에, 이에 따라 의미가 있는 분류는 아니라고 판단하여 factor로 바꾸지 않고 그대로 두었다. 

##Q6
#aggregate 함수를 사용하여 장르 별로 평균 Rating 구하였다.
head(aggregate(Rating~Genres, data=app, FUN = mean))

##Q7
#Reviews의 갯수를 기준으로 내림차순으로 정렬하였고, 예시사진만큼 출력을 위하여 head함수를 사용하였다.
head(app[order(app$Reviews, decreasing = T), ])
