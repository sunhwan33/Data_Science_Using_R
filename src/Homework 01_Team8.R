##1
#  각 영화이름들을 통하여 movie라는 벡터 생성
movie <- c("The Avengers", "Harry Potter" , "Home Alone" , "Toy Story", "Frozen", "The Notebook" , "Interstellar") 
movie
##2
# 본인 점수로 my_rating 벡터 생성
my_rating <- c(5, 4.8, NA, NA, 4, 3.8, 3.6)
my_rating
##3
# TA점수로 TA_rating 벡터 생성
TA_rating <- c(4.3, NA, 3.8, 3.0, 2.8, NA, 1.6)
TA_rating
##4
KY_rating <- c(2.8, 3.4, 5.0, 4.2, 1.6, 2.7, 4.3) #기연학우의 점수로 KY_rating 벡터 생성
SH_rating <- c(4.2, NA, 4.5, NA, 4.8, NA, NA) #선환학우의 점수로 SH_rating 벡터 생성
HR_rating <- c(2.5, 4.0, 5.0, 5.0, 3.0, 3.7, NA) #하린학우의 점수로 HR_rating 벡터 생성

#행 길이를 5로 설정하여 team_matrix 설정
team_matrix <- matrix(c(my_rating, TA_rating,KY_rating, SH_rating, HR_rating), byrow = TRUE, nrow = 5)
team_matrix

##5
# “na.rm = TRUE”를 통하여 결측값(NA)을 통계 분석 시 제외(미포함)
stu_mean <- rowMeans(team_matrix, na.rm = TRUE) 
stu_mean


##6
#“na.rm = TRUE”를 통하여 결측값(NA)을 제외하여 colSums()함수를 통하여 각 열의 합으로 movie_sum 벡터 생성
movie_sum <- colSums(team_matrix,na.rm=TRUE)
movie_sum

##7
#“cbind”를 통하여 학생 별로 부여한 영화 평점의 평균 (“stu_mean”)을 team_matrix 마지막 열에 추가
team_matrix <- cbind(team_matrix,stu_mean)
team_matrix



##8
#“rbind”를 통하여 각 영화 별로 받은 평점의 합(“movie_sum”)을 team_matrix 마지막 행에 추가
team_matrix <- rbind(team_matrix, movie_sum)
team_matrix

# 경고 메세지: Warning message:
#In rbind(team_matrix, movie_sum) : number of columns of result is not a multiple of vector length (arg 2)
# 6번 문제의 벡터 길이는 7이고, 문제 7번 team_matrix의 열의 수는 8이므로 서로 길이가 달라 경고 메시지가 뜨게 된다. 
#만일 문제 7번 team_matrix의 열의 길이가 7이었다면 경고 메시지는 나타나지 않았을 것이다.
# 그 결과 기존 movie_sum에서 존재하지 않았던 8번째 원소(각 학생의 평균 점수의 총합에 대한 정보)가 movie_sum 벡터의 첫 번째 원소 값이 자동으로 추가 되었다.


##9
# 마지막 행의 마지막 열의 값을 구하는 것이기 때문에, 곧 각 행과 열의 길이값을  nrow(), ncol()을 통해 구하였다. 이를 이용하여 마지막 값을 NA로 변경할 때에 사용하였다
team_matrix[nrow(team_matrix),ncol(team_matrix)] <- NA
team_matrix


##10
#각 이름들을 rownames와 colnames 함수를 이용하여 벡터의 이름으로 지정하였다 
rownames(team_matrix) <- c("SY", "TA","KY", "SH", "HR", "movie_sum")
colnames(team_matrix) <- c("Avengers", "H.P", "Home Alone", "T.S", "Frozen", "Notebook", "Interstellar", "stu_mean")
team_matrix

##11
# 관객들의 영화 평점만 centering을 적용하기 위해 마지막 행(movie_sum)과 마지막 열(stu_mean)을 제외하였다.
# 각 학생들의 평점 평균 벡터인 stu_mean을 전체벡터에서 빼줌으로서 centering을 실행하였다.
centering <- team_matrix[1:(nrow(team_matrix) - 1), 1:(ncol(team_matrix) - 1)] - stu_mean
centering

##12

#영화별 평균 평점만 출력하기 위해 마지막 행(movie_sum)과 마지막 열(stu_mean)을 제외하였다.
#위 벡터에서 영화 별 평균을 구하기 위하여 colMeans() 함수를 사용하였고, NA값들을 계산에서 제외하기 위해 na.rm=TRUE 옵션을 사용하였다.

# Centering 전의 영화별 평균
before_centering <- colMeans((team_matrix[1:(nrow(team_matrix) - 1), 1:(ncol(team_matrix) - 1)]), na.rm=TRUE)

# Centering 후의 영화별 평균
after_centering <- colMeans(centering , na.rm = TRUE)

before_centering
after_centering
# centering을 하기 전에는 각자의 성향이 포함된 영화  평점의 편향된 결과값이 나왔지만, centering을 한 후에는 각 학생이 영화에 부여한 평점에서 학생별 평가 경향성을 제거하기 때문에 평균에서 떨어진 정도의 값이 나온다. 이 값으로 평균을 내었기에 영화들 사이에서 상대적인 평가를 더 공정하게 비교할 수 있게 되었다.
