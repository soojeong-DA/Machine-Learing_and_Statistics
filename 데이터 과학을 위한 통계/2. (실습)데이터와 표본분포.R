# 개별 데이터 포인트의 분포인 '데이터 분포'와 표본통계량의 분포인 '표본분포' 구별
# 평균과 같은 표본통계량의 분포는 데이터 자체의 분포보다 규직적이고 종 모양일 가능성이 높다.
# ex. 3개의 표본 비교를 통해 알아보자!
# install.packages('ggplot2')
library(ggplot2)

loans_income <- read.csv('data/loans_income.csv', header = TRUE)
# 1. 단순히 1,000개 값으로 이루어진 표본(단순랜덤표본)
samp_data <- data.frame(income=sample(loans_income, 1000), type='data_dist')

# 2. 5개 값의 평균 1,000개로 이뤄진 표본


# 3. 20개 값의 평균 1,000개로 이뤄진 표본
