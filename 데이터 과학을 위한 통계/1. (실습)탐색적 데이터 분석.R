## *주별 인구와 살인 비율을 담고 있는 dataframe load
state <- read.csv(file='data/state.csv')
str(state)

# 1-1. 인구의 평균, 절사평균, 중앙값 계산
mean(state[['Population']])   #  mean(state$Population)
mean(state[['Population']], trim=0.1)   # trim=0.1: 각 끝에서 10%를 제외
median(state[['Population']])
# -> 이 경우, 평균이 절사평균보다 크고, 절사평균은 중간값보다 큼


# 1-2. 미국 전체의 평균적인 살인율 계산 <- 주마다 다른 인구 고려!(인구로 가중치 적용)
# 가중평균, 가중 중간값 계산 <- matrixStats packages
# install.packages('matrixStats')
library(matrixStats)
weighted.mean(state[['Murder.Rate']], w=state[['Population']])
weightedMedian(state[['Murder.Rate']], w=state[['Population']])
# -> 이 경우, 가중평균과 가중 중간값은 거의 비슷


# 1-3. 주별 인구의 변이(산포도) 추정
# 표준편차, 사분위범위(IQR), 중위절대편차(MAD) 계산
sd(state[['Population']])
IQR(state[['Population']])
mad(state[['Population']])


# 1-4. 백분위수(quantile 함수 이용) - 주별 살인율
quantile(state[['Murder.Rate']], p=c(.05, .25, .5, .75, .95))

# 1-5. 상자그림(boxplot) - 주별 인구 분포
boxplot(state[['Population']]/1000000, ylab='Poulation (millions)')

# 1-6. 도수분포표 - 주별 인구
breaks <- seq(from=min(state[['Population']]), to=max(state[['Population']]),
                length=11)
pop_freq <- cut(state[['Population']], breaks=breaks, 
                right=TRUE, include.lowest = TRUE)
table(pop_freq)

# 1-7. 히스토그램 - 주별 인구
hist(state[['Population']], breaks=breaks)

# 1-8. 밀도추정 - 주별 살인율
hist(state[['Murder.Rate']], freq=FALSE)   # freq=FALSE : 개수가 아닌, 비율을 표시
lines(density(state[['Murder.Rate']]), lwd=3, col='blue')


## *댈러스-포트워스 공항의 항공기 운행 지연 요인에 따른 퍼센트 비율 data
dfw <- read.csv('data/dfw_airline.csv')
head(dfw)

# 1-9. 막대도표 - 댈러스-포트워스 공항의 항공기 운행 지연 요인
barplot(as.matrix(dfw)/6, cex.axis = .5)  # inbound, ATC, carrier, weather, security 순으로 높게 나타남


## * 주요 상장 주식 펀드(ETF)들의 일간 수익 data
sp500_px <- read.csv('data/sp500_data.csv')
str(sp500_px)

sp500_sym <- read.csv('data/sp500_sectors.csv')

# 1-10. ETF 수익 간의 상관관계
# install.packages('corrplot')
library(corrplot)

etfs <- sp500_px[row.names(sp500_px) > '2012-07-01',
                 sp500_sym[sp500_sym$sector=='etf', 'symbol']]

corrplot(cor(etfs), method='ellipse')
#-> 타원 모양의 방향은 양/음의 상관관계를 나타내며, 타원의 너비와 색깔이 얇고 진할수록 더 강한 관계성을 나타냄

## *워싱턴 주에 위치한 킹 카운티의 주택 시설에 대한 과세 평가 금액 정보 data
kc_tax <- read.csv('data/kc_tax.csv')
str(kc_tax)

# 1-11. 육각형 구간 도표 - 집의 크기와 과세 평가액
# 데이터 주요 부분에 집중하기 위해, 아주 비싼/싼, 너무 큰 주택들은 subset함수를 이용해 제거
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving > 100 & SqFtTotLiving < 3500)
nrow(kc_tax0)   # 제거되어 row 개수 줄어듬

# 육각형 구간 도표로 시각화
# -> 값들을 육각형 모양의 구간들로 나누고, 각 구간에 포함된 기록값의 개수에 따라 색깔을 표시
# install.packages('hexbin')
library(hexbin)
library(ggplot2)
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
        stat_binhex(colour='white') +
        theme_bw() +
        scale_fill_gradient(low='white', high='black') +
        labs(x='Finished Square Feet', y='Tax Assessed Value')
# -> 집의 크기와 과세 평가 금액이 양의 상관관계가 나타남
# -> 주요 구름 부분 위쪽에 또 하나의 구름이 보임 -> 주요 구름 부분과 같은 크기의 집이지만, 더 높은 과세 평가액을 갖음

# 1-12. 등고선 도표 - 집의 크기과 과세 평가액 
ggplot(kc_tax0, aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  theme_bw() +
  geom_point(alpha=0.1) +
  geom_density2d(colour='white') +
  labs(x='Finished Square Feet', y='Tax Assessed Value')

# 1-13. 조건화를 통해 그룹지어 시각화 - 우편번호에 따른 과세 평가액 대 실 제곱피트
# facets 조건화 변수 사용
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
          aes(x=SqFtTotLiving, y=TaxAssessedValue))+
  stat_binhex(colour='white') +
  theme_bw() +
  scale_fill_gradient(low='white', high = 'blue') +
  labs(x='Finished Square Feet', y='Tax Assessed Value')+
  facet_wrap('ZipCode')
# 1-11 시각화에서 보였던 2개의 구름에 대한 설명을 가능하게 함


## *개인 대출 정보 data
lc_loans <- read.csv('data/lc_loans.csv')
head(lc_loans)

# 1-14. 대출 등급과 상황에 대한 분할표
# install.packages('descr')
library(descr)
x_tab <- CrossTable(lc_loans$grade, lc_loans$status, 
                    prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)
x_tab

## *한달동안의 항공기 지연 data
airline_stats <- read.csv('data/airline_stats.csv')
str(airline_stats)

# 1-15. 항공사별 지연 비율(boxplot) - 특잇값을 좀 더 명확하게 보여줌
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))
# -> Alaska 항공의 지연이 가장 적으며, American항공의 지연이 가장 많았음

# 1-16. 항공사별 지연 비율(violin plot) - 밀도추정 결과를 동시에 볼 수 있음
ggplot(data=airline_stats, aes(airline, pct_carrier_delay))+
    ylim(0,50) +
    geom_violin() +
    labs(x='', y='Daily % of Delayed Flights')

