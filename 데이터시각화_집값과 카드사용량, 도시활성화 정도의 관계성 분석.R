options(scipen = 999) # full number


#load packages 
library(data.table)
library(dplyr)
library(ggplot2)

#map 관련 라이브러리
library(ggmap) # 필요시 yum install libjpeg-devel
library(raster)
library(rgeos) # 필요시 yum install geos geos-devel
library(maptools) 
library(rgdal) # 필요시 yum install gdal gdal-devel proj proj-devel
library(sp)
library(geosphere)
library(tibble)

#network 관련 라이브러리
library(igraph)
#install.packages('ggraph')
library(ggraph)

library(visNetwork)

knitr::opts_chunk$set(warning = FALSE, message=FALSE)
data1 <- read.csv(choose.files(),header = T)
pay2 <- read.csv(choose.files(),header = T)
pay1 <- read.csv(choose.files(),header = T)
bill <- read.csv(choose.files(),header = T)

# 분석 대상 데이터 할당
# 도시 재생 활성화 지역 데이터
home <- data1
head(home)


# 집값 데이터
# 아파트와 다세대 주택 실거래가 기준
apt <- pay1[c(5:64037),]
head(apt)

vilra <- pay2[c(5:8159),]
head(vilra)

# 국민카드 거래량

kb <- bill
head(kb)

### 동에서 구로 _ apt

apt$시군구 <- '대덕구'
apt$시군구 <- ifelse ((apt$법정동 %in%  c('원동', '정동', '중동', '소제동', '신안동', '인동', '신흥동',	'효동', '천동', '가오동',	'판암동', '삼정동', 
                                    '용운동' ,'대동'  ,'자양동' , '가양동', '용전동' ,'성남동', '홍도동', '삼성동' , '추동', '효평동', '직동', '비룡동', '주산동', '용계동', 
                                    '마산동', '세천동','신상동', '신하동', '신촌동', '사성동', '내탑동', '오동', '주촌동' ,'낭월동', '대별동', '이사동', '대성동', '장척동', 
                                    '소호동', '구도동', '삼괴동', '상소동', '하소동')), "동구",
                   ifelse ((apt$법정동 %in%   c('대사동','대흥동','목동','문창동','문화동','부사동','산성동', '사정동', 
                                             '안영동', '구완동', '무수동', '침산동', '목달동', '정생동', '어남동', '금동',
                                             '석교동', '호동', '옥계동','오류동','용두동','유천동','은행동', '선화동','중촌동','태평동')), "중구",
                           ifelse ((apt$법정동 %in%   c( '복수동' , '도마동' , '정림동', '괴곡동', '변동', '용문동', '탄방동', '둔산동',  
                                                      '괴정동', '가장동' , '내동','갈마동', '월평동',
                                                      '만년동', '가수원동', '도안동','관저동', '흑석동', '매노동', '산직동', '장안동', '평촌동', '오동', 
                                                      '우명동', '원정동', '용촌동', '봉곡동')), "서구", 
                                   ifelse ((apt$법정동 %in%   c('원내동', '대정동', '계산동', '교촌동', '용계동', '학하동', '송정동', '방동', '성북동', '세동', 
                                                             '원신흥동', '상대동', '봉명동' ,  '구암동', '덕명동', '복용동', '장대동', '궁동', '죽동', '어은동', 
                                                             '구성동','노은동', '지족동', '갑동', '죽동',  '죽동', '외삼동', '안산동', '수남동', '반석동', '신성동', 
                                                             '도룡동', '추목동', '신봉동', '화암동', '하기동', '가정동', '장동', '자운동', '덕진동', '방현동',	'전민동',
                                                             '문지동', '원촌동','송강동', '봉산동', '대동', '금탄동', '구룡동', '둔곡동', '금고동', '신동',	'관평동', '용산동',
                                                             '탑립동')), "유성구", "대덕구"))))
tail(apt)

### 동에서 구 _ vilra
vilra$시군구 <- '대덕구'
vilra$시군구 <- ifelse ((vilra$법정동 %in%  c('원동', '정동', '중동', '소제동', '신안동', '인동', '신흥동',	'효동', '천동', '가오동',	'판암동', '삼정동', 
                                    '용운동' ,'대동'  ,'자양동' , '가양동', '용전동' ,'성남동', '홍도동', '삼성동' , '추동', '효평동', '직동', '비룡동', '주산동', '용계동', 
                                    '마산동', '세천동','신상동', '신하동', '신촌동', '사성동', '내탑동', '오동', '주촌동' ,'낭월동', '대별동', '이사동', '대성동', '장척동', 
                                    '소호동', '구도동', '삼괴동', '상소동', '하소동')), "동구",
                   ifelse ((vilra$법정동 %in%   c('대사동','대흥동','목동','문창동','문화동','부사동','산성동', '사정동', 
                                             '안영동', '구완동', '무수동', '침산동', '목달동', '정생동', '어남동', '금동',
                                             '석교동', '호동', '옥계동','오류동','용두동','유천동','은행동', '선화동','중촌동','태평동')), "중구",
                           ifelse ((vilra$법정동 %in%   c( '복수동' , '도마동' , '정림동', '괴곡동', '변동', '용문동', '탄방동', '둔산동',  
                                                      '괴정동', '가장동' , '내동','갈마동', '월평동',
                                                      '만년동', '가수원동', '도안동','관저동', '흑석동', '매노동', '산직동', '장안동', '평촌동', '오동', 
                                                      '우명동', '원정동', '용촌동', '봉곡동')), "서구", 
                                   ifelse ((vilra$법정동 %in%   c('원내동', '대정동', '계산동', '교촌동', '용계동', '학하동', '송정동', '방동', '성북동', '세동', 
                                                             '원신흥동', '상대동', '봉명동' ,  '구암동', '덕명동', '복용동', '장대동', '궁동', '죽동', '어은동', 
                                                             '구성동','노은동', '지족동', '갑동', '죽동',  '죽동', '외삼동', '안산동', '수남동', '반석동', '신성동', 
                                                             '도룡동', '추목동', '신봉동', '화암동', '하기동', '가정동', '장동', '자운동', '덕진동', '방현동',	'전민동',
                                                             '문지동', '원촌동','송강동', '봉산동', '대동', '금탄동', '구룡동', '둔곡동', '금고동', '신동',	'관평동', '용산동',
                                                             '탑립동')), "유성구", "대덕구"))))



tail(vilra)
tail(apt)




### na를 0으로 변경
kb[is.na(kb$X3월.억원.),'X3월.억원.'] = 0 # 확진자 1062명 3월 1일# 은혜의 강 교회 발 소금물 분사 사건
kb[is.na(kb$X4월.억원.),'X4월.억원.'] = 0 
kb[is.na(kb$X5월.억원.),'X5월.억원.'] = 0
kb[is.na(kb$X6월.억원.),'X6월.억원.'] = 0
kb[is.na(kb$X7월.억원.),'X7월.억원.'] = 0
kb[is.na(kb$X7월.억원.),'X7월.억원.'] = 0

### kb 총합계 만들기
sumallkb = kb[,c(-1,-2,-3)]
kb$소비합계 <-rowSums(sumallkb)
kb[is.na(kb$소비합계),'소비합계'] = 0


### kb 총합계 만들기
# 확진자 1062명 3월 1일# 은혜의 강 교회 발 소금물 분사 사건
# 2020년 데이터와 2019년 데이터가 있음

kb19 <- subset(kb, 기준년도 == '2019년')

kb20 <- subset(kb, 기준년도 == '2020년')



kb19 %>% head()
kb20 %>% head()
apt %>% head()
vilra %>% head()



#19년도 부터 데이터 가져오기 
# 데이터 기각은 15년 1월부터 20년 8월까지임 

apt19 <- subset(apt,기준년월 >= 201901)
vilra19 <- subset(vilra,기준년월 >= 201901)

# 만들어진 데이터를 깔끔하게 저장한다.
#write.csv(apt, "C:/Users/smcom/Desktop/마이닝/apt.csv")
#write.csv(kb19, "C:/Users/smcom/Desktop/마이닝/kb19.csv")
#write.csv(kb20, "C:/Users/smcom/Desktop/마이닝/kb20.csv")
#write.csv(vilra, "C:/Users/smcom/Desktop/마이닝/vilra.csv")
#write.csv(apt19, "C:/Users/smcom/Desktop/마이닝/apt19.csv")
#write.csv(vilra19, "C:/Users/smcom/Desktop/마이닝/vilra19.csv")





### 분석 목적 : 대전시 아파트 층의 선호도 시장 조사하기
# 목표 변수는 아파트와 빌라 모두 '법정동'으로 동일 
# 설명 변수는 이름, 층, 시군구, 거래금액(범주형이라 가능 할지 모르겠음)

apt19 %>% head()
vilra19 %>% head()


# 분석에서 제외할 데이터 일부 삭제
apt1920 <- apt19[,-c(2,4)]


# 결측값 제거
apt1 <- subset(apt1920, !is.na(법정동))
apt2 <- subset(apt1, !is.na(아파트명))
apt3 <- subset(apt2, !is.na(층))
apt4 <- subset(apt3, !is.na(거래금액.일부보기))
apt5 <- subset(apt4, !is.na(시군구))
apt5$층 <- as.numeric(apt5$층)

str(apt5)


sapply(apt5, class)

# 모형구축을 위한 훈련용 자료 생성.
#(training data)와 모형의 성능을 검증하기위한 검증용 자료(test data)를 70%와 30%로 구성한다.

set.seed(1234)
ind <- sample(2, nrow(apt5), replace=TRUE, prob=c(0.7, 0.3))
ind


train_apt <- apt5[ind==1, ] # n=2136개
test_apt <- apt5[ind==2, ] # n=797개

#따로 저장 

#write.csv(train_apt, "C:/Users/smcom/Desktop/마이닝/train_apt.csv")
#write.csv(test_apt, "C:/Users/smcom/Desktop/마이닝/test_apt.csv")


### 훈련자료: winequality_red_train.csv
#data.train <- read.csv(file.choose(),header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
#data.test <- read.csv(file.choose(),header = T, stringsAsFactors = T)




# 거래 금액을 내림차순으로 정리
apt5_1_0 <- apt5[c(order(apt5$거래금액.일부보기)),]
rownames(apt5_1_0) <- NULL
apt5_1_0 %>% head()
apt5_1_0 %>% tail()






# 분석에서 제외할 데이터 일부 삭제
vilra1920 <- vilra19[,-c(2,4)]


# 결측값 제거
vilra1 <- subset(vilra1920, !is.na(법정동))
vilra2 <- subset(vilra1, !is.na(연립주택명))
vilra3 <- subset(vilra2, !is.na(층))
vilra4 <- subset(vilra3, !is.na(거래금액.일부보기))
vilra5 <- subset(vilra4, !is.na(시군구))
vilra6 <- subset(vilra5, !is.na(건축년도))
vilra6$층 <- as.numeric(vilra6$층)

str(vilra6)


sapply(vilra6, class)


# 거래 금액을 오름차순으로 정리
vilra5_1_0 <- vilra6[c(order(vilra6$거래금액.일부보기)),]
rownames(vilra5_1_0) <- NULL
vilra5_1_0 %>% head()
vilra5_1_0 %>% tail()



library(tidyverse)
library(sf)


library(tidyverse)
library(sf)


sido_shp <- st_read("C:/Users/smcom/Desktop/R/min/CTPRVN_202005_/CTPRVN.shp")
sido_shp %>% head()

sido_shp$SIG_KOR_NM <- iconv(sido_shp$CTP_KOR_NM, localeToCharset(), 'UTF-8')


sido_shp$CTP_KOR_NM <- iconv(sido_shp$CTP_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)

dj_shp <- subset(sido_shp, CTP_KOR_NM  == '대전광역시')




sido_shp
unique(sido_shp$CTP_KOR_NM)

DJ_shp <- sido_shp %>% 
  filter(str_detect(CTP_KOR_NM, "대전광역시"))

DJ_shp %>% 
  plot()

### 대전 파일을 따로 저장
rgdal::writeOGR(DJ_shp, dsn ="C:/Users/smcom/Desktop/R/min/CTPRVN_202005_/Daejeon.shp",layer = "map" ,driver = "ESRI Shapefile")



### 대전 파일과 읍면동 파일을 붙이자.
st_crs(sido_shp)

emd_shp <- st_read("C:/Users/smcom/Desktop/R/min/EMD_202005_읍면동/EMD.shp")

## 인코딩 변경
emd_shp$EMD_KOR_NM <- iconv(emd_shp$EMD_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)


unique(emd_shp$EMD_KOR_NM)



## 대전시 행정동 코드: 3000000000
DJ_emd_shp <- emd_shp %>% 
  mutate(SIGUNGU = case_when(str_detect(EMD_CD, "^30140") ~ "중구",
                             str_detect(EMD_CD, "^30110") ~ "동구",
                             str_detect(EMD_CD, "^30230") ~ "대덕구",
                             str_detect(EMD_CD, "^30200") ~ "유성구",
                             str_detect(EMD_CD, "^30170") ~ "서구")) %>% 
  filter(!is.na(SIGUNGU))

## shapefile 저장
rgdal::writeOGR(DJ_emd_shp, "C:/Users/smcom/Desktop/R/min/CTPRVN_202005_/DJ_emd_shp.shp",layer = "map" ,driver = "ESRI Shapefile" )




st_crs(DJ_shp)
st_crs(emd_shp)

DJ_emd_shp <-  DJ_emd_shp %>% 
  mutate(SIGUNGU = case_when(str_detect(EMD_CD, "^30140") ~ "중구",
                             str_detect(EMD_CD, "^30110") ~ "동구",
                             str_detect(EMD_CD, "^30230") ~ "대덕구",
                             str_detect(EMD_CD, "^30200") ~ "유성구",
                             str_detect(EMD_CD, "^30170") ~ "서구")) %>% 
  filter(!is.na(SIGUNGU))

## sf 데이터프레임 --> shapefile 변환
# sungnam_emd_shp <-  sf:::as_Spatial(sungnam_df_shp$geometry)
DJ_emd_shp <-  as(DJ_emd_shp, 'Spatial')

class(DJ_emd_shp)



## shapefile --> 데이터프레임 변환
DJ_emd_shp <- fortify(DJ_emd_shp)

## ggplot 시각화
DJ_emd_shp %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_path(color = 'blue', size = .2) +
  coord_fixed(1.3)


DJ_emd_shp %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white", fill="skyblue") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  theme_void()  


############### 법정동으로 붙여보기

dj <- deajeon_df_shp






### Using shape file
### library
library(sp)
library(rgdal)
### shape file (http://www.gisdeveloper.co.kr/?p=2332)
### 시도: CTPRVN_202005/CTPRVN.shp
### 시군구: SIG_202005/SIG.shp
shp.file <- file.choose()
### read shape file
shp <- rgdal::readOGR(shp.file,encoding = 'utf-8')

## 인코딩 변경
shp$EMD_KOR_NM <- iconv(shp$EMD_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
shp$SIGUNGU <- iconv(shp$SIGUNGU, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)





### 자료 확인
shp@data
### 좌표 확인: UTM-K(GRS-80) 좌표계에서 WGS84 경위도 좌표계로 변환
from.crs <- " +proj=longlat +datum=WGS84 +no_defs "
to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
shp <- spTransform(shp, to.crs)
### 지도 확인
plot(shp)
### 다각형 – ggplot()
ggplot() + geom_polygon(data=shp, aes(x=long, y=lat, group=group), fill="white", color="black")
### 다각형 – ggmap()
ggmap(airMap) + geom_p








dj %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white", fill="skyblue") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  theme_void()  

kb19 %>% head()
kb20 %>% head()
apt %>% head()
vilra %>% head()

apt1920 %>% head()
vilra1920 %>% head()




