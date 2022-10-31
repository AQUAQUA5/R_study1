setwd("C:/Users/51chi/Desktop/대학과제/4-2/컴퓨터공학 특강/bsl predict model")
setwd("C:/폴더 위치")
db1 <- read.csv("국민건강보험공단_건강검진정보_20211229.csv", header = T,encoding = "UTF-8", fileEncoding = "CP949")

#데이터 전처리
db2 <- db1[,c("성별코드", "연령대.코드.5세단위.", "신장.5Cm단위.", "체중.5Kg.단위.", "허리둘레", "식전혈당.공복혈당.")]
db2 <- db2[complete.cases(db2),]
db2 <- db2[1:100000,]

#성별코드 원핫 인코딩, 연령코드 평균값으로 치환
ID <- c()
male <- c()
female<- c()
age <- c()
height <- c()
weight <- c()
waist <- c()
bsl <- c()

for (i in 1:nrow(db2)) {
  ID[i] <- i
}

for (i in 1:nrow(db2)) {
  if ( db2[i,"성별코드"] == "1") {
    male[i] <- 1
    female[i] <- 0
  } else if ( db2[i,"성별코드"] == "2") {
    male[i] <- 0
    female[i] <- 1
  }
}

for (i in 1:nrow(db2)) {
  height[i] <- db2[i,"신장.5Cm단위."]
}

for (i in 1:nrow(db2)) {
  age[i] <- db2[i,"연령대.코드.5세단위."]*5-3
}

for (i in 1:nrow(db2)) {
  weight[i] <- db2[i,"체중.5Kg.단위."]
}

for (i in 1:nrow(db2)) {
  waist[i] <- db2[i,"허리둘레"]
}

for (i in 1:nrow(db2)) {
  bsl[i] <- db2[i,"식전혈당.공복혈당."]
}

db3 <- data.frame(ID, male, female, age, height, weight, waist, bsl)
#db3 <- read.csv("db3.csv", header = T,encoding = "UTF-8", fileEncoding = "CP949")
db3
str(db3)

#트레이닝 데이터, 테스트 데이터 분할
training <- sample(db3$ID, nrow(db3)*0.8)
validation <- sample(setdiff(db3$ID, training), nrow(db3)*0.2)

#선형회귀 모델
reg1 <- lm(bsl~male+female+age+height+weight+waist, data = db3[training, ])
reg1
summary(reg1)

#예측값 계산
pred_t <- predict(reg1, na.action = na.pass) # 학습데이터를 이용한 예측값 계산
pred_v <- predict(reg1, newdata = db3[validation, ], na.action = na.pass)

#오차 확인
compare_t <- data.frame(pred_t, db3[training, "bsl"], pred_t - db3[training, "bsl"])
colnames(compare_t) <- c("예상", "실제", "오차")
head(compare_t, 20)

compare_v <- data.frame(pred_v, db3[validation, "bsl"], pred_v - db3[validation, "bsl"])
colnames(compare_v) <- c("예상", "실제", "오차")
head(compare_v, 20)

mean(sqrt((compare_t[,"오차"])^2))
mean(sqrt((compare_v[,"오차"])^2))
