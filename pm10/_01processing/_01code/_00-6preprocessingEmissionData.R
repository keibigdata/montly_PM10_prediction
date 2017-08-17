##preprocess emission data

#remove variable
rm(list=ls())

#sigungu for change
sigungu = c("양주군", "여주군", "안산시", "용인시", "포천군", "청원군", "연기군", "당진군", "천안시", "마산시", "진해시", "창원시", "북제주군", "남제주군", "부천시", "고양시", "수원시")

#emission data
emission = read.csv("D:/01 Study/08 pm/processing/preprocessing/emission.csv", header = T)

#SIDO명 변경
# emission[emission[,"SIDO"]=="서울특별시", "SIDO"] <- "서울"
# emission[emission[,"SIDO"]=="부산광역시", "SIDO"] <- "부산"
# emission[emission[,"SIDO"]=="대구광역시", "SIDO"] <- "대구"
# emission[emission[,"SIDO"]=="인천광역시", "SIDO"] <- "인천"
# emission[emission[,"SIDO"]=="광주광역시", "SIDO"] <- "광주"
# emission[emission[,"SIDO"]=="대전광역시", "SIDO"] <- "대전"
# emission[emission[,"SIDO"]=="울산광역시", "SIDO"] <- "울산"
# emission[emission[,"SIDO"]=="세종특별자치시", "SIDO"] <- "울산"
# 
# emission[emission[,"SIDO"]=="경기도", "SIDO"] <- "경기"
# emission[emission[,"SIDO"]=="강원도", "SIDO"] <- "강원"
# emission[emission[,"SIDO"]=="충청북도", "SIDO"] <- "충북"
# emission[emission[,"SIDO"]=="충청남도", "SIDO"] <- "충남"
# emission[emission[,"SIDO"]=="전라북도", "SIDO"] <- "전북"
# emission[emission[,"SIDO"]=="전라남도", "SIDO"] <- "전남"
# emission[emission[,"SIDO"]=="경상북도", "SIDO"] <- "경북"
# emission[emission[,"SIDO"]=="경상남도", "SIDO"] <- "경남"
# emission[emission[,"SIDO"]=="제주도", "SIDO"] <- "제주"


#승격(양주군, 여주군, 포천군, 당진군)
emission[emission[,"SIGUNGU"]=="양주군", "SIGUNGU"] <- "양주시"
emission[emission[,"SIGUNGU"]=="여주군", "SIGUNGU"] <- "여주시"
emission[emission[,"SIGUNGU"]=="포천군", "SIGUNGU"] <- "포천시"
emission[emission[,"SIGUNGU"]=="당진군", "SIGUNGU"] <- "당진시"

#청원군(향후 sum 해야함)
emission[emission[,"SIGUNGU"]=="청원군", "SIGUNGU"] <- "청주시"
emission[emission[,"SIGUNGU"]=="청원군", "SIGUNGU2"] <- ""

#연기군(향후 sum 해야함)
emission[emission[,"SIGUNGU"]=="연기군", "SIDO"] <- "세종"
emission[emission[,"SIGUNGU"]=="연기군", "SIGUNGU"] <- "세종시"
emission[emission[,"SIGUNGU"]=="연기군", "SIGUNGU2"] <- ""

#제주(향후 sum 해야함)
emission[emission[,"SIGUNGU"]=="북제주군", "SIGUNGU"] <- "제주시"
emission[emission[,"SIGUNGU"]=="남제주군", "SIGUNGU"] <- "서귀포시"

#부천(향후 sum 해야함)
emission[emission[,"SIGUNGU2"]=="원미구", "SIGUNGU2"] <- ""
emission[emission[,"SIGUNGU2"]=="소사구", "SIGUNGU2"] <- ""
emission[emission[,"SIGUNGU2"]=="오정구", "SIGUNGU2"] <- ""

#청주 구 없애기(향후 sum 해야함)
emission[emission[,"SIGUNGU"]=="청주시", "SIGUNGU2"] <- ""

##sum
emission.aggr <- aggregate(x=emission[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")],
                           by=emission[c("YEAR","SIDO","SIGUNGU","SIGUNGU2","LARGECATE")],
                           FUN=sum,
                           na.rm=TRUE)

##나눠지는 아이들 중심으로!!!
#freq column 생성
emission.aggr[,"FREQ"] <- 1

#insert freq
emission.aggr[emission.aggr[,"SIGUNGU"]=="안산시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="용인시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 3
emission.aggr[emission.aggr[,"SIGUNGU"]=="천안시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="마산시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="창원시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="고양시" & emission.aggr[,"SIGUNGU2"]=="일산구", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="수원시" & emission.aggr[,"SIGUNGU2"]=="팔달구", "FREQ"] <- 2
emission.aggr[emission.aggr[,"SIGUNGU"]=="청주시" & emission.aggr[,"SIGUNGU2"]=="", "FREQ"] <- 4

#function to manage rownum
# check.integer <- function(N){
#   !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
# }
remainder <- function(N){
  return (as.numeric(N)%%1) # (last) 
}

#create expanded emission data 
emission.expanded <- emission.aggr[rep(row.names(emission.aggr), emission.aggr$FREQ),]
emission.expanded[,"ROWNUM"] <- as.character(rownames(emission.expanded))
emission.expanded[,"INTEGER"] <- sapply(emission.expanded$ROWNUM, remainder)

#allocation emission value 
emission.expanded[emission.expanded[,"FREQ"]>1, c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")] <- emission.expanded[emission.expanded[,"FREQ"]>1, c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")] / emission.expanded[emission.expanded[,"FREQ"]>1, "FREQ"]
# emission[emission[,"SIGUNGU"]=="창원시",]
# emission.expanded[emission.expanded[,"SIGUNGU"]=="창원시",]
# emission[emission[,"SIGUNGU"]=="용인시",]
# emission.expanded[emission.expanded[,"SIGUNGU"]=="용인시",]

#setting location name
#안산시
emission.expanded[emission.expanded[,"SIGUNGU"]=="안산시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "단원구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="안산시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "상록구" 
#용인시
emission.expanded[emission.expanded[,"SIGUNGU"]=="용인시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "수지구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="용인시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "기흥구" 
emission.expanded[emission.expanded[,"SIGUNGU"]=="용인시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0.1 & emission.expanded[,"INTEGER"]<0.3, "SIGUNGU2"] <- "처인구" 
#천안시
emission.expanded[emission.expanded[,"SIGUNGU"]=="천안시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "동남구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="천안시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "서북구" 
#마산시
emission.expanded[emission.expanded[,"SIGUNGU"]=="마산시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "마산회원구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="마산시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "마산합포구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="마산시", "SIGUNGU"] <- "창원시" 
#창원시
emission.expanded[emission.expanded[,"SIGUNGU"]=="창원시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "의창구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="창원시" & emission.expanded[,"SIGUNGU2"]=="" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "성산구" 
#고양시 일산구
emission.expanded[emission.expanded[,"SIGUNGU"]=="고양시" & emission.expanded[,"SIGUNGU2"]=="일산구" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "일산동구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="고양시" & emission.expanded[,"SIGUNGU2"]=="일산구" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "일산서구" 
#수원시 팔달구
emission.expanded[emission.expanded[,"SIGUNGU"]=="수원시" & emission.expanded[,"SIGUNGU2"]=="팔달구" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "팔달구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="수원시" & emission.expanded[,"SIGUNGU2"]=="팔달구" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "영통구" 
#청주시
emission.expanded[emission.expanded[,"SIGUNGU"]=="청주시" & emission.expanded[,"INTEGER"]==0, "SIGUNGU2"] <- "상당구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="청주시" & emission.expanded[,"INTEGER"]>0 & emission.expanded[,"INTEGER"]<0.2, "SIGUNGU2"] <- "흥덕구" 
emission.expanded[emission.expanded[,"SIGUNGU"]=="청주시" & emission.expanded[,"INTEGER"]>0.1 & emission.expanded[,"INTEGER"]<0.3, "SIGUNGU2"] <- "청원구" 
emission.expanded[emission.expanded[,"SIGUNGU"]=="청주시" & emission.expanded[,"INTEGER"]>0.2 & emission.expanded[,"INTEGER"]<0.4, "SIGUNGU2"] <- "서원구" 


#진해시 -> 창원시 진해구
emission.expanded[emission.expanded[,"SIGUNGU"]=="진해시", "SIGUNGU2"] <- "진해구"
emission.expanded[emission.expanded[,"SIGUNGU"]=="진해시", "SIGUNGU"] <- "창원시"

##join SIG_CD
library(dplyr)
stdSigungu <- read.csv("D:/01 Study/08 pm/processing/std_sigungu.csv")
emission.joined <- left_join(emission.expanded, stdSigungu, by= c("SIDO"="SIDO","SIGUNGU"="SIGUNGU","SIGUNGU2"="SIGUNGU2"))
emission.joined <- emission.joined[,c("YEAR","SIDO","SIGUNGU","SIGUNGU2","LARGECATE","SIG_CD","EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")] #,"EM_PM2.5"
#aggregation
emission.joined <- aggregate(x=emission.joined[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")], #,"EM_PM2.5"
                           by=emission.joined[c("YEAR","SIDO","SIGUNGU","SIGUNGU2","SIG_CD","LARGECATE")],
                           FUN=sum,
                           na.rm=TRUE)
length(unique(emission.joined[,"SIG_CD"]))

##LARGECATE
emission.joined.1 <- emission.joined[emission.joined$LARGECATE=="에너지산업 연소",]
emission.joined.2 <- emission.joined[emission.joined$LARGECATE=="비산업 연소",]
emission.joined.3 <- emission.joined[emission.joined$LARGECATE=="제조업 연소",]
emission.joined.4 <- emission.joined[emission.joined$LARGECATE=="생산공정",]
emission.joined.5 <- emission.joined[emission.joined$LARGECATE=="에너지수송 및 저장",]
emission.joined.6 <- emission.joined[emission.joined$LARGECATE=="유기용제 사용",]
emission.joined.7 <- emission.joined[emission.joined$LARGECATE=="도로이동오염원",]
emission.joined.8 <- emission.joined[emission.joined$LARGECATE=="비도로이동오염원",]
emission.joined.9 <- emission.joined[emission.joined$LARGECATE=="폐기물처리",]

##stdSigunguMonth
stdSigungu[, "NO"] <- 13
stdSigunguMonth <- stdSigungu[rep(row.names(stdSigungu), stdSigungu$NO),]
stdSigunguMonth[, "YEAR"] <- rep(2001:2013, 250)
#standard
stdSigungu <- stdSigungu[, c("SIDO","SIGUNGU","SIGUNGU2","SIG_CD")]
stdSigunguMonth <- stdSigunguMonth[,c("SIG_CD","YEAR")]

###중간테스트
write.csv(emission.joined, "D:/01 Study/08 pm/processing/preprocessing/result/emission_joined.csv")
write.csv(emission.joined.1, "D:/01 Study/08 pm/processing/preprocessing/result/energyIndustry.csv")
write.csv(emission.joined.temp, "D:/01 Study/08 pm/processing/preprocessing/result/emissionJoinedTemp.csv")

##join
# emission.joined.temp <- left_join(stdSigunguMonth, emission.joined.1, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.2, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.3, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.4, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.5, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.6, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.7, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined.temp <- left_join(emission.joined.temp, emission.joined.8, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
# emission.joined <- left_join(emission.joined.temp, emission.joined.9, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))

##join
emission.joined.temp <- left_join(stdSigunguMonth, emission.joined.1, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.2, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.3, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.4, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.5, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.6, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.7, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined.temp <- left_join(emission.joined.temp, emission.joined.8, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))
emission.joined <- left_join(emission.joined.temp, emission.joined.9, by= c("YEAR"="YEAR","SIG_CD"="SIG_CD"))

#save csv
write.csv(emission.joined, "D:/01 Study/08 pm/processing/input/emission_joined.csv", row.names = FALSE)

#########################################################################################################################################
##explore and validate the data

#overall statistics
emission.aggr.1 <- aggregate(x=emission[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")],
                             by=emission[c("YEAR")],
                             FUN=sum,
                             na.rm=TRUE)
emission.aggr.1.melted <- melt(emission.aggr.1, id="YEAR")
ggplot(data=emission.aggr.1.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 1500000000, 100000000), limits = c(0,1500000000))

#Large category
emission.joined.1.year <- aggregate(x=emission.joined.1[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")], #,"EM_PM2.5"
                                    by=emission.joined.1[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.2.year <- aggregate(x=emission.joined.2[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.2[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.3.year <- aggregate(x=emission.joined.3[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.3[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.4.year <- aggregate(x=emission.joined.4[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.4[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.5.year <- aggregate(x=emission.joined.5[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.5[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.6.year <- aggregate(x=emission.joined.6[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.6[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.7.year <- aggregate(x=emission.joined.7[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.7[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.8.year <- aggregate(x=emission.joined.8[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.8[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)
emission.joined.9.year <- aggregate(x=emission.joined.9[c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3")],
                                    by=emission.joined.9[c("YEAR")],
                                    FUN=sum,
                                    na.rm=TRUE)

library(ggplot2)
library(reshape2)

emission.joined.1.year.melted <- melt(emission.joined.1.year, id="YEAR")
emission.joined.2.year.melted <- melt(emission.joined.2.year, id="YEAR")
emission.joined.3.year.melted <- melt(emission.joined.3.year, id="YEAR")
emission.joined.4.year.melted <- melt(emission.joined.4.year, id="YEAR")
emission.joined.5.year.melted <- melt(emission.joined.5.year, id="YEAR")
emission.joined.6.year.melted <- melt(emission.joined.6.year, id="YEAR")
emission.joined.7.year.melted <- melt(emission.joined.7.year, id="YEAR")
emission.joined.8.year.melted <- melt(emission.joined.8.year, id="YEAR")
emission.joined.9.year.melted <- melt(emission.joined.9.year, id="YEAR")


ggplot(data=emission.joined.1.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  #ylab(expression("PM "[10] ~ "³óµµ"  ~ (mu ~ g/m^{3}))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 400000000, 100000000), limits = c(0,450000000)) #+
  #geom_text(aes(y = PM10, label = as.character(pm10Label), size = 4, hjust = -0.3, vjust = 0.1))

ggplot(data=emission.joined.2.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 100000000, 10000000), limits = c(0,100000000))

ggplot(data=emission.joined.3.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 200000000, 50000000), limits = c(0,200000000))

ggplot(data=emission.joined.4.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 200000000, 50000000), limits = c(0,200000000))

ggplot(data=emission.joined.5.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 40000000, 5000000), limits = c(0,40000000))

ggplot(data=emission.joined.6.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 200000000, 500000000), limits = c(0,200000000.5000000000))

ggplot(data=emission.joined.7.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 700000000, 100000000), limits = c(0,700000000))

ggplot(data=emission.joined.8.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 250000000, 50000000), limits = c(0,250000000))

ggplot(data=emission.joined.9.year.melted, aes(x=YEAR, y=value, colour=variable)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2013, 1)) +
  scale_y_continuous(breaks=seq(0, 50000000, 10000000), limits = c(0,50000000))
