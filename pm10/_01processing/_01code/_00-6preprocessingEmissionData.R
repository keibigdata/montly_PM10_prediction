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

#제주(향후 sum 해야함)
emission[emission[,"SIGUNGU"]=="북제주군", "SIGUNGU"] <- "제주시"
emission[emission[,"SIGUNGU"]=="남제주군", "SIGUNGU"] <- "서귀포시"

#부천(향후 sum 해야함)
emission[emission[,"SIGUNGU2"]=="원미구", "SIGUNGU2"] <- ""
emission[emission[,"SIGUNGU2"]=="소사구", "SIGUNGU2"] <- ""
emission[emission[,"SIGUNGU2"]=="오정구", "SIGUNGU2"] <- ""


##나눠지는 아이들 중심으로!!!
#freq column 생성
emission[,"FREQ"] <- 1

#insert freq
emission[emission[,"SIGUNGU"]=="안산시", "FREQ"] <- 2
emission[emission[,"SIGUNGU"]=="용인시", "FREQ"] <- 3
emission[emission[,"SIGUNGU"]=="천안시", "FREQ"] <- 2
emission[emission[,"SIGUNGU"]=="마산시", "FREQ"] <- 2
emission[emission[,"SIGUNGU"]=="창원시", "FREQ"] <- 2
emission[emission[,"SIGUNGU"]=="고양시" & emission[,"SIGUNGU2"]=="일산구", "FREQ"] <- 2
emission[emission[,"SIGUNGU"]=="수원시" & emission[,"SIGUNGU2"]=="팔달구", "FREQ"] <- 2

#function to manage rownum
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}
remainder <- function(N){
  return (N%%1)
}

#create expanded emission data 
emission.expanded <- emission[rep(row.names(emission), emission$FREQ),]
emission.expanded[,"ROWNUM"] <- as.numeric(rownames(emission.expanded))
emission.expanded[,"INTEGER"] <- sapply(emission.expanded$ROWNUM, remainder)

#allocation emission value 
emission.expanded[emission.expanded[,"FREQ"]>1, c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")] <- emission.expanded[emission.expanded[,"FREQ"]>1, c("EM_CO","EM_NOx","EM_SOx","EM_TSP","EM_PM10","EM_VOC","EM_NH3","EM_PM2.5")] / emission.expanded[emission.expanded[,"FREQ"]>1, "FREQ"]
# emission[emission[,"SIGUNGU"]=="창원시",]
# emission.expanded[emission.expanded[,"SIGUNGU"]=="창원시",]
# emission[emission[,"SIGUNGU"]=="용인시",]
# emission.expanded[emission.expanded[,"SIGUNGU"]=="용인시",]


#진해시 -> 창원시 진해구
emission[emission[,"SIGUNGU"]=="진해시", "SIGUNGU2"] <- "진해구"
emission[emission[,"SIGUNGU"]=="진해시", "SIGUNGU"] <- "창원시"