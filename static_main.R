#function implementation
readAllPackgesAndLibrary <- function(){
  library("dplyr")
  source("function_set.r")
  library(xlsx)
  library(moonBook)  
}


# conn <- getDbConnection()
# sql <- getTotalTableQuerry()
# totalTable <- querySql(conn,sql)
# totalTable
# saveRDS(totalTable,"totalTable.rds")
# conditionCountSumByGender <- getSumConditionCount(totalTable)
# conditionCountSumByGender

# 
# df1 <- data.frame(name=c("abc","abc","bbb","jmh","jmh","yym"),age=c(30,31,33,36,33,43))
# 
# df2 <- data.frame(name=c("abc","jmh","abc"),age=c(30,36,31),grade=c(1,2,3))


#PIM characteristics

totalPimTable <- totalTable %>% subset(pimCount>0)
totalPimTable
totalPimPerson <- totalPimTable %>% distinct(PERSON_ID)
totalPimPerson

manPimTable <- totalPimTable %>% subset(trimws(GENDER)=="M")
manPimTable
manPimPerson <- manPimTable %>% distinct(PERSON_ID)
manPimPerson


womanPimTable <- totalPimTable %>% subset(trimws(GENDER)=="F")
womanPimTable
womanPimPerson <- womanPimTable %>% distinct(PERSON_ID)
womanPimPerson

# 질병 진단 통계 표준편차
sdTotal <- totalTable %>% summarise(sd=sd(CONDITION_COUNT))
sdMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(CONDITION_COUNT))
sdWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(CONDITION_COUNT))

# 나이별 성별 수 집계

forGenderTable <- totalTable %>% select(AGE,GENDER)
ratioGender <- getRatioGender(forGenderTable)
ratioGender

####

setwd("c:\\Git\\patterns-of-PIM")
carboplatin <- read.xlsx("carboplatin.xlsx",2)
carboplatin

####



# 나이 통계 (성별) 표준편차 / 평균
# 전체



totalTable <- readRDS("finalTotalTable.rds")

pt  <- totalTable %>% subset(pimCount>0)
npt <- totalTable %>% subset(pimCount==0)
getAgeMeanSd(pt)
getAgeMeanSd(npt)

apt <- pt %>% select(AGE)
anpt <- npt %>% select(AGE)



mpt <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="M")
mnpt<- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="M")
getAgeMeanSd(mpt)
getAgeMeanSd(mnpt)

wpt <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="F")
wnpt<- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="F")
getAgeMeanSd(wpt)
getAgeMeanSd(wnpt)

# t-test====================================



# # meanAgeTotal <- totalTable %>% summarise(mean = round(mean(AGE),3))
# # sdAgeTotal <- totalTable %>% summarise(sd = round(sd(totalTable$AGE),3))
# 
# meanAgeTotal
# sdAgeTotal
# 
# # 남자
# ageManTalbe <- totalTable %>% filter(trimws(GENDER)=="M")
# meanAgeMan <- ageManTalbe %>% summarise(mean = mean(AGE))
# sdAgeMan <- ageManTalbe %>% summarise(sd=sd(AGE))
# meanAgeMan
# sdAgeMan
# 
# # 여자
# ageWomanTable <- totalTable %>% filter(trimws(GENDER)=="F")
# meanAgeWoman <- ageWomanTable %>% summarise(mean = mean(AGE))
# sdAgeWoman <-  ageWomanTable %>% summarise(sd=sd(AGE))
# meanAgeWoman
# sdAgeWoman

# 나이 범위에 따른 통계 
totalTable <- totalPimTable
countAgeTotal <- totalTable %>% select(AGE,GENDER)
getRatioAge(countAgeTotal)

# 약물(beers criteria 기준) 정보 통계 (성별 기준) 백분율
drug_inform <- getInformXlsx("drug_inform.xlsx")
drug_inform
drugStatic <- totalTable %>% group_by(PERSON_ID,CONDITION_START_DATE) %>%
  select(PERSON_ID,CONDITION_START_DATE,DRUG_COUNT,DRUG_CONCEPT_ID,GENDER)
# 약물 표준편차차
sdDrug <- totalTable %>% summarise(sd = sd(DRUG_COUNT))
sdDrugMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(DRUG_COUNT))
sdDrugWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(DRUG_COUNT))
sdDrug <- data.frame(total=sdDrug$sd,
                     man=sdDrugMan$sd,
                     woman=sdDrugWoman$sd)
sdDrug

#condition 통계 숫자 카운트 ( 특정한 만성질환에 대한 통계 시작)
##만성질환별 카운트 구하는 구간 ===========Characteristics of the study population
totalTable <- readRDS("finalTotalTable.rds")
str(totalTable)
head(totalTable)

newtt <- subset(totalTable,select=-cbdCount)
head(newtt)
newtt <- subset(newtt,select=-cbdList)
head(newtt)
# setRdsEachYear(newtt,"new",1999,2018) #세팅
# calcAndSaveComorbidity("new",1999,2018) #입력한 년도별 만성질환 리스트와 수를 구하고 rds저장 (완료함)

comobidityListXlsx <- getInformXlsx("Comorbidity_List.xlsx")
comobidityListXlsx

comorResult <- getCombineComorbidityDataframes("newResult",1999,2018)
comorResult

ccs <- comorResult %>% summarise(sum(cbdCount))

ctp <- comorResult %>% subset(pimCount>0) %>% summarise(sum=sum(cbdCount))
ctp
ccs
head(newtt)
final <- cbind(newtt,comorResult)
final
saveRDS(final,"realFinal.rds")
realFinal <- readRDS("realFinal.rds")
realFinal

# conditionStatic <- getCondtionCountStatic(comobidityListXlsx)
# conditionStatic %>% arrange(desc(count))

conditionStaticList <- getConditionListStatic(conditionListXlsx)
conditionStaticList

setGlobalConditionList(conditionStaticList)

getComorCountPerList <- function(tt){
  pt <- tt %>% select(pimCount,cbdCount,cbdList) %>% subset(pimCount>0&cbdCount>0)
  pt <- pt %>% select(cbdList)
  ptSplit <- getCondtionSplitCount(pt)
  ptc <- getConditionStaticCount(ptSplit)
  ptc <- as.data.frame(ptc)
  ptc
  
  npt <- tt %>% select(pimCount,cbdCount,cbdList) %>% subset(pimCount==0&cbdCount>0)
  npt <- npt %>% select(cbdList)
  nptSplit <- getCondtionSplitCount(npt)
  nptc <- getConditionStaticCount(nptSplit)
  nptc <- as.data.frame(nptc)
  nptc
  
  names(ptc) <- c("conditionList","pimCount")
  names(nptc) <- c("conditionList","noPimCount")
  rl <- merge(ptc,nptc,by='conditionList',all=T)
  return(rl)
}
### 전체  - 패스
repim <- realFinal
print(repim %>% subset(cbdCount>0) %>% summarise(n()))
repim %>% subset(cbdCount>0)

repim <- realFinal %>% subset(pimCount>0 & cbdCount>0)
repim <- realFinal %>% subset(pimCount==0 & cbdCount>0)
repim
print(repim %>% summarise(sum(cbdCount)))

# a18 <- getComorbidityColumn2(repim)
# a18
# print(a18 %>% summarise(sum(cbdCount)))
# 
# gogo <- a18 %>% summarise(sum(cbdCount))

cc <- repim %>% select(cbdList)
ccc <- cc %>% subset(cbdList!='x')
ccc

ptSplit <- getCondtionSplitCount(ccc)
head(ptSplit)

ps <- ptSplit %>% subset(cbdList!='x')
print(ptSplit %>% summarise(n()))

ptc <- getConditionStaticCount(ps)
ptc

write.xlsx(ptc,"newTotalComor3.xlsx")

### 진단 - 남자
mt <- repim %>% subset(GENDER=="M")
mc <- getComorCountPerList(mt)
mc
write.xlsx(mc,"manComor.xlsx")

### 진단 - 여자
wt <- repim %>% subset(GENDER=="F")
wc <- getComorCountPerList(wt)
write.xlsx(wc,"womanComor.xlsx")

# 만성질환 데이터 전처리 (funciton_set.R로 옮길 예정 )
totalTable <- readRDS("withoutPim.rds")
totalTable

conditionListXlsx <- getInformXlsx("Comorbidity_List.xlsx")
conditionTotal <- totalTable %>% select(CONDITION_CONCEPT_ID)

splitAndReturnDf <- function(x){
  splitS <- unlist(strsplit(x,","))
  return(splitS)
}


clx <- conditionListXlsx %>% select(condition_concept_id)
clx
chChr <- function(x){
  temp <- as.character(x)
  temp <- trimws(temp)
  return(temp)
}

clx<-apply(clx,1,chChr)
head(clx)
comorbidityList <- splitAndReturnDf(clx)
comorbidityList <- as.data.frame(comorbidityList)
names(comorbidityList) <- c("id")
head(comorbidityList)
comorbidityList

comorbidityList <- apply(comorbidityList,1,chChr)
comorbidityList <- data.frame(comorbidityList)
names(comorbidityList) <- c("id")
comorbidityList

sf <- function(x){
  count <-0
  trimStr <- trimws(as.character(x))
  tempDf <- splitAndReturnDf(trimStr)
  for(i in 1:length(tempDf)){
    index <- which(comorbidityList== tempDf[[i]])
    if(length(index) >0){
      count <- count +1
    }
  }
  print(count)
  return(count)
  # result <- grepSearch(t)
  # return(result)
}

cbdDf <- apply(totalTable,1,sf)
cbdDf <- as.data.frame(cbdDf)
names(cbdDf) <- c("cbdCount")
cbdDf <- as.data.frame(cbdDf)

saveRDS(cbdDf,"cormorbidity.rds")
cbdDf <- readRDS("cormorbidity.rds")
names(cbdDf) <- c("cbdCount")



#만성질환 리스트 생성하기
totalTable <- readRDS("pimTable.rds")
tt <- totalTable
tc <- tt %>% summarise(sum(cbdCount))
tc

inputTable <- totalTable %>% select(CONDITION_CONCEPT_ID)


cbdList <- getComorbidityColumn(inputTable)

#만성질환 리스트 계산 테스트===================================

tyt <- readRDS("")
tyt

flag="comorbidity"
setRdsEachYear(tyt,flag,1998,2018)

content = "comorbidity"

clacAndSaveComorbidity(content,1998,2000)
clacAndSaveComorbidity(content,2001,2014)
clacAndSaveComorbidity(content,2015,2018)

#만성질환 통계 =================================
#데이터 결합 (전체 데이터 성별 + 만성질환 카운트)
totalWithGender <- totalTable %>% filter(trimws(GENDER))
totalWithGender <- cbind(totalWithGender,cbdDf)
totalWithGender
totalTable
#전체 대상 (만성질환 으로만 통계 가능)

## 최종 테이블 (만성질환 /PIM ) 포함 테이블 불러와서 시작
totalTable <- readRDS("totalResultTable.rds")
cbdDf<- totalTable
totalTable
##===처방전당 숫자 숫자 계산=====================================================
pt  <- totalTable %>% subset(pimCount>0)
c <- pt %>% summarise(count = n())
c
ptc  <- totalTable %>% subset(pimCount>0& cbdCount==0)
cc <- ptc %>% summarise(count =n())
cc

npt <- totalTable %>% subset(pimCount==0)

mpt <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="M")
mnpt<- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="M")

wpt <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="F")
wnpt<- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="F")
#================================================================================

sdCbdTotal <- pt %>% summarise(sd = sd(cbdCount))
sdCbdTotal
meanCbdTotal <- pt %>% summarise(mean = mean(cbdCount))
meanCbdTotal

comordityCountStatic <- getComorbidityCountStatic(pt)
comordityCountStatic

sdCbdTotal <- npt %>% summarise(sd = sd(cbdCount))
sdCbdTotal
meanCbdTotal <- npt %>% summarise(mean = mean(cbdCount))
meanCbdTotal

comordityCountStatic <- getComorbidityCountStatic(cbdDf)
comordityCountStatic


#남자
manTotal <- totalWithGender %>% filter(trimws(GENDER)=="M")
manTotal <- manTotal %>% select(cbdCount)
sdCbdMan <- manTotal %>% summarise(sd = sd(cbdCount))
sdCbdMan
meanCbdToMan <- manTotal %>% summarise(mean = mean(cbdCount))
meanCbdToMan

comordityManCountStatic <- getComorbidityCountStatic(manTotal)
comordityManCountStatic
#여자
womanTotal <- totalWithGender %>% filter(trimws(GENDER)=="F")
womanTotal <- womanTotal %>% select(cbdCount)

sdCbdWoman <- womanTotal %>% summarise(sd = sd(cbdCount))
sdCbdWoman
meanCbdToWoman <- womanTotal %>% summarise(mean = mean(cbdCount))
meanCbdToWoman

comordityWomanCountStatic <- getComorbidityCountStatic(womanTotal)
comordityWomanCountStatic

#pim 통계 -( 각 처방전이 pim에 해당되도록 하는 약물 수 파악  )
# saveRDS(addyearTotal,"addYearTotal0315.rds")

addyearTotal <- totalTable %>% group_by(PERSON_ID,CONDITION_START_DATE,AGE,CONDITION_CONCEPT_ID,CONDITION_COUNT,DRUG_CONCEPT_ID,DRUG_COUNT,GENDER)%>% summarise(year = as.numeric(format(CONDITION_START_DATE,'%Y')))

addyearTotal <- readRDS("addYearTotal0315.rds")
addyearTotal <- ungroup(addyearTotal)
addyearTotal

content = "pim"

clacAndSavePim(content,1999,2005)
clacAndSavePim(content,2005,2010)
clacAndSavePim(content,2011,2015)
clacAndSavePim(content,2016,2018)

# pimRows <- getPimBindRows(1999,2018) # bind all


#PIM mean sd by pimcount
saveRDS(total,"totalTableWithPim.rds")

withPimTotal <- readRDS("totalTableWithPim.rds")

totalSd <- withPimTotal %>% select(pimCount) %>% summarise(sd=sd(pimCount))
totalSd
totalMean <- withPimTotal %>% select(pimCount) %>% summarise(mean=mean(pimCount))
totalMean
pimTotal <- withPimTotal

manSd <- withPimTotal %>% filter(trimws(GENDER)=="M") %>% select(pimCount) %>% summarise(sd=sd(pimCount))
manMean <- withPimTotal %>% filter(trimws(GENDER)=="M") %>% select(pimCount) %>% summarise(mean=mean(pimCount))
manSd
manMean

womanSd <- withPimTotal %>% filter(trimws(GENDER)=="F") %>% select(pimCount) %>% summarise(sd=sd(pimCount))
womanMean <- withPimTotal %>% filter(trimws(GENDER)=="F") %>% select(pimCount) %>% summarise(mean=mean(pimCount))
womanSd
womanMean

pimTotal
pimManTotal <- pimTotal %>% filter(trimws(GENDER)=="M")
pimWomanTotal <- pimTotal %>% filter(trimws(GENDER)=="F")

pimsPerTotal <- getPimsPerPrescription(pimTotal)
pimsPerMan <- getPimsPerPrescription(pimManTotal)
pimsPerWoman <- getPimsPerPrescription(pimWomanTotal)

bind_cols(pimsPerTotal,pimsPerMan,pimsPerWoman)
#사람당 처방전 수 통계
pimTotal
totalPerson <- pimTotal %>% distinct(PERSON_ID) %>% summarise(n=n())
manPerson <- pimManTotal %>% distinct(PERSON_ID) %>% summarise(n=n())
womanPerson <- pimWomanTotal %>% distinct(PERSON_ID) %>% summarise(n=n())

# 연도별 처방전 건수 분류
totalTable <- readRDS("totalTableWithPim.rds")
totalTable <- totalTable %>% subset(pimCount>0)
totalTable

totalYear <- totalTable %>% group_by(year) %>% summarise(total = n())

manYear <- totalTable %>% filter(trimws(GENDER)=="M") %>% group_by(year) %>% summarise(man = n())
manYear <- manYear %>% select(man)

staticYear <- cbind(totalYear,manYear)
staticYear <- staticYear %>% group_by(year,total,man)%>%  summarise(woman = (total-man))
staticYear

# PIM 처방전에서 PIM에 해당하는 약물 통계
# totalPim <- readRDS("onlyPimTable.rds")
totalPim <- totalPim %>% select(pimList)
totalPim
splitPim <- splitAndReturnDf(totalPim)

# 테이블2 pim 별로 사용된 수
pimDf <- readRDS("totalResultTable.rds")
pimDf
str(pimDf)

#total
pimPerDescription <- getPimPerDscription(pimDf)
totalPPD <- pimPerDescription
startPPD <- totalPPD %>% select(name,count)
startPPD

tempPPD <- data.frame()
tempPPD
count <- 0
for(i in 1999:2018){
  yearPPD <- pimDf %>% subset(year==i)
  resultPPD <- getPimPerDscription(yearPPD)
  names(resultPPD) <- c('name',i)
  if(count == 0){
    tempPPD <- resultPPD
    cat("==",i,"== NO MERGE ==",count,"\n")
    print(head(tempPPD))
  }else{
    tempPPD <- merge(tempPPD,resultPPD,key='name',all=T)
    cat("==",i,"== OK MERGE ==",count,"\n")
    print(head(tempPPD))
  }
  count <- count +1
}
tempPPD

lotationPPD <- t(tempPPD)
head(lotationPPD)
View(lotationPPD)

test <- lotationPPD %>% select(phenobarbital)

head(tempPPD)

write.xlsx(lotationPPD,"lotationPim.xlsx")
write.xlsx(tempPPD,"pimByYear.xlsx")

#man
pimManDf <- pimDf %>% subset(trimws(GENDER)=="M")
pimPerDescription <- getPimPerDscription(pimManDf)
manPPD <- pimPerDescription

#woman
pimWomanDf <- pimDf %>% subset(trimws(GENDER)=="F")
pimPerDescription <- getPimPerDscription(pimWomanDf)
pimPerDescription[is.na(pimPerDescription)]<-0
count <- pimPerDescription %>% filter(!is.na(count)) %>%  summarise(sum=sum(count))
count
womanPPD <- pimPerDescription

totalPPD
manPPD
womanPPD

head(totalPPD)
head(manPPD)
head(womanPPD)

pimCountListPerYear <- list()
getPimCountPerYear <- function(start,end){
  for(i in start:end){
    yearDf <- pimDf %>% subset(year==i)
    yearDf <- getPimPerDscription(yearDf)
    
  }  
}

# 만성질환 횟수 결합하기
combineResult <- getCombineComorbidityDataframes(1998,2018)
tt <- cbind(tyty,combineResult)
saveRDS(tt,"withoutPimFinal.rds") #만성질환 붙이고 저장

pt <- readRDS("pimTable.rds")


pt <- pt %>% select(PERSON_ID,AGE,CONDITION_START_DATE,CONDITION_CONCEPT_ID,CONDITION_COUNT,DRUG_COUNT,
                    DRUG_CONCEPT_ID,GENDER,pimList,pimCount,year,cbdCount,cbdList)
head(pt)
head(tt)
pt$year <- as.character(pt$year)
str(pt)
str(tt)

final <- rbind(tt,pt)
#== pvalue 구하기 만성질환 
totalTable <- readRDS("finalTotalTable.rds")
totalTable

totalComorList <- read.xlsx("mw.xlsx",1)
totalComorList
comorP <- getComorbidityPvalue(totalComorList)
comorP

write.xlsx(comorP,"comorP2.xlsx")

## 
totalTable <- readRDS("finalTotalTable.rds")
totalTable

withPim  <- totalTable %>% subset(pimCount>0) %>% select(AGE)
withoutPim <- totalTable %>% subset(pimCount==0) %>% select(AGE)

wp <- withPim$AGE
wop <- withoutPim$AGE

wp
wop

var.test(wp,wop)
t.test(wp, wop,  paired = FALSE, var.equal = TRUE, conf.level = 0.95)


mWithPim <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="M") %>% select(AGE)
mWithoutPim <- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="M") %>% select(AGE)
wp <- mWithPim$AGE
wop <- mWithoutPim$AGE
var.test(wp,wop)
t.test(wp, wop,  paired = FALSE, var.equal = TRUE, conf.level = 0.95)

wWithPim <- totalTable %>% subset(pimCount>0) %>% subset(GENDER=="F") %>% select(AGE)
wWithoutPim <- totalTable %>% subset(pimCount==0) %>% subset(GENDER=="F") %>% select(AGE)
wp <- wWithPim$AGE
wop <- wWithoutPim$AGE
var.test(wp,wop)
t.test(wp, wop,  paired = FALSE, var.equal = TRUE, conf.level = 0.95)

getVtest <- function(wp,wop){
  var.test(wp,wop)
}

getTtest <- function(wp,wop){
  t.test(wp, wop,  paired = FALSE, var.equal = TRUE, conf.level = 0.95)
}

repim

pimRepim <- repim %>% subset(pimCount==0)

my = getCount1234(pimRepim)
my
write.xlsx(my,"cbd1234nopim.xlsx")
r<-data.frame()
ip <- repim %>% subset(pimCount>0)
pm <- ip %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
pm
pp <- repim %>% subset(pimCount>0) %>% select(cbdCount)
pp <- pp$cbdCount

inp <- repim %>% subset(pimCount==0)
npm <- inp %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
npm
npp <- repim %>% subset(pimCount==0) %>% select(cbdCount)
npp <- npp$cbdCount

getVtest(pp,npp)
getTtest(pp,npp)


imp <- repim %>% subset(pimCount>0&GENDER=="M")
pm <- ip %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
pm

imnp <- repim %>% subset(pimCount==0&GENDER=="M")
npm <- inp %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
npm

iwp <- repim %>% subset(pimCount>0&GENDER=="F")
pm <- ip %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
pm

iwnp <- repim %>% subset(pimCount==0&GENDER=="F")
npm <- inp %>% summarise(m = round(mean(cbdCount),3),s= round(sd(cbdCount),3))
npm

getCbdPvalue(repim)
getAgePvalue(repim)

pp <- repim %>% subset(pimCount>0)
np <- repim %>% subset(pimCount==0)

write.xlsx(getCount5(pp),"p5.xlsx")
write.xlsx(getCount5(np),"np5.xlsx")

# === 약물 5개 이상 미만에 대한 카이 스퀘어
chisq=function(o) {
  row.sum=apply(o,1,sum)
  col.sum=apply(o,2,sum)
  n=sum(o)
  e=(row.sum %*% t(col.sum))/n
  x=sum(((o-e)^2/e))
  Observed=o
  Expected=e
  Stat=c('chisq'=x,'p-value'=pchisq(x,df=((nrow(o)-1)*(ncol(o)-1)),lower.tail=F))
  return(list(Observed=Observed,Expected=Expected,Stat=Stat))
}

mc <-function(a,b,c,d){
  df1 <- c(a,b)
  df2 <- c(c,d)
  
  df3 <- cbind(df1,df2)
  df3 <- as.matrix(df3)
  chisq(df3)  
}

mc(47176,33448,598826,421608) # 하드코딩함
mc(50141,38467,640353,492329)

#약물 5개 이상 미만에 대한 평균 표준편차
ip <- repim %>% subset(pimCount >0) %>% select(DRUG_COUNT)
pm <- ip %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
pm
pp <- repim %>% subset(pimCount>0) %>% select(DRUG_COUNT)
pp

inp <- repim %>% subset(pimCount==0) %>% select(DRUG_COUNT)
npm <- inp %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
npm
npp <- repim %>% subset(pimCount==0) %>% select(DRUG_COUNT)
npp 

getVtest(pp$DRUG_COUNT,npp$DRUG_COUNT)
getTtest(pp$DRUG_COUNT,npp$DRUG_COUNT)

imp <- repim %>% subset(pimCount>0&GENDER=="M") %>% select(DRUG_COUNT)
pm <- imp %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
pm


imnp <- repim %>% subset(pimCount==0&GENDER=="M") %>% select(DRUG_COUNT)
npm <- imnp %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
npm

getVtest(imp$DRUG_COUNT,imnp$DRUG_COUNT)
getTtest(imp$DRUG_COUNT,imnp$DRUG_COUNT)

iwp <- repim %>% subset(pimCount>0&GENDER=="F") %>% select(DRUG_COUNT)
pm <- iwp %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
pm

iwnp <- repim %>% subset(pimCount==0&GENDER=="F") %>% select(DRUG_COUNT)
npm <- iwnp %>% summarise(m = round(mean(DRUG_COUNT),3),s= round(sd(DRUG_COUNT),3))
npm

getVtest(iwp$DRUG_COUNT,iwnp$DRUG_COUNT)
getTtest(iwp$DRUG_COUNT,iwnp$DRUG_COUNT)

getYearCountTable1 <- function(df){
  gy <- function(f){
    r <- f %>% group_by(year) %>% summarise(count = n())
    tt <- r %>% summarise(sum = sum(count))
    r <- ungroup(r)
    # ra <- r %>% group_by(year) %>% summarise(avg = round(count/tt$sum*100,3 ))
    # r <- cbind(r,ra,by='year')
    return(r)
  }
  pf <- df %>% subset(pimCount>0)
  npf <- df %>% subset(pimCount==0)
  py <- gy(pf)
  npy <- gy(npf)
  result <- merge(py,npy,by='year')
  
  mpf <- df %>% subset(pimCount>0 & GENDER=="M")
  mnpf <- df %>% subset(pimCount==0 & GENDER=="M")
  mpy <- gy(mpf)
  mnpy <- gy(mnpf)
  result <- merge(result,mpy,by='year')
  result <- merge(result,mnpy,by='year')
  
  wpf <- df %>% subset(pimCount>0 & GENDER=="F")
  wnpf <- df %>% subset(pimCount==0 & GENDER=="F")
  wpy <- gy(wpf)
  wnpy <- gy(wnpf)
  result <- merge(result,wpy,by='year')
  result <- merge(result,wnpy,by='year')
  
}
r <- getYearCount(repim)
r 



yp <- r[,6:7]

yp <- as.matrix(yp)

pp <- chisq(yp)
pp <- format(pp$Stat[2],scientific=F)
pp <- as.numeric(pp)
pp # 년도별 P value 계산

#========== train
df2 <- data.frame(name=c("abc","jmh","abc"),age=c(30,36,31),grade=c(1,2,3))

df2$grade <- ifelse(df2$grade >= 3,"333",
                    ifelse(df2$grade >= 2,"222",
                           ifelse(df2$grade >= 1,"111",
                                  "???"))
                    )


#=== study
state.x77

state <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
state

fit = lm(Murder~ Population + Illiteracy + Income + Frost, data=state)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
install.packages("car",dependencies=T)
library(car)
vif(fit)
sqrt(vif(fit))

fit
#### 이상 관측치 ####
influencePlot(fit,id.method="identify")

#### 회귀모형의 교정 ####
state
t <- powerTransform(state$Murder)
summary(t)

boxTidwell(Murder~ Population + Illiteracy, data=state)

ncvTest(fit)
spreadLevelPlot(fit)

#### orc #####

total <- readRDS("realFinal.rds")
org <- total
str(org)
#########################
getOr <- function(df,str){
  print("PIM 데이터 더미변환 - 기준 NO PIM")
  df$pimCount <- ifelse(df$pimCount > 0,1,0) #pim 1 / no pim 0
  
  print("성별 데이터 더미변환 - 기준 WOMEN")
  df$GENDER <- ifelse(df$GENDER =="M",1,0) #F 1 / M 0 
  df$GENDER <- as.factor(df$GENDER)
  
  print("나이 데이터 더미변환 기준 - 65~69세")
  df$AGE <- ifelse(df$AGE>=65&df$AGE<=69,0,
                    ifelse(df$AGE>=70&df$AGE<=74,1,
                           ifelse(df$AGE>=75&df$AGE<=79,2,
                                  ifelse(df$AGE>=80&df$AGE<=84,3,
                                         ifelse(df$AGE>=85,4,
                                                5)))))
  
  df$AGE <-  as.factor(df$AGE)
  
  print("만성질환 데이터 더미변환 기준 - 질환수 0개")
  df$cbdCount <- ifelse(df$cbdCount==0,0,
                         ifelse(df$cbdCount==1,1,
                                ifelse(df$cbdCount==2,2,
                                       ifelse(df$cbdCount==3,3,
                                              ifelse(df$cbdCount >= 4,4
                                                     ,5)))))
  df$cbdCount <- as.factor(df$cbdCount)
  
  print("약물 데이터 더미변환 기준 - 약물 수 5개 미만")
  
  df$DRUG_COUNT <- ifelse(df$DRUG_COUNT>0&df$DRUG_COUNT<5,0,
                          ifelse(df$DRUG_COUNT>=5,1,2))
                                 
  df$DRUG_COUNT <- as.factor(df$DRUG_COUNT)
  
  print("데이터 변환 완료")
  

  # print("성별 - 로지스틱 회귀분석")
  # orGender <- glm(pimCount~GENDER, family=binomial, data=df)
  # print("OR 계산")
  # orG <- extractOR(orGender)
  # print("완료")
  
  print("나이 - 로지스틱 회귀분석")
  orAge <- glm(pimCount~AGE, family=binomial, data=df)
  print("OR 계산")
  orA <- extractOR(orAge)
  print("완료")
  
  print("만성질환- 로지스틱 회귀분석")
  orCbd <- glm(pimCount~cbdCount, family=binomial, data=df)
  print("OR 계산")
  summary(orCbd)
  orC <- extractOR(orCbd)
  print("완료")
  
  print("약물 수 - 로지스틱 회귀분석")
  orDrug<- glm(pimCount~DRUG_COUNT, family=binomial, data=df)
  print("OR 계산")
  orD <- extractOR(orDrug)
  print("완료")
  
  ###
  print("결과 종합")
  orG <- orG[-c(1),]
  orA <- orA[-c(1),]
  orC <- orC[-c(1),]
  orD <- orD[-c(1),]
  
  print("바인딩")
  orResult <- rbind(orG,orA,orC,orD)
  orResult$p <- round(orResult$p,3)
  print("P값 정리")
  orResult$p <- ifelse(orResult$p<0.0001,"<0.0001",orResult$p)
  orResult
  print("결과 포맷팅")
  kk <- orResult %>% mutate(result = paste0(OR," (",lcl,"~",ucl,") (",p,")"))
  row.names(kk) <- row.names(orResult)
  saveName <- paste0(str,".xlsx")
  print("엑셀 저장")
  write.xlsx(kk,saveName)
  print("최종 완료")
  kk
}

org
getOr(org,"tOR")

man <- total %>% subset(GENDER=="M")
getOr(man,"mOR")

women <- total %>% subset(GENDER=="F")
getOr(women,"wOR")

# fit <- glm(formula=pimCount~GENDER+AGE, family=binomial, data=org)
# require(moonBook)
# extractOR(fit)
# summary(fit)
# ORplot(fit,type=2,show.CI=TRUE,xlab="This is xlab",main="Main Title")
# 
# f1 <- glm(formula=pim~ gender,family=binomial,data=org)
# 
# summary(f1)
# f2 <- glm(formula=pim~ gender,family=quasibinomial,data=org)
# pchisq(summary(f2)$dispersion*f1$df.residual,f1$df.residual,lower=F)
# plot()
# ORplot(orgFit,show.CI=T,main="plot")
# 
# require(survival)
# data(colon)
# head(colon)
# out1=glm(status~sex+age+rx+obstruct+node4+nodes+extent,data=colon)
# summary(out1)
# extractOR(out1)
# out2=glm(status~rx+node4,data=colon)
# out1
# ORplot(out1,type=2,show.CI=TRUE,xlab="This is xlab",main="Main Title")
# ORplot(out2,type=1,main="Main Title")

#### 만성질환 별 로지스틱 회귀 분석 시작 ####



#### 만성질환 인덱스랑 이름 구분 ####
cbdFull <- getComorbidityFullList()
cbdFull
cf <- cbdFull %>% select(id)
cf
comorList <- getComorbidityIndexAndName(cbdFull)
comorList

total <- readRDS("realFinal.rds")
total
cbd <- total %>% select(cbdCount, cbdList)
cbd



names(test) <- c("count","cbdList")

setComorFlag <- function(df){
  comorFlag <- data.frame(ast=c(0),arr=c(0),hea=c(0),cop=c(0),isc=c(0),
                          dep=c(0),dia=c(0),hyperl=c(0),hypert=c(0),liv=c(0),
                          lym=c(0),met=c(0),myo=c(0),osteoa=c(0),osteop=c(0),
                          ren=c(0),sol=c(0),str=c(0))
  
  count <- df['cbdCount']
  count <- as.numeric(count)
  cat("[",count,"] \n")
  if(count>0){
    b <- df['cbdList']
    sp <- strsplit(b,",")
    sp <- as.data.frame(unlist(sp))
    len <- length(sp[,1])
    for(i in 1:len){
      idx <- which(cbdFull$id==trimws(sp[i,1]))
      temp <- comorList %>% subset(idx >=start & idx<=end)
      index <- temp$index
      comorFlag[index] <- 1
      cat("index : ",index," - ",temp$name," \n")
    }
  }else{
  }
  return(comorFlag)
}

test <- total %>% select(cbdList,cbdCount)
str(test)
count(test)

result <- apply(test,1,setComorFlag)
result

mylist = do.call("rbind",result)

count(mylist)
View(mylist)

count(mylist)
count(total)

total<-cbind(total,mylist)
str(total)

### 만성질환 로지스틱 회귀 분석 OR값 구하고 리턴 ###
getComorOr <- function(df,gd,str){ # gd= "T","M","F" 으로 구분
  df<- df %>% select(pimCount,GENDER,14:31)
  if(gd =="M"){
    print("남자 기준 로지스틱 회귀 시작")
    df <- df %>% subset(GENDER=="M")
    print(paste0("데이터 수 : ",count(df)))
  }else if(gd =="F"){
    print("여자 기준 로지스틱 회귀 시작")
    df <- df %>% subset(GENDER=="F")
    print(paste0("데이터 수 : ",count(df)))
  }else{
    print("남여 전체 기준 로지스틱 회귀 시작")
    print(paste0("데이터 수 : ",count(df)))
  }
  
  print("PIM 데이터 더미변환 - 기준 NO PIM")
  df$pimCount <- ifelse(df$pimCount > 0,1,0) #pim 1 / no pim 0
  
  print("성별 데이터 더미변환 - 기준 WOMEN")
  df$GENDER <- ifelse(df$GENDER =="M",1,0) #F 1 / M 0 
  df$GENDER <- as.factor(df$GENDER)
  
  print("만성질환 데이터 더미변환 기준 - Asthma")
  print("데이터 변환 완료")
  
  
  # print("성별 - 로지스틱 회귀분석")
  # orGender <- glm(pimCount~GENDER, family=binomial, data=df)
  # print("OR 계산")
  # orG <- extractOR(orGender)
  # print("완료")
  
  # print("나이 - 로지스틱 회귀분석")
  # orAge <- glm(pimCount~AGE, family=binomial, data=df)
  # print("OR 계산")
  # orA <- extractOR(orAge)
  # print("완료")
  
  print("만성질환 별 - 로지스틱 회귀분석 시작")
  df <- df %>% select(pimCount,3:20)
  rdf <- NULL
  for(i in 2:19){
    colName <- colnames(df)[i]
    cat(colName," 분석 \n")
    rg <- glm(pimCount~df[,i], family=binomial, data=df)
    cat("OR 계산\n")
    summary(rg)
    org <- extractOR(rg)
    org <- org[-c(1),]
    cat("완료\n") 
    if(i==2){
      rdf <- org
    }else{
      rdf <- rbind(rdf,org)
    }
  }
  rdf
  
  # print("약물 수 - 로지스틱 회귀분석")
  # orDrug<- glm(pimCount~DRUG_COUNT, family=binomial, data=df)
  # print("OR 계산")
  # orD <- extractOR(orDrug)
  # print("완료")
  
  ###
  print("결과 종합")
  # orG <- orG[-c(1),]
  # orA <- orA[-c(1),]
  # orC <- orC[-c(1),]
  # orD <- orD[-c(1),]
  
  # print("바인딩")
  # orResult <- rbind(orG,orA,orC,orD)
  # orResult$p <- round(orResult$p,3)
  print("P값 정리")
  orResult <- rdf
  orResult$p <- ifelse(orResult$p<0.0001,"<0.0001",orResult$p)
  orResult
  print("결과 포맷팅")
  finalResult <- orResult %>% mutate(result = paste0(OR," (",lcl,"~",ucl,") (",p,")"))
  row.names(kk) <- row.names(orResult)
  # saveName <- paste0(str,".xlsx")
  # print("엑셀 저장")
  # write.xlsx(kk,saveName)
  print("최종 완료")
  finalResult
  return(finalResult)
}

### 데이터 변환 Table 3 용 ###
setFlagDataframe <- function(df){
  
  print("PIM 데이터 더미변환 - *기준 NO PIM")
  df$pimCount <- ifelse(df$pimCount > 0,1,0) #pim 1 / no pim 0
  
  print("성별 데이터 더미변환 - *기준 WOMEN")
  df$GENDER <- ifelse(df$GENDER =="M",1,0) #F 1 / M 0 
  print("성별 데이터 Factor 변환")
  df$GENDER <- as.factor(df$GENDER)
  
  print("나이 데이터 더미변환 *기준 - 65~69세")
  df$AGE <- ifelse(df$AGE>=65&df$AGE<=69,0,
                   ifelse(df$AGE>=70&df$AGE<=74,1,
                          ifelse(df$AGE>=75&df$AGE<=79,2,
                                 ifelse(df$AGE>=80&df$AGE<=84,3,
                                        ifelse(df$AGE>=85,4,
                                               5)))))
  print("나이 데이터 Factor 변환")
  df$AGE <-  as.factor(df$AGE)
  
  print("만성질환 데이터 더미변환 *기준 - 질환수 0개")
  df$cbdCount <- ifelse(df$cbdCount==0,0,
                        ifelse(df$cbdCount==1,1,
                               ifelse(df$cbdCount==2,2,
                                      ifelse(df$cbdCount==3,3,
                                             ifelse(df$cbdCount >= 4,4
                                                    ,5)))))
  print("만성질환 수 데이터 Factor 변환")
  df$cbdCount <- as.factor(df$cbdCount)
  
  print("처방 약물 수 데이터 더미변환 *기준 - 5개미만")
  
  df$DRUG_COUNT <- ifelse(df$DRUG_COUNT>0&df$DRUG_COUNT<5,0,
                          ifelse(df$DRUG_COUNT>=5,1,2))
  
  print("처방 약물 수 데이터 Factor 변환")
  df$DRUG_COUNT <- as.factor(df$DRUG_COUNT)
  print("변환 완료")
  
  return(df)
  
}
### 만설질환 OR 출력을 위해 포맷팅 ###
getFormatComorRo <- function(x){
  cn <- colnames(total %>% select(14:31))
  cn
  rownames(x) <- cn  
  ro <- x %>% select(result)
  newRo <- NULL
  for(i in 1:18){
    newRo <- rbind(newRo,c(1))
    newRo <- rbind(newRo,c(1))
    newRo <- rbind(newRo,ro[i,1])
  }
  return(newRo)
}

### 만설질환 OR 호출 ###
resultOR <- getComorOr(total,"T","test")
resultManOr <- getComorOr(total,"M","test")
resultWomenOr <- getComorOr(total,"F","test")

### 포맷팅 호출 ###
ftor <- getFormatComorRo(resultOR)
fmor <- getFormatComorRo(resultManOr)
fwor <- getFormatComorRo(resultWomenOr)

finalOr <- ftor
finalOr <- cbind(finalOr,fmor)
finalOr <- cbind(finalOr,fwor)
colnames(finalOr) <- c("total","man","women")

### 엑셀에 저장 ###
write.xlsx(finalOr,"comorOr1st.xlsx")

### 로지스틱 회귀 분석 다중 변수에 대해 적용 ###
multiGlm <- function(df,gen,str){
  if(gen=="T"){
    print("성별 전체 대상")
    print("로지스틱 회귀분석 시작")
    rg <- glm(pimCount~GENDER+AGE+cbdCount+ast+arr+hea+cop+isc+dep+dia+hyperl+hypert+lym+met+myo+osteoa+ren+sol+str,
              family=binomial, data=df)  
  }else{
    if(gen=="M"){
      print("남자 대상 데이터 추출")
      df <- df %>% subset(GENDER=="1")
      print("로지스틱 회귀분석 시작")
      rg <- glm(pimCount~AGE+cbdCount+ast+arr+hea+cop+isc+dep+dia+hyperl+hypert+liv+lym+met+myo+osteoa+ren+sol+str, family=binomial, data=df)    
    }else{
      print("여자 대상 데이터 추출")
      df <- df %>% subset(GENDER=="0")
      print("로지스틱 회귀분석 시작")
      rg <- glm(pimCount~AGE+cbdCount+ast+arr+hea+cop+isc+dep+dia+hyperl+hypert+met+myo+osteoa+ren+sol+str, family=binomial, data=df)    
    }
    
  }
  
  summary(rg)
  print("분석완료")
  print("포맷팅 시작")
  org <- getOr(rg)
  print("포맷팅 완료")
  saveName <- paste0(str,".xlsx")
  print("엑셀 저장")
  write.xlsx(org,saveName)
  return(org)
}
### oR값 구하고 지수 소수점으로 포맷팅 ###
getOr <- function(df){
  eo <- extractOR(df)
  orResult <- eo
  orResult$p <- ifelse(orResult$p<0.0001,"<0.0001",orResult$p)
  orResult
  kk <- orResult %>% mutate(result = paste0(OR," (",lcl,"~",ucl,") (",p,")"))
  row.names(kk) <- row.names(orResult)
  return(kk)
}

### 다중 변수에 대한 로지스틱 회귀 분석 호출 ###
oraTable <- total
oraTable <- setFlagDataframe(oraTable)
ora <- oraTable %>% select(pimCount,GENDER,AGE,cbdCount,14:31)
backwardOra <- oraTable %>% select(pimCount,GENDER,AGE,DRUG_COUNT,cbdCount,14:31)

multiOrTotal <- multiGlm(ora,"T","tOra")
multiOrMan <- multiGlm(ora,"M","manOra")
multiOrWomen <- multiGlm(ora,"F","womenOra1")

### pim's per P value ###

setpim <- function(x){
  pim <- x[1]
  man <- x[2]
  women <- x[3]
  man <- sub(",","",strsplit(man,' ')[[1]][[1]])
  women <- sub(",","",strsplit(women,' ')[[1]][[1]])
  man <- as.numeric(man)
  women <- as.numeric(women)
  return(data.frame(pims=c(pim),man=c(man),women=c(women)))
}

indPim <- read.xlsx("individualPim.xlsx",1)
result <- apply(indPim,1,setpim)
result <- do.call("rbind",result)
result

### backward ROA ###
str(backwardOra)

rg <- glm(pimCount~GENDER+AGE+cbdCount+DRUG_COUNT+ast+arr+hea+cop+isc+dep+dia+hyperl+hypert+liv+lym+met+myo+osteoa+osteop+ren+sol+str,
          family=binomial, data=backwardOra) 
summary(rg)

reduce.model = step(rg)

summary(reduce.model)

eo <- extractOR(reduce.model)
#################################################

getCh <- function(df,gen){
  
  if(gen=="T"){
    total <- df
  }else if(gen=="M"){
    total <- df %>% subset(GENDER=="M")
  }else if(gen=="F"){
    total <- df %>% subset(GENDER=="F")
  }
  
  pimok <- total %>% subset(pimCount>0)
  count(pimok)
  pimokhj <- pimok %>% group_by(PERSON_ID) %>% summarise(count=n())
  test <- pimokhj %>% summarise(n=n())
  
  
  pimno <- total %>% subset(pimCount==0)
  pimnohj <- pimno %>% group_by(PERSON_ID) %>% summarise(count=n())
  test1 <- pimnohj %>% summarise(n=n())
  
  cat("count - pimOk : ",count(pimok)$n," pimNo : ",count(pimno)$n,"\n")
  cat("hwanja - pimOK : ",test$n," pimNO : ",test1$n,"\n")
}

### 보완 사항. 전체 처방전/환자 수 명시 ###
getCh(total,"T")
getCh(total,"M")
getCh(total,"F")

