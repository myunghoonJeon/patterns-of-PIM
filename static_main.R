#function implementation
library("dplyr")
source("function_set.r")

# conn <- getDbConnection()
# sql <- getTotalTableQuerry()
# totalTable <- querySql(conn,sql)
# totalTable
# saveRDS(totalTable,"totalTable.rds")
conditionCountSumByGender <- getSumConditionCount(totalTable)
conditionCountSumByGender

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


# 나이 통계 (성별) 표준편차 / 평균
# 전체
totalTable <- readREDS("totalResultTable.rds")
meanAgeTotal <- totalTable %>% summarise(mean = round(mean(AGE),3))
sdAgeTotal <- totalTable %>% summarise(sd = round(sd(totalTable$AGE),3))
meanAgeTotal
sdAgeTotal

# 남자
ageManTalbe <- totalTable %>% filter(trimws(GENDER)=="M")
meanAgeMan <- ageManTalbe %>% summarise(mean = mean(AGE))
sdAgeMan <- ageManTalbe %>% summarise(sd=sd(AGE))
meanAgeMan
sdAgeMan

# 여자
ageWomanTable <- totalTable %>% filter(trimws(GENDER)=="F")
meanAgeWoman <- ageWomanTable %>% summarise(mean = mean(AGE))
sdAgeWoman <-  ageWomanTable %>% summarise(sd=sd(AGE))
meanAgeWoman
sdAgeWoman

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

#condition 통계 ( 특정한 만성질환에 대한 통계 시작)
totalTable <- readRDS("totalResultTable.rds")
tt <- totalTable %>% subset(cbdCount>0) %>% select(cbdList,pimList)
tt
comobidityListXlsx <- getInformXlsx("Comorbidity_List.xlsx")
comobidityListXlsx

conditionStatic <- getCondtionCountStatic(comobidityListXlsx)
conditionStatic %>% arrange(desc(count))

conditionStaticList <- getConditionListStatic(conditionListXlsx)
conditionStaticList

setGlobalConditionList(conditionStaticList)
### 전체 진단 
totalInput <- totalTable %>% select(CONDITION_CONCEPT_ID)
totalInput <- getCondtionSplitCount(totalInput)
getConditionStaticCount(totalInput)
### 진단 - 남자
manInput <- totalTable %>% filter(trimws(GENDER)=="M") %>% select(CONDITION_CONCEPT_ID)
manInput <- getCondtionSplitCount(manInput)
getConditionStaticCount(manInput)
### 진단 - 여자
womanInput <- totalTable %>% filter(trimws(GENDER)=="F") %>% select(CONDITION_CONCEPT_ID)
womanInput <- getCondtionSplitCount(womanInput)
womanInput
getConditionStaticCount(womanInput)

# 만성질환 데이터 전처리 (funciton_set.R로 옮길 예정 )
# totalTable <- readRDS("withoutPim.rds")
totalTable
conditionListXlsx <- getInformXlsx("Comorbidity_List.xlsx")
conditionTotal <- totalTable %>% select(CONDITION_CONCEPT_ID)

splitAndReturnDf <- function(x){
  splitS <- unlist(strsplit(x,","))
  return(splitS)
}
str(conditionListXlsx)

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
totalTable <- readRDS("withoutPim.rds")

inputTable <- totalTable %>% select(CONDITION_CONCEPT_ID)


cbdList <- getComorbidityColumn(inputTable)

#만성질환 리스트 계산 테스트===================================

tyt <- readRDS("withoutPim.rds")
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
##========================================================
sdCbdTotal <- cbdDf %>% summarise(sd = sd(cbdCount))
sdCbdTotal
meanCbdTotal <- cbdDf %>% summarise(mean = mean(cbdCount))
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

head(pt)
head(tt)
pt <- pt %>% 
final <- bind_rows(tt,pt) # 최종 결합

final
