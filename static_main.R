#function implementation
library("dplyr")
source("function_set.r")

# conn <- getDbConnection()
# 
# sql <- getTotalTableQuerry()
# 
# totalTable <- querySql(conn,sql)

conditionCountSumByGender <- getSumConditionCount(totalTable)
conditionCountSumByGender

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
meanAgeTotal <- totalTable %>% summarise(mean = mean(AGE))
sdAgeTotal <- totalTable %>% summarise(sd = sd(totalTable$AGE))
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
#약물 데이터 통계
drugStatus <- getDrugStatus(totalTable)
drugStatus

test <- drugStatic %>% filter(PERSON_ID==14084 & CONDITION_START_DATE=='2011-10-12')
test

splitDrug <- unlist(strsplit(test$DRUG_CONCEPT_ID,","))
splitDrug 
head(drug_inform)

#condition 통계 ( 특정한 만성질환에 대한 통계 시작)
conditionListXlsx <- getInformXlsx("condition_inform.xlsx")
conditionListXlsx

conditionStatic <- getCondtionCountStatic(conditionListXlsx)
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
l <- data.frame(list = c("195483,200054","196048","1203949"))
l <- conditionTotal
l

cbdDf <- apply(l,1,sf)
cbdDf <- as.data.frame(cbdDf)
names(cbdDf) <- c("cbdCount")
cbdDf <- as.data.frame(cbdDf)

saveRDS(cbdDf,"cormorbidity.rds")
cbdDf <- readRDS("cormorbidity.rds")
names(cbdDf) <- c("cbdCount")

#만성질환 통계 =================================
#데이터 결합 (전체 데이터 성별 + 만성질환 카운트)
totalWithGender <- totalTable %>% filter(trimws(GENDER))
totalWithGender <- cbind(totalWithGender,cbdDf)
totalWithGender
totalTable
#전체 대상 (만성질환 으로만 통계 가능)
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

setRdsByYear(addyearTotal,1999,2000)
setRdsByYear(addyearTotal,2001,2005)
setRdsByYear(addyearTotal,2006,2006)
# split1999to2009 <- addyearTotal %>% subset(year>=1999&year<=2009)
# split1999to2009
# split2010to2013 <- addyearTotal %>% subset(year>=2010&year<=2013)
# split2010to2013
# split2014to2015 <- addyearTotal %>% subset(year>=2014&year<=2015)
# split2014to2015
# split2016to2018 <- addyearTotal %>% subset(year>=2016)
# split2016to2018

pim1999to2009 <- split1999to2009 %>% select(DRUG_CONCEPT_ID)
saveRDS(pim1999to2009,"pim1999to2009.rds")

pim1999to2009 <- getPimInform(pim1999to2009)

pim1999to2009
View(pim1999to2009)
split2010to2013 <- split2010to2013 %>% select(DRUG_CONCEPT_ID)
split2010to2013 <- getPimInform(split2010to2013)

split2014to2015 <- split2014to2015 %>% select(DRUG_CONCEPT_ID)
split2014to2015 <- getPimInform(split2014to2015)

split2016to2018 <- split2016to2018 %>% select(DRUG_CONCEPT_ID)
split2016to2018 <- getPimInform(split2016to2018)

pimInput <- totalTable %>% select(DRUG_CONCEPT_ID)
pimInput <- pimInput %>% arrange(desc(DRUG_CONCEPT_ID))
pimInput
pimCount <- getPimInform(pimInput)
pimCount

head(test)
saveRDS("pimInfom.rds")
pimCount

# 연도별 처방전 건수 분류
totalTable <- readRDS("addYearTotal0315.rds")
totalTable

totalYear <- totalTable %>% group_by(year) %>% summarise(total = n())
manYear <- totalTable %>% filter(trimws(GENDER)=="M") %>% group_by(year) %>% summarise(man = n())
manYear <- manYear %>% select(man)
staticYear <- cbind(totalYear,manYear)
staticYear <- staticYear %>% group_by(year,total,man)%>%  summarise(woman = (total-man))
