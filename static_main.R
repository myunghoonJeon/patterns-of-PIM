#function implementation
library("dplyr")
source("function_set.r")

# conn <- getDbConnection()
# 
# sql <- getTotalTableQuerry()
# 
# totalTable <- querySql(conn,sql)
# 
# saveRDS(totalTable,"totalTable.rds")
# 
# getTotalTable <- readRDS("totalTable.rds")

# getTotalTable

# test <- getTotalTable %>% group_by(PERSON_ID,AGE,CONDITION_START_DATE) %>% 
#   summarise(CONDITION_CONCEPT_ID, 
#             CONDITION_COUNT = getConditionDrugCount(CONDITION_CONCEPT_ID),
#             DRUG_COUNT = getConditionDrugCount(DRUG_CONCEPT_ID),
#             DRUG_CONCEPT_ID,GENDER
#             )
# saveRDS(test,"finalTotalTable2.rds")

# test
# test2 <- test %>% ungroup()
# read database
totalTable <- readRDS("c:\\Git\\patterns-of-PIM\\finalTotalTable2.rds")
totalTable <- totalTable %>% ungroup()
# 성별에 따른 진단 수 
conditionCountSumByGender <- getSumConditionCount(totalTable)
conditionCountSumByGender

# 질병 진단 통계 표준편차/백분율
sdTotal <- totalTable %>% summarise(sd=sd(CONDITION_COUNT))
sdMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(CONDITION_COUNT))
sdWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(CONDITION_COUNT))

# 성별 통계 백분율
forGenderTable <- totalTable %>% select(AGE,GENDER)
ratioGender <- getRatioGender(forGenderTable)
ratioGender

# 나이 통계 (성별) 표준편차 / 백분율
sdAgeTotal <- totalTable %>% summarise(sd = sd(totalTable$AGE))
sdAgeMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(AGE))
sdAgeWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(AGE))
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
totalInput totalInput <- getCondtionSplitCount(totalInput)
getConditionStaticCount(totalInput)
### 진단 - 남자
manInput <- totalTable %>% filter(trimws(GENDER)=="M") %>% select(CONDITION_CONCEPT_ID)
manInput <- getCondtionSplitCount(manInput)
getConditionStaticCount(manInput)
### 진단 - 여자
womanInput <- totalTable %>% filter(trimws(GENDER)=="F") %>% select(CONDITION_CONCEPT_ID)
womanInput <- getCondtionSplitCount(womanInput)
getConditionStaticCount(womanInput)
