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
getTotalTable <- readRDS("totalTable.rds")

getTotalTable

test <- getTotalTable %>% group_by(PERSON_ID,AGE,CONDITION_START_DATE) %>% 
  summarise(CONDITION_CONCEPT_ID, 
            CONDITION_COUNT = getConditionDrugCount(CONDITION_CONCEPT_ID),
            DRUG_COUNT = getConditionDrugCount(DRUG_CONCEPT_ID),
            DRUG_CONCEPT_ID,GENDER
            )
# saveRDS(test,"finalTotalTable2.rds")

# test
# test2 <- test %>% ungroup()
# read database
totalTable <- readRDS("c:\\Git\\patterns-of-PIM\\finalTotalTable2.rds")
totalTable <- totalTable %>% ungroup()
# 성별에 따른 진단 수 
conditionCountSumByGender <- getSumConditionCount(totalTable)
conditionCountSumByGender

# 표준편차
sdTotal <- totalTable %>% summarise(sd=sd(CONDITION_COUNT))
sdMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(CONDITION_COUNT))
sdWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(CONDITION_COUNT))

forGenderTable <- totalTable %>% select(AGE,GENDER)
ratioGender <- getRatioGender(forGenderTable)
ratioGender

sdAgeTotal <- totalTable %>% summarise(sd = sd(totalTable$AGE))
sdAgeMan <- totalTable %>% filter(trimws(GENDER)=="M") %>% summarise(sd=sd(AGE))
sdAgeWoman <- totalTable %>% filter(trimws(GENDER)=="F") %>% summarise(sd=sd(AGE))
sdAgeTotal
sdAgeMan
sdAgeWoman
countAgeTotal
