#OHDSI DB 

# install.packages("DatabaseConnector")
# install.packages("devtools")
# devtools::install_github("ohdsi/DatabaseConnectorJars")
# devtools::install_github("ohdsi/DatabaseConnector")
setwd("c:\\Git\\patterns-of-PIM")
source("propertiesParameters.R")
cl <- data.frame()
library(DatabaseConnector)

# db connection =============================================================================
# getDbConnection <- function(){
#   connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbParameter$dbms , 
#                                                server=dbParameter$server,
#                                                user=dbParameter$user,
#                                                password=dbParameter$password,
#                                                schema=dbParameter$schema)
#   conn <- connect(connectionDetails)
#   
#   return(conn)
#  
# }
# get total table querry ====================================================================
getTotalTableQuerry <- function(){
  return(getTableSql)
}

# get condition drug count===================================================================
getConditionDrugCount <- function(str){
  temp <- unlist(strsplit(str,","))
  return(length(temp))
}
# get drug count===================================================================
drugStatus <- c(drug=c())
# get gender count===================================================================
getSdByGender <- function(gender){
  gender <- trimws(gender)
  if(gender=="M"){
   
  }
  else{
  
  }
}
# get sum by condition count ===================================================================
getConditionCountSumByCount <- function(df,flag){
  return(df %>% group_by(CONDITION_COUNT)%>% count(CONDITION_COUNT) %>%  filter(CONDITION_COUNT==flag))
}

getSumConditionCount <- function(totalTable){
 
  c1Sum <-getConditionCountSumByCount(totalTable,1)
  c2Sum <-getConditionCountSumByCount(totalTable,2)
  c3Sum <-getConditionCountSumByCount(totalTable,3)
  c4Sum <- totalTable %>% group_by(CONDITION_COUNT)%>% count(CONDITION_COUNT) %>%
    filter(CONDITION_COUNT>3) %>% ungroup() %>% summarise(n = sum(n))
  
  c1ManSum <- getConditionCountSumByCount(totalTable %>% filter(trimws(GENDER)=="M"),1)
  c2ManSum <- getConditionCountSumByCount(totalTable %>% filter(trimws(GENDER)=="M"),2)
  c3ManSum <- getConditionCountSumByCount(totalTable %>% filter(trimws(GENDER)=="M"),3)
  c4ManSum <- totalTable %>% filter(trimws(GENDER)=="M") %>% 
    group_by(CONDITION_COUNT) %>% count(CONDITION_COUNT) %>%  
    filter(CONDITION_COUNT>3) %>% ungroup() %>% summarise(n = sum(n))
  
  returnDf <- data.frame(conditionCount=c(1,2,3,4),
                         totalSum=c(c1Sum$n,c2Sum$n,c3Sum$n,c4Sum$n),
                         manSum=c(c1ManSum$n,c2ManSum$n,c3ManSum$n,c4ManSum$n),
                         womanSum=c(c1Sum$n-c1ManSum$n,c2Sum$n-c2ManSum$n,c3Sum$n-c3ManSum$n,c4Sum$n-c4ManSum$n)
                         )
  return(returnDf)
}

#get ratio gender ======================================================================
getRatioGender <- function(df){
  total <- df %>% group_by(AGE) %>% summarise(count = n())
  man <- df %>% filter(trimws(df$GENDER)=="M") %>% group_by(AGE) %>% summarise(manCount = n())
  totalWithMan <- merge(total, man, by="AGE", all=T)
  totalWithMan[is.na(totalWithMan)] <-0
  totalWithManWoman <- totalWithMan %>% mutate(womanCount = totalWithMan$count-totalWithMan$manCount)
  print(totalWithManWoman)  
}
#get Trim gender dataframe
getTrimGenderDf <- function(df,gender){
  df <- df %>% filter(trimws(GENDER)==gender)
  return(df)
}

#get ratio age  ======================================================================
getAgeCount <- function(df,start,end){
  df <- df %>% subset(AGE>=start & AGE <=end) %>% summarise(count=n())
  return(df$count)
}

getAgeCountVector <- function(df){
  c65to69 <- getAgeCount(df,65,69)
  c70to74 <- getAgeCount(df,70,74)
  c75to79 <- getAgeCount(df,75,79)
  c80to84 <- getAgeCount(df,80,84)
  c85over <- getAgeCount(df,85,200)
  count <- c(c65to69,c70to74,c75to79,c80to84,c85over)
  return(count)
}

getRatioAge <- function(df){
  criteriaList <- c("count65to69","count70to74","count75to79","count80to84","count85over")
  #total
  totalAge <- getAgeCountVector(df)
  #man
  manAge <- getAgeCountVector(getTrimGenderDf(df,"M"))
  #woman
  womanAge <- getAgeCountVector(getTrimGenderDf(df,"F"))
  
  ageRatio <- data.frame(criteria=criteriaList,total=totalAge,man=manAge,woman=womanAge)
  
  totalCount <- ageRatio %>% summarise(count = sum(total))
  
  ageRatio <- ageRatio %>% group_by(criteria) %>% summarise(total =total/totalCount$count*100,
                                                            manPer = man/totalCount$count*100,
                                                            woman = woman/totalCount$count*100)
  print(ageRatio)
}


#excel load  ======================================================================
getInformXlsx <- function(file){
  # install.packages("xlsx")
  library(xlsx)
  setwd("c:\\Git\\patterns-of-PIM")
  drug_inform <- read.xlsx(file,1)
  return(drug_inform)
}
#get sd   ======================================================================
getSd <- function(df){
  
}


#drug status ======================================================================
getDrugStatus <- function(totalTable){
  totalDrugCount <- totalTable %>% summarise(sum = sum(DRUG_COUNT))
  totalDcugCount <- totalDrugCount$sum
  
  avgDrugUnder4Total <- totalTable %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  
  avgDrugOver4Total <- totalTable %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  
  
  manDrug <- totalTable %>% filter(trimws(GENDER)=="M")
  womanDrug <- totalTable %>% filter(trimws(GENDER)=="F")
  avgDrugUnder4Man <- manDrug %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  avgavgDrugOver4Man <- manDrug %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  
  avgDrugUnder4Woman <- womanDrug %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  avgDrugOver4Woman <- womanDrug %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT)/totalDcugCount*100)
  
  avgDrugUnder4Total
  avgDrugOver4Total
  avgDrugUnder4Man
  avgavgDrugOver4Man
  avgDrugUnder4Woman
  avgDrugOver4Woman
  
  criteria <- c("<5",">=5")
  t <- c(avgDrugUnder4Total$ratio,
             avgDrugOver4Total$ratio)  
  m <- c(avgDrugUnder4Man$ratio,
           avgavgDrugOver4Man$ratio)
  w <- c(avgDrugUnder4Woman$ratio,
             avgDrugOver4Woman$ratio)
  df <- data.frame(criteria=criteria,total=t,man=m,woman=w)
}
# drug infrom xlsx to list format ======================================
splitAndReturnList <- function(x){
  splitS <- strsplit(x,",")
  return(splitS)
}

splitAndReturnDf <- function(x){
  splitS <- unlist(strsplit(x,","))
  return(splitS)
}

getCondtionSplitCount <- function(df){
  df <- df %>% select(CONDITION_CONCEPT_ID)
  df <- apply(df,2,splitAndReturnDf)
  df <- as.data.frame(df)
  # df <- df %>% group_by(condition_concept_id) %>% summarise(count = n())
  return(df)
}

getConditionListStatic <-function(conditionList){
  cl <- apply(conditionList,1,splitAndReturnList)
  return(cl)
}

setGlobalConditionList <- function(x){
  cl <<- x
}

kk <- function(){
  cl
}

search <- function(df){
  for(i in 1:length(test)){
    grepResult <- grep(df,test[[i]]$id)
    if(length(grepResult)>0){
      return(test[[i]]$name)
    }
  }  
}

searchCondition <- function(input){
  for(i in 1:length(cl)){
    grepResult <- grep(input,cl[[i]]$condition_concept_id)
    if(length(grepResult)>0){
      return(cl[[i]]$condition_name)
    }
  }
}


getConditionStaticCount <- function(df){
  conditionList <- apply(df,1,searchCondition)
  conditionList <- unlist(conditionList)
  conditionList <- as.data.frame(conditionList)
  conditionList <- conditionList %>% group_by(conditionList) %>% summarise(count = n())
  conditionList
}
