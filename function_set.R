#OHDSI DB 

# install.packages("DatabaseConnector")
# install.packages("devtools")
# devtools::install_github("ohdsi/DatabaseConnectorJars")
# devtools::install_github("ohdsi/DatabaseConnector")
setwd("c:\\Git\\patterns-of-PIM")
source("propertiesParameters.R")

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