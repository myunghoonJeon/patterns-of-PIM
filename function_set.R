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
getDbConnection <- function(){
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbParameter$dbms ,
                                               server=dbParameter$server,
                                               user=dbParameter$user,
                                               password=dbParameter$password,
                                               schema=dbParameter$schema)
  conn <- connect(connectionDetails)

  return(conn)

}
#setDrugConditionCount ======================================================================
getDrugConditionCount <- function(totalTable){
  addconditionDrugCount <- totalTable %>% group_by(PERSON_ID,AGE,CONDITION_START_DATE) %>%
    summarise(CONDITION_CONCEPT_ID,
              CONDITION_COUNT = getConditionDrugCount(CONDITION_CONCEPT_ID),
              DRUG_COUNT = getConditionDrugCount(DRUG_CONCEPT_ID),
              DRUG_CONCEPT_ID,GENDER
    )
  addconditionDrugCount <- ungroup(addconditionDrugCount)
  return(addconditionDrugCount)
}

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

  ageSumRatio <- ageRatio %>% group_by(criteria)
    
  ageRatio <- ageRatio %>% group_by(criteria) %>% summarise(total =total/totalCount$count*100,
                                                            manPer = man/totalCount$count*100,
                                                            woman = woman/totalCount$count*100)
  print(ageSumRatio)
  
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
getMeanSd <- function(tt,str){
  meanTotal <- tt %>% summarise(mean = mean(DRUG_COUNT))
  sdTotal <- tt %>% summarise(sd = sd(DRUG_COUNT))
  cat(str," - mean : ",meanTotal$mean,"  sd : ",sdTotal$sd,"\n")
}

getDrugStatus <- function(totalTable){
  totalDrugCount <- totalTable %>% summarise(sum = sum(DRUG_COUNT))
  totalDcugCount <- totalDrugCount$sum
  
  getMeanSd(totalTable,"total")
  avgDrugUnder4Total <- totalTable %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT))
  avgDrugOver4Total <- totalTable %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT))
  
  
  manDrug <- totalTable %>% filter(trimws(GENDER)=="M")
  getMeanSd(manDrug,"Man")
  totalManDrugCount <- manDrug %>% summarise(sum = sum(DRUG_COUNT))
  
  womanDrug <- totalTable %>% filter(trimws(GENDER)=="F")
  getMeanSd(womanDrug,"Woman")
  totalWomanDrugCount <- womanDrug %>% summarise(sum = sum(DRUG_COUNT))
  
  avgDrugUnder4Man <- manDrug %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT))
  avgavgDrugOver4Man <- manDrug %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT))
  
  avgDrugUnder4Woman <- womanDrug %>% filter(DRUG_COUNT<5) %>% summarise(ratio = sum(DRUG_COUNT))
  avgDrugOver4Woman <- womanDrug %>% filter(DRUG_COUNT>=5) %>% summarise(ratio = sum(DRUG_COUNT))
  
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
  print("1")
  conditionList <- unlist(conditionList)
  print("2")
  conditionList <- as.data.frame(conditionList)
  print("3")
  conditionList <- conditionList %>% group_by(conditionList) %>% summarise(count = n())
  conditionList
}
#grep str


grepSearch<-function(str){
  count <- 0
  for(i in 1:length(comorbidityList$id)){
    temp <- trimws(as.character(comorbidityList$id[[i]]))
    gr <- grep(temp,str)
    if(length(gr)>0){
        count <- count+1
    }
  }
  
  return(count)
}
#cormobidity static ===========================================================
getComorbidityCount<- function(df,count){
  if(count == 4){
    df<- df %>% filter(df$cbdCount>3) %>% summarise(count = n())
  }
  else{
    df<-df %>% filter(df$cbdCount==count) %>% summarise(count = n())
  }
  return(df$count)
}

getComorbidityCountStatic <- function(df){
  count0 <- getComorbidityCount(df,0)
  count1 <- getComorbidityCount(df,1)
  count2 <- getComorbidityCount(df,2)
  count3 <- getComorbidityCount(df,3)
  count4Over <- getComorbidityCount(df,4)
  resultDf <- data.frame(criteria=c("0","1","2","3","4>"), 
                         count=c(count0,count1,count2,count3,count4Over))
  resultDf
}



#PIM(Potentially inappropriate medication) static ===========================================================
#PIM 분류
getPimInform <- function(df){
  
  
  splitPimReturnDf <- function(x){
    splitS <- unlist(strsplit(x,","))
    return(splitS)
  }
  
  chChr <- function(x){
    temp <- as.character(x)
    temp <- trimws(temp)
    return(temp)
  }
  
  # comorbidityList <- apply(comorbidityList,1,chChr)
  # comorbidityList <- data.frame(comorbidityList)
  # names(comorbidityList) <- c("id")
  # comorbidityList
  
  sf <- function(x){
    result <-data.frame()
    count <-0
    tempStr <-NULL
    trimStr <- trimws(as.character(x))
    tempDf <- splitAndReturnDf(trimStr)
    for(i in 1:length(tempDf)){
      index <- which(drugList == tempDf[[i]])
      if(length(index) >0){
        cat("PIM : ",tempDf[[i]],"\n")
        count <- count +1
        if(length(tempStr)==0){
          tempStr <-c(tempDf[[i]])
        }
        else{
          tempStr <- paste(tempStr,tempDf[[i]],sep=",")
        }
      }else{
        print(tempDf[[i]])
      }
    }
    
    if(count==0){
      result <- data.frame(pimCount=c(count),pimList=c("x"))
    }else{
      result <- data.frame(pimCount=c(count),pimList=c(tempStr))
    }
    # print(result)
    return(result)
    # result <- grepSearch(t)
    # return(result)
  }
  drugListXlsx <- getInformXlsx("drug_inform.xlsx")
  drugListXlsx
  
  dtx <- drugListXlsx %>% select(Drug_Concept_ID)
  dtx
  drugTotalChr<-apply(dtx,1,chChr)
  drugTotalChr
  drugList <- splitPimReturnDf(drugTotalChr)
  drugList <- as.data.frame(drugList)
  names(drugList) <- c("id")
  head(drugList)
  drugList
  
  l <- data.frame(list = c("1384360,1300978,1597756","196048","1203949"))
  l
  totalTable <- l
  
  totalTable <- readRdsByYear(2006,2006)
  totalTable <- totalTable %>% select(DRUG_CONCEPT_ID)
  totalTable
  drugListCount <- apply(totalTable,1,sf)
  drugListCount
  
  pimDf <- do.call(rbind.data.frame, drugListCount)
  pimDf
  test <- pimDf %>% group_by(pimCount) %>% summarise(c = n())
  test
  # pimDf
  return(pimDf)
  
  # saveRDS(cbdDf,"cormorbidity.rds")
  # cbdDf <- readRDS("cormorbidity.rds")
  # names(cbdDf) <- c("cbdCount")
  
}

# year base RDS set get

setRdsByYear <- function(df,start,end){
  yearDf <- data.frame()
  yearDf <- df %>% subset(year>=start&year<=end)
  saveName <- paste0("pim",start,"to",end,".rds")
  saveRDS(yearDf,saveName)
  
}
readRdsByYear <- function(start,end){
  yearDf <- data.frame()
  fileName <- paste0("pim",start,"to",end,".rds")
  cat(fileName," reading...\n")
  yearDf <- readRDS(fileName)
  cat(fileName," read complete\n")
  return(yearDf)
}
