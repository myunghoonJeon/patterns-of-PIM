#OHDSI DB 

# install.packages("DatabaseConnector")
# install.packages("devtools")
# devtools::install_github("ohdsi/DatabaseConnectorJars")
# devtools::install_github("ohdsi/DatabaseConnector")
# 

setwd("c:\\Git\\patterns-of-PIM")
source("propertiesParameters.R")
cl <- data.frame()
library(DatabaseConnector)

# db connection =============================================================================
getDbConnection <- function(){
  dbParameter <- dbP
  dbParameter
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbParameter$dbms,
                                               server=dbParameter$server,
                                               user=dbParameter$user,
                                               password=dbParameter$password,
                                               schema=dbParameter$schema)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  
  return(conn)
}
#set Drug count Condition count ======================================================================
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

# get gender count===================================================================
getSdByGender <- function(gender){
  gender <- trimws(GENDER)
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
    
  # ageRatio <- ageRatio %>% group_by(criteria) %>% summarise(total =total/totalCount$count*100,
  #                                                           manPer = man/totalCount$count*100,
  #                                                           woman = woman/totalCount$count*100)
  print(ageSumRatio)

  
  
}
#get pims per prescription ========================================================
getPimsPerPrescription <- function(df){
  pim0 <- df %>% subset(pimCount==0) %>% summarise(count = n())
  pim1 <- df %>% subset(pimCount==1) %>% summarise(count = n()) 
  pim2 <- df %>% subset(pimCount==2) %>% summarise(count = n()) 
  pim3 <- df %>% subset(pimCount==3) %>% summarise(count = n())
  pim4 <- df %>% subset(pimCount>=4) %>% summarise(count = n()) 
  returnDf <- data.frame(criteria=c("0","1","2","3",">=4"),pimCount=c(pim0$count,pim1$count,pim2$count,pim3$count,pim4$count))
}


#excel load  ======================================================================
getInformXlsx <- function(file){
  # install.packages("xlsx")
  library(xlsx)
  setwd("c:\\Git\\patterns-of-PIM")
  inform <- read.xlsx(file,1)
  return(inform)
}
#age Mean Sd =============================
getAgeMeanSd <- function(tt){
  meanTotal <- tt %>% summarise(mean = round(mean(AGE),3))
  sdTotal <- tt %>% summarise(sd = round(sd(AGE),3))
  cat("mean : ",meanTotal$mean,"  sd : ",sdTotal$sd,"\n")
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
  df <- df %>% select(cbdList)
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
  
  for(i in 1:length(x)){
    x[[i]]$condition_concept_id <- trimws(x[[i]]$condition_concept_id)
    print(x[[i]]$condition_concept_id)
  }
  cl <<- x
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
    grepResult <- which(input==cl[[i]]$condition_concept_id)
    len <- length(grepResult)
    if(len>0){
      cat("[[ MATCHING ]] == ",input,"== [[ INDEX : ",grepResult," ]]\n")
      return(cl[[i]]$condition_name)
    }
  }
}

getConditionStaticCount <- function(df){
  conditionList <- apply(df,1,searchCondition)
  print("[COMPLETE APPLY]")
  conditionList <- unlist(conditionList)
  conditionList <- as.data.frame(conditionList)
  print("[TRASLATE DATA FRAME]")
  groupCondition <- conditionList %>% group_by(conditionList) %>% summarise(count = n())
  print("[COMPLETE GROUPING]")
  groupCondition
  return(groupCondition)
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
getPimColumn <- function(df,year){
  
  
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
  
  # l <- data.frame(list = c("1384360,1300978,1597756","196048","1203949"))
  # l
  # totalTable <- l
  
  
  totalTable <- df
  totalTable <- totalTable %>% select(DRUG_CONCEPT_ID)
  
  # totalTable <-l
  
  
  drugListCount <- apply(totalTable,1,sf)
  drugListCount
  pimDf <- do.call(rbind.data.frame, drugListCount)
  pimDf
  # pimDf
  cat("[[[ complete : ",year," ]]]\n")
  return(pimDf)
  
  # saveRDS(cbdDf,"cormorbidity.rds")
  # cbdDf <- readRDS("cormorbidity.rds")
  # names(cbdDf) <- c("cbdCount")
  
}

# year base RDS set get
setComorbidityRdsByYear <- function(df,f,start,end){
  yearDf <- data.frame()
  yearDf <- df
  saveName <- paste0(f,"Result",start,"to",end,".rds")
  cat(saveName," saving...\n")
  saveRDS(yearDf,saveName)
  cat(saveName," complete save...\n")
  
}

getComorbidityRdsByYear <- function(f,start,end){
  yearDf <- data.frame()
  fileName <- paste0(f,"Result",start,"to",end,".rds")
  cat(fileName," reading...\n")
  yearDf <- readRDS(fileName)
  cat(fileName," complete read\n")
  return(yearDf)
}
getPimRdsByYear <- function(f,start,end){
  yearDf <- data.frame()
  fileName <- paste0(f,"Result",start,"to",end,".rds")
  cat(fileName," reading...\n")
  yearDf <- readRDS(fileName)
  cat(fileName," complete read\n")
  return(yearDf)
}
setRdsByYear <- function(df,f,start,end){
  yearDf <- data.frame()
  yearDf <- df %>% subset(year>=start&year<=end)
  saveName <- paste0(f,start,"to",end,".rds")
  cat(saveName," saving...\n")
  saveRDS(yearDf,saveName)
  cat(saveName," complete save...\n")
  
}
getRdsByYear <- function(f,start,end){
  yearDf <- data.frame()
  fileName <- paste0(f,start,"to",end,".rds")
  cat(fileName," reading...\n")
  yearDf <- readRDS(fileName)
  cat(fileName," complete read\n")
  return(yearDf)
}

setRdsEachYear <- function(df,f,start,end){
  for(i in start:end){
    setRdsByYear(df,f,i,i)
  }
}
#comorbidity list extraction ==========================================
getComorbidityColumn <- function(comorbidiTyDf,year){ #만성질환 리스트와 숫자 계산
  
  
  splitPimReturnDf <- function(x){
    splitS <- unlist(strsplit(x,","))
    return(splitS)
  }
  
  chChr <- function(x){
    temp <- as.character(x)
    temp <- trimws(temp)
    return(temp)
  }
  
  checkCormorbidity <- function(x){
    result <-data.frame()
    count <-0
    tempStr <-NULL
    trimStr <- trimws(as.character(x))
    tempDf <- splitAndReturnDf(trimStr)
    for(i in 1:length(tempDf)){
      index <- which(conditionList == tempDf[[i]])
      if(length(index) >0){
        cat("Comorbidity : ",tempDf[[i]],"\n")
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
      result <- data.frame(cbdCount=c(count),cbdList=c("x"))
    }else{
      result <- data.frame(cbdCount=c(count),cbdList=c(tempStr))
    }
    # print(result)
    return(result)
    # result <- grepSearch(t)
    # return(result)
  }
  listXlsx <- getInformXlsx("Comorbidity_List.xlsx")
  listXlsx
  
  dtx <- listXlsx %>% select(condition_concept_id)
  dtx
  conditionTotalChr<-apply(dtx,1,chChr)
  conditionTotalChr
  conditionList <- splitPimReturnDf(conditionTotalChr)
  conditionList <- as.data.frame(conditionList)
  names(conditionList) <- c("id")
  str(conditionList)
  conditionList['id'] <- trimws(conditionList$id)
  str(conditionList)

  # test <- readRDS("onlyPimTable.rds")
  # test
  totalTable <- comorbidiTyDf
  totalTable <- totalTable %>% select(CONDITION_CONCEPT_ID)
 
  conditionListCount <- apply(totalTable,1,checkCormorbidity)
  conditionListCount
  comorbidityDf <- do.call(rbind.data.frame, conditionListCount)
  # comorbidityDf
  # summary(comorbidityDf)
  # comorbidityResultDf <- bind_cols(test,comorbidityDf)
  # saveRDS(comorbidityResultDf,"totalResultTable.rds")
  cat("[[[ complete : ",year," ]]]\n")
  return(comorbidityDf)
}
getComorbidityColumn2 <- function(comorbidiTyDf){ #만성질환 리스트와 숫자 계산
  
  
  splitPimReturnDf <- function(x){
    splitS <- unlist(strsplit(x,","))
    return(splitS)
  }
  
  chChr <- function(x){
    temp <- as.character(x)
    temp <- trimws(temp)
    return(temp)
  }
  
  checkCormorbidity <- function(x){
    result <-data.frame()
    count <-0
    tempStr <-NULL
    trimStr <- trimws(as.character(x))
    tempDf <- splitAndReturnDf(trimStr)
    for(i in 1:length(tempDf)){
      index <- which(conditionList == tempDf[[i]])
      if(length(index) >0){
        cat("Comorbidity : ",tempDf[[i]],"\n")
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
      result <- data.frame(cbdCount=c(count),cbdList=c("x"))
    }else{
      result <- data.frame(cbdCount=c(count),cbdList=c(tempStr))
    }
    # print(result)
    return(result)
    # result <- grepSearch(t)
    # return(result)
  }
  listXlsx <- getInformXlsx("Comorbidity_List.xlsx")
  listXlsx
  
  dtx <- listXlsx %>% select(condition_concept_id)
  dtx
  conditionTotalChr<-apply(dtx,1,chChr)
  conditionTotalChr
  conditionList <- splitPimReturnDf(conditionTotalChr)
  conditionList <- as.data.frame(conditionList)
  names(conditionList) <- c("id")
  str(conditionList)
  conditionList['id'] <- trimws(conditionList$id)
  str(conditionList)
  
  
  # test <- readRDS("onlyPimTable.rds")
  # test
  totalTable <- comorbidiTyDf
  totalTable <- totalTable %>% select(CONDITION_CONCEPT_ID)
  
  conditionListCount <- apply(totalTable,1,checkCormorbidity)
  conditionListCount
  comorbidityDf <- do.call(rbind.data.frame, conditionListCount)
  # comorbidityDf
  # summary(comorbidityDf)
  # comorbidityResultDf <- bind_cols(test,comorbidityDf)
  # saveRDS(comorbidityResultDf,"totalResultTable.rds")
  return(comorbidityDf)
}
# comorbidity result save to RDS========================
calcAndSaveComorbidity <- function(content,startYear,endYear){
  for(i in startYear:endYear){
    tempDf <- getRdsByYear(content,i,i)
    resultDf <- getComorbidityColumn(tempDf,i)
    setComorbidityRdsByYear(resultDf,content,i,i)
  }
}
# PIM result save to RDS========================
clacAndSavePim <- function(content,startYear,endYear){
  for(i in startYear:endYear){
    tempDf <- getRdsByYear(content,i,i)
    resultDf <- getPimColumn(tempDf,i)
    setComorbidityRdsByYear(resultDf,content,i,i)
  }
}
#read Pim list by Year and binds PIM rows
getPimBindRows <-function(startYear,endYear){
  temp <- data.frame()
  c <-0
  for(i in startYear:endYear){
    df <- getPimRdsByYear("pim",i,i)
    if(c==0){
      cat(i," - ","\n")
      temp <- df
    }else{
      cat(i-1," - ",i,"\n")
      temp <- bind_rows(temp,df)  
    }
    c <- c+1
    
  }
  return(temp)
}
#exposure patterns of the study poplation to individual PIMs during the study period
getPimNameMatchingResult <- function(df,year){
  
  
  splitPimReturnDf <- function(x){
    splitS <- unlist(strsplit(x,","))
    return(splitS)
  }
  
  chr <- function(x){
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
    
    return(result)
    
  }
  # drugListXlsx <- getInformXlsx("drug_inform.xlsx")
  # drugListXlsx
  
  pimTotal
  pimTotal<-apply(pimTotal,1,chr)
  pimTotal
  drugList <- splitPimReturnDf(drugTotalChr)
  drugList <- as.data.frame(drugList)
  names(drugList) <- c("id")
  head(drugList)
  drugList
  
  # totalTable <- l
  
  drugListCount <- apply(totalTable,1,sf)
  drugListCount
  
  pimDf <- do.call(rbind.data.frame, drugListCount)
  pimDf
  
  # pimDf
  cat("[[[ complete : ",year," ]]]\n")
  return(pimDf)
  
  # saveRDS(cbdDf,"cormorbidity.rds")
  # cbdDf <- readRDS("cormorbidity.rds")
  # names(cbdDf) <- c("cbdCount")

}

#No. of chronic diseases per prescription, mean (SD) ===============================

#No. of medications per prescription, mean (SD)  ===============================
# mean sd
getDfGender <- function(df){
  m <- df %>% subset(trimws(GENDER)=="M")
  w <- df %>% subset(trimws(GENDER)=="F")
  return(list(man=m,woman=w))
}

getMeanSdPerGender <- function(df){
  ms <- function(df){
    mean <- df %>% select(DRUG_COUNT) %>% summarise(mean=mean(DRUG_COUNT))
    sd <- df %>% select(DRUG_COUNT) %>% summarise(sd=sd(DRUG_COUNT))
    return(c(mean$mean,sd$sd))
  }
  manTotal <- df %>% subset(trimws(GENDER)=="M")
  womanTotal <- df %>% subset(trimws(GENDER)=="F")
  return(data.frame(criteria=c("mean","sd"),ms(df),man=ms(manTotal),woman=ms(womanTotal)))
}

# count <5, >=5
getCount5 <- function(df){
  g5 <- function(x){
    under5 <- x %>% subset(DRUG_COUNT<5) %>% summarise(count = n())
    over5 <- x %>% subset(DRUG_COUNT>=5) %>% summarise(count = n())
    return(c(under5$count,over5$count))
  }
  gender <- getDfGender(df)
  t <- g5(df)
  m <- g5(gender$man)
  w <- g5(gender$woman)
  return(data.frame(criteria=c("<5",">=5"),total=t,man=m,woman=w))
}

# No. of PIMs per presciprtion, mena(SD)============================================

# mean sd
getDfGender <- function(df){
  m <- df %>% subset(trimws(GENDER)=="M")
  w <- df %>% subset(trimws(GENDER)=="F")
  return(list(man=m,woman=w))
}

getMeanSdPerGender <- function(df){
  ms <- function(df){
    mean <- df %>% select(pimCount) %>% summarise(mean=mean(pimCount))
    sd <- df %>% select(pimCount) %>% summarise(sd=sd(pimCount))
    return(c(mean$mean,sd$sd))
  }
  manTotal <- df %>% subset(trimws(GENDER)=="M")
  womanTotal <- df %>% subset(trimws(GENDER)=="F")
  return(data.frame(criteria=c("mean","sd"),ms(df),man=ms(manTotal),woman=ms(womanTotal)))
}

getCount123 <- function(df){
  g5 <- function(x){
    g0 <- x %>% subset(pimCount==0) %>% summarise(count = n())
    g1 <- x %>% subset(pimCount==1) %>% summarise(count = n())
    g2 <- x %>% subset(pimCount==2) %>% summarise(count = n())
    g3over <- x %>% subset(pimCount>=3) %>% summarise(count = n())
    return(c(g0$count,g1$count,g2$count,g3over$count))
  }

  gender <- getDfGender(df)
  t <- g5(df)
  m <- g5(gender$man)
  w <- g5(gender$woman)
  return(data.frame(criteria=c("0","1","2",">=3"),total=t,man=m,woman=w))
}

#age(years),mean(SD), by PIM ===============================================
getAgepimCount <- function(df,start,end){
  df <- df %>% subset(age>=start & age <=end) %>% summarise(count=sum(pimCount))
  return(df$count)
}

getAgepimCountVector <- function(df){
  c65to69 <- getAgeCount(df,65,69)
  c70to74 <- getAgeCount(df,70,74)
  c75to79 <- getAgeCount(df,75,79)
  c80to84 <- getAgeCount(df,80,84)
  c85over <- getAgeCount(df,85,200)
  count <- c(c65to69,c70to74,c75to79,c80to84,c85over)
  return(count)
}


getRatioAgePim <- function(df){
  cl <- c("count65to69","count70to74","count75to79","count80to84","count85over")
  #total
  ta <- getAgeCountVector(df)
  #man
  ma <- df %>% subset(trimws(GENDER)=="M")
  ma <- getAgepimCountVector(ma)
  #woman
  wa <- df %>% subset(trimws(GENDER)=="F")
  wa <- getAgepimCountVector(wa)
  ageRatio <- data.frame(criteria=cl,total=ta,man=ma,woman=wa)
  
  # totalCount <- ageRatio %>% summarise(count = sum(total))
  # 
  # ageSumRatio <- ageRatio %>% group_by(criteria)
 
  ageRatio
}


# No. of chronic diseases per prescription, mean (SD) =====================================
# total <- readRDS("totalResultTable.rds")

# mean sd
getDfGender <- function(df){
  m <- df %>% subset(trimws(GENDER)=="M")
  w <- df %>% subset(trimws(GENDER)=="F")
  return(list(man=m,woman=w))
}

getMeanSdPerGender <- function(df){
  ms <- function(df){
    mean <- df %>% select(cbdCount) %>% summarise(mean=mean(cbdCount))
    sd <- df %>% select(cbdCount) %>% summarise(sd=sd(cbdCount))
    return(c(mean$mean,sd$sd))
  }
  manTotal <- df %>% subset(trimws(GENDER)=="M")
  womanTotal <- df %>% subset(trimws(GENDER)=="F")
  return(data.frame(criteria=c("mean","sd"),ms(df),man=ms(manTotal),woman=ms(womanTotal)))
}


getCount1234 <- function(df){
  g5 <- function(x){
    g0 <- x %>% subset(cbdCount==0) %>% summarise(count = n())
    g1 <- x %>% subset(cbdCount==1) %>% summarise(count = n())
    g2 <- x %>% subset(cbdCount==2) %>% summarise(count = n())
    g3 <- x %>% subset(cbdCount==3) %>% summarise(count = n())
    g4over <- x %>% subset(cbdCount>=4) %>% summarise(count = n())
    return(c(g0$count,g1$count,g2$count,g3$count,g4over$count))
  }
  
  gender <- getDfGender(df)
  t <- g5(df)
  m <- g5(gender$man)
  w <- g5(gender$woman)
  return(data.frame(criteria=c("0","1","2","3",">=4"),total=t,man=m,woman=w))
}




# Table 2. Patterns of PIM use according to various characteristics 
#PIM COUNT
# total <- readRDS("totalResultTable.rds")


# manpimCount <- total %>% subset(trimws(GENDER)=="M") %>% summarise(sum(pimCount))
# womanpimCount <- total %>% subset(trimws(GENDER)=="F") %>% summarise(sum(pimCount))


#table2 exposure patterns of the study population to individual PIMs during the study period

srdf <- function(x){
  splitS <- unlist(strsplit(x,","))
  resultDf <- as.data.frame(splitS)
  return(resultDf)
}

getCbdCount <- function(df){
  cbdList <- as.character(df['id'])
  pimCount <- df['pCount']
  
  cat(cbdList," - ",pimCount,"\n")
  
  tempDf <- srdf(cbdList)
  pc <- as.character(pimCount)
  tempDf<-tempDf %>% mutate(pim=c(pc))
  # cat(df['id']," <-> ",df['pCount'],"\n")
  return(tempDf)
}

# result <- apply(cbd,1,getCbdCount)
# resultDf <- do.call(rbind.data.frame, result)
# resultDf
# str(resultDf)
# comorGroup <- resultDf %>% group_by(splitS) %>% summarise(pimSum = sum(as.numeric(pim)))
# comorGroup
#get comorbidity per pim count
# checkSum <- comorGroup %>% summarise(c = sum(pimSum))
# checkSum

#xlsx setting
getPimPerDscription <- function(input){
  
  chrr <- function(x){
    temp <- as.character(x)
    temp <- trimws(temp)
    return(temp)
  }
  
  splitDf <- function(x){
    sp <- strsplit(x,",")
    # print(sp)
    return(sp)
  }
  
  splitDfXlsx <- function(x){
    sp <- unlist(strsplit(x,","))
    # print(sp)
    return(sp)
  }
  
  getPimListXlsx <- function(df){
    pName <- trimws(as.character(df['name']))
    pId <- as.character(df['id'])
    tempDf <- splitDfXlsx(pId)
    tempDf <- data.frame(tempDf)
    tempDf <- tempDf %>% mutate(id=c(pName))
    return(tempDf)
  }
  
  
  total<-input  
  # total <- readRDS("totalResultTable.rds")
  # total <- total %>% subset(trimws(GENDER)=="M")
  onlyPim <- total %>% select(pimList)
  onlyPim <- as.data.frame(onlyPim)
  onlyPim
  
  splitPim <- apply(onlyPim,1,splitDf)
  pimDf <- do.call(rbind.data.frame, splitPim)
  pimGroup <- pimDf %>% group_by(pimList) %>% summarise(count = n())
  pimGroup <- ungroup(pimGroup)
  names(pimGroup) <- c("id","count")
  totalCount <- pimGroup %>% summarise(sum = sum(count))
  totalCount
  
  drugInform <- getInformXlsx("drug_inform.xlsx")
  names(drugInform) <- c("name","id")
  
  drugNames <- drugInform %>% select(name)
  drugNames <- drugNames %>% arrange(name)
  drugNames
  
  pimName <- apply(drugInform,1,getPimListXlsx)
  
  pimList <- do.call(rbind.data.frame, pimName)
  names(pimList) <- c("id","name")
  pimList
  
  mergePim <- merge(pimGroup,pimList,key='id',all=F)
  mergePim <- mergePim %>% arrange(name) %>% select(name,count)
  totalSum <- mergePim %>% summarise(sum(count))
  mergePim <- ungroup(mergePim)
  mergePim <- merge(drugNames,mergePim,key='name',all=T)
  mergePim[is.na(mergePim)]<-0
  mergePim

  tt <-function(df){
    n <- df['name']
    print(n)
    cnt <-as.numeric(trimws(df['count']))
    print(cnt)
    p <- round(cnt/totalSum*100,3)
    if(p==0){
      p=0
    }
    c(name=c(n),count=c(cnt),percent=c(p))
  }
  
  # res <- apply(mergePim,1,tt)
  # resDf <- do.call(rbind.data.frame, res)
  # names(resDf) <- c("name","count","percent")
  return(mergePim)
}

# combine comorbidity dataframes 
getCombineComorbidityDataframes <- function(f,start,end){
  df <- data.frame()
  count <- 0
  for(i in start:end){
    temp <- getRdsByYear(f,i,i)
    df<- bind_rows(df,temp)
    # if(count==0){
    #   cat(i,"(year) dataframe setting\n")
    #   df <- temp
    #   dc <- df %>% summarise(count = n())
    #   print(dc$count)
    # }else{
    #   cat((i-1),"-",i," combine\n")  
    #   bind_rows(df,temp)
    #   tc <- temp %>% summarise(count = n())
    #   dc <- df %>% summarise(count = n())
    #   cat(tc$count," add - ",dc$count,"\n")
    # }
    # count <- count +1
  }
  print("complete combine")
  return(df)
}
#p value calcurate ====================================



#flag setup
# totalTable


### get COmorbidity index and Name ###
getComorbidityFullList <- function(){
  cbdTotal <- read.xlsx("Comorbidity_List.xlsx",1)
  
  cbdTotal$condition_concept_id <- as.character(cbdTotal$condition_concept_id)
  cbdTotal$condition_name <- as.character(cbdTotal$condition_name)
  head(cbdTotal)
  size <- length(cbdTotal[,1])
  
  
  
  finalCbdList <- data.frame()
  
  for(i in 1:size){
    n <- cbdTotal[i,1]
    id <- cbdTotal[i,2]
    ids <- sp(id)
    ids <- as.data.frame(ids)
    ids$name <- n
    names(ids) <- c("id","name")
    finalCbdList <- rbind(finalCbdList,ids)
  }
  
  finalCbdList$id <- as.character(finalCbdList$id)
  str(finalCbdList)
  fd <- finalCbdList
  fd
  return(fd)
}
  
getComorbidityIndexAndName <- function(fc){
    comorSet <- data.frame(start=c(0),end=c(0),name=c(""))
    tempSet <- comorSet
    
    origin<-"";
    temp<-"";
    st<-0
    ed<-0
    flag <-0
    for(i in 1:255){
      if(i==1){
        origin <- fc[i,2]
        st <- 1
      }else{
        temp <- fc[i,2]
        if(origin!=temp){
          cat("\n")
          ed <- i-1
          if(flag==0){
            comorSet$start = 1
            comorSet$end = ed
            comorSet$name = origin
            cat(st," - ",ed," : ",comorSet$name,"\n")
            flag <- 1
          }else{
            cat("NEW")
            tempSet$start = st
            tempSet$end = ed
            tempSet$name = origin
            cat(st," - ",ed," : ",tempSet$name,"\n")
            comorSet <- rbind(comorSet,tempSet)
            
          }
          st <- i
          origin <- temp
        }else{
          cat("=")
        }
      }
    }
    tempSet$start <- st
    tempSet$end <- 255
    tempSet$name <- temp
    comorSet <- rbind(comorSet,tempSet)
    comorSet$index <- 1:18
    return(comorSet)
}


