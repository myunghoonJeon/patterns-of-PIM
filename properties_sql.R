# properties setting

#나이 구간 설정
#age 65 69 = 6569
#

#나이 구간별 p 값 구하기

getCbdPvalue <- function(tt){
  gac <- function(df,f){
    if(f==4){
      df <- df %>% subset(cbdCount>=f) %>% summarise(count=n())
    }else{
      df <- df %>% subset(cbdCount==f) %>% summarise(count=n())
    }
    return(df$count)
  }
  getCbd <- function(df){
    c0 <- gac(df,0)
    c1 <- gac(df,1)
    c2 <- gac(df,2)
    c3 <- gac(df,3)
    c4 <- gac(df,4)
    count <- c(c0,c1,c2,c3,c4)
    return(count)
  }
  
  getCbdSet <- function(ttt){
    pt <- ttt %>% subset(pimCount==0)
    npt <- ttt %>% subset(pimCount > 0)
    
    pCbd <- getCbd(pt)
    npCbd <- getCbd(npt)
    
    pdf <- as.data.frame(pCbd)
    npdf <- as.data.frame(npCbd)
    
    tpt <- cbind(pdf,npdf)
    mpt <- as.matrix(tpt)
    return(mpt)
  }
  
  
  indep.test=function(o) {
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
  tt <- totalTable
  tt
  total <- tt
  man <- tt %>% subset(GENDER=="M")
  man
  woman <- tt %>% subset(GENDER=="F")
  woman
  
  totalAge <- getCbdSet(total)
  manAge <- getCbdSet(man)
  womanAge <- getCbdSet(woman)

  tp <- indep.test(totalAge)
  mp <- indep.test(manAge)
  wp <- indep.test(womanAge)

  
  tp <- format(tp$Stat[2],scientific=F)
  mp <- format(mp$Stat[2],scientific=F)
  wp <- format(wp$Stat[2],scientific=F)
  print(tp)
  print(mp)
  print(wp)
  
  
}

getAgePvalue <- function(tt){
  gac <- function(df,start,end){
    df <- df %>% subset(AGE>=start & AGE <=end) %>% summarise(count=n())
    return(df$count)
  }
  getAge <- function(df){
    c65to69 <- gac(df,65,69)
    c70to74 <- gac(df,70,74)
    c75to79 <- gac(df,75,79)
    c80to84 <- gac(df,80,84)
    c85over <- gac(df,85,200)
    count <- c(c65to69,c70to74,c75to79,c80to84,c85over)
    return(count)
  }
  
  getAgeSet <- function(ttt){
    pt <- ttt %>% subset(pimCount==0)
    npt <- ttt %>% subset(pimCount > 0)
    
    pAge <- getAge(pt)
    npAge <- getAge(npt)
    
    pdf <- as.data.frame(pAge)
    npdf <- as.data.frame(npAge)
    
    tpt <- cbind(pdf,npdf)
    mpt <- as.matrix(tpt)
    return(mpt)
  }


indep.test=function(o) {
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
  tt <- totalTable
  tt
  total <- tt
  man <- tt %>% subset(GENDER=="M")
  man
  woman <- tt %>% subset(GENDER=="F")
  woman
  
  totalAge <- getAgeSet(total)
  manAge <- getAgeSet(man)
  womanAge <- getAgeSet(woman)
  
  totalAge
  manAge
  womanAge
  
  tp <- indep.test(totalAge)
  mp <- indep.test(manAge)
  wp <- indep.test(womanAge)
  
  ageTotal <- cbind(totalAge,manAge,womanAge)
  ageTotal <- as.data.frame(ageTotal)
  names(ageTotal) <- c("tp","tnp","mp","mnp","wp","wnp")
  ageTotal

  
  tp <- format(tp$Stat[2],scientific=F)
  mp <- format(mp$Stat[2],scientific=F)
  wp <- format(wp$Stat[2],scientific=F)
  print(tp)
  print(mp)
  print(wp)
  
}
# 만성질환 p value 구하기 

getComorbidityPvalue <- function(df){
  options(scipen=100)
  indep.test=function(o) {
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
  
getChi <- function(x){
    a <- x[1]
    b <- x[2]
    c <- x[3]
    d <- x[4]
    m <- matrix(c(a,c,b,d),nrow=2)
    r <- indep.test(m)
    p <- format(r$Stat[2],scientific=F)
    cp <- round(as.double(p),4)
    if(cp < 0.0001){
      cp <- "< 0.0001"
    }else{
      cp <- as.character(cp)
    }
    cat(x," : ",cp,"\n")
    return(cp)
  }
  
  setTotal<-function(df,index){
    index <- index*2
    t <- df %>% select(index,index+1)
    t <- t %>% mutate(op=sum(t[,1])-t[,1],onp=(sum(t[,2]))-t[,2])
    p <- apply(t,1,getChi)
   
    return(p)
  }
  # tt <- read.xlsx("mw.xlsx",1)
  # pv1 <- setTotal(tt,1)
  # print("====================")
  # pv2 <- setTotal(tt,2)
  # print("====================")
  # pv3 <- setTotal(tt,3)
  # print("====================")
  # pv1 <- as.data.frame(pv1)
  # pv1
  # pv2 <- as.data.frame(pv2)
  # pv2
  # pv3 <- as.data.frame(pv3)
  # pv3
  # result <- cbind(pv1,pv2,pv3)
  # result
  
  tt <- setTotal(df,1)
  result <- tt
  
  return(result)
}

aa <- getComorbidityPvalue()
aa
write.xlsx(aa,"ppp.xlsx")
#0.0001
# condition 에서 외래환자면서 65세 노인인 경우
comment="
SELECT condition_occurrence_id,ce.person_id, p.year_of_birth,cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) as age, ce.condition_start_date as condition_date,
vo.visit_concept_id as visit_concept
FROM [DB].dbo.condition_occurrence ce
left join [DB].dbo.person p
on ce.person_id = p.person_id and cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) >64
left join '[DB].dbo.visit_occurrence vo
on ce.visit_occurrence_id = vo.visit_occurrence_id
where p.year_of_birth is not null and year(condition_start_date)>1994 and vo.visit_concept_id=9202
"
#condition concept id로변환 and concept id 0 제거
comment="
SELECT ce.person_id, p.year_of_birth,cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) as age, ce.condition_start_date as condition_date,
vo.visit_concept_id as visit_concept,condition_concept_id
FROM [DB].dbo.condition_occurrence ce
left join [DB].dbo.person p
on ce.person_id = p.person_id and cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) >64
left join [DB].dbo.visit_occurrence vo
on ce.visit_occurrence_id = vo.visit_occurrence_id
where p.year_of_birth is not null and year(condition_start_date)>1994 and vo.visit_concept_id=9202 and condition_concept_id != 0
"

# drug 에서 65세 이상 ( year_of_birth 가 null 인거 제외 )
comment="
SELECT drug_exposure_id,drug_concept_id,drug_source_value,de.person_id, p.year_of_birth,cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) as age, year(de.drug_exposure_start_date) as drug_year,drug_exposure_start_date
  FROM drug_exposure de
  left join person p
  on de.person_id = p.person_id and cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) >64 
  where p.year_of_birth is not null and year(drug_exposure_start_date)>1994 and drug_exposure_start_date is not null
"

#복사할 drug테이블

comment="SELECT 	de.person_id,
				cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) as age,
				drug_exposure_start_date,
				drug_concept_id
		FROM [DB].dbo.drug_exposure de
		left join [DB].dbo.person p
		on de.person_id = p.person_id and cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) >64 
		where p.year_of_birth is not null and year(drug_exposure_start_date)>1994 and drug_exposure_start_date is not null"

#drug 튜플 합체 쿼리

comment="select distinct person_id,age,drug_exposure_start_date,
	stuff((
		select','+cast(drug_concept_id as nvarchar)
		from JMH_Paper.dbo.drug_copy 
		where person_id = dc.person_id and drug_exposure_start_date = dc.drug_exposure_start_date
		for XML PATH('')
	),1,1,'') AS drug_concept_id
from JMH_Paper.dbo.drug_copy dc
"

#최종 전처리 데이터 테이블에 저장
comment="insert into JMH_Paper.dbo.final_condition_drug_description
select * from
(select ced.person_id, 
		ced.age, 
		condition_concept_id,
		condition_start_date,
		ded.drug_concept_id ,
		p.gender_source_value as gender
 from JMH_Paper.dbo.condition_exposure_distinct ced
left join JMH_Paper.dbo.drug_exposure_distinct ded
on ced.person_id = ded.person_id and ced.condition_start_date = ded.drug_exposure_start_date
left join cdmpv531.dbo.person p
on ced.person_id = p.person_id) temp"

