
df <- data.frame(name=c("Asthma","Cardiac arrhythmias"),id=c(" 317009","318448,320744,316135,457008"))
df



splitFunction <- function(x){
  splitS <- strsplit(x,",")
  return(splitS)
}

test <- apply(df,1,splitFunction)

test
class(test)


input <- data.frame(id=c("317009","318448","457008","457008","457008","457008",
                         "317009","317009","317009","317009","317009"))
input

distinctInput <- input %>% select(id)
distinctInput

test


search <- function(df){
  for(i in 1:length(test)){
    grepResult <- grep(df,test[[i]]$id)
    if(length(grepResult)>0){
      return(test[[i]]$name)
    }
  }  
}

input
c <- apply(distinctInput,1,search)
class(c)
c
c <- as.data.frame(c)
c
c <- c %>% group_by(c) %>% summarise(count = n())
c

