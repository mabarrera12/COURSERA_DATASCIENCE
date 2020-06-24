#Semana 3
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hist(as.numeric(outcome[,11]))
library(pracma)

best <- function(state=as.character(), outcome_name=as.character())
{  # state is 2 letters
   # heart attack, heart failure, pneumonia

  if (state %in% outcome$State)
    {if (strcmpi(outcome_name,"heart failure")|strcmpi(outcome_name,"heart attack")|strcmpi(outcome_name,"pneumonia"))
    { data<- data.frame(outcome$Hospital.Name,
                        outcome$State, 
                        as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                        as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                        as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    colnames(data)[c(1,2,3,4,5)]<-c("Name","State","H_Failure","H_Attack","Pneumonia")
    data_new<-split(data,factor(data$State))
    
      if (strcmpi(outcome_name,"heart failure"))
      { data_work<-data_new[state]
        data_work<-data_work[[1]]
        data_work<-na.omit(data_work[,c(1,2,3)])
        ans<-data_work[order(data_work$H_Failure),]
      }
      if (strcmpi(outcome_name,"heart attack"))
      { data_work<-data_new[state]
        data_work<-data_work[[1]]
        data_work<-na.omit(data_work[,c(1,2,4)])
        ans<-data_work[order(data_work$H_Attack),]
      }
      if (strcmpi(outcome_name,"pneumonia"))
      { data_work<-data_new[state]
        data_work<-data_work[[1]]
        data_work<-na.omit(data_work[,c(1,2,5)])
        ans<-data_work[order(data_work$Pneumonia),]
      }
      
   } 
        else {print("invalid outcome")}
     }
        else {print("invalid state")}
  ans[1,1]
}

rankHospital <- function(state=as.character(), outcome_name=as.character(), num="best")
{  # state is 2 letters
  # heart attack, heart failure, pneumonia
  
  if (state %in% outcome$State)
  {if (strcmpi(outcome_name,"heart failure")|strcmpi(outcome_name,"heart attack")|strcmpi(outcome_name,"pneumonia"))
  { data<- data.frame(outcome$Hospital.Name,
                      outcome$State, 
                      as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                      as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                      as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  colnames(data)[c(1,2,3,4,5)]<-c("Name","State","H_Failure","H_Attack","Pneumonia")
  data_new<-split(data,factor(data$State))
  
  if (strcmpi(outcome_name,"heart failure"))
  { data_work<-data_new[state]
  data_work<-data_work[[1]]
  data_work<-na.omit(data_work[,c(1,2,3)])
    if (num=="best"){ans<-data_work[order(data_work$H_Failure),]; index<-1}
    if (num=="worst"){ans<-data_work[order(-data_work$H_Failure),];index<-1}
    if (is.numeric(num)){ans<-data_work[order(data_work$H_Failure),];index<-num}
  }
  
  if (strcmpi(outcome_name,"heart attack"))
  { data_work<-data_new[state]
  data_work<-data_work[[1]]
  data_work<-na.omit(data_work[,c(1,2,4)])
  if (num=="best"){ans<-data_work[order(data_work$H_Attack),]; index<-1}
  if (num=="worst"){ans<-data_work[order(-data_work$H_Attack),];index<-1}
  if (is.numeric(num)){ans<-data_work[order(data_work$H_Attack),];index<-num}
  
  
  }
  
  if (strcmpi(outcome_name,"pneumonia"))
  { data_work<-data_new[state]
  data_work<-data_work[[1]]
  data_work<-na.omit(data_work[,c(1,2,5)])
  if (num=="best"){ans<-data_work[order(data_work$Pneumonia),]; index<-1}
  if (num=="worst"){ans<-data_work[order(-data_work$Pneumonia),];index<-1}
  if (is.numeric(num)){ans<-data_work[order(data_work$Pneumonia),];index<-num}
  }
  
  } 
    else {print("invalid outcome")}
  }
  else {print("invalid state")}
  print(ans[index,1])
  ans
}

### Rank hospital
rankall<-function(outcome_name=as.character(), num='best')
{if (strcmpi(outcome_name,"heart failure")|strcmpi(outcome_name,"heart attack")|strcmpi(outcome_name,"pneumonia"))
{
  data<- data.frame(outcome$Hospital.Name,
                    outcome$State, 
                    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  new_names <- c("H_Name", "State", "DM_HFailure", "DM_HAttack", "DM_Pneumonia")
  data <- setNames(data, new_names)
  
  split_data<-split(data,data$State)
  
  if(num=="best"){num_order<-FALSE}
  if(num=="worst"){num_order<-TRUE}
  if(outcome_name=="heart failure"){index<-3}
  if(outcome_name=="heart attack"){index<-4}
  if(outcome_name=="Pneumonia"){index<-5}
  
    ans_fun <- function(x) 
    {
      x<-data.frame(x)[,c(1,index)]
      x <- setNames(x, c("H_Name","DM"))
      x<-x[order(x$DM, x$H_Name,na.last=TRUE, decreasing = num_order ),]
      x[1,]
    }
  
 
  FinalAns <-data.frame(t(sapply(split_data, ans_fun)))
  FinalAns
  
}
}









#data <- arrange(data,H_Name)
group_data <- group_by(data,State)

summarise(group_data, best=min(DM_HFailure, na.rm=TRUE),worst=max(DM_HFailure, na.rm=TRUE))
          
HeartFailure <-select(arrange(group_data, DM_HFailure, na.rm = TRUE), c(H_Name,State,DM_HFailure))
HeartAttack <-select(arrange(group_data, DM_HAttack, na.rm = TRUE), c(H_Name,State,DM_HAttack))
Pneumonia <-select(arrange(group_data, DM_Pneumonia, na.rm = TRUE), c(H_Name,State,DM_Pneumonia))

head(HeartFailure)
head(HeartAttack)
head(Pneumonia)




#### Quiz 1 Semana 3
url <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
destfile <-"US_communities.csv"
download.file(url, destfile)

US_comm <-read.table("US_communities.csv", sep=",", header = TRUE)
# How many properties are worth $1,000,000 or more?
sum(na.omit(US_comm["VAL"])>=24)

library(xlsx)
dat<-read.xlsx("NGAP1.xlsx",1, rowIndex=18:23, colIndex = 7:15)
dat

url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
destfile <- "ACS.csv"
download.file(url, destfile)
DT<-fread("ACS.csv")


sapply(split(DT$pwgtp15,DT$SEX),mean)
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
mean(DT$pwgtp15,by=DT$SEX)
DT[,mean(pwgtp15),by=SEX]
tapply(DT$pwgtp15,DT$SEX,mean)
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]


#### Quiz Semana 4
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
destfile <- "ACS_housing.csv"
download.file(url, destfile)
ACS_data<-fread("ACS_housing.csv")

names(ACS_data)
ACS_data <- ACS_data[,agrlogical := ACR==3 & AGS==6]
which(ACS_data$agrlogical)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
destfile <- "Photo.jpg"
download.file(url, destfile, mode= 'wb')
photo <-readJPEG("Photo.jpg", native = TRUE)
quantile(photo, c(0.3,0.8))

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
destfile <- "GDP.csv"
download.file(url, destfile)
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
destfile <- "Education.csv"
download.file(url, destfile)
GDP_data<-fread("GDP.csv")
Edu_data<-fread("Education.csv")

GDP_data_modify <-GDP_data[6:236,c(1,2,4,5)]
GDP_data_modify <-rename(GDP_data_modify, "Long Name"=V4, "CountryCode"=V1, "Rank"=V2)
#Delete blank rows
empty_rows<-GDP_data_modify==""
empty_rows<-which((empty_rows[,1])==TRUE)
GDP_data_modify<-GDP_data_modify[-empty_rows,]


merged_data<- merge(GDP_data_modify,Edu_data, by="CountryCode")
merged_data$Rank <- as.numeric(merged_data$Rank)
merged_data <-merged_data[order(merged_data$Rank),]


grouped <- group_by(merged_data, `Income Group`)
summarise(grouped, GDPAverage=mean(Rank, na.rm = TRUE))


