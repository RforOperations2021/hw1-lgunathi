library(tidyverse)
library(data.table)
library(lubridate)
library(reshape)


myMergedData <- 
  do.call(rbind,
          lapply(list.files(pattern = "^healthy"), fread))


#Removing User Type missing
myMergedData<- myMergedData%>%filter(Usertype!="")




#Converting to Date formats to Month and Year info
myMergedData$Month<-month(as.Date(myMergedData$Starttime,format='%m/%d/%Y'),label = TRUE)
myMergedData$Year<-year(as.Date(myMergedData$Starttime,format='%m/%d/%Y'))


monthly_total <- myMergedData%>%
                 group_by(Month)%>%
                 summarise(total=n())

cust <-   myMergedData%>%
            filter(Usertype=="Customer")%>%
            group_by(Month)%>%
            summarise(customer=n())  

sub <-   myMergedData%>%
  filter(Usertype=="Subscriber")%>%
  group_by(Month)%>%
  summarise(subscriber=n())  


monthly_ridesummary<- cbind(monthly_total,cust,sub)

monthly_ridesummary <- monthly_ridesummary[c(-3,-5)]


# Melting Dataframe for alternative thing
test<- melt(monthly_ridesummary)


fwrite(myMergedData,here("bikeshare-healthyrides/bikes.csv"))
fwrite(monthly_ridesummary,here("bikeshare-healthyrides/ridesummary.csv"))
fwrite(test,here("bikeshare-healthyrides/test.csv"))



#------Code to Test the Plots
ridesummary<- fread(here("bikeshare-healthyrides/ridesummary.csv"))
ridesummary$Month <- factor(ridesummary$Month, levels = month.abb) 


#ggplot(data = ridesummary,aes(x = Month,y = subscriber,group=1))+geom_point()+geom_line()
