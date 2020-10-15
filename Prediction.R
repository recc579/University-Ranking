# Making the ranking predictions
library(dplyr)
library(ggplot2)
library(caret)

setwd('C:/Users/robyn/Documents/Krannert/Using R for Analytics/Final Project')
getwd()
d<- read.table('FinalProgramDataImputed.csv', header = TRUE, sep = ',')
my_model <- readRDS("model.rds")

D<-d%>%
  select("BusinessSchool","Analytics","StudentAge","StudentGPA","Pctwomen","PctInternational",
         "PlacementRate","AvgStartingSalary","NumDSCources","NumBenefits","NumAppFeatures","Rigour" )
preProcValues <- preProcess(D, method = c("range"))
D <- predict(preProcValues, D)
rm(preProcValues)
dd<- read.table('FinalProgramData.csv', header = TRUE, sep = ',')
pred<- predict(my_model, newdata = D)
pred<- exp(pred)
#pred
dd<-cbind(PredRank = pred, dd)
rm(D,my_model,pred)

sum(d$University==dd$University)==nrow(d)

Rank<- rank(dd$PredRank, ties.method = 'min')
dd<-cbind(Rank,dd)
dd<-dd%>%
  arrange(Rank)
write.csv(dd,'RankedPrograms.csv', row.names = FALSE)
rm(d,dd,Rank)
