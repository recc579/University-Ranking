## Import Data, call it "d"
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)

setwd('C:/Users/robyn/Documents/Krannert/Using R for Analytics/Final Project')
getwd()

d<- data.frame(read_xlsx('Program Data - FullCleaned.xlsx', sheet = 1))
add<- data.frame((read_xlsx('2018 program data.xlsx', sheet='Raw data')))
################################################################################
# Data cleaning
################################################################################
d1<-d
d<-d1

source('DataQualityReport.R')
##### Getting more PlacementRate and AvgStartingSalary using University and Degree as identifiers.
add<-add%>%
  select(University, Degree = Degree.name, Placement.rate, Avg.starting.salary)%>%
  filter(!is.na(Placement.rate)|!is.na(Avg.starting.salary))

add$Placement.rate<-ifelse(add$Placement.rate=='-'|add$Placement.rate=='na',NA,add$Placement.rate)
add$Avg.starting.salary<-ifelse(add$Avg.starting.salary=='-',NA,add$Avg.starting.salary)

add$Placement.rate<-as.numeric(add$Placement.rate)
add$Avg.starting.salary<-as.numeric(add$Avg.starting.salary)


add$Placement.rate<-ifelse(add$Placement.rate>1,add$Placement.rate/100,add$Placement.rate)
str(add)


dadd<- left_join(d,add)

d$PlacementRate<-ifelse(is.na(d$PlacementRate),dadd$Placement.rate,d$PlacementRate)
d$AvgStartingSalary<- ifelse(is.na(d$AvgStartingSalary),dadd$Avg.starting.salary,d$AvgStartingSalary)

rm(add,dadd,d1)
######### Combine the classes
courses<- c('R','Python','SAS','DatabaseSQL','DataMiningDescriptiveAnalytics','PredictiveAnalytics'
            ,'MachineLearning','DeepLearning','PrescriptiveAnalyticsOptimization','Simulation'
            ,'DataVisualizationTableau','BigDataHadoopSparkHive','CommunicationPublicspeaking'
            ,'TextAnalyticsNLP','WebminingWebCrawlingDataScraping','CloudAWSGCP','ContainersKubernetesDocker'
            ,'LinuxUbuntu','DataEthics')

appFeatures<- c('RequireSOP','GMATGRE','MinGMATGRE','TOEFLIELTSPTEscores','NumRecLetters','Interview','Appfee')

benefits<- c('Scholarships','STEMCertified','IndustryProjects','CareerFairs','CorpInfoSessions','CompanySponsorsListed')

program<- c('FT','PT','Online','Hybrid')

gradinfo<- c('PlacementRolesListed','HiringCompaniesListed')
Remove<- c(courses,appFeatures,benefits,program,gradinfo)
d2<-d
d<-d2
d<-d%>%
  mutate(NumDSCources = ifelse(rowSums(is.na(d[,courses]))==length(courses),NA,rowSums(d[,courses],na.rm = TRUE))
         ,NumBenefits = ifelse(rowSums(is.na(d[,benefits]))==length(benefits),NA,rowSums(d[,benefits],na.rm = TRUE))
         ,NumAppFeatures = ifelse(rowSums(is.na(d[,appFeatures]))==length(appFeatures),NA,rowSums(d[,appFeatures],na.rm = TRUE))
         ,NumProgramTypes = ifelse(rowSums(is.na(d[,program]))==length(program),NA,rowSums(d[,program],na.rm = TRUE))
         ,NumPostGradStatListed = ifelse(rowSums(is.na(d[,gradinfo]))==length(gradinfo),NA,rowSums(d[,gradinfo],na.rm = TRUE))
         ,Rigour = CreditHrs/DurationMonths)%>%
  select(-all_of(Remove))

rm(courses,benefits,appFeatures,d2,program,gradinfo, Remove)
##### Removing variables with low percent complete or will not use in app/does not impact the ranking
##### StudentGMATGRE is not standardized
Remove<- DataQualityReport(d)%>%
  filter(PercentComplete <11)%>% # 11 chosen to keep PctInternational
  select(Attributes)

Remove<- c('Student','Website','StudentGMATGRE',
           Remove[,1])


d<- d%>%
  select(-all_of(Remove))
rm(Remove)

##### Removing rows with more than 70% data missing
d<-cbind(PctMissing=apply(d,1,function(x) sum(is.na(x))/(ncol(d)-5)*100),d) #not counting university or degree
d<- d%>%
  filter(PctMissing <70)
d<-d%>%
  select(-PctMissing)




DataQualityReport(d)%>%
  select(-NumberLevels)#%>%
  #filter(NumberMissing > 0
         #    , PercentComplete <40
  #)

write.csv(d,'FinalProgramData.csv', row.names = FALSE)

