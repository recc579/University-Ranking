## Import Data, call it "d"
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)

setwd('C:/Users/robyn/Documents/Krannert/Using R for Analytics/Final Project')
getwd()
d<- read.table('FinalProgramData.csv', header = TRUE, sep = ',')
r<- as.data.frame(read_xlsx('Ranking_from_websites.xlsx', sheet = 2, na=c('','.')))



################################################################################
# Impute missing
charCol<-d%>%
  select_if(is.character)
d<-d%>%
  select_if(is.numeric)


library(mice)
str(d)
imputedValues <- mice(d, m=10, method = 'rf', seed= 2016)

myList <- imputedValues$imp
myList <- myList[!sapply(myList, is.null)] 
myVars <- names(myList)

# Create a list that contains all 10 imputed datasets
myList <- vector(mode="list", length=10) # Initialize list
# Impute the values
for(i in 1:10){
  d2 <- complete(imputedValues,i)
  myList[[i]] <- d2
}
lengths(myList)
rm(d2,i,imputedValues)

for (j in 1:length(myVars)){
  (var = myVars[[j]])
  
  # put the 10 vectors of imputations in a dataframe
  its <- data.frame(matrix(nrow=nrow(d), ncol=10))
  for (i in 1:10){
    #i=10
    its[,i] <- as.vector(myList[[i]][[var]])
  }
  
  its <- data.frame(values=apply(its, MARGIN=1, FUN=function(x) as.numeric(names(sort(-table(x)))[1])))
  
  d[,var] <- its
}
rm(its,myList,i,j,myVars,var)
d<-cbind(charCol,d)
write.csv(d,'FinalProgramDataImputed.csv',row.names = FALSE)
rm(charCol)
################################################################################
# Sort by Web ranking
################################################################################

#clean r
rQS<-r%>%
  select(University,Degree,Rank = QS)%>%
  filter(!is.na(Rank), Rank !='51+')%>%
  mutate(Rank=as.numeric(Rank))%>%
  arrange(Rank)
  
rUSN<-r%>%
  select(University,Degree,Rank = USN)%>%
  filter(!is.na(Rank))%>%
  mutate(Rank=as.numeric(Rank))%>%
  arrange(Rank)

rMiDS.DS<-r%>%
  select(University,Degree,Rank = MiDS.DS)%>%
  filter(!is.na(Rank), Rank != '>23' )%>%
  mutate(Rank=as.numeric(Rank))%>%
  arrange(Rank)

rMiDS.BA<-r%>%
  select(University,Degree, Rank = MiDS.BA)%>%
  filter(!is.na(Rank), Rank != '>25')%>%
  mutate(Rank=as.numeric(Rank))%>%
  arrange(Rank)

rBeMa<-r%>%
  select(University,Degree, Rank = BeMa)%>%
  filter(!is.na(Rank))%>%
  mutate(Rank=as.numeric(Rank))%>%
  arrange(Rank)

rQS<- inner_join(rQS,d)
rUSN<- inner_join(rUSN, d)
rMiDS.DS<-inner_join(rMiDS.DS,d)
rMiDS.BA<-inner_join(rMiDS.BA,d)
rBeMa<-inner_join(rBeMa,d)

data<- list(rQS,rUSN,rMiDS.DS,rMiDS.BA,rBeMa)
sets<-c('rQS','rUSN','rMiDS.DS','rMiDS.BA','rBeMa')

  
all<-data.frame(matrix(ncol = (length(data[[1]])+1)))
names(all)<-c('WebRank',names(data[[1]]))
for (I in 1:length(data)) {
  data[[I]]<- data.frame(WebRank = rep(sets[I],nrow(data[[I]])), data[[I]])
  names(data)[I]<- sets[I]
  all<- rbind(all,data[[I]])
}
rm(I)
all<-all[-1,]
str(all)


#write.csv(all,'AllRankings.csv',row.names = FALSE)
write.csv(all,'AllRankingsImputed.csv',row.names = FALSE)
rm(d,data,r,rQS,rUSN,rMiDS.DS,rMiDS.BA,rBeMa,sets,all)


################################################################################
#Ranking Analysis
setwd('C:/Users/robyn/Documents/Krannert/Using R for Analytics/Final Project')

d<- read.table('AllRankingsImputed.csv', header = TRUE, sep = ',')
str(d)

################################################################################
# Remove duplicates, leaving in their highest ranking

d1<- d%>%
  group_by(University, Degree)%>%
  filter(Rank == min(Rank))%>%
  arrange(Rank)%>%
  distinct(University, Degree, .keep_all = TRUE)%>%
  ungroup()
  
str(d1)
#### Skip to 'After running...'

source('DataQualityReport.R')

DataQualityReport(d)%>%
  select(-NumberLevels)%>%
  filter(NumberMissing > 0
         ,PercentComplete <=30
         )
  
d<- d1%>%
  select(-WebRank,-University,-Degree,-City,-State,-School)

rm(d1)
################################################################################
#Exploratory plots

d<-as.data.frame(d)
graphIt<- function(var){
  ggplot(d,aes(x= Rank, y=d[,var])) +
    geom_jitter(width = 0, height = 0.1) + 
    geom_smooth(method='lm')+
    labs(title = paste('Scatter Plot of',var,'vs Rank'), x='Rank', y=var)
}
names(d)[1]<-'Rank'
myVars<- names(d)[-1]
library(purrr)
expl<- map(myVars, ~graphIt(.x))

expl[1]
pdf("ExplPlotsImputed.pdf")
expl
dev.off()
rm(expl,myVars)

##### Remove with variance<.2, PercentComplete <= 20, columns clearly represented by another.

nzv <- nearZeroVar(d, freqCut = 80/20, saveMetrics = TRUE)
d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])]
rm(nzv)


#Remove<- DataQualityReport(d)%>% #Not needed after imputation
#  filter(PercentComplete <=20)%>%
#  select(Attributes)

Remove<- c('MinDuration','MaxDuration','AvgDuration','InstateCost','OutofstateCost',Remove[,1])

d<-d%>%
  select(-all_of(Remove))
rm(Remove)
################################################################################

################################################################################
################################################################################
################################################################################
# Run through caret

D<- read.table('AllRankingsImputed.csv', header = TRUE, sep = ',')
d<-D
d<- d%>%
  group_by(University, Degree)%>%
  filter(Rank == min(Rank))%>%
  arrange(Rank)%>%
  distinct(University, Degree, .keep_all = TRUE)%>%
  ungroup()

RemoveNew<- c('YearBegan','AvgCost','StudentWorkExpMonths','AvgTOEFL','AppFeeCost.','NumProgramTypes','NumPostGradStatListed')
Remove<- c('MinDuration','MaxDuration','AvgDuration','InstateCost','OutofstateCost',RemoveNew)
d<- d%>%
  select(-CreditHrs,-DurationMonths)%>%
  select(-all_of(Remove))%>%
  select(-WebRank,-University,-Degree,-City,-State,-School)
rm(Remove, RemoveNew,D)
names(d)[1] <- "y"
# clean environment 
################################################################################
# Identify Correlated Predictors and remove them
################################################################################

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])
descrCor<- ifelse(is.na(descrCor),0,descrCor)
(highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80)) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
(highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80))
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)
descrCor2<- ifelse(is.na(descrCor2),0,descrCor2)
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up
################################################################################
# Identifying linear dependencies and remove them
################################################################################
#No linear combos
#y <- d$y
#d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
#names(d)[1] <- "ones"
#comboInfo <- findLinearCombos(d)
#d <- d[, -comboInfo$remove]
#d <- d[, c(2:ncol(d))]
#d <- cbind(y, d)
#rm(y, comboInfo)  # clean up
################################################################################
# Remove features with limited variation
################################################################################
nzv <- nearZeroVar(d,freqCut = 80/20, saveMetrics = TRUE)
d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])]
rm(nzv)
################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
dd<-d
d<-dd
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)
rm(preProcValues)
################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results

inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set


################################################################################
# Specify cross-validation design
################################################################################
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

################################################################################
# Train Model
################################################################################

logTrain<-train%>%
  mutate(y = log(y))
logTest<-test%>%
  mutate(y=log(y))

logTrain<- logTrain[,-3]
# train a random forest on down-sampled dataset
myGrid <-  expand.grid(mtry = c(10,15,20))  # number of times to bag
rf <- train(y ~ .,              # model specification
            data = logTrain,     # train set used to build model
            method = "rf",      # type of model you want to build
            trControl = ctrl,   # how you want to learn
            tuneGrid = myGrid,  # tuning parameter combos to try
            metric = "RMSE"      # performance measure
)
rf
rf_tr<-predict(rf, newdata= logTrain)
rf_te<-predict(rf,newdata = logTest)

# model rf
defaultSummary(data = data.frame(obs = logTrain$y,pred=predict(rf,newdata = logTrain)), model = rf)
defaultSummary(data = data.frame(obs = logTest$y,pred=predict(rf,newdata = logTest)), model = rf)

logit <- train(y ~ .,                 # model specification
               data = logTrain,        # train set used to build model
               method = "lm",      # type of model you want to build
               trControl = ctrl,
               metric = "RMSE"       # performance measure
)
logit

lm_tr<-predict(logit, newdata= logTrain)
lm_te<-predict(logit,newdata = logTest)

# model logit
defaultSummary(data = data.frame(obs = logTrain$y,pred=predict(logit,newdata = logTrain)), model = logit)
defaultSummary(data = data.frame(obs = logTest$y,pred=predict(logit,newdata = logTest)), model = logit)
################################################################################
# Attempt to see what variables impact rank the most
names(d)
d<-data.frame(d)
reg<-data.frame(NA,NA)
for(i in 2:ncol(d)){
  regression<- lm(y~d[,i],d)
  reg[i-1,]<- c(names(d)[i],summary(regression)$coefficients[2,4])

}
names(reg)<-c("Variable","pVal")
reg%>%
  arrange(pVal)
rem<-reg%>%
  filter(pVal>0.5)
##############

#check residuals
# code from Dr. Tawarmalan's business analytics class
library(broom)
model<- lm(y~.,logTrain)
c2 <- augment(model)
ggplot(c2, aes(x=.fitted, y=.resid)) + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Fitted Values", y="Residuals", title = "Residual Plot") +
  geom_point() + geom_hline(yintercept = 0)
qqnorm(c2$.resid)
ggplot(c2, aes(x=.resid)) + theme_classic() +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals") +
  geom_histogram(bins=20, fill="steelblue", color="black")
summary(model)

#rm(c2,ctrl,inTrain,logit,model,myGrid,reg,regression,rf,test,train,i)
#have settled on the logit model
#saveRDS(logit, "model.rds")
#rm(list=ls())

