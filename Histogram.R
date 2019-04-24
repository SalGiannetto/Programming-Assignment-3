#change of Working directory
setwd("C:/Users/giannetto_sa/Documents/data science/R codes vari/R Programming/Ass No3")
#reading the outcome file
outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

outcome[,11]<-as.numeric(outcome[,11])

hist(outcome[,11],ylim=c(0,800))