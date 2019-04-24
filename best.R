best <- function(state,outcome){
        ##Read outcome data
        #1 - setting Working directory
        setwd("C:/Users/giannetto_sa/Documents/data science/R codes vari/R Programming/Ass No3")
        #2 - ingestion of outcome
        outcome_frame<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        #2a - i took the following columns: HospitalName, State, HeartAttack, HeartFailure, Pneumonia
        outcome_fr_c<-outcome_frame[,c(2,7,11,17,23)]
        
        #-#Check that state and outcome are valid
        #a# check for a valid outcome
        if (!(outcome=="heart attack" || outcome=="heart failure"|| outcome=="pneumonia")) {
          stop("invalid outcome")
        }
        #b# check for a valid State
        if (!(state %in% levels(factor(outcome_fr_c$State)))) { #using the combination of function factor (create attribute) and levels (get attribute's levels) to get distinct values of state
          stop("invalid state")
        }
        
        #selection of data in terms of input (state, outcome)
        if (outcome =="heart attack"){
        outcome_fr_c<-outcome_fr_c[outcome_fr_c$State==state,c(1,3)] 
        } else if (outcome =="heart failure"){
          outcome_fr_c<-outcome_fr_c[outcome_fr_c$State==state,c(1,4)] 
        } else if (outcome =="pneumonia"){
          outcome_fr_c<-outcome_fr_c[outcome_fr_c$State==state,c(1,5)] 
        }
        names(outcome_fr_c)[2]="Mortality"
        
        outcome_fr_c[,2]=suppressWarnings(as.numeric(outcome_fr_c[,2]))
        outcome_fr_c=outcome_fr_c[!is.na(outcome_fr_c$Mortality),]
        
        outcome_fr_c_s=outcome_fr_c[order(outcome_fr_c$Mortality, outcome_fr_c$Hospital.Name),]
        return(outcome_fr_c_s$Hospital.Name[1])
        
        
        
        
        
        
  
        ##Return hospital name in that state with lowest 30-day death rate
       
  
}