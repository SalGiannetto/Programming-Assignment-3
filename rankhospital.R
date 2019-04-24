rankhospital<-function(state,outcome,num="best") {
              ##Read outcome data
              #1 - setting Working directory
                setwd("C:/Users/giannetto_sa/Documents/data science/R codes vari/R Programming/Ass No3")
              
              #2 - ingestion of outcome
                outcome_frame<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
                outcome_fr_c<-outcome_frame[,c(2,7,11,17,23)]
              
              ##Check that state and outcome are valid
                #a - Check on valid outcome
                if (!(outcome == "hear failure" || outcome=="heart attack" || outcome=="pneumonia")){
                  stop("invalid outcome")
                }
                #b - Check on valid state
                
                if (!(state %in% levels(factor(outcome_frame$State)))) {
                  stop("invalid state")
                }
                #c - Check on num
                
                if (class(num) == "character"){
                  if (!(num == "best" || num == "worst")){
                    stop("invalid number")
                  }
                }
              ## Return hospital name in that state with the given rank 30-day death rate
              
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
                
                if (class(num) == "numeric" && 
                      num>nrow(outcome_fr_c_s)){
                  return (NA)
                  } 
                else if (class(num)== "character") {
                       if (num=="best"){return(outcome_fr_c_s$Hospital.Name[1])} 
                       else if (num=="worst"){return(outcome_fr_c_s$Hospital.Name[nrow(outcome_fr_c_s)])}
                }
                else {
                  return(outcome_fr_c_s$Hospital.Name[num])
                }
}