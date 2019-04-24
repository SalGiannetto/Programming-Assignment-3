rankall<-function(outcome,num="best"){
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
        if (class(num) == "character"){
          if (!(num == "best" || num == "worst")){
            stop("invalid number")
          }
        }
        ## Return hospital name in that state with the given rank 30-day death rate
        
        if (outcome =="heart attack"){
          outcome_fr_c<-outcome_fr_c[,c(1,2,3)] 
        } else if (outcome =="heart failure"){
          outcome_fr_c<-outcome_fr_c[,c(1,2,4)] 
        } else if (outcome =="pneumonia"){
          outcome_fr_c<-outcome_fr_c[,c(1,2,5)] 
        }
        names(outcome_fr_c)[3]="Mortality"
        
        outcome_fr_c[,3]=suppressWarnings(as.numeric(outcome_fr_c[,3]))
        outcome_fr_c=outcome_fr_c[!is.na(outcome_fr_c$Mortality),]
        
        outcome_fr_s=split(outcome_fr_c,outcome_fr_c$State)
        
        result=lapply(outcome_fr_s, function(x, num){
          x=x[order(x$Mortality, x$Hospital.Name),]
          if (class(num)=="character") {
            if (num=="best"){
                              return(x$Hospital.Name[1])}
            else if (num=="worst") {
                              return(x$Hospital.Name[nrow(x)])}
          }
          else return(x$Hospital.Name[num])
          
        },num)
        
        return(data.frame(hospital=unlist(result),state=names(result)))
        
        
  
}