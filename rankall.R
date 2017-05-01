rankall <- function(outcome, num="best") {
  ## Read outcome data
  outcomedat<-read.csv("newfile.csv", colClasses = "character")
  
  ## check that outcome is valid
  outlist<-(c("HEART ATTACK!!!","pneumonia","heart failure"))
  if (is.na(match(outcome,outlist))) stop("invalid outcome")
  
  ## check that num is "best", "worst" or a number
  if (is.na(match(num,c("best","worst")))) {
    if (is.numeric(num)) {num<-as.integer(num)}
    else {stop("invalid num")}    
  }
  
  ## select just the outcome we want
  if (outcome == "heart attack") {sortdat<-as.numeric(outcomedat[,11])}
  else if (outcome == "heart failure") {sortdat<- as.numeric(outcomedat[,17])}
  else if (outcome == "pneumonia") {sortdat<- as.numeric(outcomedat[,23])}
  
  ## For each state, find the hospital of the given rank
  staterank<-character()
  for (state in unique(outcomedat$State)) {
    ## select data for just the state we want
    statedat<-outcomedat[which(outcomedat$State==state),]
    outdat <-sortdat[which(outcomedat$State==state)]
    
    ## get rid of the missing values
    ok <- complete.cases(outdat)
    hospname <- statedat[ok,2]  
    outdat<-outdat[ok]
    
    ## sort the data from lowest->highest value of outcome
    ##  (hospital name breaks ties)
    o<-order(outdat,hospname)
    
    ## find hospital name in that state with given ranking 
    statehosp<- {
      if (num == "best") {hospname[o][1]}  
      else if (num == "worst") {hospname[o][length(hospname)]}
      else if (num <= length(hospname)) {hospname[o][num]}
      else {NA}
    }
    staterank<-c(staterank, statehosp)
  } # end of loop over states

  # Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame("hospital"=staterank,"state"=unique(outcomedat$State),row.names=unique(outcomedat$State))
  }
