best = function(state,outcome){
  #read outcome data
  data = read.csv("outcome-of-care-measures.csv")
  #check state and outcome are valid
  if(!(state %in% data$State)){
    message(paste(state,"not found"))
    stop()
  }
  if(!(outcome %in% c("heart attack","pneumonia","heart failure"))){
    message(paste(outcome,"not found"))
    stop()
  }

  colNum = if(outcome == "heart attack"){11}
  else if(outcome == "heart failure"){17}
  else{23}
    
  data[,colNum] = as.numeric(as.character(data[,colNum]))
  data$State = as.character(data$State)
  #return hospital name in the state with lowest 30-day death rate
  m = data[data$State == state,]
  i = min(m[,colNum],na.rm=T)
  ret = which(as.numeric(m[,colNum]) == as.numeric(i))
  hospitals = m[ret,2]
  hospitals = sort(hospitals)
  hospitals[1]
}