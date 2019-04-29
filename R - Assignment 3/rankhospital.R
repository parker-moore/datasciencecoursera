rankhospital = function(state,outcome,num = "best"){
  data = read.csv("outcome-of-care-measures.csv")
  data = data[,c(2,7,11,17,23)]
  if(state %in% data$State == F){
    message(paste(state,"not found"))
    stop()
  }
  else if(outcome %in% c("heart attack","heart failure",
                         "pneumonia") == F){
    message(paste(outcome,"not found"))
    stop()
  }
  colNum = if(outcome == "heart attack"){3}
  else if(outcome == "heart failure"){4}
  else{5}
  stateRows = data[data$State == state,]
  stateRows[,colNum] = as.numeric(as.character(stateRows[,colNum]))
  outcomeRows = stateRows[order(stateRows[colNum],stateRows$Hospital.Name),]
  outcomeRows = outcomeRows[!(outcomeRows[colNum] == 'Not Available'),]
  outcomeRows = na.omit(outcomeRows)
  
  num = if(num == 'best'){
    as.numeric(1)
  }
  else if(num == 'worst'){
    as.numeric(nrow(outcomeRows))
  }else{
    as.numeric(num)
  }
  if(num > nrow(outcomeRows)){
    NA
  }else{
    outcomeRows$Hospital.Name[num]
  }
}