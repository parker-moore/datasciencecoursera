rankall = function(outcome,num="best"){
  data = read.csv("outcome-of-care-measures.csv")
  data = data[,c(2,7,11,17,23)]

  if(outcome %in% c("heart attack","heart failure",
                         "pneumonia") == F){
    message(paste(outcome,"not found"))
    stop()
  }
  
  colNum = if(outcome == "heart attack"){3}
  else if(outcome == "heart failure"){4}
  else{5}
  
  #make data numeric so it can be compared when ordering
  data[,colNum] = as.numeric(as.character(data[,colNum]))
  data = data[order(data[colNum],data$Hospital.Name),]
  #get rid of NA
  data = data[!data[colNum] == 'Not Available',]
  data = na.omit(data)
  
  #grab rows for each state with rank == num
  stateData = split(data,data$State)
  rankData = data.frame(matrix(ncol = 5,nrow = 0))
  colnames(rankData) = colnames(data)
  for(i in stateData){
    #grab the appropriate row
    num = if(num == 'best'){
      as.numeric(1)
    }
    else if(num == 'worst'){
      as.numeric(nrow(i))
    }else{
      as.numeric(num)
    }
    if(num > nrow(i)){
      DF = data.frame(matrix(ncol = 5,nrow = 1))
      colnames(DF) = colnames(rankData)
      rankData = rbind(rankData,DF)
      rankData[1][nrow(rankData),] = NA
      rankData[2][nrow(rankData),] = as.character(i[2][1,])
      rankData[3][nrow(rankData),] = NA
      rankData[4][nrow(rankData),] = NA
      rankData[5][nrow(rankData),] = NA
    }else{
      DF = data.frame(matrix(ncol = 5,nrow = 1))
      colnames(DF) = colnames(rankData)
      rankData = rbind(rankData,DF)
      rankData[1][nrow(rankData),] = as.character(i[1][num,])
      rankData[2][nrow(rankData),] = as.character(i[2][num,])
      rankData[3][nrow(rankData),] = i[3][num,]
      rankData[4][nrow(rankData),] = i[4][num,]
      rankData[5][nrow(rankData),] = i[5][num,]
    }
  }
  rankData = rankData[order(rankData[2]),]
  rankData
}