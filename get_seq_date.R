get_seq_date <- function(x){
  y <- NULL
  s_y <- NULL
  if(length(x) > 1){
    for(i in 1:(length(x)-1)){
      y[i]  <- round((as.Date(x[i+1],"%m/%d/%Y" )-as.Date(x[i], "%m/%d/%Y"))/91) 
    }
    if(length(y) != 1){
      for(j in 1:(length(y)+1)){
        if(j == 1){
          s_y[j] <- 1
        }else{
          s_y[j] <- s_y[j-1] + y[j-1]    
        }
      }
    }else{
      s_y[1] <- 1
      s_y[2] <- 1+y[1]
    }
  }else{
    s_y <- 1
  }
  
  return(s_y- 1)
}
