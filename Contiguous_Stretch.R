Contiguous_Stretch <- function(data, indx){
  seq_list <- list(list())
  data_subset <- data[,indx]
  na_count <- data.frame(apply(is.na(data_subset),1,sum))
  names(na_count) <- "missing_count"
  na_count[which(na_count$missing_count != 0),] <- NA
  na_ind <- which(is.na(na_count) == T)
  #########################
  for(i in na_ind){
    ind_of_na <- which(i == na_ind)
    diff <- na_ind[ind_of_na+1] - na_ind[ind_of_na]
    seq_list[[ind_of_na]] <- c(diff,i)  
  }
  #########################
  temp <- data.frame(do.call(rbind,seq_list))
  ind_with_long_stretch <- temp[which(temp[,1] == max(temp[,1], na.rm = T)),2]
  ind_with_long_stretch_next <- na_ind[(which(na_ind == ind_with_long_stretch) + 1)]
  data <- data[((ind_with_long_stretch+1) : (ind_with_long_stretch_next-1)),]
  return(data)
}







