makeitmelt<-function(data,idvars=c(),followupvar=c(),followupcount=integer(),start=integer()){
  dataframes_to_melt <- list(list())
  for (i in  followupvar){
    var<-rep(i,followupcount )
    series<-seq(start,(followupcount+start-1), by=1)
    follow <-c(idvars,paste(var,series,sep=""))
    index <- which(i==followupvar)
    #print(c(index," Going to sleep"))
    #Sys.sleep(time = 5)
    
    dataframes_to_melt[[index]] <- melt(data[,which(colnames(data)%in%follow)],id=c(idvars))
    colnames(dataframes_to_melt[[index]])[ncol(dataframes_to_melt[[index]])] <- i
    dataframes_to_melt[[index]][ncol(dataframes_to_melt[[index]])-1] <- NULL
    
  
  }
  final_molten<-do.call("cbind", dataframes_to_melt)
  final_molten1 <- final_molten[,unique(colnames(final_molten))]
  final_molten1$time_point<-rep(seq(1,followupcount,by=1),times=1,each=nrow(data))
  return(final_molten1)
}
#followupvar should be in of the form _F


