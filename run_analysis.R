run_analysis<-function(){
  library(dplyr)
  
  rows <- -1    # for test: set rows = 100 (e.g.) to read small subset
                 # set rows = -1 to read all rows
  features <- read.table("features.txt")
  
  # replace "-" and replace with "_" in column name
  # eliminate the "()" from column name 
  cn <- gsub("-|\\()","_",features[,2])  

  g <- grepl("_mean_|_std_",cn) # TRUE for column of interest
  
  activity_labels <- read.table("activity_labels.txt",stringsAsFactors=FALSE)
  
  
  X <- rbind(
    read.table("X_test.txt",col.names=cn,nrows=rows),
    read.table("X_train.txt",col.names=cn,nrows=rows))
  
  y <- rbind(
    read.table("y_test.txt",stringsAsFactors=FALSE,nrows=rows),
    read.table("y_train.txt",nrows=rows))
  
  subject <- rbind(
    read.table("subject_test.txt",stringsAsFactors=FALSE,nrows=rows),
    read.table("subject_train.txt",stringsAsFactors=FALSE,nrows=rows))
  
  X_trim <- X[,g]  # X_trim contains only column of interest
  X_trim <- cbind(subject,lapply(y,function(n){activity_labels[n,2]}),X_trim,stringsAsFactors=FALSE)
  
  colnames(X_trim)[1:2]=c("subject","activity")
  colnames(X_trim)=gsub("_+$","",colnames(X_trim))
    
  
  X_tidy <- X_trim %>%
    group_by(activity,subject) %>%
    summarise_each(funs(mean))
  
  end <- length(names(X_tidy))
  names(X_tidy)[3:end]=paste0("Avg_",names(X_tidy)[3:end])
  write.table(format(X_tidy,scientific=TRUE),"X_tidy.txt",quote = FALSE,row.names=FALSE,col.names=TRUE)

}


 
