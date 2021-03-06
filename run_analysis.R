run_analysis <- function(){
    xtest <- read.table("X_test.txt",fill = TRUE , header = FALSE)  
    ytest <- read.table("Y_test.txt",fill = TRUE , header = FALSE, colClasses = "character")
    test <- read.table("subject_test.txt",fill = TRUE , header = FALSE)
    
    xtrain <- read.table("X_train.txt",fill = TRUE , header = FALSE)
    ytrain <- read.table("Y_train.txt",fill = TRUE , header = FALSE, colClasses = "character")
    train <- read.table("subject_train.txt",fill = TRUE , header = FALSE)
    
           
    xbind <- rbind(xtrain, xtest)       
    ybind <- rbind(ytrain, ytest)    
    subject <- rbind(train, test)
    
    
    features <- read.table("features.txt",fill = TRUE , header = FALSE)
    names(xbind) <- features[,2]        
    names(ybind) <- "activity"
    names(subject) <- "subject"
    
    
    for(i in 1:nrow(ybind)){
        if(ybind[i,1]=="1"){
            ybind[i,1] <- "WALKING"
        }else if(ybind[i,1]=="2"){
            ybind[i,1] <- "WALKING_UPSTAIRS"
        }else if(ybind[i,1]=="3"){
            ybind[i,1] <- "WALKING_DOWNSTAIRS"
        }else if(ybind[i,1]=="4"){
            ybind[i,1] <- "SITTING"
        }else if(ybind[i,1]=="5"){
            ybind[i,1] <- "STANDING"
        }else{
            ybind[i,1] <- "LAYING"
        }
    }    
    
    xy <- cbind(xbind, ybind, subject)   
    
    
    meanstd <- vector("numeric", length=nrow(features))    
    meanFreq <- vector("numeric", length=nrow(features))   
    
    for(i in 1:nrow(features)){        
        if(length(grep("mean()",features[i,2]))>0 || length(grep("std()",features[i,2]))>0)  meanstd[i] <- i
    }
    meanstd[!meanstd %in% 0]
    
    for(i in 1:nrow(features)){     
        if(length(grep("meanFreq",features[i,2]))>0)  meanFreq[i] <- i
    }
    meanFreq[!meanFreq %in% 0]
    
    scale <- xy[,c(meanstd[!meanstd %in% meanFreq],562,563)]        
    
    
    measurement <- head(names(scale), 66)       
    subjectActivity <- tail(names(scale), 2)    
    
    library(reshape2)       
    scaleMelt <- melt(scale, id=subjectActivity, measure.vars=measurement)
    
    final <- dcast(scaleMelt, activity+subject~variable, mean)
    write.table(final, "~/Desktop/Programming/R language/tidy.txt", sep="\t")
    
}
