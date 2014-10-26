Getting-and-cleaning-data
=========================

<!-- -->

    run_analysis <- function(){
        xtest <- read.table("X_test.txt",fill = TRUE , header = FALSE)  ##  load in ##
        ytest <- read.table("Y_test.txt",fill = TRUE , header = FALSE, colClasses = "character")
        test <- read.table("subject_test.txt",fill = TRUE , header = FALSE)
        
        xtrain <- read.table("X_train.txt",fill = TRUE , header = FALSE)
        ytrain <- read.table("Y_train.txt",fill = TRUE , header = FALSE, colClasses = "character")
        train <- read.table("subject_train.txt",fill = TRUE , header = FALSE)
        
        features <- read.table("features.txt",fill = TRUE , header = FALSE)
                
        xbind <- rbind(xtrain, xtest)       ## row merge ##
        ybind <- rbind(ytrain, ytest)    
        subject <- rbind(train, test)
        
        names(xbind) <- features[,2]        ## names ##
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
        
        xy <- cbind(xbind, ybind, subject)      ##column merge ##
        
        meanstd <- vector("numeric", length=nrow(features))     ## set mean + std vector ##
        meanFreq <- vector("numeric", length=nrow(features))    ## set meanFreq vector ##
        
        for(i in 1:nrow(features)){         ## select vector from features contain mean() ##
            if(length(grep("mean()",features[i,2]))>0 || length(grep("std()",features[i,2]))>0)  meanstd[i] <- i
        }
        meanstd[!meanstd %in% 0]
        
        for(i in 1:nrow(features)){     ## select vector from features contain meanFreq() ##
            if(length(grep("meanFreq",features[i,2]))>0)  meanFreq[i] <- i
        }
        meanFreq[!meanFreq %in% 0]
        
        scale <- xy[,c(meanstd[!meanstd %in% meanFreq],562,563)]        ## extracts only the measurement on mean and std for each measurement ##
        
        measurement <- head(names(scale), 66)       ## set up measure.vars for melt function ##
        subjectActivity <- tail(names(scale), 2)    ## set up id for melt function ##
        
        library(reshape2)       # melt function for data classification  ##
        scaleMelt <- melt(scale, id=subjectActivity, measure.vars=measurement)
        
        final <- dcast(scaleMelt, activity+subject~variable, mean)      ## tidy data for each activity and each subject ##  
        
    }
