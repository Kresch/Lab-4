#which combination of parties(voting) explains the size of a swedish city?

#From the dataset of votes we have a column of Rostb. (Allowed to vote in each city.). Can this
#size be explained by fitting an LM with the partysizes(in percentage) as covariates?


#extract the data we need:
library(caret)
ex_data<-function(theData){
        data <- data.frame(theData[,unlist(lapply(colnames(theData),function(y) substr(y,start=nchar(y)-2,stop=nchar(y))))=="tal"])
        data<-data.frame(data,theData$Rostb)
        colnames(data)<-c(colnames(theData)[unlist(lapply(colnames(theData),function(y) substr(y,start=nchar(y)-2,stop=nchar(y))))=="tal"],"Rostb")
        in_train <- createDataPartition(y = data$Rostb,p=0.75,list = FALSE)
        #how to actually get the coefficients???
        model1<-train(data$Rostb[in_train]~.,data=data[in_train,],method="leapForward")
        }