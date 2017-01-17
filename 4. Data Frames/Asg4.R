
#Setting the working directory to the path of input exam data excel sheet
setwd("//fs2/16231160/Desktop/R") 

#Installing the xlsx package in order to perform excel read and write operations
install.packages("xlsx")
library(xlsx)

#Reading the input excel sheets into data frames
rawResultsDF = read.xlsx("Exam Data.xlsx", sheetName = "Results")
weightingsDF = read.xlsx("Exam Data.xlsx", sheetName = "Weightings")

#In order to use subset.data.frame function, loading the 'plyr' library
library(plyr)


#Defining function to filter out the raw data based on missing values & incorrect percentages
dataFilter=function(x,lowerRange=0,upperRange=100,na.rm = F){
  x>=lowerRange & x<= upperRange & !is.na(x)
}

#Filtering the raw data to get correctly formatted records
resultsDF = subset.data.frame(rawResultsDF, dataFilter(CX1000) & dataFilter(CX1001) &
                                dataFilter(CX1002) & dataFilter(CX1003) &
                                dataFilter(CX1004) & dataFilter(CX1005) &
                                dataFilter(CX1006) & dataFilter(CX1007) &
                                dataFilter(CX1008) & dataFilter(CX1009))

#Filtering out the error records into a new data frame
errorDF = subset.data.frame(rawResultsDF, !(dataFilter(CX1000) & dataFilter(CX1001) &
                              dataFilter(CX1002) & dataFilter(CX1003) &
                              dataFilter(CX1004) & dataFilter(CX1005) &
                              dataFilter(CX1006) & dataFilter(CX1007) &
                              dataFilter(CX1008) & dataFilter(CX1009)))

#Defining a function to retreive weightage of each subject
getWeightage = function(subject, df=weightingsDF){
  df[df$Subject == subject,3]
}

#Calculating the Overall value for each student
resultsDF$Overall = (resultsDF$CX1000*getWeightage("CX1000") + resultsDF$CX1001*getWeightage("CX1001")+
                     resultsDF$CX1002*getWeightage("CX1002") + resultsDF$CX1003*getWeightage("CX1003") +
                     resultsDF$CX1004*getWeightage("CX1004") + resultsDF$CX1005*getWeightage("CX1005") +
                     resultsDF$CX1006*getWeightage("CX1006") + resultsDF$CX1007*getWeightage("CX1007") +  
                     resultsDF$CX1008*getWeightage("CX1008") + resultsDF$CX1009*getWeightage("CX1009"))/60

#Defining a Function to grade the students based on their overall percentages calculated in last step
getGrade=function(overallPercent){
  if(overallPercent <=100 && overallPercent >= 70)
    "H1"
  else if(overallPercent <70 && overallPercent >= 60)
    "H2-1"
  else if(overallPercent <60 && overallPercent >= 50)
    "H2-2"
  else if(overallPercent <50 && overallPercent >= 40)
    "Pass" 
  else
    "Fail"
}

#Adding a new column- Grade to result data frame 
resultsDF$Grade = lapply(resultsDF$Overall,getGrade)

#Writing the output into two seperate excel sheets(one for correctly formatted result & another for error records)
write.xlsx(resultsDF, "CorrectlyFormattedRecords.xlsx")
write.xlsx(errorDF, "ErrorRecords.xlsx")
