#Loading the xlsx, tidyr & dplyr libraries
library(xlsx)
library(tidyr)
library(dplyr)

#Reading the input excel sheets into data frames
resultsDFUntidy = read.xlsx("ExamData.xlsx", sheetName = "Results")
weightingsDFUntidy = read.xlsx("ExamData.xlsx", sheetName = "Weightings")

#Joining(left join) the result sheet with weight sheet
resultsTidyJoined=resultsDFUntidy %>% 
  gather(key=Subject, value = Score, CX1000:CX1009) %>%
  inner_join(weightingsDFUntidy)
str(resultsTidyJoined)

# Merit list part of assignment
meritDF = resultsTidyJoined %>% group_by(Student.ID) %>% 
  summarise(Overall=sum(Score*Weighting)/60,
            Max=max(Score),Min=min(Score),
            H1=sum(Score<=100 & Score >=70), 
            H2.1=sum(Score<70 & Score >=60),
            H2.2=sum(Score<60 & Score >=50),
            Pass=sum(Score<50 & Score >=40),
            Fail=sum(Score<40)) %>% 
  arrange(desc(Overall))
str(meritDF)
  
# Module analysis part of assignment
moduleDF = resultsTidyJoined %>% group_by(Subject, Description) %>%
  summarise(Average=sum(Score)/50, SD= sd(Score),
            Max=max(Score),Min=min(Score),
            H1=sum(Score<=100 & Score >=70), 
            H2.1=sum(Score<70 & Score >=60),
            H2.2=sum(Score<60 & Score >=50),
            Pass=sum(Score<50 & Score >=40),
            Fail=sum(Score<40)) %>% 
  arrange(desc(Average)) %>% data.frame()
str(moduleDF)

# Writing the result dataframes to seperate excel sheets
write.xlsx(meritDF, "MeritList.xlsx")
write.xlsx(moduleDF, "ModuleAnalysis.xlsx")