
setwd("~/Desktop/Discipline")
library(readr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

Report_Card_Discipline_for_2014_15_to_2021_22_School_Year <- 
  read_csv("Report_Card_Discipline_for_2014-15_to_2021-22_School_Year.csv")
Report_Card_Discipline_for_2022_23 <- read_csv("Report_Card_Discipline_for_2022-23.csv")

Discipline = rbind(Report_Card_Discipline_for_2014_15_to_2021_22_School_Year,Report_Card_Discipline_for_2022_23) %>%
  filter(DistrictName == "Seattle School District No. 1")

silly <- Discipline %>% group_by(DistrictName) %>% summarize(mycount = n())
silly <- Discipline %>% group_by(Rate) %>% summarize(mycount = n())
rm(Report_Card_Discipline_for_2014_15_to_2021_22_School_Year,Report_Card_Discipline_for_2022_23 )


table(Discipline$`Student Group`)

Discipline$SchoolYr = as.numeric(substr(Discipline$SchoolYear, start = 1, stop = 4))
Discipline$Rate = ifelse(Discipline$DisciplineRate == 'N<10',NA,Discipline$DisciplineRate)
Discipline$Rate <- as.numeric(gsub("<|%","",Discipline$Rate))/100

MyData <- Discipline %>% select(SchoolName, `Student Group`,GradeLevel, SchoolYr,Rate) %>% 
  filter(!`Student Group` %in% 
           c("Non Migrant","Non Military Parent","Non Section 504","Non-English Language Learners","Non-Foster Care",
             "Non-Highly Capable","Non-Homeless","Non-Low Income","Students without Disabilities") &
           GradeLevel == "All")

TempSchool = "Adams Elementary School"
TempCat = "Black/ African American"

silly <-  MyData %>% filter(SchoolName == TempSchool)

Plot1Data <- MyData %>% filter(SchoolName == TempSchool & `Student Group` %in% c("All Students",TempCat))
Plot1Data %>% ggplot(aes(x=SchoolYr,y=Rate,color=`Student Group`)) + geom_line() + geom_point() +
  scale_x_continuous(name="School Year") +
  scale_y_continuous(name="Percent of students excluded from classroom\n in response to a behavioral violation",
                     labels=scales::percent) + 
  theme_bw()

Plot2Data <- Plot1Data %>% select(`Student Group`,SchoolYr,Rate) %>% dcast(.,SchoolYr ~ `Student Group` ) %>%
  mutate(Discrepancy = get(TempCat)-`All Students`,
         DiscColor = factor(case_when(Discrepancy > 0 ~ paste(TempCat,"Higher"),
                               Discrepancy < 0 ~ paste(TempCat,"Lower"),
                               TRUE ~ NA),ordered=TRUE,levels=c(paste(TempCat,"Higher"),paste(TempCat,"Lower"))))

Plot2Data %>% ggplot(aes(x=as.factor(SchoolYr),y=Discrepancy,fill=DiscColor)) + geom_col() +
  scale_x_discrete(name="School Year") +
  scale_y_continuous(name=paste("Difference in Percent Disciplined between\n",TempCat," Students & All Students"),
                     labels=scales::percent) + 
  scale_fill_manual(values=c("red","blue"),
                    labels=c(paste(TempCat,"Higher"),paste(TempCat,"Lower")),
                    drop=FALSE) +
  guides(fill=guide_legend(title="")) +
  theme_bw()
  
 
