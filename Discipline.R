
setwd("~/Desktop/Discipline")
library(readr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(forcats)
library(colorspace)

#Report_Card_Discipline_for_2014_15_to_2021_22_School_Year <- 
#  read_csv("Report_Card_Discipline_for_2014-15_to_2021-22_School_Year.csv")
#Report_Card_Discipline_for_2022_23 <- read_csv("Report_Card_Discipline_for_2022-23.csv")

#Discipline = rbind(Report_Card_Discipline_for_2014_15_to_2021_22_School_Year,Report_Card_Discipline_for_2022_23) %>%
#  filter(DistrictName == "Seattle School District No. 1")

#silly <- Discipline %>% group_by(DistrictName) %>% summarize(mycount = n())
#silly <- Discipline %>% group_by(Rate) %>% summarize(mycount = n())
#rm(Report_Card_Discipline_for_2014_15_to_2021_22_School_Year,Report_Card_Discipline_for_2022_23 )


table(Discipline$`Student Group`)

Discipline$SchoolYr = as.numeric(substr(Discipline$SchoolYear, start = 1, stop = 4))
Discipline$Rate = ifelse(Discipline$DisciplineRate == 'N<10',NA,Discipline$DisciplineRate)
Discipline$Rate <- as.numeric(gsub("<|%","",Discipline$Rate))/100

MyData <- Discipline %>% select(SchoolName, `Student Group`,GradeLevel, SchoolYr,Rate) %>% 
  filter(!`Student Group` %in% 
           c("Non Migrant","Non Military Parent","Non Section 504","Non-English Language Learners","Non-Foster Care",
             "Non-Highly Capable","Non-Homeless","Non-Low Income","Students without Disabilities") &
           GradeLevel == "All")

TempSchool = "Bailey Gatzert Elementary School"
TempCat = "Female"

SchoolData <-  MyData %>% filter(SchoolName == TempSchool) %>% select(`Student Group`,SchoolYr,Rate)
SchoolData_A <- SchoolData %>% filter(`Student Group` != "All Students")
SchoolData_B <- SchoolData %>% filter(`Student Group` == "All Students")
SchoolData_C = left_join(SchoolData_A,SchoolData_B,by = join_by(SchoolYr)) %>% 
  mutate(Discrepancy = Rate.x/Rate.y,
         HigherY = ifelse(Rate.x>Rate.y,1,0),
         LowerY = ifelse(Rate.x<Rate.y,1,0))

SchoolHeatData <- SchoolData_C %>% group_by(`Student Group` = `Student Group.x`) %>%
  summarize(#MeanRatio = mean(Discrepancy,na.rm=T),
            MedianRatio = median(Discrepancy,na.rm=T),
            PercentHigher = mean(HigherY,na.rm=T),
            #PercentLower = mean(LowerY,na.rm=T)
            ) %>%
  mutate(PercentHigher = ifelse(is.nan(PercentHigher) == TRUE,NA,PercentHigher))

h1 <- SchoolHeatData %>% ggplot(aes(x=factor(1),y =  fct_rev(`Student Group`), 
                                 fill = PercentHigher)) +
  ggtitle(label="Percent of years with group's\ndisciplinary rate higher than total rate")+
  scale_x_discrete(name="",labels="",expand = c(0,0))+
  scale_y_discrete(name="",expand = c(0,0)) +
  geom_tile(color = "black") + 
  #scale_fill_viridis_c(na.value="white",option="turbo")+
  scale_fill_viridis_c(na.value="white",option="C",direction=-1,labels = scales::percent,limits=c(0,1))+
  #scale_fill_continuous_diverging("Blue-Red 3",mid=.5,na.value="grey80") + 
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
        
h2 <- SchoolHeatData %>% ggplot(aes(x=factor(1),y =  fct_rev(`Student Group`), 
                              fill = MedianRatio)) +
  ggtitle("Median ratio of group's\n disciplinary rate to total rate") + 
  scale_x_discrete(name="",labels="",expand = c(0,0))+
  scale_y_discrete(name="",position="right",expand = c(0,0)) +
  geom_tile(color = "black") + 
  #scale_fill_viridis_c(na.value="grey80",option="turbo")+
  scale_fill_viridis_c(na.value="white",option="viridis",direction = -1)+
  #scale_fill_continuous_diverging("Purple-Green",mid=2,na.value="white") + 
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))

grid.arrange(h1,h2,ncol=2)

Plot1Data <- MyData %>% filter(SchoolName == TempSchool & `Student Group` %in% c("All Students",TempCat))
RateTrend <- Plot1Data %>% ggplot(aes(x=SchoolYr,y=Rate,color=`Student Group`)) + 
  geom_line() + geom_point() +
  scale_x_continuous(name="School Year") +
  scale_y_continuous(name="Percent of students excluded from classroom\n in response to a behavioral violation",
                     labels=scales::percent) + 
  scale_color_manual(values = c("grey50","blue")) +
  theme_bw()

Plot2Data <- Plot1Data %>% select(`Student Group`,SchoolYr,Rate) %>% dcast(.,SchoolYr ~ `Student Group` ) %>%
  mutate(Discrepancy = get(TempCat)/`All Students`,
         DiscColor = factor(case_when(Discrepancy > 1 ~ paste(TempCat,"Higher"),
                               Discrepancy < 1 ~ paste(TempCat,"Lower"),
                               TRUE ~ NA),ordered=TRUE,levels=c(paste(TempCat,"Higher"),paste(TempCat,"Lower"))))
RatioColumn <- Plot2Data %>% ggplot(aes(x=as.factor(SchoolYr),y=Discrepancy,fill=DiscColor)) + geom_col() +
  scale_x_discrete(name="School Year") +
  scale_y_continuous(name=paste("Ratio of Discipline Rates between\n",TempCat," Students & All Students")) + 
  scale_fill_manual(values=c("red","blue"),
                    labels=c(paste(TempCat,"Higher"),paste(TempCat,"Lower")),
                    drop=FALSE) +
  guides(fill=guide_legend(title="")) +
  theme_bw()
  
 
