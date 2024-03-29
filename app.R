library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(forcats)
library(gridExtra)

load('Discipline.RData')

PrettyPercent <- function(x) {paste0(round(100*x,digits=1),"%")}


# Define server logic 
server <- function(input, output,session) {
  
  session <- sessionInfo()
  version <- paste0(session$R.version$major,".",session$R.version$minor)
  
  DistrictData <- reactive({
    
    DistrictData_A <- MyData %>% filter(SchoolName %in% get(input$school_level)) %>% group_by(SchoolName,`Student Group`) %>%
    summarize(MedianRate = median(Rate,na.rm=T)) %>%
    mutate(StudentGroup = factor(`Student Group`)) %>% ungroup()
  
  MeanOfMedians <- DistrictData_A %>% group_by(StudentGroup) %>% summarize(MeanOfMedians = mean(MedianRate,na.rm=T)) %>%
    mutate(MeanOfMedians = ifelse(is.nan(MeanOfMedians) == TRUE,-Inf,MeanOfMedians)) %>% ungroup()
  
  left_join(DistrictData_A,MeanOfMedians) %>%
    mutate(ShortSchool = str_remove_all(SchoolName,"Elementary|K-8||PK-8"),
           StudentGroup = fct_reorder(StudentGroup,MeanOfMedians,.default=-Inf,.na_rm = TRUE,.fun=max)) %>%
    mutate(ShortSchool = str_remove_all(ShortSchool,"Middle School")) %>%
    mutate(ShortSchool = str_remove_all(ShortSchool,"High School")) %>%
    mutate(ShortSchool = str_remove_all(ShortSchool,"School"))
  
  })
  
  SchoolHeatData <-  reactive({
    
    SchoolData <- MyData %>% filter(SchoolName == input$school_name) %>% select(`Student Group`,SchoolYr,Rate)
    SchoolData_A <- SchoolData %>% filter(`Student Group` != "All Students")
    SchoolData_B <- SchoolData %>% filter(`Student Group` == "All Students")
    SchoolData_C = left_join(SchoolData_A,SchoolData_B,by = join_by(SchoolYr)) %>% 
      mutate(Discrepancy = Rate.x/Rate.y,
           HigherY = ifelse(Rate.x>Rate.y,1,0),
           LowerY = ifelse(Rate.x<Rate.y,1,0))
  
    SchoolData_C %>% group_by(`Student Group` = as.factor(`Student Group.x`)) %>%
      summarize(#MeanRatio = mean(Discrepancy,na.rm=T),
        MedianRate = median(Rate.x,na.rm=T),
        PercentHigher = mean(HigherY,na.rm=T),
        #PercentLower = mean(LowerY,na.rm=T)
      ) %>%
      mutate(PercentHigher = ifelse(is.nan(PercentHigher) == TRUE,NA,PercentHigher))
  })
  
  
  TeacherCompare <- reactive({
    Teachers2 <- Teachers %>% filter(DemographicCategory == input$teacher_demog) %>%
    inner_join(StudentTempData,Teachers,by=c("SchoolYr","SchoolName" )) 
  })
  
  output$OverviewGraph = renderPlot({
    
    DistrictData() %>% 
      ggplot(aes(x=ShortSchool,y=StudentGroup,fill=MedianRate)) + 
      scale_x_discrete(name="",expand = c(0,0),position='top')+
      scale_y_discrete(name="",expand = c(0,0)) +
      geom_tile(color = "grey") + 
      #scale_fill_viridis_c(na.value="white",option="turbo")+
      scale_fill_viridis_c(na.value="white",option="viridis",direction=-1,labels = scales::percent)+
      #scale_fill_continuous_diverging("Blue-Red 3",mid=.5,na.value="grey80") + 
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 45,hjust=0),
            #axis.text = element_blank(),
            #legend.position = "top",
            legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  output$click_info <- renderTable({
    silly <- nearPoints(DistrictData(),input$plot_click,threshold = 10) %>% 
      mutate(
        MedianRate = PrettyPercent(MedianRate)
      ) %>% select(SchoolName,StudentGroup,MedianRate)
    names(silly) <- c("School","Student Group","Median Rate")
    silly
  })
  
  
  output$MyGraph = renderPlot({
    h1 <- SchoolHeatData() %>% mutate(`Student Group` = fct_reorder(`Student Group`,PercentHigher,.default=-Inf,.na_rm = TRUE)) %>% 
      ggplot(aes(x=factor(1),y =  `Student Group`,#fct_rev(`Student Group`), 
                 fill = PercentHigher)) +
      ggtitle(label="Percent of years with group's\ndisciplinary rate higher than total rate")+
      scale_x_discrete(name="",labels="",expand = c(0,0))+
      scale_y_discrete(name="",expand = c(0,0)) +
      geom_tile(color = "grey") + 
      #scale_fill_viridis_c(na.value="white",option="turbo")+
      scale_fill_viridis_c(na.value="white",option="C",direction=-1,labels = scales::percent,limits=c(0,1))+
      #scale_fill_continuous_diverging("Blue-Red 3",mid=.5,na.value="grey80") + 
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            #axis.text = element_blank(),
            legend.position = "top",
            legend.title=element_blank(),
            legend.key.size = unit(1.5, "cm"),
            plot.title = element_text(hjust = 0.5))
    
    h2 <- SchoolHeatData() %>% mutate(`Student Group` = fct_reorder(`Student Group`,MedianRate,.default=-Inf,.na_rm=TRUE)) %>%
      ggplot(aes(x=factor(1),y =  `Student Group`,#fct_rev(`Student Group`), 
                 fill = MedianRate)) +
      ggtitle("Median disciplinary rate\n ") + 
      scale_x_discrete(name="",labels="",expand = c(0,0))+
      scale_y_discrete(name="",position="right",expand = c(0,0)) +
      geom_tile(color = "black") + 
      #scale_fill_viridis_c(na.value="grey80",option="turbo")+
      scale_fill_viridis_c(na.value="white",option="viridis",direction = -1,labels = scales::percent)+
      #scale_fill_continuous_diverging("Purple-Green",mid=2,na.value="white") + 
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "top",
            legend.title=element_blank(),
            legend.key.size = unit(1.5, "cm"),
            plot.title = element_text(hjust = 0.5))
    
    grid.arrange(h1,h2,ncol=2)
  })
  
  output$MyGraph2 = renderPlot({
    
    Plot1Data <- MyData %>% filter(SchoolName == input$school_name & `Student Group` %in% c("All Students",input$student_group))
    RateTrend <- Plot1Data %>% ggplot(aes(x=SchoolYr,y=Rate,color=`Student Group`)) + 
      geom_line() + geom_point() +
      scale_x_continuous(name="School Year") +
      scale_y_continuous(name="Percent of students excluded from classroom\n in response to a behavioral violation",
                         labels=scales::percent) + 
      scale_color_manual(values = c("grey50","blue")) +
      theme_bw()
    
    Plot2Data <- Plot1Data %>% select(`Student Group`,SchoolYr,Rate) %>% dcast(.,SchoolYr ~ `Student Group` ) %>%
      mutate(Discrepancy = get(input$student_group)/`All Students`,
             DiscColor = factor(case_when(Discrepancy > 1 ~ paste(input$student_group,"Higher"),
                                          Discrepancy < 1 ~ paste(input$student_group,"Lower"),
                                          TRUE ~ NA),ordered=TRUE,levels=c(paste(input$student_group,"Higher"),paste(input$student_group,"Lower"))))
    RatioColumn <- Plot2Data %>% ggplot(aes(x=as.factor(SchoolYr),y=Discrepancy,fill=DiscColor)) + geom_col() +
      scale_x_discrete(name="School Year") +
      scale_y_continuous(name=paste("Ratio of Discipline Rates between\n",input$student_group," Students & All Students")) + 
      scale_fill_viridis_d(
                        labels=c(paste(input$student_group,"Higher"),paste(input$student_group,"Lower")),
                        drop=FALSE) +
      guides(fill=guide_legend(title="")) +
      theme_bw()
    
    grid.arrange(RatioColumn,RateTrend,ncol=2)
    
  })
  
  
  output$TeachersDemographics = renderPlot({
    TeacherCompare() %>%  filter(`Student Group` != "All Students" & SchoolYr == input$teacher_year) %>%
    ggplot(aes(x=TeacherPercent,y=RateRatio, color=`Student Group`)) + geom_point() + 
    facet_wrap(~`Student Group`,scale="free_y") +
    scale_x_continuous(name = paste("Percent of Teachers Who Identify as",input$teacher_demog), labels= scales::percent) +
    scale_color_discrete(guide= "none") +
    scale_y_continuous(name = "Ratio of Student Group's Disciplinary Rate to All Students' Disciplinary Rate\n
                     in the same school and same year")+
    theme_bw()
  })
  
  
  output$click_info2 <- renderTable({
    silly2_a <- TeacherCompare() %>%  filter(`Student Group` != "All Students" & SchoolYr == input$teacher_year)
    silly2 <- nearPoints(silly2_a,input$plot_click2,threshold = 10) %>% 
      mutate(
        TeacherPercent = PrettyPercent(TeacherPercent),
        TeacherDemo = input$teacher_demog
      ) %>% select(SchoolName,`Student Group`,TeacherDemo,TeacherPercent,RateRatio)
    names(silly2) <- c("School","Student Group","Teacher Demographic","Teacher Percent","Disciplinary Rate Ratio")
    silly2
  })
}
  
  
 
# Define UI for application
ui <- fluidPage(
  
  titlePanel('SPS Disciplinary Rate Exploration Tool'),
  sidebarLayout(
    sidebarPanel(
      
      h6("This tool examines the disciplinary rate for multiple student groups for the 2014/5 - 2022/3 school years. OSPI defines the disciplinary rate as the number of student in that group who were excluded from the classroom divided by the number of students in that group during the school year.  As such, a %5 disicplinary rate means that 5% of the students in that group were excluded from the classroom at least once during that school year.
         Due to privacy concerns, most of the numbers provided by OSPI are set to the upper range of the estimate, with the actual estimate possibly being lower.  Disciplinary rates for student groups with fewer than 10 students are not reported (and appear blank in the figures) "),

      h5("District Overview Tab:",style="color:darkgreen"),
      
      h6("This plot shows the median disciplinary rate across years for each student group at each school. Users can choose to display Elementary, K-8, Middle, and High Schools using the 'School Level' drop-down below."),
      
      selectInput("school_level", div(style = "font-size:12px","Select a school level"),
                  SchoolLevel,
                  multiple=FALSE,selected="Elementary"
      ),

      h5("School-level Tab:",style="color:darkgreen"),
      h6("This panel allows users to drill down into the temporal patterns for specific groups at specific schools. The upper-left plot shows the % of years in which the group's rate was above that of the overall student population of that school.  A consistent pattern of being above or below the overall rate typically indicates systemic bias. However, due to the calculations used by OSPI to protect student privacy, the reported estimate for all students may be below that of all reported groups in 100% of the years.
      The plot on the upper-right shows each group's median disciplinary rate.  The plot on the lower-left shows the group's disciplinary rate over time along with that of the overall student population.
         The plot on the lower-right shows the ratio of the student group's disciplinary rate to that of the overall student population.  A value of 10 can be interpreted as the student group having a disicplinary rate that is 10-times that of the overall student population. The specific school and student group can be chosen using the drop-downs below"),
      selectInput("school_name", div(style = "font-size:12px","Select a school"),
                  AllSchools,
                  multiple=FALSE,selected="District Total"
      ),
      selectInput("student_group", div(style = "font-size:12px","Select a group"),
                  AllGroups,
                  multiple=FALSE,selected=AllGroups[1]
      ),

      h5("Teacher Demographics Tab:",style="color:darkgreen"),
      
      h6("These plots combine OSPI's school-level teacher demographics with the OSPI disciplinary rate data. Each plot shows the student groups's disciplinary rate compared to the % of teachers in the selected teacher demographic in that school in that year.  Each point is a single school in a specific year.  The teacher demographic and year can be chosen using the drop-down below."),
   
      selectInput("teacher_demog", div(style = "font-size:12px","Select a teacher demographic"),
                  TeacherDemographics,
                  multiple=FALSE,selected="White"
      ),
      
      selectInput("teacher_year", div(style = "font-size:12px","Select a year"),
                  AllYears,
                  multiple=FALSE,selected=AllYears[1]
      ),
      hr(),
      HTML("Student disciplinary rates and teacher demographic data was downloaded from OSPI's <a href='https://www.k12.wa.us/data-reporting/data-portal'>Data Portal</a>. The data can be downloaded directly 
           via <a href='https://data.wa.gov/education/Report-Card-Discipline-for-2022-23/ixvm-ww8s/about_data'>Report Card Discipline for 2022-23</a>, 
           <a href='https://data.wa.gov/education/Report-Card-Discipline-for-2014-15-to-2021-22-Scho/fwbr-3ker/about_data'>Report Card Discipline for 2014-15 to 2021-22 School Year</a>, and
           <a href='https://data.wa.gov/education/Report-Card-Teacher-Demographics-School-Years-2017/pbem-h4nb/about_data'>Report Card Teacher Demographics School Years 2017-18 to 2022-23</a>.
           "),
      br(),
      br(),
      HTML("Methods used here are based, in part, on <a href='https://publications.aap.org/pediatrics/article/151/3/e2022058848/190637/Equity-Dashboards-Data-Visualizations-for'>Darren Migita, Andrew Cooper, Dwight Barry, Brendan Bettinger, Alicia Tieder, Paul J. Sharek; Equity Dashboards: Data Visualizations for Assessing Inequities in a Hospital Setting. Pediatrics March 2023; 151 (3): e2022058848. 10.1542/peds.2022-058848</a>"),
      br(),
      br(),
      HTML("Please direct questions, comments, and suggestions to <a href='mailto:acooper@alumni.washington.edu'>Andy Cooper</a>"),
      HTML("Data and code can be found at <a href='https://github.com/andrewbcooper/SPS_Discipline'>Andy's GitHub Page</a>.")
      
      
    
   
  ),
    mainPanel(
      tabsetPanel(type="tabs",
            tabPanel("District Overview",
                      plotOutput("OverviewGraph",height='700px',click="plot_click"),
                     h6("Click a cell in the plot, above, for more information"),
                     tableOutput("click_info")),
            tabPanel("School-level Data",                    
                  plotOutput("MyGraph"),
                  plotOutput("MyGraph2")),
            tabPanel("Teacher Demographics",
                    plotOutput("TeachersDemographics",height='700px',click="plot_click2"),
                    h6("Click a point in the plot, above, for more information"),
                    tableOutput("click_info2")
                    ),

            )
      )
  ))


shinyApp(ui = ui, server = server)
