library(shiny)
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)


Regions<- c('Northeast', 'Southeast', 'Midwest','Southwest', 'West')
state<-as.data.frame(read_xlsx('StateByRegion.xlsx'))
programs<- read.table('RankedPrograms.csv',header = TRUE,sep = ',')%>%
  arrange(University)
allRank<- read.table('AllRankings.csv',header = TRUE,sep = ',')

#https://ggplot2-book.org/polishing.html
# Used to help create ggplot2 theme
myTheme<- theme_update(plot.margin= margin(t = 50, r = 100, b = 20, l = 10),
                       plot.background = element_rect(colour = "grey", size = 2),
                       plot.title = element_text(size = 20),
      axis.title = element_text(size = 15),
      axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0, size = 13),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15, face = "bold"))


ui <- fluidPage(titlePanel(title = "Master's Programs in Business Analytics and Data Science Rankings"),
  tabsetPanel(
  tabPanel("Our University Ranking",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = 'region', 'Choose a region', c('Northeast', 'Southeast', 'Midwest','Southwest', 'West'),
                           selected = 'Midwest'),
               uiOutput(outputId = 'chooseUni'),
               uiOutput(outputId = 'chooseDegree')
             ),
             mainPanel(
               fluidRow(column(width = 6,h2(textOutput(outputId = 'uniName'))),
                        column(width = 6,offset = 6, h5('Rank'),h1(strong(textOutput(outputId = 'rank'))))),
               fluidRow(column(width = 9,h4(textOutput(outputId = 'uniSchool')),
                               h4(textOutput(outputId = 'uniDegree'))
                               )),
               fluidRow(dataTableOutput(outputId = 'uniStat'))
             )
           )
           ),
  tabPanel("Established Rankings",
           sidebarLayout(
             sidebarPanel(radioButtons(inputId = 'website',
                                       label = "Which website ranking do you want to see?",
                                       choices = c('Top Universities','US News',
                                                   'Masters in Data Science: Business Analytics',
                                                   'Masters in Data Science: Data Science','Best Masters')),
                          dataTableOutput('webrank')),
           mainPanel(verticalLayout(plotOutput('women'),
                                    plotOutput('international'),
                                    plotOutput('placement'),
                                    plotOutput('salary'),
                                    plotOutput('appRec')))
           ))
  )
  )


server <- function(input, output, session) {
  states<- reactive({as.character(state[,names(state)==input$region])})
  output$chooseUni<- renderUI({
    selectInput(inputId = 'University', label = 'Choose a University',
                choices = as.character(programs[programs$State %in% state[,names(state)==input$region],'University']),
                selected = 'Purdue University')
  }) # Taken from the Pokemon shiny app
  output$chooseDegree<- renderUI({
    selectInput(inputId = 'Degree', label = 'Choose a Degree',
                choices = as.character(programs[programs$University== input$University,'Degree']))
  })
  output$uniName<- renderText({as.character(input$University)})
  output$uniDegree<- renderText({paste('Degree:',as.character(input$Degree))})
  output$uniSchool<- renderText({paste('School:',as.character(programs[input$University==programs$University&input$Degree==programs$Degree,
                                         'School']))})
  output$rank<-renderText({as.character(programs[input$University==programs$University&input$Degree==programs$Degree,
                                                 'Rank'])})
  output$uniStat<- renderDataTable({
    programs[input$University==programs$University&input$Degree==programs$Degree,c("StudentGPA","AvgStartingSalary",'NumBenefits','NumDSCources')]}
    ,options = list(dom = 't'))
  
  r<- data.frame(r=c('rQS','rUSN','rMiDS.BA','rMiDS.DS','rBeMa'), 
                 web= c('Top Universities','US News','Masters in Data Science: Business Analytics' ,'Masters in Data Science: Data Science','Best Masters'))
  web<- reactive({r[r$web==input$website,'r']})
  output$webrank<- renderDataTable(allRank[allRank$WebRank==web(),c('Rank','University','Degree')],
                                   options = list(dom = 't',
                                                  autoWidth = TRUE))
  output$appRec<- renderPlot(
    ggplot(allRank[allRank$WebRank==web(),]
           ,aes(x=reorder(University, Rank), y= NumAppFeatures, fill=Rank))+
      geom_bar(stat = 'identity')+
      labs(title = 'Number of Application Features by Program', x = 'University')
  )
  output$women<- renderPlot(
    ggplot(allRank[allRank$WebRank==web(),], aes(x=reorder(University, Rank), y= Pctwomen, fill=Rank))+
      geom_col()+
      labs(title = 'Percent Women by Program', x = 'University')
  )
  output$international<- renderPlot(
    ggplot(allRank[allRank$WebRank==web(),], aes(x=reorder(University, Rank), y= PctInternational, fill=Rank))+
      geom_col()+
      labs(title = 'Percent International by Program', x = 'University')
  )
  output$placement<- renderPlot(
    ggplot(allRank[allRank$WebRank==web(),], aes(x=reorder(University, Rank), y= PlacementRate, fill=Rank))+
      geom_col()+
      labs(title = 'Placement Rate by Program', x = 'University')
  )
  output$salary<- renderPlot(
    ggplot(allRank[allRank$WebRank==web(),], aes(x=reorder(University, Rank), y= AvgStartingSalary, fill=Rank))+
      geom_col()+
      labs(title = 'Average Starting Salary by Program', x = 'University')
  )
}

shinyApp(ui, server)

