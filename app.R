#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(shinyWidgets)
library(shinydashboardPlus)
library(dashboardthemes)
library(ggthemes)
library(quantmod)


ui <- dashboardPage(
  dashboardHeader(title =span(tagList(icon("dollar"),"USA Inflation Rate")), dropdownMenuOutput("msgoutput"),
                  dropdownMenu(type = "notifications", 
                               notificationItem(
                                 text = "Sri Lankan announced broke",
                                 icon("exclamation-triangle"),
                                 status="danger"
                               ))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("list-alt")),
      menuItem("World Map and US Inflation", tabName = "page2", icon = icon("area-chart")),
      menuItem("U.S. Cityzens' Income Changes", tabName = "page3", icon = icon("map-o")),
      menuItem("Oil and Gold", tabName = "page4", icon = icon("bar-chart-o")),
      menuItem("More Infomation", tabName = "page5", icon = icon("info"))
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme="blue_gradient"),
   tabItems(
      tabItem(tabName = "page1",
              column(width = 12, img(src = "", width = 600), align = "center"),
              fluidRow(
                box(
                  title = "The big Inflation in the United States of America in 2022 and Its Consequences - Post-COVID Economic Situation in the USA", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = FALSE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("We would like to create a web application that shows the influence of ",tags$strong("the big inflation in 2022 USA after the Covid 19 pandemic"), 
                               "and the large QE monetary policy by the Federal Reserve."),
                           )
                         )
                  )
                )
              ),
              fluidRow (
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("This dataset is about "),
                           br(), br(),
                           tags$li(tags$strong("Source: https://www.kaggle.com/mathurinache/world-happiness-report-20152021 "),tags$a(href = "https://www.kaggle.com/mathurinache/world-happiness-report-20152021")),
                           tags$li("The filtered dataset for this application contains total",tags$strong("935"), "cases (in ", tags$strong("8"), "columns) from 2015 to 2020.")
                         )
                  )
                )
              ),
              fluidRow (
                         box(
                           title = "Team",
                           solidHeader = FALSE,
                           status = "warning",
                           collapsible = TRUE,
                           width = 12,
                           socialBox(
                             title = "xxx",
                             subtitle = "xxxx",
                             src = 'xxx',
                             width = 6,
                             closable = FALSE,
                             color = "aqua-active",
                             "xxx"
                           ),
                           socialBox(
                             title = "XXX",
                             subtitle = "xxx",
                             src = 'xxx',
                             width = 6,
                             closable = FALSE,
                             color = "aqua-active",
                             " xxx"
                           ),
                           socialBox(
                             title = "xxx",
                             subtitle = "xxx",
                             src = 'xxx',
                             width = 6,
                             closable = FALSE,
                             color = "aqua-active",
                             "xxx"
                           ),
                           socialBox(
                             title = "xxx",
                             subtitle = "xxxx",
                             src = 'xxx',
                             width = 6,
                             closable = FALSE,
                             color = "aqua-active",
                             "xxx"
                  ),
                  tags$iframe(width="560", height="315", src="//www.youtube.com/embed/UMAELCrJxt0",
                              frameborder="0", allowfullscreen="true")
                  
                )
              )
      ),
      tabItem(tabName = "page2",
              titlePanel("Tittle"),
              tabItem(tabName = "page2", h2("Inflation around the world"),
                      #sliderInput("Year","Number of year:",min = 2015,max = 2021,value = 1, 
                                  #step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                      #plotlyOutput("map", width = 800)
                      
                      checkboxInput("holiday", label = "Show big events", value = FALSE),
                      plotlyOutput("plot1", height = 500))
              ),
      tabItem(tabName = "page3",
              titlePanel("U.S. Cityzens' Income Changes"),
              tabsetPanel(
                tabPanel("Tab1 for data1", 
                         fluidRow(
                           column(9,plotlyOutput("plot2", height = 500)),
                           column(3,box(title = "Explaination", solidHeader = TRUE,
                                       status = "success", width = 12, collapsible = FALSE)),
                         )),
                tabPanel("Tab2 for data2", 
                         fluidRow(
                           column(9,"plotlyOutput"),
                           column(3,box(title = "Explaination", solidHeader = TRUE,
                                        status = "success", width = 12, collapsible = FALSE)),
                ))
              
      )),
      tabItem(tabName = "page4",
              
              h2("Relationship between price and date"),
              br(),
              
              plotlyOutput("plot3", height = 500,width = "100%"),
              p('As can be seen from the above chart, the highest point for oil prices in the past 39 years was on June 2, 2008, reaching $140. 
                The event that led to the price increase at the time was the US financial crisis. 
                The second is 2022, the main event is the outbreak of the Russian-Ukrainian war and the US sanctions against Russia. 
                Furthermore, the third highest point was the spring of 2011, when the main event was the outbreak of the Egyptian revolution.', style = "font-size:20px;"),
              
              br(),
              plotlyOutput("plot3_2", height = 500),
              p('As can be seen from the chart above, the highest point for gold in the past 22 years will be in 2022. 
                Global inflation accelerated due to the Russian-Ukrainian war. 
                Second in 2020, the main event was the outbreak of covid-19 and the Fed raised interest rates. 
                The third was during 2011-2012, when the Federal Reserve introduced new monetary easing policies to promote economic recovery.', style = "font-size:20px;"),      
              
              br(),
              h2(icon('youtube'), 'Price News', style = "font-size:30px;"),
              p('Here is a video covers crude oil and gold price change, click ', a('here', href = 'https://youtu.be/IPKdzOhM3uY'),
                'to watch it on Youtube, or click the video below to watch it here.', style = "font-size:20px;"),
              HTML('<iframe width="50%" height="300" src ="https://www.youtube.com/embed/IPKdzOhM3uY" frameborder="0" allowfullscreen ></iframe>')
              
      ),
      tabItem(tabName = "page5",
              titlePanel("Tittle"),
              fluidRow(
                box(title="Type your stock symbol ", solidHeader = TRUE, status = "info",collapsible = TRUE, width = 12,
                    sidebarLayout(
                      sidebarPanel(
                        textInput("symb", h4("Symbol"), "^GSPC"),
                        radioButtons(inputId="time", label=h4("Time"), 
                                     choices=c("last 1 year","last 3 years","last 5 years")),
                      ),
                      
                      mainPanel(
                        textOutput("stocktitle"),
                        br(),
                        plotOutput("plot")
                      ))
                   )),
              titlePanel("Our Data"),
              fluidRow(
                box(title= "3 data sets", solidHeader = TRUE, status = "info",collapsible = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Tab1 for data1", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(23,103,124,0.2);", dataTableOutput("myTable1")))
                                 
                               )),
                      tabPanel("Tab2 for data2", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(0,198,179,0.2);", dataTableOutput("myTable2")))
                                 
                               )),
                      tabPanel("Tab3 for data3", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(0,198,179,0.2);", dataTableOutput("myTable3")))
                                 
                               ))))),
              br(),
              br(),
              absolutePanel(actionButton("Button1","Reference"),  bottom = 10, right = 100),
      )         
    )
  )
)



server <- function(input, output, session) {
  inf = read_csv("inflation.csv")
  strDates <- c(inf$Date)
  inf$Date <- as.Date(strDates, "%Y/%m/%d")
  adj = read_csv("interestrateadjustment.csv")
  adj[2] <- NULL
  strDates2 <- c(adj$Date)
  adj$Date <- as.Date(strDates2, "%Y/%m/%d")
  words=unique(adj$`Discount Rate`)
  Abb=c("1.75%" ,"1.00%", "0.50%", "0.25%" ,"2.75%", "3.00%",
      "2.50%" ,"2.25%" ,"2.00%" ,"1.50%", "1.25%", "0.35%")
  adj$Abb=adj$`Discount Rate` 
  for (i in 1:length(words)) {adj$Abb=str_replace(adj$Abb,words[i],Abb[i]) }
  data = left_join(inf, adj, by = "Date")

  
  
  
  
  output$plot1 = renderPlotly({
    f = ggplot(data = data) +
    geom_line(mapping = aes(x = Date, y = Inflation))+
    geom_smooth(mapping = aes(x = Date, y= Inflation))+
    labs(title = "USA inflation", x = "Date", y = "inflation (%)")
  
    f + theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'))

    data_subset = data %>% 
    filter(!is.na(`Discount Rate`))

    if(input$holiday==TRUE){
      data_subset = data %>% 
        filter(!is.na(date))
      
    f=f+geom_point(data = data_subset, aes(x=Date, y=Inflation),color="purple")+
    geom_text(data = data_subset, aes(x=Date, y=Inflation,label=Abb))}
    ggplotly(f)
    
  })
  
  
  output$plot2 = renderPlotly({
    

      income <- zzc_data <- read_csv("zzc_data.csv", col_types = cols(`date` = col_datetime(format = "%m/%d/%Y")))
      m1 <- ggplot(data = income, aes(x=date)) + 
      ggtitle("Weekly Income from 2013 to 2022 by Gender",
              subtitle = "in U.S. dollars")+
      geom_line(aes(y = Wage), color = "darkred") + 
      geom_line(aes(y = Women), color="steelblue", linetype="twodash")+

      ggtitle("Income Change from 2013 to 2022 by gender")

    
        
    m1 <- ggplotly(m1)

    inflation <- T5YIE <- read_csv("T5YIE.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
    m2 <- ggplot(data = inflation, aes(x=T5YIE)) +
      geom_line(aes(y = date), color = "darkred")
    
    m2 <- ggplotly(m2)
    
    m2
    
  })
  
  output$plot3 = renderPlotly({
    oil <- read_csv("crude-oil-price.csv")
    date_New <- as.Date(oil$date, "%m/%d/%Y")
    oil$date <- date_New
    
    f <-ggplot(data = oil, aes(x=date,y=price))+ ggtitle("Crude Oil Price Change")+
      geom_line()+
      geom_smooth()
    f <- ggplotly(f)
    f
    
    
  })
  
  
  output$plot3_2 = renderPlotly({
    gold <- read_csv("gold.csv")
    Date_New <- as.Date(gold$Date, "%m/%d/%Y")
    gold$Date <- Date_New
    
    g <- ggplot(data = gold, aes(x=Date,y=Close))+ ggtitle("Gold Price Change")+
      geom_line()+
      geom_smooth()
    g <- ggplotly(g)
    g
    
  })
  
  output$myMap = renderLeaflet({
    
  })
  
  
  output$stocktitle <- renderText({
    paste("Your stock (^GSPC for S&P500): ", input$symb)
  })
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               auto.assign = FALSE)
  })
  output$plot <- renderPlot({
    print(chartSeries(dataInput(), theme = chartTheme("black"), 
                      up.col = "green", dn.col = "red", 
                      TA = NULL, name = input$symb, subset = input$time))
    
    
  })
  output$myTable1=renderDataTable({
    return(datatable(data, rownames= FALSE))
  })
  
  observeEvent(
    input$Button1, {
      input$Reference
      showModal(modalDialog(title="Reference", 
                            "list of Reference..."))
    })
  
  
  
}

shinyApp(ui = ui, server = server)