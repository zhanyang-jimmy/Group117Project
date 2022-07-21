#Group 117 Code
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
library(shinyalert)
library(dplyr)
library(maps)


oil <- read_csv("crude-oil-price.csv")
gold <- read_csv("gold.csv")
inf = read_csv("inflation.csv")
tags$style(HTML("
      #border1 {        

  
  border: 6px lightgrey;
  padding: 5px;
  margin: 2px;
  border-style: groove;
      }
      #borderM {
          border-radius: 25px;
  border: 2px solid #485461;
  padding: 20px;
    border-style: groove;

 
      }
       #sidebar {
            background-color: #485461;
            padding: 10px;
            margin: 60px;
            border: 6px lightgrey;
            border-style: groove;

       }
       #sidebar1 {
            background-color: rgba(0,198,179,0.2);
            padding: 10px;
            margin: 30px;
            border: 7px rgba(0,198,179,0.2);
            border-style: groove;

       }
        
    "))

ui <- dashboardPage(
  
  
  dashboardHeader(title =span(tagList(icon("dollar"),"USA Inflation Rate")), dropdownMenuOutput("msgoutput"),
                  dropdownMenu(type = "notifications", 
                               notificationItem(
                                 text = "Sri Lankan announced broke",
                                 icon("exclamation-triangle"),
                                 status="danger"
                               ))),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("list-alt")),
      menuItem("World Map and US Inflation", tabName = "page2", icon = icon("globe")),
      menuItem("Income, Spending, Housing", tabName = "page3", icon = icon("map-o")),
      menuItem("Oil and Gold", tabName = "page4", icon = icon("bar-chart-o")),
      menuItem("More Infomation", tabName = "page5", icon = icon("info"))
      
    )
  ),
  dashboardBody(
    
    shinyDashboardThemes(theme="grey_dark"),
   tabItems(
      tabItem(tabName = "page1",
              h2(strong("World & US Inflation"), align = "center"),
              br(),
              fluidRow(
                # A static infoBox
                infoBox("US Inflation", "9.1%", icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              ),              
              titlePanel("Inflation in our REAL LIFE"),
                column(width = 9, img(src = "1.png", width = 900)),
              box(
                title = "Fun Fact about US Inflation History", solidHeader = TRUE,
                status = "info", width = 3, collapsible = TRUE,
                column(12, 
                       tags$div(
                         tags$span(
                           p("The inflation rate in 1979 was 11.35%. The inflation
                           rate in 1980 was 13.50%. The 1980 inflation rate is 
                           higher compared to the average inflation rate of 3.09% 
                           per year between 1980 and 2022.Inflation rate is 
                             calculated by change in the consumer price index 
                             (CPI). The CPI in 1980 was 82.40. It was 72.60 in 
                             the previous year, 1979. The difference in CPI between 
                             the years is used by the Bureau of Labor Statistics 
                             to officially determine inflation."))))),

              
              mainPanel(
                h1("Background"),
             div(id="borderB", p("Inflation in the United States was relatively low for so long that, for entire generations of 
                               Americans, rapid price hikes may have seemed like
                               a relic of the distant past. 
                               Between the start of 1991 and the end of 2019, 
                               year-over-year inflation averaged about 2.3% a month, 
                               and exceeded 5.0% only four times.
                               Today, Americans rate inflation as the nation’s 
                               top problem, and President Joe Biden has said 
                               addressing the problem is his top domestic priority."),
                               p("But the U.S. is hardly the only place where people 
                               are experiencing inflationary whiplash. 
                               A Pew Research Center analysis of data from 44 
                               advanced economies finds that, in nearly all of 
                               them, consumer prices have risen substantially 
                               since pre-pandemic times."),
                             p("Annual U.S. inflation in the first quarter of 
                               this year averaged just below",tags$strong("8.0%")," – 
                               the 13th-highest rate among the 44 countries examined. 
                               The first-quarter inflation rate in the U.S. was almost 
                               four times its level in 2020’s first quarter."))),
     
              column(width = 9, img(src = "2.png", width = 900)),
              box(
                title = "US 2022 June Inflation Report", solidHeader = TRUE,
                status = "info", width = 3, collapsible = TRUE,
                column(12, 
                       tags$div(
                         tags$span(
                           p("The consumer price index rose 9.1% from a year 
                             earlier in a broad-based advance, the largest gain
                             since the end of 1981, Labor Department data showed
                             Wednesday. The widely followed inflation gauge increased 
                             1.3% from a month earlier, the most since 2005, 
                             reflecting higher gasoline, shelter and food costs."))))),
              mainPanel(
                h1("About the Dataset"),
                div(id="borderB",
                p("We choose the dataset from various sources. 
                  For the inflation dataset, we would like to see the global 
                  situation under the pre-covid 19 and what has happened after 
                  the covid 19 pandemic, that is post-covid 19 period. 
                  As we heard from the news that the department of labor in the
                  US has anoucned that in 2022, June, the CPI inflation rate is 
                  about 9.1% which has been the highest over 30 years. This is an 
                  important issue thus the statistics from the official website is a must."),
                
                p("However, when we think back about the reason behind the covid 
                  19 pandemic, the federal reserves' monetary policy has become 
                  a great reason due to the covid 19 large QE policy made by the 
                  administration. As a result, the Fed's interest rate dataset 
                  and large QE dataset is a must as well."),
                p("Also, the news report from the WALL STREET JOURNAL, 
                  the statistics from Statista, the gold price, 
                  stock market price and the performance of the economic 
                  around the world including the US will be our datasets to 
                  reveal how serious the inflation is in summer 2022.")),
                h2("Resources"),
                div(id="borderB",
              tags$ul(tags$li("THE WALL STREET JOURNAL"),
                      tags$li("The Federal Reserve "),
                      tags$li("Gold & Stock Market Price"),
                      tags$li("World Inflation dataset including US")
              ))),
             br(),
              fluidRow (
                         box(
                           title = "Team",
                           solidHeader = FALSE,
                           status = "warning",
                           collapsible = TRUE,
                           width = 12,
                           socialBox(
                             title = "Zijie Chen",
                             subtitle = "Master of Science in Business Analytics and Risk Management, Johns Hopkins University",
                             color = "aqua-active",
                             "Zijie Chen is a Master student at Johns Hopkins 
                             University, majoring in Business Analytics and Risk
                             Management. Has previous work experience in the loan 
                             department of the bank. Graduated from Penn State Univ
                             ersity with a Bachelor’s degree, concentrated on the 
                             Economics. Shows great passion on Data Analytics and Marketing."
                           ),
                           socialBox(
                             title = "Zihao Fang",
                             color = "aqua-active",
                             "Zihao Fang is a Master student at Carey Business School, majoring in Business Analytics and Risk
                             Management. Graduated from Penn State Univ
                             ersity with a Bachelor’s degree, concentrated on the 
                             Supply Chian Management and Information System."
                           ),
                           socialBox(
                             title = "Changqing Ye",
                             color = "aqua-active",
                             "Majoring in Business Analytics and 
                             Risk Management at John's Hopkins University. 
                             Experienced Underwriting Analyst with a demonstrated 
                             history of working in the financial real estate industry. 
                             Skilled in R, HTML/JS, Public Speaking, and Microsoft Office. 
                             Strong finance professional with a Bachelor's degree 
                             focused in Business Management, Marketing, and finance 
                             from New York University."
                           ),
                           socialBox(
                             title = "Zhan Yang",
                             color = "aqua-active",
                             "Background education is in public accounting and data analytics. 
                             Zhan has some experience in professional accounting in companies and banks.
                             Zhan joined JHU Carey Business School in 2021 August as Business Analytics and Risk Management student.
                            "
                             ),
                           socialBox(
                             title = "Zhaochen Zhu",
                             color = "aqua-active",
                             "Zhaocheng Zhu is Business Analytics and Risk Management student at Johns Hopkins who is passionate about analyzing 
                             financial market using analytical skills such as R, python, SQL. Graduated from UC Irvine, he pursued Economics major 
                             and accounting minor, which provide him with solid foundation of the business world.His previous experience in finance
                             and accounting industry equipped him of deep understanding of the market."
                           )),
                         mainPanel(
                                 
                                 
                           div( id="center",
                         h1("Youtube Videos about Inflation"),
                  tags$iframe(width="700", height="400", src="//www.youtube.com/embed/UMAELCrJxt0",
                              frameborder="0", allowfullscreen="true")
                  
                
              )))
      ),
      tabItem(tabName = "page2",
            
              
              
              tabItem(
                tabName = "page2",
                div(
                  style = 
                    "height: 80px; background-color: #537895; width: 100%; position: absolute; right:0;",
                  h2("Inflation around the world", 
                     style="text-align:center;color: white")),
                  br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                
                      fluidRow(
                  
                        
                        column( width=5, 
                                box(solidHeader = TRUE, status = "info",collapsible = F, width = 7,
                                  sliderInput("Year","Number of year:",min = 2015,max = 2021,value = 1, 
                                  step = 1, animat = animationOptions(interval = 2000, loop = FALSE)),
                          
                      div(id="border1",
                        h4('According to the heat map, we can see that the inflation 
                           rate in Brazil is 8.3% in 2021, which is much higher than
                           3.2% in 2020 and the highest since 2015.  Among developed
                           countries, the US inflation rate is the most prominent.  
                           Before the epidemic, the inflation rate of the world economy, 
                           especially developed economies, was below 2% for a long time. 
                           The United States began to be affected by the epidemic in March 2020. 
                           After a few months, the inflation rate of the United States gradually increased.  
                           Because the epidemic has greatly affected the degree of inflation.'))),
                     
                       column(4,  offset = 1, plotlyOutput("plot4",height = 600, width = 800)))),
                br(),
                br(),
                      div(
                        style = 
                          "height: 80px; background-color: #537895; width: 100%; position: absolute; right:0;",
                        h2(HTML("More graph"), 
                           style="text-align:center;color: white"),
                        ),
              
                      
                br(),
                br(),
                br(),
                br(),
                br(),
                br(), 
            
                      fluidRow(
                        column(1,), 
                               column(id="borderB", width= 5, 
                                      plotlyOutput("plot1")),
                        column(id="border1", width= 5,offset=1, 
                               h4(("Based on what we can see from the graph, that 
                                   during the pre covid time period, that means before 
                                   2019, the inflation rate in the US is relatively stable
                                   for around 2%. However, in the beginning of the covid 19
                                   , the society economy immediately went down and the as we can
                                   click on the show big events to see the large QE and Fed's
                                   monetary policy, the interest rate dropped immediately which is 
                                   almost close to 0. However, the Fed keep the low interest rate until
                                   the end of 2021 and beginning of 2022, when they start to think about
                                    the consequence of large QE on CPI. At that time, there is still hesitation
                                    about raising interest rate but so we can see moderate adjustment. However, after
                                   9.1% inflation comes out, the raising interest rate will be a sure thing in the near future."), height = 400, width = 50))),
                        
                        absolutePanel(
                          checkboxInput("holiday", label = "Show Fed Interest Rate Adjustment", value = FALSE),bottom = 20, right = 450),
                        
                   
                      br(),
                      br(),

                    
             
              
              )) ,
      tabItem(tabName = "page3",
              titlePanel("Income, Spending, Housing Price with Inflation"),
              tabsetPanel(
                tabPanel("Income", 
                         fluidRow(img(src = 'wallstreet.png', height = 200, width = 1700)),
                         fluidRow(
                           column(6,
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2", height = 400, width = 800))),
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2_2", height = 400, width = 800)))),
                           column(6,
                                  h2("Inflation Gave Income Downward Pressure"),
                                  br(),
                                  p(id="borderB",
                                    "Weekly income grew steadily from 2013 to the beginning of the 2020. During this time, 
                                    the inflation rate was roughly between 1% - 2%, which was stable.Starting from Mar 2020 to 
                                    Feb 2021, the U.S. government signed three acts: CRRSA, CARES,ARPA to stimulate the economy.
                                    The income goes up three times during this period but quickly
                                    falls. Meanwhile, the inflation rate bounces up three times after each of three acts were 
                                    signed from 0.14% to 3.59%.We can tell from the graph that when the inflation rate was stable, the real disposable
                                    personal income grows steadily. After the pandemic started, three stimulation acts caused 
                                    increases on income, but also pushed the inflation rate up to a high level. High level 
                                    inflation rate, in return, may negatively impact the income.",
                                    style = "font-size:20px;"),
                                  h2("What Are 3 Stimulations"),
                                  div(box(
                                    title = "CRRSA", solidHeader = TRUE,
                                    status = "info", width = 4, collapsible = TRUE,
                                    column(12, 
                                           tags$div(
                                             tags$span(
                                               p("The Coronavirus Response and Relief Supplemental Appropriations (CRRSA) 
                                                     Act provided $10 billion in supplemental Child Care and Development Fund 
                                                     (CCDF) funding to prevent, prepare for, and respond to coronavirus. "))))),
                                    box(
                                      title = "CARES", solidHeader = TRUE,
                                      status = "info", width = 4, collapsible = TRUE,
                                      column(12, 
                                             tags$div(
                                               tags$span(
                                                 p("The Coronavirus Aid, Relief, and Economic Security Act or, CARES Act, was 
                                                     passed by Congress on March 27th, 2020. This bill allotted $2.2 trillion to 
                                                     provide fast and direct economic aid to the American people negatively 
                                                     impacted by the COVID-19 pandemic."))))),
                                    box(
                                      title = "ARPA", solidHeader = TRUE,
                                      status = "info", width = 4, collapsible = TRUE,
                                      column(12, 
                                             tags$div(
                                               tags$span(
                                                 p("The American Rescue Plan Act of 2021 is a US$1.9 trillion economic stimulus 
                                                     bill passed by Joe Biden on March 11, 2021 to speed up the country's recovery 
                                                     from the economic and health effects of the COVID-19 pandemic and the ongoing 
                                                     recession"))))),
                                    
                                  ),
                                  ))),
                tabPanel("Spending", 
                         fluidRow(img(src = 'spending.png', height = 200, width = 1700)),
                         fluidRow(
                           column(6,
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2_4", height = 400, width = 800))),
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2_5", height = 400, width = 800))),
                           ),
                           column(6,
                                  h2("Increasing Inflation Drove Increasing Spending"),
                                  br(),
                                  p(id="borderB",
                                    "-Personal consumer expenditure plummeted right after the pandemic 
                                    began. This is because during the pandemic, many cities were locked 
                                    down, and so the spending shrank. Increasing inflation during this 
                                    time represents that the economy grows fast. Meanwhile, disposal 
                                    income is increasing. Therefore, after the lockdown was over, 
                                    personal consumer expenditure grows fast with a high inflation rate.",
                                    style = "font-size:20px;"),
                                  h2("Spending Change During Covid-19"),
                                  br(),
                                  img(src = 'Spending Change.png', height = 500, width = 700),
                                  p(h3("As we can see from the graph, during the pandemic, the consumer 
                                    spending decreased nearly 50% at most."))
                                  
                                  
                                  
                           ))),
                
                tabPanel("Housing Price", 
                         fluidRow(img(src = 'housing.png', height = 200, width = 1700)),
                         fluidRow(
                           column(6,
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2_3", height = 400, width = 800))),
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("plot2_6", height = 400, width = 800)))),
                           column(6,
                                  h2("Housing Price piked with High Inflation"),
                                  br(),
                                  p(id="borderB",
                                    "Housing price index was increasing at a higher pace since the 
                                    inflation started to pike as stimulation came out. Unlike spending 
                                    or income, housing prices kept going up without any set back. Even 
                                    though the fed rates are going up that bring a lower inflation rate
                                    as well as a cooler housing market, this is not the case now. The 
                                    main reason that the housing price is still going up is because there
                                    is a low house inventory, a lower supply pushed the price higher.",
                                    style = "font-size:20px;"),
                                  mainPanel(
                                    div( id="center",
                                         h2("How Inflation Impacts the Housing Price "),
                                         tags$iframe(width="650", height="500", src="//www.youtube.com/embed/hgWXd5hwQWo",
                                                     frameborder="0", allowfullscreen="true")
                                    ))),
                           )),
                
              )),
      tabItem(tabName = "page4",
              
              h2("Relationship between price and date"),
              br(),
              
              plotlyOutput("plot3", height = 500,width = "100%"),
              br(),
              p(id="borderB",'As can be seen from the above chart, the highest point for oil prices in the past 39 years was on June 2, 2008, reaching $140. 
                The event that led to the price increase at the time was the US financial crisis. 
                The second is 2022, the main event is the outbreak of the Russian-Ukrainian war and the US sanctions against Russia. 
                Furthermore, the third highest point was the spring of 2011, when the main event was the outbreak of the Egyptian revolution.', style = "font-size:20px;"),
              br(),
              
              fluidRow(
                column(6, img(src = 'oil-3.png', height = 400, width = 800,align = "center"))),
              br(),
              br(),
              plotlyOutput("plot3_2", height = 500),
              br(),
              p(id="borderB", 'As can be seen from the chart above, the highest point for gold in the past 22 years will be in 2022. 
                Global inflation accelerated due to the Russian-Ukrainian war. 
                Second in 2020, the main event was the outbreak of covid-19 and the Fed raised interest rates. 
                The third was during 2011-2012, when the Federal Reserve introduced new monetary easing policies to promote economic recovery.', style = "font-size:20px;"),      
              br(),
              fluidRow(
                column(6, img(src = 'gold-1.png', height = 400, width = 500)),
                column(6, img(src = 'gold-2.jpeg', height = 400, width = 500)),),
              
              br(),
              div(id="center",
              h2(icon('youtube'), 'Price News', style = "font-size:30px;"),
             
             p('Here is a video covers crude oil and gold price change, click ', a('here', href = 'https://youtu.be/IPKdzOhM3uY'),
                'to watch it on Youtube, or click the video below to watch it here.', style = "font-size:20px;"),
              HTML('<iframe width="50%" height="300" src ="https://www.youtube.com/embed/IPKdzOhM3uY" frameborder="0" allowfullscreen ></iframe>')
              )  
      ),
      tabItem(tabName = "page5",
              titlePanel("Check Today's Stock Price by Search Symbol"),

              
              br(),
              fluidRow(
                    sidebarLayout(
                     
                      sidebarPanel(id="sidebar",
                        textInput("symb", h4("Symbol"), "^GSPC"),
                        radioButtons(inputId="time", label=h4("Time"), 
                                     choices=c("last 1 year","last 3 years","last 5 years")),
                      ),
              
                      mainPanel(id="borderM",
                        textOutput("stocktitle"),
                        br(),
                        plotOutput("plot")
                      ))
                   ),
              titlePanel("Our Data"),
              fluidRow(
                box(id="borderM", title= "3 data sets", solidHeader = TRUE, status = "info",collapsible = TRUE, width = 12,
                    tabsetPanel(id="sidebar1",
                      tabPanel("Tab1 for data1", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(23,103,124,0.2);", dataTableOutput("myTable1")))
                                 
                               )),
                      tabPanel("Oil Price", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(23,103,124,0.2);", dataTableOutput("myTable2")))
                                 
                               )),
                      tabPanel("Gold Price", 
                               fluidRow(
                                 column(1,),       
                                 column(10,div(style = "background-color:rgba(23,103,124,0.2);", dataTableOutput("myTable3")))
                                 
                               )),
                      
                      ))),
              fixedPanel(actionButton("Button1","Reference"),bottom = 30, right = 80),
              br(),
              br(),
              tags$style(HTML("
      #border1 {        
  border: 5px lightgrey;
  padding: 5px;
  margin: 2px;
  border-style: groove;
      }
        #border2 {        
  border: 5px lightgrey;
  padding: 5px;
  margin: 2px;
  border-style: groove;
  background: #344d56;
      }
      #borderM {
          border-radius: 25px;
  border: 2px solid #485461;
  padding: 20px;
    border-style: groove;

 
      }
      #borderB {
      overflow: auto;
          border-radius: 25px;
  border: 2px solid #485461;
  padding: 10px;
    border-style: groove;

 
      }
       #center {
  overflow: auto;
  position: absolute;
  text-align: center;
  font-size: 18px;
  border-radius: 20px;
   border-style: groove;
 
 
       }
 
    
    
       #sidebar {
            background-color: #485461;
            padding: 10px;
            margin: 60px;
            border: 6px lightgrey;
            border-style: groove;

       }
       #sidebar1 {
            background-color: rgba(0,198,179,0.2);
            padding: 10px;
            margin: 30px;
            border: 7px rgba(0,198,179,0.2);
            border-style: groove;

       }
        
    ")),
              
      )         
    )
  )
)



server <- function(input, output, session) {

  
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
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Federal Debt", "$28.43 trillion", icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Total unemployed people", "5.9 million", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  
  })



  
  
  f = ggplot(data = data) +
    geom_line(mapping = aes(x = Date, y = Inflation))+
    geom_smooth(mapping = aes(x = Date, y= Inflation))+
    labs(title = "USA inflation", x = "Date", y = "inflation (%)")
  
  f + theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'))
  
  
  
  output$plot1 = renderPlotly({
    
  
    data_subset = data %>% 
      filter(!is.na(`Discount Rate`))
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
  world_map <- map_data("world")
  g20inf = read_csv('g20inf.csv')  %>%
    pivot_longer(c("2015", "2016", "2017", "2018",
                   "2019", "2020","2021"),
                 names_to = "Year", values_to = "Inf_Rate")
  world_map %>% 
    left_join(g20inf, by = c("region" = "Country Name")) -> act_world_map
  output$plot4 = renderPlotly({

    act_world_map=act_world_map[!is.na(act_world_map$Year),]   
   
    
    mp1=act_world_map %>% 
      filter(as.numeric(act_world_map$Year)==input$Year) 
    mp= ggplot(mp1, mapping=aes(x =as.numeric(long), y = as.numeric(lat), group = group,text=region)) +
      geom_polygon(aes(fill= Inf_Rate), colour = "grey50") +
      scale_fill_gradient(low = "steel blue", high="pink") +
      labs(title="Most country inflation rate")+
      theme_map()
    
    mp=ggplotly(mp,tooltip=c('fill','text'))
    mp 
  })
  
  DSPIC96 <- read_csv("DSPIC96.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
  
  output$plot2 = renderPlotly({
    
    
    m1 <- ggplot(data = DSPIC96, aes(x=date)) + 
      ggtitle("Real Disposable Personal Income",
              subtitle = "in U.S. dollars")+
      geom_line(aes(y = DSPIC96), color = "black") + 
      labs(x="Date", y="Billions of Chained 2012 Dollars")
      
    
    ggtitle("Income Change from 2013 to 2022 by gender")
    
    
    
    m1 <- ggplotly(m1)
    
    m1
  })
  T5YIE <- read_csv("T5YIE.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
  
  output$plot2_2 = renderPlotly({
    T5YIE = T5YIE %>%
      filter(inflation>0)
    m2 <- ggplot(data = T5YIE, aes(x=date)) +
      ggtitle("U.S. Inflation Rate since 2013")+
      geom_line(aes(y = inflation), color = "black")+
      labs(x="Date", y="Infaltion Rate (%)")
    
    
    m2 <- ggplotly(m2)
    
    m2
  })
  output$plot2_5 = renderPlotly({
    T5YIE = T5YIE %>%
      filter(inflation>0)
    m2 <- ggplot(data = T5YIE, aes(x=date)) +
      ggtitle("U.S. Inflation Rate since 2013")+
      geom_line(aes(y = inflation), color = "black")+
      labs(x="Date", y="Infaltion Rate (%)")
    
    
    m2 <- ggplotly(m2)
    
    m2
  })
  output$plot2_6 = renderPlotly({
    T5YIE = T5YIE %>%
      filter(inflation>0)
    m2 <- ggplot(data = T5YIE, aes(x=date)) +
      ggtitle("U.S. Inflation Rate since 2013")+
      geom_line(aes(y = inflation), color = "black")+
      labs(x="Date", y="Infaltion Rate (%)")
    
    
    m2 <- ggplotly(m2)
    
    m2
  })
  Housing_Price <- read_csv("Housing Price.csv", 
                            col_types = cols(date = col_date(format = "%m/%d/%Y")))
  
  output$plot2_3 = renderPlotly({
    m3 <- ggplot(data = Housing_Price, aes(x=date)) +
      ggtitle("U.S. Housing Price Since 2013")+
      geom_line(aes(y = Housing_Price), color = "black")+
      labs(x="Date", y="Housing Price Index")
    
    m3 <- ggplotly(m3)
    
    m3
  })
  
  PCE <- read_csv("PCE.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
  output$plot2_4 = renderPlotly({
    
    m4 <- ggplot(data = PCE, aes(x=date)) +
      ggtitle("Personal Consumer Expenditure")+
      geom_line(aes(y = PCE), color = "black")+
      labs(x="Date", y="Personal Consumer Expenditure")
    
    m4 <- ggplotly(m4)
    
    m4
  })
  
  date_New <- as.Date(oil$date, "%m/%d/%Y")
  oil$date <- date_New
  Date_New <- as.Date(gold$Date, "%m/%d/%Y")
  gold$Date <- Date_New
  output$plot3 = renderPlotly({
    
    
    f <-ggplot(data = oil, aes(x=date,y=price))+ ggtitle("Crude Oil Price Change")+
      geom_line()+
      geom_smooth()
    f <- ggplotly(f)
    f
    
    
  })
  
  
  output$plot3_2 = renderPlotly({
    
   
    
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
  output$myTable2=renderDataTable({
    return(datatable(oil, rownames= FALSE))
  })
  output$myTable3=renderDataTable({
    return(datatable(gold, rownames= FALSE))
  })
  

  observeEvent(input$Button1,{
    
    input$Reference
    
    showModal(modalDialog(title = "Reference: ",
                          div(a('Fed Inflation Rate ', href = 'https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=US')),
                          div(a('fed interest rate ', href = 'https://fred.stlouisfed.org/series/FEDFUNDS')),
                          div(a('G20 Inflation Rate ', href = 'https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG?end=2021&start=2021&view=map')),
                          div(a('Oil Price ', href = 'https://www.kaggle.com/datasets/sc231997/crude-oil-price')),
                          div(a('Gold Price ', href = 'https://www.kaggle.com/datasets/psycon/daily-gold-price-historical-data')),
                          
                          
    ))
  })
  
  
}

shinyApp(ui = ui, server = server)