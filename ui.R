library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
##install.packages("gganimate")
##library(gganimate)
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)

## load data


## setup global variables
maxYear_lb <- '2018'  

## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("Home Life During COVID in NYC"), 
                   disable = FALSE, 
                   titleWidth  = 550
                   
  )

## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 250,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      
      ## 1st tab show the Intro & Covid dashboard -----------
      menuItem( "Home", tabName = 'Home', icon = icon('dashboard') ),
      
      ## 2nd tab show the Domestic Violence dashboard -----------
      menuItem( "COVID-19 Overview", tabName = 'covid_dash', icon = icon('dashboard') ),
      
      ## 3rd tab show the Domestic Violence dashboard -----------
      menuItem( "Domestic Violence Instances", tabName = 'dv_dashboard', icon = icon('dashboard') ),
      
      ## 5th Covid Data and Dog Licenses ----------
      menuItem( "Pet Companionship", tabName = "pets", icon = icon('barcode'), startExpanded = F),
    
    ## 4th Map with Locations of Help Centers --------------
    menuItem("Resource Locations Directory", tabName = 'resource_dir', icon = icon("map-marker-alt") )
  ))

## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    tags$script("document.title = 'Domestic Life During COVID'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #49B9F9;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #49B9F9;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #49B9F9;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #49B9F9;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )) ,
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
  ## 3.1 Dashboard body --------------
  tabItems(
    
    #############################################################
    # Home Tab
    ############################################################
    tabItem( tabName = 'Home',
             ## contents for the dashboard tab
             # 1.1 Value  Boxes ---------------------------
             div(class = 'scroller_anchor'),
             div(class = 'scroller') ,
             
             #TITLE
             h1(style = "text-align: center;","Evaluating Home Life During COVID-19"),
             br(),
             
             #IMAGE
             img(src = "https://images.unsplash.com/photo-1606787504667-961f789e78e6?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=2940&q=80",
                 width = 500, style="display: block; margin-left: auto; margin-right: auto;"),
             br(),
             br(),
             
             #INTRO
             h4("When COVID-19 began spreading rapidly in NYC the city began shutting down, 
               forcing business and social activities to come to a halt.  As a result, many New Yorkers
               were stuck in their homes far longer than many were used to.  In this app, 
               using the data from the NYC Open Data, focusing on resources from the Mayor's Office 
               to End Domestic and Gender-Based violence, we offer a look at some
               of the effects of COVID on domestic life."),
             br(), 
             h4("Intended users for this dashboard are users from the Mayor's Office 
               to End Domestic and Gender-Based violence, indivduals running NYC's HOPE website, 
                and potentially domestic violence resource centers in NYC.")
    ),
    
    #############################################################
    # COVID Tab
    ############################################################
    tabItem( tabName = 'covid_dash',
             ## contents for the dashboard tab
             # 1.1 Value  Boxes ---------------------------
             div(class = 'scroller_anchor'),
             div(class = 'scroller' ) ,
             
             #TITLE
             h1(style = "text-align: center;","COVID-19 in NYC"),
             br(),
             
             #IMAGE
             img(src = "https://c.files.bbci.co.uk/D505/production/_115033545_gettyimages-1226314512.jpg",
                 width = 500, style="display: block; margin-left: auto; margin-right: auto;"),
             br(),
             fluidRow(
               valueBoxOutput("CovidTotalCaseCount2020"),
               valueBoxOutput("CovidTotalDeathCount2020"),
               valueBoxOutput("CovidTotalHospitalizationCount2020")),
             br(),
             fluidRow(
               valueBoxOutput("Coviddiffcases"),
               valueBoxOutput("Coviddiffdeaths"),
               valueBoxOutput("Coviddiffhospitalizations")),
             br(),
             fluidRow(
               #date chooser
               h3('COVID_19 Data in NYC'),
               h5('Red line stands for total covid cases. \n Blue line stands for death cases. Black line stands for hospitalized cases'),
               column(6,
                      dateRangeInput("CovidDatesInput", h3("Date range"),
                                     start = "2020-02-29",
                                     end = "2021-10-05",
                                     max = "2021-10-05"
                      )

                              )
               ),
             
             #output section
             fluidRow(
               br(),
               br(),
               plotOutput("CovidDataPlot"),
               br(),
               br()
             ),
             br()
             
             
    ),
    
    
    #############################################################
    # Domestic Violence Tab
    ############################################################
    tabItem( tabName = 'dv_dashboard',
             ## contents for the dashboard tab
             # 1.1 Value  Boxes ---------------------------
             div(class = 'scroller_anchor'),
             div(class = 'scroller' ) ,
             
             h2("Domestic Violence Cases Over Time in NYC",style = "text-align: center;"),
             fluidRow(
               valueBoxOutput("RecentTotals"),
               valueBoxOutput("PercDiff"),
               valueBoxOutput("PercDiffPrev")
             ),
             
             ## 1.2 Time serise plot ----------------------------------------
             h2(paste0("Reported Cases 2017 to 2020"), align = "center"),
             fluidRow( column( width = 6,h4("Domestic Violence Case Reports By Borough", align = 'center'), highchartOutput('DVTotalCountsByYear') ),
                       column( width = 6,h4("Domestic Violence Case Reports by Category", align = 'center'), highchartOutput('DVCountsByCategory') )
             ),
             h2("Visits to NYC HOPE Website - Outreach During COVID",style = "text-align: center;"),
             fluidRow(
                valueBoxOutput("PercDiffVisits"),
               valueBoxOutput("PercDiffNewVisits")
             ),
             br(),
             h4("Visits to HOPE Overtime", align = 'center'),
             fluidRow(highchartOutput('PageVisitsByYear')),
             h4("Visits to HOPE by Category", align = 'center'),
             fluidRow(highchartOutput('PageVisitsbyType')),
             br(),
             h4("Visits to HOPE Website Compared to COVID Cases", align = 'center'),
             fluidRow(plotOutput('VisitsCovidCases'))
             
    ),
    
    
    #############################################################
    # Resource Directory Tab 
    ############################################################
    tabItem(
      ##
      tabName = 'resource_dir',
      p("The Mayor’s Office to End Domestic and Gender-Based Violence coordinates 
        the citywide delivery of domestic violence services. This map shows the 
        current locations of that offer domestic violence related services."),
      # Map 
      div(class="outer",
          h3(" Domestic Violence Resources", align="center", style="color:#045a8d"),
          h5(" Locations where you can go for help or advice regarding Domestic Violence", align="center"),
          # map output
          leafletOutput("res_map", width="100%", height=620)
      ),
      #Map2 
      div(class="outer",
          h3(" Animal Resources", align="center", style="color:#045a8d"),
          h5(" Locations where you can adopt Dogs and other pets", align="center"),
          # map output
          leafletOutput("dog_res_map", width="100%", height=620)
      )
    ),
    
    #############################################################
    # Pet Companionship Tab
    ############################################################
    tabItem(tabName = 'pets',
            ## contents for the dashboard tab
            # 1.1 Value  Boxes ---------------------------
            div(class = 'scroller_anchor'),
            div(class = 'scroller') ,
            
            #intro
            fluidRow(
              h2("Animal Companionship",style = "text-align: center;"),
              img(src = "https://s3.amazonaws.com/cdn-origin-etr.akc.org/wp-content/uploads/2017/12/12194337/Yellow-Lab-High-Five.jpg", 
                  width = 500, style="display: block; margin-left: auto; margin-right: auto;"),
              
              br(),
              p("Since the dawn of time, animals have provided humans comfort and companionship.  
               When COVID-19 became prevalent in NYC and the city was forced to suddenly shut down, 
               many New Yorkers living alone found themselves growing lonely as they were effectively stuck inside their homes until phased reopening began."),
              br()
            ),
            
            #DATA SECTION
            
            fluidRow(
              column(10, align="center",
                     valueBoxOutput("dogRecentTotals"),
                     valueBoxOutput("dogPercDiff")
              )),
            
            fluidRow(
              #data source check box
              column(6, h3("Additional Data Sources"), 
                     checkboxInput("dogInput", "NYC COVID-19 Case Counts", value = TRUE)                         
              ),
              
              #date chooser
              column(6,
                     dateRangeInput("dogDatesInput", h3("Date range"),
                                    start = "2019-07-20",
                                    end = "2021-03-17",
                                    max = "2021-03-17"
                     )
              )),
            
            #output section
            fluidRow(
              br(),
              br(),
              plotOutput("dogDataPlot"),
              br(),
              br()
            ),
            
            #analysis section
            fluidRow(
              br(),
              h2("Analysis",style = "text-align: center;"),
              p("The data shows a significant spike of applications for dog licenses after Phase 4 reopening, Leading us to believe that after the long drawn out lock-down,
               NYC residents were eager to adopt a new friend in their home, possibly for the sake of companionship and to aid mental health.")
            )             
    )  
  ) 
)

## put UI together --------------------
ui <- 
  dashboardPage(header, siderbar, body )