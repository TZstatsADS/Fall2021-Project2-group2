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
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      
      ## 1st tab show the Main dashboard -----------
      menuItem( "Domestic Violence Counts", tabName = 'dv_dashboard', icon = icon('dashboard') ),
      
      ## Map with Locations of Help Centers --------------
      menuItem("Resource Locations Directory", tabName = 'resource_dir', icon = icon("map-marker-alt") ),
      
      ## Covid Data and Dog Licenses ----------
      menuItem( "COVID Cases to Dog Licenses", tabName = "covid_cases", icon = icon('barcode'), startExpanded = F),
      
       ## Call Center  -------------------------
      menuItem("Call Center Data", tabName = 'call_centers', icon = icon('search') )
    )
  )

## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    tags$script("document.title = 'New Zealand Trade Intelligence Dashboard'"),
    
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
                       background-color: #FF7F50;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #FF7F50;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #FF7F50;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #FF7F50;
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
    # Domestic Violence Tab
    ############################################################
    tabItem( tabName = 'dv_dashboard',
             ## contents for the dashboard tab
             # 1.1 Value  Boxes ---------------------------
             div(class = 'scroller_anchor'),
             div(class = 'scroller', ) ,
             
             h2(paste0("Domestic Violence in NYC")),
             fluidRow(
               valueBoxOutput("PercDiff"),
               valueBoxOutput("RecentTotals")
             ),
             
             ## 1.2 Time serise plot ----------------------------------------
             h2(paste0("Reported Cases 2017 to 2020")),
             fluidRow( column( width = 6,h4("Domestic Violence Case Reports By Borough", align = 'center'), highchartOutput('DVTotalCountsByYear') ),
                       column( width = 6,h4("Domestic Violence Case Reports by Category", align = 'center'), highchartOutput('DVCountsByCategory') )
             )
   ),
   #############################################################
   # Resource Directory Tab 
   ############################################################
     tabItem(
       ##
       tabName = 'resource_dir',
       # Map 
       div(class="outer",
           h3(" Domestic Violence Resources", align="center", style="color:#045a8d"),
           # map output
           leafletOutput("res_map", width="100%", height=620),
       )
       
     ) 
 )

)


## put UI together --------------------
ui <- 
  dashboardPage(header, siderbar, body )