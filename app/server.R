library(readr)
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
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
#library(RCurl)
#library(jsonlite)
library(comtradr)
library(memoise)
library(networkD3)
library(promises)
library(future)
plan(multiprocess)

## build server.R
server <- 
  function(input, output, session) {
    ## I. Main dashboard ---------------------------------------------------
    # 1. Value boxes  --------------------
    violence_year <- aggregate(. ~ Report_Year,select(violence_df, -Comm_Dist_Boro), FUN=sum)
    most_recent_totals <- violence_year %>% filter(Report_Year == max(Report_Year))
    prev_year_totals <-  violence_year %>% filter(Report_Year == max(Report_Year)-1)
    perc_diff <- round((most_recent_totals$totals - prev_year_totals$totals)/prev_year_totals$totals*100,2)
    
    # ## Create Value Boxes for Percent Difference and Totals 
    output$PercDiff <- renderValueBox({
      valueBox(
        value =  paste0(perc_diff, "%" ),
        subtitle = tags$p("YoY Percent Difference - 2019 to 2020", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
    
    output$RecentTotals <- renderValueBox({
      valueBox(
        value =  most_recent_totals$totals,
        subtitle = tags$p("2020 Total Reported Cases", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "blue"
      )
    })
    
    # 2. Line Charts By Borough  -------------------
    violence_df <- read_csv('../data/family_violence_grouped.csv')
    
    #Counts by Year and Borough 
    output$DVTotalCountsByYear <-renderHighchart({
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( list(name = 'Manhattan', data =violence_df$totals[violence_df$Comm_Dist_Boro=='Manhattan'], color='blue', marker = list(symbol = 'circle') ),
                   list(name = 'Bronx', data =violence_df$totals[violence_df$Comm_Dist_Boro=='Bronx'], color = 'green', marker = list(symbol = 'circle') ),
                   list(name = 'Brooklyn', data =violence_df$totals[violence_df$Comm_Dist_Boro=='Brooklyn'], color = 'pink', marker = list(symbol = 'circle') ),
                   list(name = 'Queens', data =violence_df$totals[violence_df$Comm_Dist_Boro=='Queens'], color = 'purple',marker = list(symbol = 'circle')  ),
                   list(name = 'Staten Island', data =violence_df$totals[violence_df$Comm_Dist_Boro=='Staten Island'], color = 'red',  marker = list(symbol = 'circle')  )
        )%>%
        hc_xAxis( categories = unique(violence_df$Report_Year) ) %>%
        hc_yAxis( title = list(text = "Total")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T ) 
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
        ) %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    })
    
    # 2.1 Line graph Break down by Type  --------------------
    output$DVCountsByCategory <-renderHighchart({
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( #list(name = 'Domestic Violence Reports', data = violence_year$FAM_DIR, color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                   list(name = 'Felony Assaults, Family Members', data = violence_year$FAM_Fel_Assault, color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
                   list(name = 'Felony Assaults, Domestic Violence', data = violence_year$DV_Fel_Assault, color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') ),
                   list(name = 'Felony Rape, Family Members', data =  violence_year$FAM_Rape, color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') ),
                   list(name = 'Felony Rape, Domestic Violence', data =  violence_year$DV_Rape, color = 'purple', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
        )%>%
        hc_xAxis( categories = unique(violence_df$Report_Year) ) %>%
        hc_yAxis( title = list(text = "$ million, NZD"),
                  plotLines = list(
                    list(#label = list(text = "This is a plotLine"),
                      color = "#ff0000",
                      #dashStyle = 'shortDot',
                      width = 2,
                      value = 0 ) )
        ) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T )
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
        ) %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    })
  }

















