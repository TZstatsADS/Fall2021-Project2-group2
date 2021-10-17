#Load/Import packages

#Load/Import packages

#Packages Required
packages <- c("readr", "tidyverse", "dplyr", "lubridate","igraph","shinydashboard",
              "shiny","plyr","tidyr","ggplot2", "highcharter","lubridate","stringr","withr","treemap", "DT","shinyBS", "shinyjs", "WDI",
              "geosphere", "magrittr", "shinycssloaders","timevis", "comtradr", "memoise", "networkD3","promises","future")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#Load Package
invisible(lapply(packages, library, character.only = TRUE))

options(spinner.color="#006272")
#library(RCurl)
#library(jsonlite)
plan(multiprocess)

## build server.R
server <- 
  function(input, output, session) {
    ########################################################################
    ## I. Domestic Violence dashboard --------------------------------------
    #######################################################################
          # Domestic Violence Data Loading and Processing 
          violence_df <- read_csv('../data/family_violence_grouped.csv')
          violence_year <- aggregate(. ~ Report_Year,select(violence_df, -Comm_Dist_Boro), FUN=sum)
          most_recent_totals <- violence_year %>% filter(Report_Year == max(Report_Year))
          prev_year_totals <-  violence_year %>% filter(Report_Year == max(Report_Year)-1)
          two_year_lag_total <- violence_year %>% filter(Report_Year == max(Report_Year)-2)
          perc_diff <- round((most_recent_totals$totals - prev_year_totals$totals)/prev_year_totals$totals*100,2)
          prev_perc_diff <- round((prev_year_totals$totals - two_year_lag_total$totals)/two_year_lag_total$totals*100,2)
          
          # ## Create Value Boxes for Percent Difference and Totals 
          output$PercDiff <- renderValueBox({
            valueBox(
              value =  paste0(perc_diff, "%" ),
              subtitle = tags$p("YoY Percent Difference - 2019 to 2020", style = "color:black"),
              icon = icon('export', lib = 'glyphicon'),
              color = "red"
            )
          })
          output$PercDiffPrev <- renderValueBox({
            valueBox(
              value =  paste0(prev_perc_diff, "%" ),
              subtitle = tags$p("YoY Percent Difference - 2018 to 2019", style = "color:black"),
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
          
          # 2. Line Charts By Borough  ------------------
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
              hc_yAxis( title = list(text = "Case Count")) %>%
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
          
          # Counts by Violence Type  
          output$DVCountsByCategory <-renderHighchart({
            highchart() %>%
              hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
              hc_chart(type = 'line') %>%
              hc_series( 
                         list(name = 'Felony Assaults, Family Members', data = violence_year$FAM_Fel_Assault, color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
                         list(name = 'Felony Assaults, Domestic Violence', data = violence_year$DV_Fel_Assault, color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') ),
                         list(name = 'Felony Rape, Family Members', data =  violence_year$FAM_Rape, color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') ),
                         list(name = 'Felony Rape, Domestic Violence', data =  violence_year$DV_Rape, color = 'purple', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
              )%>%
              hc_xAxis( categories = unique(violence_df$Report_Year) ) %>%
              hc_yAxis( title = list(text = "Case Count")
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
          
          #Bar Charts for Page Visits to HOPE Website 
          page_visits <- read_csv("../data/hope_webpage_visits.csv")
          agg_visits <- aggregate(. ~ year,select(page_visits, -Date), FUN=sum)
          output$PageVisitsByYear <-renderHighchart({
            highchart() %>%
              hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
              hc_chart(type = 'column') %>%
              hc_series(
                list(name = 'Total Page Visits', data = agg_visits$Visits, color = '#87CEFA' ),
                list(name = 'Total New Visits', data = agg_visits$`New Visitors`, color = 'darkblue' )
                 )%>%
              hc_xAxis( categories = unique(agg_visits$year) ) %>%
              hc_yAxis( title = list(text = "Visit Count")
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
          output$PageVisitsbyType <-renderHighchart({
            highchart() %>%
              hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
              hc_chart(type = 'column') %>%
              hc_series(
                list(name = 'Visits From Social Media', data = agg_visits$Visits[agg_visits$year==2020], color = '#87CEFA' )
                # list(name = 'Neighborhood Services Page', data = agg_visits$`Neigh Serv`[agg_visits$year==2020], color = 'darkblue' ),
                # list(name = 'Family Justice Centers', data = agg_visits$`Family Justice Center`[agg_visits$year==2020], color = 'darkblue' ),
                # list(name = 'State Service Centers', data = agg_visits$`City State Services`[agg_visits$year==2020], color = 'darkblue' ),
                # list(name = 'GBV', data = agg_visits$`Learn GBV`[agg_visits$year==2020], color = 'darkblue' )
              )%>%
              hc_yAxis( title = list(text = "Visit Count")
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
    ########################################################################
    ## I. Resource Directory Map --------------------------------------
    #######################################################################
        resources_df <- read_csv("../data/Mayor_s_Office_to_End_Domestic_and_Gender-Based_Violence__Resource_Directory.csv")
        output$res_map <- renderLeaflet({ 
            leaflet(options = leafletOptions(zoomControl = FALSE),
                    data = resources_df) %>% addTiles() %>%
                    addMarkers(~LONGITUDE, ~LATITUDE, popup = paste(
                      "<b>Name:</b>", resources_df$NAME, "<br>",
                      "<b>Address:</b>", resources_df$ADDRESS, "<br>")) %>%
              htmlwidgets::onRender(
                "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
              ) %>%
              addProviderTiles("CartoDB.Voyager") %>%
              setView(lng = -73.935242, lat = 40.730610, zoom = 10)
          })
  }

















