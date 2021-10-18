#Load/Import packages

#Load/Import packages

#Packages Required
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
library(magrittr)
library(shinycssloaders)

## build server.R
server <- 
  function(input, output, session) {
    ########################################################################
    ## I. Domestic Violence dashboard --------------------------------------
    #######################################################################
    # Domestic Violence Data Loading and Processing 
    violence_df <- read_csv('family_violence_grouped.csv')
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
    page_visits <- read_csv("hope_webpage_visits.csv",col_types = cols(Date = col_date(format = "%m/%d/%y")))
    agg_visits <- aggregate(. ~ year,select(page_visits, -Date), FUN=sum)
    
    #Import Covid Data
    CovidData <- read_csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
    #Standardize date format
    CovidData <- CovidData %>% mutate(DATE_OF_INTEREST = mdy(DATE_OF_INTEREST))
    
    covid_totals <-  agg_visits%>% filter(year == 2020)
    prev_year <- agg_visits%>% filter(year == 2019)
    visits_perc_diff <- round((covid_totals$Visits - prev_year$Visits)/prev_year$Visits*100,2)
    newvisits_perc_diff <- round((covid_totals$`New Visitors` - prev_year$`New Visitors`)/prev_year$`New Visitors`*100,2)
    
    output$PercDiffVisits <- renderValueBox({
      valueBox(
        value =  paste0(visits_perc_diff, "%" ),
        subtitle = tags$p("COVID Increase-Website Visitors", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
    
    output$PercDiffNewVisits <- renderValueBox({
      valueBox(
        value =  paste0(newvisits_perc_diff, "%" ),
        subtitle = tags$p("COVID Increase -New Visitors", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
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
    visits_type <- read_csv("visits_by_group.csv")
    output$PageVisitsbyType <-renderHighchart({
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'column') %>%
        hc_series(
          list(name = 'Page Visits by Category', data = visits_type$Count, color = '#40E0D0' )
        )%>%
        hc_xAxis( categories = unique(visits_type$Category) ) %>%
        hc_yAxis( title = list(text = "Page Visit Count")
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
                   headerFormat = '<span style="font-size: 13px">{point.key}</span>'
        ) %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
      
    })
    
    output$VisitsCovidCases <- renderPlot({
      ggplot() +
        geom_point() +
        geom_line(data=page_visits, aes(x=Date,y=Visits), color='blue')+ 
        geom_line(data=CovidData, aes(x=DATE_OF_INTEREST,y=CASE_COUNT), color='pink')+
        #labels
        geom_vline(xintercept = as.numeric(as.Date("2020-03-22")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-03-22"), label="March 22 NYC PAUSE \n", y=5000), colour="red", angle=90, text=element_text(size=20)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-06-08")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-06-08"), label="June 8 PHASE 1 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-06-22")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-06-22"), label="June 22 PHASE 2 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-07-06")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-07-06"), label="July 6 PHASE 3 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-07-20")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-07-20"), label="July 20 PHASE 4 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+ 
        labs(x= "Date", y= "Visits", color = c("Page Visits" = "blue", "Covid Cases" = "pink"))
    })
    
    ########################################################################
    ## I. Resource Directory Map --------------------------------------
    #######################################################################
    resources_df <- read_csv("Mayor_s_Office_to_End_Domestic_and_Gender-Based_Violence__Resource_Directory.csv")
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
    
    ########################################################################
    ## Dog Data
    ########################################################################
    
    #Import & process Data
    #Dog Data
    NYC_Dog_Licensing_Dataset <- read_csv("NYC_Dog_Licensing_Dataset.csv", 
                                          col_types = cols(RowNumber = col_skip(), 
                                                           AnimalName = col_skip(), AnimalGender = col_skip(), 
                                                           AnimalBirthMonth = col_skip(), BreedName = col_skip(), 
                                                           Borough = col_skip(),
                                                           LicenseIssuedDate = col_date(format = "%m/%d/%Y"), 
                                                           LicenseExpiredDate = col_skip(), 
                                                           `Extract Year` = col_skip(),
                                                           `Unique Dog ID` = col_skip()))
    
    NYC_Dog_Licensing_Dataset <- count(NYC_Dog_Licensing_Dataset,
                                       "LicenseIssuedDate")
    
    #value boxes
    output$dogRecentTotals <- renderValueBox({
      valueBox(
        value =  paste0("+",
                        round(((sum(NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= "2020-07-20" & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= "2020-12-31",]$freq)-
                                  sum(NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= "2019-07-20" & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= "2019-12-31",]$freq))/
                                 sum(NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= "2019-07-20" & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= "2019-12-31",]$freq)*100),
                              digits=2),"%"
        ),
        subtitle = tags$p("2020 YoY New Applications 07/20-12/31", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
    
    output$dogPercDiff <- renderValueBox({
      valueBox(
        value =  paste0("+",
                        (sum(NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= "2020-07-20" & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= "2020-12-31",]$freq)-
                           sum(NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= "2019-07-20" & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= "2019-12-31",]$freq))
                        
        ),
        subtitle = tags$p("2020 YoY New Applications 07/20-12/31", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
    
    #reactive code
    output$dogDataPlot <- renderPlot({
      #PLOT FOR DOG LICENSES
      dPlot <- ggplot() +
        geom_point() +
        geom_line(data=NYC_Dog_Licensing_Dataset[NYC_Dog_Licensing_Dataset$LicenseIssuedDate >= input$dogDatesInput[1] & NYC_Dog_Licensing_Dataset$LicenseIssuedDate <= input$dogDatesInput[2],], aes(x=LicenseIssuedDate,y=freq), color='blue')+ 
        #labels
        geom_vline(xintercept = as.numeric(as.Date("2020-03-22")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-03-22"), label="March 22 NYC PAUSE \n", y=5000), colour="red", angle=90, text=element_text(size=20)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-06-08")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-06-08"), label="June 8 PHASE 1 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-06-22")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-06-22"), label="June 22 PHASE 2 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-07-06")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-07-06"), label="July 6 PHASE 3 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+
        geom_vline(xintercept = as.numeric(as.Date("2020-07-20")), linetype=4,color='red') +
        geom_text(aes(x = as.Date("2020-07-20"), label="July 20 PHASE 4 REOPEN \n", y=5000), colour="red", angle=90, text=element_text(size=20))+ 
        labs(x= "Date", y= "Cases/Applications") 
      
      #ADD DATA IF CHECKBOX TRUE  
      {if(input$dogInput)dPlot + geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$dogDatesInput[1] &
                                                  CovidData$DATE_OF_INTEREST <= input$dogDatesInput[2],], aes(x=DATE_OF_INTEREST,y=CASE_COUNT), color='pink')
        }
    })
    
  



    ########################################################################
    ## COVID_19 Data
    ########################################################################
    ##Import and process covid_19 data
    covid_df <- read_csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
    covid_df <- covid_df %>% mutate(DATE_OF_INTEREST = mdy(DATE_OF_INTEREST))
    
    output$CovidTotalCaseCount2020 <- renderValueBox({
      valueBox(
          value =  paste0(
            sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-02-29" & covid_df$DATE_OF_INTEREST <= "2020-12-31",]$CASE_COUNT)
          ),
          subtitle = tags$p("2020 COVID-19 Case-Count in NYC 02/29-12/31", style = "color:black"),
          icon = icon('export', lib = 'glyphicon'),
          color = "red"
        )
    })
      
    
    output$CovidTotalDeathCount2020 <- renderValueBox({
      valueBox(
        value =  paste0(
          sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-02-29" & covid_df$DATE_OF_INTEREST <= "2020-12-31",]$DEATH_COUNT)
        ),
        subtitle = tags$p("2020 COVID-19 Death-Count in NYC 02/29-12/31", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "red"
      )
    })
    
    output$CovidTotalHospitalizationCount2020 <- renderValueBox({
      valueBox(
        value =  paste0(
          sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-02-29" & covid_df$DATE_OF_INTEREST <= "2020-12-31",]$HOSPITALIZED_COUNT)
        ),
        subtitle = tags$p("2020 COVID-19 Hospitalized-Count in NYC 02/29-12/31", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "red"
      )
    })
    
    output$Coviddiffcases <- renderValueBox({
      valueBox(
        value =  paste0(round(((sum(covid_df[covid_df$DATE_OF_INTEREST >= "2021-03-01" & covid_df$DATE_OF_INTEREST <= "2021-10-05",]$CASE_COUNT)-
                                  sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$CASE_COUNT))/
                                 sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$CASE_COUNT)*100),
                              digits=2),"%"
        ),
        subtitle = tags$p("2020-2021 Percentage Changes of Total cases", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "red"
      )
    })
    
      output$Coviddiffdeaths <- renderValueBox({
        valueBox(
          value =  paste0(round(((sum(covid_df[covid_df$DATE_OF_INTEREST >= "2021-03-01" & covid_df$DATE_OF_INTEREST <= "2021-10-05",]$DEATH_COUNT)-
                                    sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$DEATH_COUNT))/
                                   sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$DEATH_COUNT)*100),
                                digits=2),"%"
          ),
          subtitle = tags$p("2020-2021 Percentage Changes of Death cases", style = "color:black"),
          icon = icon('export', lib = 'glyphicon'),
          color = "green"
        )
      })
    
    output$Coviddiffhospitalizations <- renderValueBox({
      valueBox(
        value =  paste0(round(((sum(covid_df[covid_df$DATE_OF_INTEREST >= "2021-03-01" & covid_df$DATE_OF_INTEREST <= "2021-10-05",]$HOSPITALIZED_COUNT)-
                                  sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$HOSPITALIZED_COUNT))/
                                 sum(covid_df[covid_df$DATE_OF_INTEREST >= "2020-03-01" & covid_df$DATE_OF_INTEREST <= "2020-10-05",]$HOSPITALIZED_COUNT)*100),
                              digits=2),"%"
        ),
        subtitle = tags$p("2020-2021 Percentage Changes of Hospitalized cases", style = "color:black"),
        icon = icon('export', lib = 'glyphicon'),
        color = "green"
      )
    })
    
    output$CovidDataPlot <- renderPlot({
      
      dPlot <- ggplot() +
        geom_point() +
        geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$CovidDatesInput[1] &
                                   CovidData$DATE_OF_INTEREST <= input$CovidDatesInput[2],], aes(x=DATE_OF_INTEREST,y=CASE_COUNT), color='red')+
        geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$CovidDatesInput[1] &
                                   CovidData$DATE_OF_INTEREST <= input$CovidDatesInput[2],], aes(x=DATE_OF_INTEREST,y=DEATH_COUNT), color='blue')+
        geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$CovidDatesInput[1] &
                                   CovidData$DATE_OF_INTEREST <= input$CovidDatesInput[2],], aes(x=DATE_OF_INTEREST,y=HOSPITALIZED_COUNT), color='black')
        labs(x= "Date", y= "Cases")
        {if(TRUE)dPlot + geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$CovidDatesInput[1] &
                                                    CovidData$DATE_OF_INTEREST <= input$CovidDatesInput[2],], aes(x=DATE_OF_INTEREST,y=HOSPITALIZED_COUNT), color='black')
        }
      
    })
    
    output$DistrictDataPlot <- renderPlot({
      
      dPlot <- ggplot() +
        geom_point() +
        geom_line(data=CovidData[CovidData$DATE_OF_INTEREST >= input$DistrictDatesInput[1] &
                                   CovidData$DATE_OF_INTEREST <= input$DistrictDatesInput[2],], aes(x=DATE_OF_INTEREST,y=get(paste(input$DistrictInput,"_",input$CategoryInput))), color='red')
       +labs(x= "Date", y= "Cases")
    })
    
  }








