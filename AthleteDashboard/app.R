library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyverse)
library(sparkline)
library(echarts4r)
library(DT)
library(googlesheets4)
library(shinycustomloader)

#_ -----
# Data -----

# Deauthorize Google account protocols
gs4_deauth()

#_ -----
# UI Components -----

##_ -----
## Header -----
header <- dashboardHeader(
  ### Title -----
  title = "Athlete Dashboard", titleWidth = 200,
  ### User Icon -----
  userOutput("user"),
  ### Github Icon Link -----
  tags$li(
    class="dropdown", 
    tags$a(
      # link reference
      href="https://github.com/project-greenhouse/AthleteDB",
      # icon
      icon("github"), 
      # display text
      "Source Code", 
      target="_blank"
    )
  )
)

##_ -----
## Sidebar -----
sidebar <- dashboardSidebar(
  width = 250,
  ### Sidebar Menu -----
  sidebarMenu(
    #### Menu Items -----
    # Dashboard
    menuItem("Dashboard", tabName = "tabDB", icon = icon("table-columns")),
    # Training
    menuItem("Training", tabName = "tabTrain", icon = icon("dumbbell")),
    # Recovery
    menuItem("Recovery", tabName = "tabRec", icon = icon("recycle")),
    # Sleep
    menuItem("Sleep", tabName = "tabSleep", icon = icon("bed")),
    #### Date Range Input ----
    dateRangeInput(
      inputId = "inputDate",
      label = "Date Range",
      format = "M d, yy",
      start = Sys.Date() - days(x=30),
      end = Sys.Date(),
      startview = "month",
      width = 250
    )
  )
)
  
##_ -----
## Body -----
body <- dashboardBody(
  tabItems(
    ###_ -----
    ### Dashboard Tab -----
    tabItem(
      tabName = "tabDB",
      fluidPage(
        fluidRow(
          #### R-Score Sparkline -----
          column(
            width = 4,
              h4("Readiness Score: ", textOutput("rText")),
            withLoader(
              sparklineOutput(outputId = "rScore"), 
              type="html", 
              loader="loader10")
          ),
          #### Actv-Score Sparkline -----
          column(
            width = 4,
            h4("Activity Score: ", textOutput("aText")),
            withLoader(
            sparklineOutput(outputId = "aScore"), 
            type="html", 
            loader="loader10")
          ),
          #### Sleep-Score Sparkline -----
          column(
            width = 4,
            h4("Sleep Score: ", textOutput("sText")),
            withLoader(
            sparklineOutput(outputId = "sScore"), 
            type="html", 
            loader="loader10")
          )
        ),
        column(
          width = 6,
          fluidRow(
            #### Rolling7 vs Rolling28 vs A:C Ratio -----
            withLoader(
            echarts4rOutput(
              outputId = "rollingAC",
              height = "300px"
            ), 
            type="html", 
            loader="dnaspin"),
            #### Actv Score vs Total Calories -----
            withLoader(
            echarts4rOutput(
              outputId = "ActVsCals",
              height = "300px"
            ), 
            type="html", 
            loader="dnaspin")
          )
        ),
        column(
          width = 6,
          fluidRow(
            #### TRIMP vs Readiness Score -----
            withLoader(
            echarts4rOutput(
              outputId = "EffVsDur",
              height = "300px"
            ), 
            type="html", 
            loader="dnaspin"),
            #### AVG HRV vs Avg HR -----
            withLoader(
            echarts4rOutput(
              outputId = "HrvVsHr",
              height = "300px"
            ), 
          type="html", 
          loader="dnaspin")
          )
        )
      )
    ),
    ###_ -----
    ### Training Tab -----
    tabItem(
      tabName = "tabTrain",
      fluidPage(
        h1("Training Page"),
        withLoader(
        echarts4rOutput("trainCal", height = "200px"), 
        type="html", 
        loader="loader3"),
        withLoader(
        dataTableOutput("wout", height = "500px"), 
        type="html", 
        loader="loader3")
      )
    ),
    ###_ -----
    ### Recovery Tab -----
    tabItem(
      tabName = "tabRec",
      fluidPage(
        h1("Recovery Page"),
        dataTableOutput("rness")
      )
    ),
    ###_ -----
    ### Sleep Tab -----
    tabItem(
      tabName = "tabSleep",
      fluidPage(
        h1("Sleep Page"),
        #### Sleep Stages River -----
        withLoader(
        echarts4rOutput("sleepStages", height = "250px"), 
        type="html", 
        loader="loader3"),
        #### Sleep Quality Table -----
        withLoader(
        dataTableOutput("dSleepP", height = "450px"), 
        type="html", 
        loader="loader3")
      )
    )
  )
)



#----------#
#
#
#
#----------#

#_ -----
# Server Components -----

server <- function(input, output, session) {
  
  faIcon <- function(x) as.character(icon(x, lib = "font-awesome"))
  
  ##_ -----
  ## Data -----
  
  ###_ -----
  ### Wellness Data -----
  dataWell <- read_sheet("https://docs.google.com/spreadsheets/d/1_GMnI7E9Lj4FByuQeQo_EC-eTD5ehEdTD1ATogTjA_M")
  dataWell <- dataWell %>% filter(User == "First Player")
  
  ### Clean Gsheet data
  wlns <- reactive({
    
    x <- dataWell %>%
      transmute(
        "date" = mdy(Date),
        "SleepQ" = round(scale(SleepQuality), 2),
        "Stress" = round(scale(Stress), 2),
        "Mood" = round(scale(Mood), 2),
        "sRPE" = sRPE,
        "TRIMP" = TRIMP,
        "7Day" = round(`7Day`, 0),
        "28Day" = round(`28Day`, 0),
        "ACratio" = round(A_C, 2)
      ) %>%
      filter(
        date > input$inputDate[1] & date < input$inputDate[2]
      ) %>%
      mutate(
        "Date" = date(date)
      )
    
    return(x)
  })
  
  ###_ -----
  ### API info -----
  
  #### Personal Access Token -----
  auth <- "Bearer G7W427Y3PE7NF5VLWDWXWVJCLHTEGRO5"
  
  #### Base URL -----
  url <- "https://cloud.ouraring.com/v2/usercollection/"
  payload <- ""
  encode <- "raw"
  
  #### Date Query -----
  queryString_D <- reactive({
    list(
      start_date = input$inputDate[1],
      end_date = input$inputDate[2]
    )
  })
  
  #### Date Query -----
  queryString_Dcal <- reactive({
    x <- Sys.Date()
    
    return(list(
      start_date = "2022-01-01",
      end_date = paste0(year(x), "-", month(x), "-", day(x))
    ))
  })

  ###_ -----
  ### Reactive GETs -----
  #### Body Weight -----
  bw <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"personal_info"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_D(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)
    
    # Create data frame from response
    df <- as.data.frame(goGETdf)
    
    # Select weight
    df <- df %>%
      select(weight)
    
    df <- df$weight[1]
    
    return(df)
    
  })
  
  #### Daily Activity -----
  actv <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"daily_activity"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_D(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    conts <- goGETdf$contributors
    
    conts <- conts %>% select(training_frequency, training_volume)
    
    df <- goGETdf %>% 
      select(
        day, 
        score,
        steps, 
        total_calories, 
        active_calories,
        sedentary_time, 
        resting_time, 
        high_activity_time,
        medium_activity_time,
        low_activity_time
      )
    
    df <- bind_cols(df,conts)
    
    return(df)
    
  })
  
  #### Calendar Activity -----
  actvCal <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"daily_activity"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_Dcal(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    conts <- goGETdf$contributors
    
    conts <- conts %>% select(training_frequency, training_volume)
    
    df <- goGETdf %>% 
      select(
        day, 
        score,
        steps, 
        total_calories, 
        active_calories,
        sedentary_time, 
        resting_time, 
        high_activity_time,
        medium_activity_time,
        low_activity_time
      )
    
    df <- bind_cols(df,conts)
    
    return(df)
    
  })
  
  #### Daily Readiness -----
  Rness <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"daily_readiness"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_D(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    # Contributors sub-df from original GET call
    conts <- goGETdf$contributors
    
    # Select columns from Contributors sub-df
    conts <- conts %>% 
      select(
        activity_balance,
        body_temperature,
        hrv_balance,
        recovery_index,
        resting_heart_rate,
        sleep_balance
      )
    
    df <- goGETdf %>% 
      select(
        day, 
        score,
        temperature_deviation,
        temperature_trend_deviation
      )
    
    dfRness <- bind_cols(df,conts)
    
    return(dfRness)
  })
  
  #### Daily Sleep -----
  dSleep <- reactive({
    goGET <- VERB(
      "GET", 
      paste0(url,"daily_sleep"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_D(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    # Get Contributors sub-df
    conts <- goGETdf$contributors
    
    # select columns from original GET call
    df <- goGETdf %>% 
      select(
        day, 
        score
      )
    
    # Combine contributors with selected files
    dfSleep <- bind_cols(df,conts)
    
    return(dfSleep)
  })
  
  #### Sleep Periods -----
  SleepP <- reactive({
    goGET <- VERB(
      "GET", 
      paste0(url,"sleep"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_D(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    # select columns from original GET call
    sleepP <- goGETdf %>% 
      select(
        day,
        type,
        bedtime_start,
        bedtime_end,
        time_in_bed,
        total_sleep_duration,
        efficiency,
        latency,
        awake_time,
        deep_sleep_duration,
        light_sleep_duration,
        rem_sleep_duration,
        restless_periods,
        average_heart_rate,
        lowest_heart_rate,
        average_breath,
        average_hrv
      )
    
    return(sleepP)
  })
  
  #### Workouts -----
  wout <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"workout"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = list(
        start_date = "2022-01-01",
        end_date = paste0(year(Sys.Date()),"-",month(Sys.Date()), "-", day(Sys.Date()))
      ),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    return(goGETdf)
  })
  
  #### Calendar Activity -----
  woutCal <- reactive({
    # GET call to URL and response
    goGET <- VERB(
      "GET", 
      paste0(url,"workout"), 
      body = payload, 
      add_headers(
        Host = 'api.ouraring.com', 
        Authorization = auth
      ), 
      query = queryString_Dcal(),
      content_type("application/json"), 
      encode = encode
    )
    
    # Unpack content from goGET into text/JSON format
    getResp <- content(goGET, "text")
    
    # Create DF from JSON
    goGETdf <- fromJSON(getResp)$data
    
    return(goGETdf)
    
  })

  
  ##_ -----
  
  ## Header -----
  ### User Output -----
  output$user <- renderUser({
    dashboardUser(
      name = "Greenhouse Sports Performance", 
      image = "GSPlogo.png", 
      title = "Lauren Green",
      subtitle = "Author", 
      footer = p("Together We Grow", class = "text-center"),
      fluidRow(
        # Website
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.greenhousesp.com",
            icon = icon("home")
          )
        ),
        # Github
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://github.com/project-greenhouse",
            icon = icon("square-github")
          )
        ),
        # Instagram
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.instagram.com/greenhouse_sp/",
            icon = icon("square-instagram")
          )
        ),
        #YouTube
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.youtube.com/@greenhouseperformance",
            icon = icon("square-youtube")
          )
        )
      )
    )
  })
  
  ##_ -----
  
  ## Body -----
  
  ###_ -----
  ### Dashboard Tab -----
  
  #### Body Weight Value box -----
  output$weight <- renderValueBox({
    n <- bw()
    # create value box
    valueBox(
      value = paste0(n, "kg"),
      subtitle = "Body Weight",
      icon = icon("weight-scale")
    )
  })
  
  #### Readiness Score Text -----
  output$rText <- renderText({
    x <- Rness()
    return(tail(x$score, n=1))
  })
  
  #### Readiness Score Sparkline -----
  output$rScore <- renderSparkline({
    x <- Rness()
    x <- x$score
    xAvg <- mean(x)
    xSD <- sd(x)

    
    sparkline(
      values = x,
      height = 50,
      width = 300,
      chartRangeMin = 30,
      chartRangeMax = 100,
      lineWidth = 3,
      lineColor = "#27727b",
      fillColor = "#27727b50",
      spotRadius = 3,
      normalRangeMin = xAvg - xSD,
      normalRangeMax = xAvg + xSD,
      normalRangeColor = "#00000020",
      drawNormalOnTop = TRUE
    )
  })
  
  #### Activity Score Text -----
  output$aText <- renderText({
    x <- actv()
    return(tail(x$score, n=1))
  })
  
  #### Activity Score Sparkline -----
  output$aScore <- renderSparkline({
    x <- actv()
    x <- x$score
    xAvg <- mean(x)
    xSD <- sd(x)
    
    
    sparkline(
      values = x,
      height = 50,
      width = 300,
      chartRangeMin = 30,
      chartRangeMax = 100,
      lineWidth = 3,
      lineColor = "#27727b",
      fillColor = "#27727b50",
      spotRadius = 3,
      normalRangeMin = xAvg - xSD,
      normalRangeMax = xAvg + xSD,
      normalRangeColor = "#00000020",
      drawNormalOnTop = TRUE
    )
  })
  
  #### Sleep Score Text -----
  output$sText <- renderText({
    x <- dSleep()
    return(tail(x$score, n=1))
  })
  
  #### Sleep Score Sparkline -----
  output$sScore <- renderSparkline({
    x <- dSleep()
    x <- x$score
    xAvg <- mean(x)
    xSD <- sd(x)
    
    
    sparkline(
      values = x,
      height = 50,
      width = 300,
      chartRangeMin = 30,
      chartRangeMax = 100,
      lineWidth = 3,
      lineColor = "#27727b",
      fillColor = "#27727b50",
      spotRadius = 3,
      normalRangeMin = xAvg - xSD,
      normalRangeMax = xAvg + xSD,
      normalRangeColor = "#00000020",
      drawNormalOnTop = TRUE
    )
  })
  
  #### HRV vs HR eChart -----
  output$HrvVsHr <- renderEcharts4r({
    x <- SleepP()
    
    x <- x %>%
      filter(type == "long_sleep") %>%
      group_by(day) %>%
      summarise(
        "avgHR" = mean(average_heart_rate),
        "avgHRV" = mean(average_hrv)
      )
    
    x |>
      e_chart(day) |>
      e_bar(avgHR, name = "Avg HR", y_index = 1) |>
      e_line(avgHRV, name = "Avg HRV", smooth = TRUE, y_index = 0) |>
      e_tooltip(trigger = "axis") |>
      e_title(
        text = "Heart Rate vs HRV",
        subtext = "Overnight / Sleeping averages") |>
      e_legend(right = 20) |>
      e_theme("infographic")
  })
  
  #### Activity Score vs Calories eChart -----
  output$ActVsCals <- renderEcharts4r({
    x <- actv()
    
    x <- x %>%
      mutate("pass_calories" = total_calories-active_calories)
    
    x |>
      e_chart(day) |>
      e_bar(active_calories, stack = "grp", name = "Active", y_index = 0) |>
      e_bar(pass_calories, stack = "grp", name = "Passive", y_index = 0) |>
      e_line(score, name = "Score", smooth = TRUE, y_index = 1) |>
      e_tooltip(trigger = "axis") |>
      e_title(text = "Activity Score vs Daily Calories") |>
      e_legend(right = 20) |>
      e_theme("infographic")
  })
  
  #### Sleep Efficiency vs Sleep Time eChart -----
  output$EffVsDur <- renderEcharts4r({
    x <- SleepP()
    
    x <- x %>%
      filter(type == "long_sleep") %>%
      group_by(day) %>%
      summarise(
        "duration" = mean(total_sleep_duration),
        "eff" = mean(efficiency)
      )
    
    x |>
      e_chart(day) |>
      e_bar(duration, stack = "grp", name = "Duration", y_index = 0) |>
      e_line(eff, name = "Efficiency", smooth = TRUE, y_index = 1) |>
      e_tooltip(trigger = "axis") |>
      e_title(text = "Sleep Duration vs Efficiency") |>
      e_legend(right = 20) |>
      e_theme("infographic")
  })
  
  #### AC Ratio eChart -----
  output$rollingAC <- renderEcharts4r({
    x <- wlns()
    
    x |>
      e_chart(Date) |>
      e_area(`28Day`, name = "Chronic", smooth = TRUE, y_index = 0) |>
      e_bar(`7Day`, name = "Acute", smooth = TRUE, y_index = 0, z = 3) |>
      e_line(ACratio, name = "Ratio", y_index = 1, symbol = "emptyCircle", symbolSize = 5) |>
      #e_mark_line(data = list(list(yAxis = 1))) |>
      #e_mark_line(data = list(list(yAxis = 0.7))) |>
      #e_mark_line(data = list(list(yAxis = 1.2))) |>
      #e_mark_area(data = list(
      #    list(yAxis = 1.2),
      #    list(yAxis = 0.7)
      #  ),
      #  itemStyle = list(color = "lightgreen")
      #) |>
      e_tooltip(trigger = "axis") |>
      e_y_axis(index = 0, min = 300, max = 1000) |>
      e_y_axis(index = 1, min = 0, max = 1.5) |>
      e_title(text = "Training Balance") |>
      e_legend(right = 20) |>
      e_theme("infographic")
  })
  
  
  ###_ -----
  ### Training Tab -----
  
  #### Activity Table -----
  output$actv <- renderDataTable({
    actv()
  })
  
  #### Workout Table -----
  output$wout <- renderDataTable({
    x <- wout()
    
    x <- x %>%
      arrange(desc(start_datetime)) %>%
      mutate("time2" = str_replace(str_sub(start_datetime,0,19), "T", " "),
             "time1" = str_replace(str_sub(end_datetime,0,19), "T", " ")) %>%
      transmute(
        "Date" = paste0(wday(day, label = TRUE, abbr = TRUE)," ", month(day, label = TRUE, abbr = TRUE)," ", day(day)),
        "Icon" = ifelse(activity == "walking", 
                            faIcon("person-walking"),
                            ifelse(
                              activity == "running",
                              faIcon("person-running"),
                              ifelse(
                                activity == "hiking",
                                faIcon("person-hiking"),
                                faIcon("person-rays")
                              ))),
        "Activity" = activity,
        "Intensity" = intensity,
        "Calories" = round(calories,0),
        "duration" = round(difftime(time1 = time1, time2 = time2, units = "mins"),1),
        "Source" = source)
    
    datatable(
      x, escape = FALSE, rownames = FALSE, extensions = c('Buttons', 'ColReorder', 'Scroller'), options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        colReorder = TRUE,
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE
      )
    ) %>%
      formatStyle(
        'Intensity',
        color = styleEqual(c("easy", "moderate", "hard"), c('#f5eba4', '#e7b98d', '#bf444c')),
        fontWeight = 'bold',
        background = styleEqual(c("easy", "moderate", "hard"), c('#f5eba430', '#e7b98d30', '#bf444c30')),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  #### Workout Calendar -----
  output$trainCal <- renderEcharts4r({
    x <- woutCal()
    
    tdate <- Sys.Date() + 65
    tdate <- paste0(year(tdate), "-",month(tdate))
    pdate <- Sys.Date() - 240
    pdate <- "2022-01"
    
    x <- x %>%
      select(day, calories)
    
    x |> 
      e_charts(day) |> 
      e_calendar(
        range = c(pdate, tdate),
        yearLabel = list(show = FALSE),
        top = "bottom") |> 
      e_heatmap(calories, coord_system = "calendar") |> 
      e_visual_map(max = max(x$calories)) |> 
      e_title("Workout Calendar", "Calories", left = "center") |>
      e_tooltip(trigger = "item") |>
      e_theme("inforgraphic")
  })
  
  
  ###_ -----
  ### Recovery Tab -----
  
  #### Readiness Table -----
  output$rness <- renderDataTable({
    Rness()
  })
  
  ###_ -----
  ### Sleep Tab -----
  
  #### Sleep Stages River -----
  output$sleepStages <- renderEcharts4r({
    # Sleep Stage df
    x <- SleepP()
    
    # Clean Sleep stage data and convert secs to mins
    x <- x %>%
      transmute(
        "Date" = day,
        "Deep" = deep_sleep_duration,
        "Light" = light_sleep_duration,
        "REM" = rem_sleep_duration,
        "Awake" = awake_time
      ) %>%
      group_by(Date) %>%
      summarise(
        "Deep" = sum(Deep),
        "Light" = sum(Light),
        "REM" = sum(REM),
        "Awake" = sum(Awake)
      ) %>%
      transmute(
        "Date" = Date,
        "Deep" = Deep/60,
        "Light" = Light/60,
        "REM" = REM/60,
        "Awake" = Awake/60
      )
    
    # Create smooth area chart
    x |> 
      e_charts(Date) |> 
      e_area(Deep,stack = "grp", smooth = TRUE) |> 
      e_area(Light, stack = "grp", smooth = TRUE) |> 
      e_area(REM, stack = "grp", smooth = TRUE) |> 
      e_area(Awake, stack = "grp", smooth = TRUE) |>
      e_tooltip(trigger = "axis") |> 
      e_title(text = "Sleep Stages", subtext = "time in minutes") |>
      e_theme("inforgraphic")
  })
  
  #### Sleep Table -----
  output$dSleepP <- renderDataTable({
    x<- dSleep()
    
    x <- x %>%
      select(day, score, efficiency, restfulness) %>%
      group_by(day) %>%
      summarise(
        "score" = mean(score),
        "eff" = mean(efficiency),
        "rest" = mean(restfulness)
      ) %>%
      transmute(
        "Date" = paste0(wday(day, label = TRUE, abbr = TRUE)," ", month(day, label = TRUE, abbr = TRUE)," ", day(day)),
        "Score" = score,
        "Efficiency" = eff,
        "Restfulness" = rest
      )
    
    datatable(
      x, rownames = FALSE, extensions = c('Buttons', 'ColReorder', 'Scroller'), options = list(
        dom = 'Brtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        colReorder = TRUE,
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE
      )
    ) %>%
      formatStyle(
        'Score',
        color = styleInterval(c(75, 90), c('#c1232a', '#000000', '#0bb603')),
        fontWeight = 'bold',
        background = styleInterval(c(75, 90), c('#c1232a30', '#ffffff', '#0bb60330')),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Efficiency',
        color = styleInterval(c(75, 90), c('#c1232a', '#000000', '#0bb603')),
        fontWeight = 'bold',
        background = styleInterval(c(75, 90), c('#c1232a30', '#ffffff', '#0bb60330')),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Restfulness',
        color = styleInterval(c(75, 90), c('#c1232a', '#000000', '#0bb603')),
        fontWeight = 'bold',
        background = styleInterval(c(75, 90), c('#c1232a30', '#ffffff', '#0bb60330')),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
}

#_ -----
# UI -----
ui <- dashboardPage(
  skin = "black",
  title = "Athlete Dashboard",
  header = header,
  sidebar = sidebar,
  body = body
)

#_ -----
# Run the application 
shinyApp(ui = ui, server = server)
