library(httr)
library(jsonlite)
library(dplyr)

#_# -----
# Goggle Fit Data -----
urlGfit <- "https://v1.nocodeapi.com/greenhousesp/fit/hMXArdwWKOBeuRMh/aggregatesDatasets"

queryGfit <- list(
  dataTypeName = "activity_summary,weight,active_minutes,heart_minutes,calories_expended,steps_count",
  customTimePeriod = "[2022-10-03T09:15:36.755000-07:00,2022-10-03T09:43:09.131000-07:00]"
)

payload <- ""

encode <- "raw"

response <- VERB("GET", urlGfit, body = payload, query = queryGfit, content_type("application/octet-stream"), encode = encode)

gfit <- content(response, "text")

#_# -----
# Oura -----
## API info -----
### Personal Access Token -----
auth <- "Bearer G7W427Y3PE7NF5VLWDWXWVJCLHTEGRO5"

## API URLs -----

### Base URL -----
url <- "https://cloud.ouraring.com/v2/usercollection/"

payload <- ""

encode <- "raw"

### URL subs -----

#### Heart Rate -----
# Uses datetime
hr <- "heartrate"

#### Personal Info -----
pinfo <-"personal_info"

#### Session -----
sess <- "session"

#### Sleep Periods -----
pSleep <- reactive({
  # GET call to URL and response
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
})

#### Tags -----
oTags <- "tag"


#### Personal Info -----
pinfo <-"personal_info"

#### Daily Activity -----
actv <- "daily_activity"

#### Daily Readiness -----
rness <- "daily_readiness"

#### Heart Rate -----
# Uses datetime
hr <- "heartrate"

#### Session -----
sess <- "session"

#### Sleep Periods -----
sleep <- "sleep"

#### Workouts -----
wout <- "workout"

#### Tags -----
oTags <- "tag"

### Date and Datetime -----

# Date Query
queryString_D <- list(
  start_date = "2021-09-01",
  end_date = "2022-12-01"
)

# DateTime Query 
queryString_DT <- list(
  start_datetime = "2022-10-03T09:15:36.755000-07:00",
  end_datetime = "2022-10-03T09:43:09.131000-07:00"
)

#_# -----

### Personal Info Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"personal_info"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
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

bw <- df$weight[1]
  
#---------#




#_# -----

### Daily Sleep Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"daily_sleep"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
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

#---------#



#_# -----

### Sleep Stages Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"workout"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
  content_type("application/json"), 
  encode = encode
)

# Unpack content from goGET into text/JSON format
getResp <- content(goGET, "text")

# Create DF from JSON
goGETdf <- fromJSON(getResp)$data

dfwout <- goGETdf

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

#---------#



#_# -----
### Daily Activity Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"daily_activity"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
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

dfActv <- bind_cols(df,conts)


#---------#



#_# -----

### Daily Readiness Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"daily_readiness"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
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

# Select columns from original GET call df
df <- goGETdf %>% 
  select(
    day, 
    score,
    temperature_deviation,
    temperature_trend_deviation
  )

# Combine columns from df and conts
dfRcvry <- bind_cols(df,conts)


#_# -----

### Workout Call -----
# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"workout"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  query = queryString_D,
  content_type("application/json"), 
  encode = encode
)

# Unpack content from goGET into text/JSON format
getResp <- content(goGET, "text")

# Create DF from JSON
goGETdf <- fromJSON(getResp)$data


# Select columns from original GET call df
dfWout <- goGETdf %>% 
  select(
    day, 
    activity,
    source,
    label,
    start_datetime,
    end_datetime,
    calories,
    intensity
  )


#_# -----

### Heart Rate Call -----

# DateTime Query
queryString_DT <- list(
  start_datetime = dfWout$start_datetime[1],
  end_datetime = dfWout$end_datetime[1]
)

# GET call to URL and response
goGET <- VERB(
  "GET", 
  paste0(url,"heartrate"), 
  body = payload, 
  add_headers(
    Host = 'api.ouraring.com', 
    Authorization = auth
  ), 
  #query = queryString_DT,
  content_type("application/json"), 
  encode = encode
)

# Unpack content from goGET into text/JSON format
getResp <- content(goGET, "text")

# Create DF from JSON
goGETdf <- fromJSON(getResp)$data


# Select columns from original GET call df
dfWout <- goGETdf %>% 
  select(
    day, 
    activity,
    source,
    label,
    start_datetime,
    end_datetime,
    calories,
    intensity
  )


