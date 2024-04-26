library(tidyverse)
library(fastDummies)


data = read.csv("cardata.csv")

# select target variable and remove missing data
data = data %>%
  select(Crash.Date.Time,
         Route.Type,
         Surface.Condition,
         Light,
         Drivers.License.State) %>%
  na.omit()
names(data) = c("date", "route", "surface", "light", "license")
data = data[-which(
  data$route == "" | data$surface == "" |
    data$light == "" | data$license == "" |
    data$route == "N/A" | data$surface == "N/A" |
    data$light == "N/A" | data$license == "N/A"
),]
data$license = data$license == "MD" 


# remove unkonwn data and combine some categories
data = data %>% filter(light != "UNKNOWN", surface != "UNKNOWN", route != "Unknown")

data = data %>% group_by(route, surface, light, license) %>% summarise(n =
                                                                         n())

data = data %>% mutate(route = if_else(
  route %in% c("County", "Government", "Municipality"),
  "Highway (Other)",
  if_else(
    route %in% c("Service Road", "Ramp", "Other Public Roadway"),
    "Non-Highway",
    route
  )
))

data = data %>% mutate(light = if_else(
  light %in% c("DARK -- UNKNOWN LIGHTING", "DARK LIGHTS ON", "DARK NO LIGHTS"),
  "DARK",
  light
))

data = data %>% mutate(surface = ifelse(
  surface %in% c("SLUSH", "SNOW", "WATER(STANDING/MOVING)"),
  "SLUSH/SNOW/WATER",
  ifelse(
    surface %in% c("MUD, DIRT, GRAVEL", "SAND"),
    "MUD/DIRT/SAND",
    ifelse(surface %in% c("OIL", "OTHER"), "OTHER", surface)
  )
))

data = data %>% group_by(route, surface, light, license) %>% summarise(n = sum(n))

data = data %>% filter(light != "OTHER", surface != "OTHER")

data = data %>% mutate(license = ifelse(license == TRUE, "MD", "OTHER"))

# create dummy variables
data = dummy_cols(data)

# add missing combinations
data = complete(data, route, surface, light, license)

data = data[, 1:5] %>% replace_na(list(n = 0))

data = dummy_cols(data)
