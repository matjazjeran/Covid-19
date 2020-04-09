# Original code from
# Tidying the new Johns Hopkins Covid-19 time-series datasets (old code before 30.03.2020)
# March 24, 2020 in R
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/

# adapted for daily use on different platforms
# added reading number of recovered patients
# Matjaz Jeran 09.04.2020

# getting epidemic data
# input: data on number of infected, deaths and recovered from
#        https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series
# output: csv and RData file
#         jh_covid19_data_%s.csv   (file name dependent on current date)
#         jh_covid19_data.RData

# correction on 09.04.2020: SYR ISO code given to Syria instead of SRB that belongs to Serbia

rm (list = ls (all = TRUE))

### Warning: adjust working directory as needed for operating systems Windows, macOS, linux

if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/Corona/Podatki")
if (Sys.info () ["sysname"] == "Darwin")  setwd ("/Users/matjaz/Corona/Podatki")
if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Corona/Podatki")


library(tidyverse)
library(lubridate)
library(rvest)
library(stringdist)

# warning: output file name contains current date
covid.file <- "jh_covid19_data_%s.csv"
covid.dfile <- "jh_covid19_data.RData"
UN.dfile <- "UN_data.RData"

raw.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recovereds.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"


# Function to read the raw CSV files. The files are aggregated to the country
# level and then converted to long format

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% dplyr::group_by(`Country/Region`) %>%
    dplyr::filter(`Country/Region` != "Cruise Ship") %>%
    dplyr::select(-`Province/State`, -Lat, -Long) %>%
    dplyr::mutate_at(vars(-group_cols()), sum) %>% 
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::rename(country = `Country/Region`) %>%
    tidyr::pivot_longer(
      -country, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    dplyr::mutate(date = lubridate::mdy(date_str)) %>%
    dplyr::select(country, date, !! sym(var_str)) 
}

confirmed_raw <- readr::read_csv(raw.file)
deaths_raw <- readr::read_csv(deaths.file)
recovered_raw <- readr::read_csv(recovereds.file)

tmp.conf <- clean_jhd_to_long(confirmed_raw)
tmp.dead <- clean_jhd_to_long(deaths_raw)
tmp.recv <- clean_jhd_to_long(recovered_raw)

tmp <- tmp.conf %>% dplyr::full_join(tmp.dead)
jh_covid19_data <- tmp %>% dplyr::full_join(tmp.recv)

##jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
##  dplyr::full_join(clean_jhd_to_long(deaths_raw))

# Next, I pull official country level indicators from the UN Statstics Division
# to get country level identifiers.

jhd_countries <- tibble::tibble(country = unique(jh_covid19_data$country)) %>% dplyr::arrange(country)

load (file = UN.dfile)  # un_m49

# Merging by country name is messy. I start with a fuzzy matching approach
# using the {stringdist} package

ctry_names_dist <- matrix(NA, nrow = nrow(jhd_countries), ncol = nrow(un_m49))
for(i in 1:length(jhd_countries$country)) {
  for(j in 1:length(un_m49$country)) { 
    ctry_names_dist[i,j]<-stringdist::stringdist(tolower(jhd_countries$country[i]), tolower(un_m49$country[j]))      
  }  
}

min_ctry_name_dist <- apply(ctry_names_dist, 1, min)

matched_ctry_names <- NULL

for(i in 1:nrow(jhd_countries)) {
  un_m49_row <- match(min_ctry_name_dist[i], ctry_names_dist[i,])
  if (length(which(ctry_names_dist[i,] %in% min_ctry_name_dist[i])) > 1) un_m49_row <- NA
  matched_ctry_names <- rbind(matched_ctry_names,
                         tibble::tibble( 
                           jhd_countries_row = i, 
                           un_m49_row = un_m49_row,
                           jhd_ctry_name = jhd_countries$country[i], 
                           un_m49_name = ifelse(is.na(un_m49_row), NA, 
                                                un_m49$country[un_m49_row])
                         ))
}

# This matches most cases well but some cases need to be adjusted by hand.
# In addition there are two jurisdictions (Kosovo, Taiwan)
# that cannot be matched as they are no 'country' as far as the U.N.
# Statistics Devision is concerned.

# WATCH OUT: The data from JHU is subject to change without notice.
# New countries are being added and names/spelling might change. 
# Also, in the long run, the data provided by the UNSD might change.
# Inspect 'matched_ctry_names' before using the data.

matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Bolivia"] <- 27
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Brunei"] <- 35
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Brazzaville)"] <- 54
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Kinshasa)"] <- 64
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "East Timor"] <- 222
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Iran"] <- 109
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Korea, South"] <- 180
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Kosovo"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Moldova"] <- 181
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Russia"] <- 184
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Taiwan*"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Tanzania"] <- 236
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "United Kingdom"] <- 235
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "US"] <- 238
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Venezuela"] <- 243

# Last Step: Match country identifier data and save file (commented out here)
jhd_countries %>% 
  dplyr::left_join(matched_ctry_names %>% 
              dplyr::select(jhd_ctry_name, un_m49_row), 
            by = c(country = "jhd_ctry_name")) %>%
  dplyr::left_join(un_m49 %>% dplyr::mutate(un_m49_row = dplyr::row_number()), by = "un_m49_row") %>%
  dplyr::rename(country = country.x) %>%
  dplyr::select(country, iso3c)  -> jhd_countries

jh_covid19_data <- jh_covid19_data %>% dplyr::left_join(jhd_countries) %>%
  dplyr::select(country, iso3c, date, confirmed, deaths, recovered)

# correction of data with mixed ISO 3 character codes  SRB is used for Serbia and Syria
# the correct codes are
# SRB is for Serbia
# SYR is for Syria
jh_covid19_data$iso3c [jh_covid19_data$country == "Syria"] <- "SYR"


readr::write_csv(jh_covid19_data, sprintf(covid.file, Sys.Date()))

save (jh_covid19_data, file = covid.dfile)
