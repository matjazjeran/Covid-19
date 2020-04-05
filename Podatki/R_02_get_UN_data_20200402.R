# Original code from
# Tidying the new Johns Hopkins Covid-19 time-series datasets (old code before 30.03.2020)
# March 24, 2020 in R
# the code base on
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/

# adapted for daily use on different platforms
# Matjaz Jeran 30.03.2020

# getting UN statistical data
# input: UN statistical data
# output: "UN_data.RData"

rm (list = ls (all = TRUE))

### Warning: adjust working directory as needed for operating systems Windows, macOS, linux

if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/Corona/Podatki")
if (Sys.info () ["sysname"] == "Darwin")  setwd ("/Users/matjaz/Corona/Podatki")
if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Corona/Podatki")

library (xml2)
library (rvest)

# Warning: csv output file name contains current date
covid.file <- "jh_covid19_data_%s.csv"
UN.dfile <- "UN_data.RData"

raw.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

UN.stats.file <- "https://unstats.un.org/unsd/methodology/m49/"


# I pull official country level indicators from the UN Statistics Division
# to get country level identifiers.

ctry_ids <- xml2::read_html(UN.stats.file) %>%
  rvest::html_table()
un_m49 <- ctry_ids[[1]]
colnames(un_m49) <- c("country", "un_m49", "iso3c")

save (un_m49, file = UN.dfile)
