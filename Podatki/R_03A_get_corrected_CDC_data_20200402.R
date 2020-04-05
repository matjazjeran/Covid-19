# Based on
# Tidying the new Johns Hopkins Covid-19 time-series datasets (old code before 30.03.2020)
# March 24, 2020 in R
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/

# adapted for daily use on different platforms
# Matjaz Jeran 30.03.2020

# getting manually corrected epidemic data from csv file 
# input: data on number of infected, deaths and recovered from
#        jh_covid19_data_%s.csv   (file name dependent on current date)
# output: RData file
#         jh_covid19_data.RData

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
  
jh_covid19_data <- readr::read_csv(file = sprintf(covid.file, Sys.Date()))
save (jh_covid19_data, file = covid.dfile)

