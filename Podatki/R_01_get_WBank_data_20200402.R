# Original code from
# Tidying the new Johns Hopkins Covid-19 time-series datasets (old code before 30.03.2020)
# March 24, 2020 in R
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/

# adapted for daily use on different platforms
# Matjaz Jeran 02.04.2020

# getting world bank data
# input: World Bank statistical data
# output: "jh_add_wbank_data.csv"
#         "jh_add_wbank_data.Rdata"

rm (list = ls (all = TRUE))

### Warning: adjust working directory as needed for operating systems Windows, macOS, linux

if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/Corona/Podatki")
if (Sys.info () ["sysname"] == "Darwin")  setwd ("/Users/matjaz/Corona/Podatki")
if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Corona/Podatki")


library(tidyverse)
library(wbstats)

wbank.file <- "jh_add_wbank_data.csv"
wbank.dfile <- "jh_add_wbank_data.RData"


pull_worldbank_data <- function(vars) {
  new_cache <- wbstats::wbcache()
  all_vars <- as.character(unique(new_cache$indicators$indicatorID))
  data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
    dplyr::rename(var_name = indicatorID) %>%
    dplyr::mutate(var_def = paste(indicator, "\nNote:",
                           indicatorDesc, "\nSource:", sourceOrg)) %>%
    dplyr::select(var_name, var_def) -> wb_data_def
  new_cache$countries %>%
    dplyr::select(iso3c, iso2c, country, region, income) -> ctries
  dplyr::left_join(data_wide, ctries, by = "iso3c") %>%
    dplyr::rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    dplyr::select(iso3c, iso2c, country, region, income, everything()) %>%
    dplyr::select(-iso2c.x, -country.x) %>%
    dplyr::filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data
  wb_data$year <- as.numeric(wb_data$year)
  wb_data_def<- dplyr::left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:6] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income group classification",
    "Calendar year of observation"
  )
  wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                       rep("numeric", ncol(wb_data) - 6))
  return(list(wb_data, wb_data_def))
}

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")
wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]
wb_data_def <- wb_list[[2]]

wb_data %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(iso3c, year) %>%
  dplyr::summarise(
    population = last(na.omit(SP.POP.TOTL)),
    land_area_skm = last(na.omit(AG.LND.TOTL.K2)),
    pop_density = last(na.omit(EN.POP.DNST)),
    pop_largest_city = last(na.omit(EN.URB.LCTY)),
    gdp_capita = last(na.omit(NY.GDP.PCAP.KD)),
    life_expectancy = last(na.omit(SP.DYN.LE00.IN))
  ) %>% left_join(wb_data %>% select(iso3c, region, income) %>% distinct()) -> wb_cs

readr::write_csv(wb_cs, wbank.file)

save (wb_cs, file = wbank.dfile)
