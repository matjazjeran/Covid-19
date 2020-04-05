# Original code from
# Tidying the new Johns Hopkins Covid-19 time-series datasets (old code before 30.03.2020)
# March 24, 2020 in R
# https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/

# adapted for daily use on different platforms and include graph for Slovenia
# Matjaz Jeran 02.04.2020

# visualize data
# input: 
#       jh_covid19_data.RData    
#       jh_add_wbank_data.RData
# output:
#       various graphs left opened

rm (list = ls (all = TRUE))

### Warning: adjust working directory as needed for operating systems Windows, macOS, linux

if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/Corona/Podatki")
if (Sys.info () ["sysname"] == "Darwin")  setwd ("/Users/matjaz/Corona/Podatki")
if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Corona/Podatki")

covid.dfile <- "jh_covid19_data.RData"
wbank.dfile <- "jh_add_wbank_data.RData"

country.selection <- c ("China", "US", "Italy", "Germany", "Austria", "Switzerland", "Croatia", "Hungary", "Singapore", "Japan", "Iceland", "Slovenia")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(gghighlight)
  library(ggrepel)
})

load (file = covid.dfile)  # jh_covid19_data
dta <- jh_covid19_data

load (file = wbank.dfile)  # wb_cs

# I define event time zero where, for a given country, the confirmed
# cases match or exceed the Chinese case number at the beginning of the
# data so that all countries can be compared across event time.
# Also a require each country to have at least 7 days post event day 0

dta %>% 
  dplyr::group_by(country) %>%
  dplyr::filter(confirmed >= min(dta$confirmed[dta$country == "China"])) %>%
  dplyr::summarise(edate_confirmed = min(date)) -> edates_confirmed

  
dta %>% 
  dplyr::left_join(edates_confirmed, by = "country") %>%
  dplyr::mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  dplyr::filter(edate_confirmed >= 0) %>%
  dplyr::group_by(country) %>%
  dplyr::filter (dplyr::n() >= 7) %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(wb_cs, by = "iso3c") %>% 
  dplyr::mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  ) -> df

  
# data selection for Slovenia and neighbourhood

# I define event time zero where, for a given country, the confirmed
# cases match or exceed the Itallian case number at the beginning of the
# data so that all countries can be compared across event time.
# Also a require each country to have at least 7 days post event day 0

dta %>% 
  dplyr::group_by(country) %>%
  dplyr::filter(date >= '2020-03-01' ) %>%
  dplyr::summarise(edate_confirmed = min(date)) -> edates_confirmed_SVN
 
dta %>% 
  dplyr::left_join(edates_confirmed_SVN, by = "country") %>%
  dplyr::filter (country %in% country.selection) %>% 
  dplyr::mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  dplyr::filter(edate_confirmed >= 0) %>%
  dplyr::group_by(country) %>%
  dplyr::filter (dplyr::n() >= 7) %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(wb_cs, by = "iso3c") %>% 
  dplyr::mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  ) -> dSVN


lab_notes <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on March 25, 2020. ",
  "The sample is limited to countries with at least seven days of positive\n", 
  "event days data. Code and walk-through: https://joachim-gassen.github.io."
)

lab_x_axis_confirmed <- sprintf(paste(
  "Days since confirmed cases matched or exceeded\n", 
  "initial value reported for China (%d cases)\n"
), min(dta$confirmed[dta$country == "China"]))

lab_x_axis_confirmed_SVN <- sprintf(paste(
  "Days since 2020-03-01"))

gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes,
       x = lab_x_axis_confirmed,
       y = "Confirmed cases (logarithmic scale)"),
  gghighlight::gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)


dev.new ()
ggplot2::ggplot(df %>% filter (edate_confirmed <= 40), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Confirmed Cases\n"
  ) + 
  gg_my_blob

# add graph for Slovenia and neghbours

lab_notes_SVN <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on March 25, 2020. ",
  "The selection of countries versus Slovenia.\n", 
  "Code and walk-through: https://joachim-gassen.github.io. and adaptation by MJ"
)

gg_my_blob_SVN <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes_SVN,
       x = lab_x_axis_confirmed_SVN,
       y = "Confirmed cases (logarithmic scale)"),
  gghighlight::gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

dev.new ()
ggplot2::ggplot(dSVN %>% filter (edate_confirmed <= 200), 
       aes(x = edate_confirmed, color = country, y = confirmed_1e5pop)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Confirmed Cases per 10^5 population\n"
  ) +
  gg_my_blob_SVN

# add graph of deaths versus infected - world


lab_notes_linear <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on March 25, 2020. ",
  "The sample is limited to countries with at least seven days of positive\n", 
  "event days data. Code and walk-through: https://joachim-gassen.github.io. and adaptation by MJ"
)

gg_my_blob_linear <- list(
  scale_y_continuous(labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes_linear,
       x = lab_x_axis_confirmed,
       y = "Confirmed deaths / cases (linear scale)"),
  gghighlight::gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
                           label_params = list(segment.color = NA, nudge_x = 1))
)

dev.new ()
ggplot2::ggplot(df %>% filter (edate_confirmed <= 200), 
                aes(x = edate_confirmed, color = country, y = deaths/confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: daily total deaths / infected ratio"
  ) +
  gg_my_blob_linear

  
# add graph of deaths versus infected - Slovenia and neighbours

gg_my_blob_linear_SVN <- list(
  scale_y_continuous(labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes_SVN,
       x = lab_x_axis_confirmed_SVN,
       y = "Confirmed deaths / cases (linear scale)"),
  gghighlight::gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
                           label_params = list(segment.color = NA, nudge_x = 1))
)

dev.new ()
ggplot2::ggplot(dSVN %>% filter (edate_confirmed <= 200), 
                aes(x = edate_confirmed, color = country, y = deaths/confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: daily total deaths / infected ratio"
  ) +
  gg_my_blob_linear_SVN

# remove all graph windows
# graphics.off ()
