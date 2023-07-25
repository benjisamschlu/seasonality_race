
##----------------------- SEASONALITY BY RACE ----------------------------------
##
##  
## 
## 
##  Author: Benjamin Schlüter
##  Date: July 2023
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
## restatus: state of death=residence? -> does not matter if national analysis
## monthdth: month of death
## sex
## hspanicr: Hispanic/race
## year
## ager27
## ucod: underlying COD with ICD10
##  
##
##  Checks
## -------
##  
## 
##------------------------------------------------------------------------------

rm(list = ls())



## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "ggplot2", "here", "utils")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}



## Functions -------------------------------------------------------------------



## Load data -------------------------------------------------------------------

## Loop and download + cleand data by year
download_url <- "https://data.nber.org/mortality/2011/mort2011.csv.zip"

download.file(download_url,
              here("data_private", basename(download_url)))

unzip(here("data_private", basename(download_url)),
      exdir = here("data_private", "nber_data"))

data <- read.csv(file=here("data_raw", "nber_data", "mort2011.csv"))

# file.remove(here("data_private", basename(download_url)))

## Select vars of interest
df <- data |> 
    dplyr::select(year, monthdth, ager27, sex, hspanicr, ucod) |> 
    mutate(
        ## tidy age in 5y age groups
        age = case_when(ager27 <= 2 ~ 0, # infant
                        ager27 %in% 3:6 ~ 1, # 1-4
                        ager27 %in% 7:26 ~ (ager27-6)*5,
                        TRUE ~ NA),
        ## tidy race_eth categories
        race_eth = case_when(hspanicr <= 5 ~ "Hispanic",
                             hspanicr == 6 ~ "NH-White",
                             hspanicr == 7 ~ "NH-Black",
                             hspanicr == 8 ~ "NH-Other",
                             TRUE ~ "Hisp. Origin Unkwn")
    ) |> 
    group_by(
        ## Does not account for CoD 
        ## neither age and sex for the moment
        year, monthdth, race_eth
    ) |> 
    summarize(
        N = n()
    )



## Visu ------------------------------------------------------------------------

df |> 
    ggplot(aes(x = monthdth, y = N, col = race_eth)) +
    facet_wrap(~ race_eth,
               scales = "free_y") +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = 1:12)

