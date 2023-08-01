
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

source(here("code", "regex_helpers.R"))

## Load data -------------------------------------------------------------------

## Loop and download + cleand data by year
download_url <- "https://data.nber.org/mortality/2011/mort2011.csv.zip"

download.file(download_url,
              here("data_private", basename(download_url)))

unzip(here("data_private", basename(download_url)),
      exdir = here("data_private", "nber_data"))

data <- read.csv(file=here("data_private", "nber_data", "mort2011.csv"))

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

## Cause of death classification
# https://github.com/mkiang/narcan/blob/master/R/regex_helpers.R
# https://github.com/mkiang/narcan/blob/master/R/flag_drug_deaths.R
# T-codes should be checked in enicon_1 ... enicon_20

df.cod <- data |> 
  dplyr::select(year, monthdth, ager27, sex, hspanicr, ucod, contains("enicon_")) |>
  ## enicon_20 is logical and creates problem with unite()
  mutate_if(is.logical, as.character) |> 
  ## remove na for unite()
  replace_na(list(enicon_20 = "")) |> 
  unite(enicon_all, enicon_1:enicon_20, remove = TRUE, sep = " ") |> 
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
                         TRUE ~ "Hisp. Origin Unkwn"),
    ## flag drug death
    drug_death = (
      grepl(.regex_drug_icd10(ucod_codes = TRUE), ucod) &
      grepl(.regex_drug_icd10(t_codes = TRUE), enicon_all)) 
      + 0,
    ## flag opiod death
    opioid_death = (
      grepl(.regex_opioid_icd10(ucod_codes = TRUE), ucod) &
        grepl(.regex_opioid_icd10(t_codes = TRUE), enicon_all)) 
    + 0
    ) |> 
  group_by(
    ## Does not account for CoD 
    ## neither age and sex for the moment
    year, monthdth, race_eth
  ) |> 
  summarize(
    n_deaths = n(),
    n_drugs = sum(drug_death),
    n_opioid = sum(opioid_death)
  )
## For comparison: group_by(year, race_eth)
## Not exactly the same number but similar. Due to race?
df.check <- readRDS("../parental_loss_by_race/data/lt_US.rda")


## Visu ------------------------------------------------------------------------

## All death
df |> 
    ggplot(aes(x = monthdth, y = N, col = race_eth)) +
    facet_wrap(~ race_eth,
               scales = "free_y") +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = 1:12)

## Drug deaths
df.cod |> 
  ggplot(aes(x = monthdth, y = n_drugs, col = race_eth)) +
  facet_wrap(~ race_eth,
             scales = "free_y") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = 1:12)

