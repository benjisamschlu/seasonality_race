
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
packages <- c("tidyverse", "ggplot2", "here", "utils", "data.table")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}



## Functions -------------------------------------------------------------------

source(here("code", "regex_helpers.R"))



## Load data -------------------------------------------------------------------

## Container
df <- list()
## Loop and download + cleand data by year
## year 2021 has different race vars and categories
for (y in 2000:2020) {
  
  ## Iteration stage
  cat("Loading data ", y, "\n")
  
  ## fread() from data.table usefull for big data set in .csv
  data <- fread(
    here("data_private", "nber_data", paste0("mort", y, ".csv")),
    header = T,
    select = c("year", "monthdth", "ager27", "sex", "hspanicr", 
               "ucod", paste0("enicon_", 1:20))
    )
  
  ## Iteration stage
  cat("Cleaning data \n")
  
  df.cod <- data |> 
    ## remove observations with missing age
    ## and missing ucod (might create diff with Matt)
    filter(
      ager27 != 27,
      !is.na(ucod)
      ) |> 
    ## "NA" will be contained in enicon_all but
    ## does not create a prblm as we use grepl()
    unite(
      enicon_all, enicon_1:enicon_20, 
      remove = TRUE, sep = " "
    ) |> 
    mutate(
      ## tidy age in 5y age groups
      age = case_when(
        ager27 <= 2 ~ 0, # infant
        ager27 %in% 3:6 ~ 1, # 1-4
        ager27 %in% 7:26 ~ (ager27-6)*5
                      ),
      ## tidy race_eth categories
      race_eth = case_when(
        hspanicr <= 5 ~ "Hispanic",
        hspanicr == 6 ~ "NH-White",
        hspanicr == 7 ~ "NH-Black",
        hspanicr == 8 ~ "NH-Other",
        hspanicr == 9 ~ "Hisp. Origin Unkwn"
                           ),
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
  
  ## Release memory
  rm(data)
  
  ## Iteration stage
  cat("Storing data \n")
  
  df[[as.character(y)]] <- df.cod
  
  ## Release memory
  rm(df.cod)
  
}

## Merge data sets into one
df <- do.call("rbind", df)

## Save cleaned and merged data set
saveRDS(df,
        here("data", "df.rda"))

## For comparison: group_by(year, race_eth)
## Not exactly the same number but similar. Due to race?
df.check <- readRDS("../parental_loss_by_race/data/lt_US.rda")

df.check |> 
  filter(year %in% c(2000,2010, 2019),
         race_eth %in% c("white", "black", "hispanic")) |>
  mutate(race_eth = factor(race_eth,
                           levels = c("hispanic", "black", "white"),
                           labels = c("Hispanic", "NH-Black", "NH-White"))) |> 
  group_by(year, race_eth) |> 
  summarise(n_deaths = sum(n_deaths),
            n_drug = sum(n_drug),
            n_opioid = sum(n_opioid))

df |> 
  filter(year %in% c(2000,2010, 2019),
         race_eth %in% c("NH-White", "NH-Black", "Hispanic")) |>
  group_by(year, race_eth) |> 
  summarise(n_deaths = sum(n_deaths),
            n_drug = sum(n_drugs),
            n_opioid = sum(n_opioid))


## Visu ------------------------------------------------------------------------

## All death
df |> 
  ## year 2020 is affected by Covid19
  ## and has much higher counts
  filter(year != 2020) |> 
  ggplot(aes(x = monthdth, y = n_deaths, col = year, group = year)) +
  facet_wrap(~ race_eth,
             scales = "free_y") +
  geom_line() +
  theme_bw() +
  theme(legend.position = c(0.85, 0.2)) +
  scale_x_continuous(breaks = 1:12)

## Drug deaths
df |> 
  filter(year != 2020) |> 
  ggplot(aes(x = monthdth, y = n_drugs, col = year, group = year)) +
  facet_wrap(~ race_eth,
             scales = "free_y") +
  geom_line() +
  theme_bw() +
  theme(legend.position = c(0.85, 0.2)) +
  scale_x_continuous(breaks = 1:12)

