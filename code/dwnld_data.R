#----------------------- SEASONALITY BY RACE ----------------------------------
##
##  DOWNLOAD DATA 
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



## Download data ---------------------------------------------------------------

for (y in 2000:2021) {
    
    ## Iteration stage
    cat("Working on year ", y, "\n")
    
    ## Download and load data
    ## url names change over recent years
    download_url <- case_when(
        
        y %in% 2018:2020 ~ paste0("https://data.nber.org/nvss/mortality/csv/",
                                  "Mort", y, "US.PubUse.csv"),
        y == 2021 ~ paste0("https://data.nber.org/nvss/mortality/csv/",
                           "mort", y, "us.csv"),
        TRUE ~ paste0("https://data.nber.org/mortality/", y,
                      "/mort", y, ".csv.zip")
    )
    
    ## earlier files are zipped
    if (y < 2018) {
        download.file(download_url,
                      here("data_private", basename(download_url)))
        
        unzip(here("data_private", basename(download_url)),
              exdir = here("data_private", "nber_data"))
        
        file.remove(here("data_private", basename(download_url)))
        
    } else {
        download.file(download_url,
                      here("data_private", "nber_data",
                           paste0("mort", y, ".csv")))
    }
    
}


