## Regex helpers
.regex_drug_icd10 <- function(ucod_codes = FALSE, t_codes = FALSE) {
    ## Just returns the regex for all drug deaths as defined by the ISW7
    ##
    ## For ICD10, you need one UCOD below **AND** one T-code in contributory.
    ##
    ## ICD-10 UCOD codes that designate drug poisoning (all intents):
    ##      X40-X44 -- "\\<X[46][01234]\\d{0,1}\\>"
    ##      X60-X64 -- "\\<X[46][01234]\\d{0,1}\\>"
    ##      X85 -- "\\<X85\\d{0,1}\\>"
    ##      Y10-Y14 -- "\\<Y1[01234]\\d{0,1}\\>"
    ##
    ## ICD-10 T codes in contributing causes that designates drug poisoning:
    ##      T36-T39.9 -- "\\<T3[6789]\\d{0,1}\\>"
    ##      T40.0-T50.9 -- "\\<T[45]\\d{1,2}\\>"
    
    search_term <- NULL
    
    if (ucod_codes) {
        u_1 <- "\\<X[46][01234]\\d{0,1}\\>"
        u_2 <- "\\<X85\\d{0,1}\\>"
        u_3 <- "\\<Y1[01234]\\d{0,1}\\>"
        
        search_term <- c(u_1, u_2, u_3)
    }
    
    if (t_codes) {
        t_1 <- "\\<T3[6789]\\d{0,1}\\>"
        t_2 <- "\\<T[45]\\d{1,2}\\>"
        
        search_term <- c(search_term, t_1, t_2)
    }
    
    return(paste0(search_term, collapse = "|"))
}


.regex_opioid_icd10 <- function(ucod_codes = FALSE, t_codes = FALSE) {
    ## Just returns the regex for all poisoning deaths as defined by the ISW7
    ##
    ## For ICD10, you need one UCOD below **AND** one T-code in contributory.
    ##
    ## ICD-10 UCOD codes that designate opioid poisoning (all intents):
    ##      X40-X44 -- "\\<X[46][01234]\\d{0,1}\\>"
    ##      X60-X64 -- "\\<X[46][01234]\\d{0,1}\\>"
    ##      X85 -- "\\<X85\\d{0,1}\\>"
    ##      Y10-Y14 -- "\\<Y1[01234]\\d{0,1}\\>"
    ##
    ## ICD-10 T codes in contributing causes that designates opioid poisoning:
    ##      T40.0-T40.4 -- "\\<T40[01234]\\>"
    
    search_term <- NULL
    
    if (ucod_codes) {
        u_1 <- "\\<X[46][01234]\\d{0,1}\\>"
        u_2 <- "\\<X85\\d{0,1}\\>"
        u_3 <- "\\<Y1[01234]\\d{0,1}\\>"
        
        search_term <- c(u_1, u_2, u_3)
    }
    
    if (t_codes) {
        t_1 <- "\\<T40[012346]\\>"
        
        search_term <- c(search_term, t_1)
    }
    
    return(paste0(search_term, collapse = "|"))
}

