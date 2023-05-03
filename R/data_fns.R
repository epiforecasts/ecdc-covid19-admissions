
# Load case data ----------------------------------------------------------

load_ecdc_cases <- function() {

  dat_raw <- read_csv(file = "https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/ECDC/truncated_ECDC-Incident%20Cases.csv") %>%
    mutate(target_variable = "inc case")

  return(dat_raw)
}

load_ecdc_deaths <- function() {

  dat_raw <- read_csv(file = "https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/ECDC/truncated_ECDC-Incident%20Deaths.csv") %>%
    mutate(target_variable = "inc death")

  return(dat_raw)
}

# Load admissions data ----------------------------------------------------

load_owid_hosps <- function(){
 
  dat_raw <- read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/OWID/truncated_OWID-Incident%20Hospitalizations.csv") %>%
    mutate(target_variable = "inc hosp")
  
  return(dat_raw)
}


# Load raw data -----------------------------------------------------------

load_data <- function(end_date){
  
  case_data <- load_ecdc_cases()
  death_data <- load_ecdc_deaths()
  adm_data <- load_owid_hosps()
  
  out <- bind_rows(adm_data, case_data, death_data)

  return(out)
  
}

# Load ensemble case forecast ---------------------------------------------

load_hub_ensemble <- function(forecast_date = "2021-09-27", locs){
  
  url <- paste0(
    "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-processed/EuroCOVIDhub-ensemble/",
    forecast_date,
    "-EuroCOVIDhub-ensemble.csv"
  )
  
  dat <- read_csv(file = url)
  
  if(missing(locs)){
    locs <- unique(dat$location)
  }
  
  # Raw forecast
  out <- dat %>%
    filter(grepl("inc case", target),
           type == "quantile",
           location %in% locs)
  
  # Point forecast
  out_point <- out %>%
    filter(quantile == 0.5)
  
  return(list(raw_forecast = out,
              point_forecast = out_point))
  
}
