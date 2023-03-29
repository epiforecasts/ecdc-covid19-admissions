
# Get vector of country IDs that we will make admissions forecasts for
get_forecast_ids <- function(dat, forecast_date, max_trunc = 7) {
  
  # Get last reported week
  last_rep <- dat %>%
    filter(!is.na(value),
           date <= forecast_date) %>%
    group_by(id) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    filter(date >= forecast_date - 8*7) %>%
    select(id, last_rep = date) %>%
    mutate(trunc = as.numeric(forecast_date - last_rep)) %>%
    filter(trunc <= max_trunc)
  
  # Remove locations missing data in the last 8 weeks
  out <- dat %>%
    left_join(last_rep, by = "id") %>%
    filter(date >= last_rep - 8*7,
           date < last_rep) %>%
    group_by(id, last_rep, trunc) %>%
    summarise(all = sum(value),
              .groups = "drop") %>% 
    filter(!is.na(all)) %>%
    select(id = id, last_rep, trunc)
  
  return(out)
  
}

# Get samples from hub-ensemble quantile forecast
get_ensemble_samples <- function(dat, n.samples = 1000){
  
  id_int <- unique(dat$location)
  target_int <- unique(dat$target_end_date)
  
  message(paste0("Getting samples for ", id_int, " (", target_int, ")"))
  
  metalog_dn <- metalog(x = dat$value,
                        probs = dat$quantile,
                        bounds = c(0),
                        term_limit = 6)
  
  samples <- round(rmetalog(metalog_dn, n = n.samples, term = 5))
  
  out <- tibble(forecast_date = unique(dat$forecast_date),
                target = unique(dat$target),
                target_end_date = target_int,
                location = id_int,
                sample = 1:n.samples,
                value = samples)
  
  return(out)
  
}
