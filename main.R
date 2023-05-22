# Date --------------------------------------------------------------------
# a Saturday
fdate <- lubridate::floor_date(lubridate::today(),
                               unit = "week",
                               week_start = 6)


# Set up ------------------------------------------------------------------

# Using functions from covid19-hospital-activity (Trust-level admissions forecasts)
devtools::source_url("https://raw.githubusercontent.com/epiforecasts/covid19-hospital-activity/main/R/forecast_fns.R")

devtools::load_all()

if (!dir.exists(here::here("data", "figures"))) {
  dir.create(here::here("data", "figures"))
}

unlink(here::here("data-processed", ""), recursive = TRUE)
dir.create(here::here("data-processed"))
dir.create(here::here("data-processed", "epiforecasts-tsensemble"))

# Load data ---------------------------------------------------------------
dat <- load_data(end_date = fdate)

id_dat <- dat %>%
  unite(col = "id", location, target_variable, sep = "-") %>%
  select(id, date, value)

fcast_ids <- get_forecast_ids(dat = id_dat,
                              forecast_date = fdate,
                              max_trunc = 30)

obs_dat <- id_dat |>
  dplyr::filter(id %in% fcast_ids$id)

# Dates of forecast horizons (to allow for truncated data)
fhorizons <- seq.Date(from = fdate - 2 * 7, to = fdate + 4 * 7, by = "week")

tsensemble_summary <- list()

# Time series ensemble ----------------------------------------------------
safe_timeseries_samples <- purrr::safely(timeseries_samples)

tsensemble_samples <- purrr::map_df(
  .x = sort(unique(fcast_ids$trunc)),
  .f = ~ {
    dat_int <- obs_dat %>%
      filter(id %in% fcast_ids$id[which(fcast_ids$trunc == .x)])
    fdate_int <- unique(fcast_ids$last_rep[which(fcast_ids$trunc == .x)])
    out_samples <- safe_timeseries_samples(data = dat_int,
      yvar = "value",
      horizon = 4 + .x/7,
      train_from = fdate_int - 8*7,
      forecast_from = fdate_int,
      models = "aez")
    return(out_samples$result)
  }) %>%
  bind_rows() %>%
  ungroup() %>%
  mutate(model = "Time series ensemble")

tsensemble_summary <- forecast_summary(
  samples = tsensemble_samples,
  quantiles = c(0.01, 0.025,
                seq(from = 0.05, to = 0.95, by = 0.05),
                0.975, 0.99)) %>%
  ungroup() %>%
  mutate(date_horizon = forecast_from + (7*horizon),
         horizon = as.numeric(date_horizon - fdate)/7,
         forecast_from = fdate) %>%
  filter(date_horizon %in% fhorizons,
         quantile_label != "upper_0") %>%
  select(-quantile_label) %>%
  separate(col = "id", into = c("location", "target_variable"), sep = "-")

file_name <- paste0("timeseries_ensemble_", fdate, ".csv")
write_csv(tsensemble_summary,
          file = here::here("data", "forecasts-raw", "timeseries_ensemble", file_name))
format_forecast(forecast_summary = tsensemble_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-tsensemble.csv"),
                file_path = here::here("data-processed", "epiforecasts-tsensemble"))

models = c("Time series ensemble")

g_admissions <- plot_forecasts(dat_obs = dat,
                               forecast_date = fdate,
                               regions = fcast_ids$id,
                               models = models)

ggsave(plot = g_admissions,
       filename = here::here("data", "figures", "current_forecast.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)

file.copy(
  Sys.glob(here::here("data-processed", "*", "*")),
  here::here("data", "forecasts-format")
)
