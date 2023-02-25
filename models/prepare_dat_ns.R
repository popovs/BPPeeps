# Prepare dat_ns
# Create north/south dataset (previously referred to as dat3 in initial draft code)
# Peeps observation data grouped by north/south station
# This can only be run after binomial_sr.Rmd has been run

# DAT_NS ===========================================================
dat_ns <- sqldf::sqldf("select year, 
                        survey_date, 
                        ordinal_day, 
                        min(start_time) as start_time, 
                        n_s, 
                        sum(final_count) as final_count, 
                        sum(predicted_wesa) as predicted_wesa, 
                        sum(predicted_dunl) as predicted_dunl, 
                        p_wesa, 
                        predicted_ratio, 
                        avg(raptor_count) as raptor_count, 
                        tide,
                        elev_min, 
                        elev_max, 
                        elev_median, 
                        elev_mean, 
                        elev_range, 
                        flow, 
                        total_precip, 
                        mean_temp, 
                        u, 
                        v, 
                        windspd, 
                        wind_deg 
                        from dat 
                        group by survey_date, sweep, n_s;") %>%
  dplyr::mutate(dos = scale(ordinal_day)[,1],
                log_wesa = log(predicted_wesa + 1),
                log_dunl = log(predicted_dunl + 1),
                year_n = as.numeric(year),
                year_c = scale(year_n)[,1]) %>%
  dplyr::select(year, survey_date, ordinal_day, dos, start_time, n_s,
                final_count, predicted_wesa, predicted_dunl, log_wesa, 
                log_dunl, dplyr::everything()) %>%
  dplyr::filter(!is.na(flow),
                !is.na(total_precip), 
                !is.na(tide))
