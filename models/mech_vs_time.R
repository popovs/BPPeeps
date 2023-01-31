## ----mech vs time setup, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE, 
                      message = FALSE)

# This RMD is NOT intended as a standalone script, but rather to be sourced
# from models.Rmd. The data used in this Rmd file is generated in models.Rmd
# and binomial_sr.Rmd.


## ----aggregate by NS, include=FALSE-----------------------------------------------
dat3 <- sqldf::sqldf("select year, 
                      survey_date, 
                      julian_day, 
                      min(start_time) as start_time, 
                      n_s, 
                      sum(final_count) as final_count, 
                      sum(predicted_wesa) as predicted_wesa, 
                      sum(predicted_dunl) as predicted_dunl, 
                      p_wesa, 
                      predicted_ratio, 
                      avg(raptor_count) as raptor_count, 
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
                      group by survey_date, n_s;") %>%
  dplyr::mutate(dos = scale(julian_day),
                log_wesa = log(predicted_wesa + 1),
                log_dunl = log(predicted_dunl + 1),
                year_n = as.numeric(year),
                year_c = scale(year_n)) %>%
  dplyr::select(year, survey_date, julian_day, dos, start_time, n_s,
                final_count, predicted_wesa, predicted_dunl, log_wesa, 
                log_dunl, dplyr::everything()) %>%
  dplyr::filter(!is.na(flow), !is.na(total_precip))


## ----head dat3--------------------------------------------------------------------
head(dat3)


## ----summary of dat3--------------------------------------------------------------
summary(dat3)


## ----base model-------------------------------------------------------------------
base_model <- lme4::lmer(log_wesa ~ n_s + dos + I(dos^2) + (dos + I(dos^2) | year),
                         data = dat3)
summary(base_model)


## ----mechanism model--------------------------------------------------------------
mech_model <- lme4::lmer(log_wesa ~ n_s * scale(flow) + dos + I(dos^2) + (dos + I(dos^2) | year),
                         data = dat3)
summary(mech_model)


## ----make mech plot data----------------------------------------------------------
mech_plot <- broom.mixed::augment(mech_model, dat3)
mech_plot <- janitor::clean_names(mech_plot)


## ----mech plot: observed vs predicted---------------------------------------------
ggplot(mech_plot, aes(x = log_wesa, y = fitted)) +
  geom_point() + 
  ggtitle("Observed vs. Fitted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----mech plot: heteroskedasticity------------------------------------------------
ggplot(mech_plot, aes(x = fitted, y = resid)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----mech plot: qq plot-----------------------------------------------------------
ggplot(mech_plot, aes(sample = resid)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----temporal model---------------------------------------------------------------
temp_model <- lme4::lmer(log_wesa ~ n_s * year_c + dos + I(dos^2) + (dos + I(dos^2) | year),
                         data = dat3)
summary(temp_model)


## ----make temp plot data----------------------------------------------------------
temp_plot <- broom.mixed::augment(temp_model, dat3)
temp_plot <- janitor::clean_names(temp_plot)


## ----temp plot: observed vs predicted---------------------------------------------
ggplot(temp_plot, aes(x = log_wesa, y = fitted)) +
  geom_point() + 
  ggtitle("Observed vs. Fitted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----temp plot: heteroskedasticity------------------------------------------------
ggplot(temp_plot, aes(x = fitted, y = resid)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----temp plot: qq plot-----------------------------------------------------------
ggplot(temp_plot, aes(sample = resid)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ---------------------------------------------------------------------------------
anova(mech_model, temp_model)

