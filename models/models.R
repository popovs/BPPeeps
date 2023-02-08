## ----setup ---------------------------------------------------------
dat <- read.csv("peeps_model_dat.csv")
summary(dat)

library(ggplot2)
library(formatR)

# Color palette (using blue & yellow from wesanderson Zissou1 palette)
pal <- c(rgb(86, 152, 175, maxColorValue = 255), rgb(229, 205, 79, maxColorValue = 255))


# ----canham extended---------------------------------------------------------------

# Base model
# Exactly the same model as in Canham et al. (2021), but using dataset with 
# birds aggregated by station (dat2) vs. by total birds per survey date
# Model still fails to converge if using data aggregated by N/S

# Response variable: log-transformed predicted WESA count (log_wesa)
# Predictor variables:
#   - Scale-transformed survey year (year_c)
#   - Scale-transformed Julian date, aka Day of Season (dos)
wesa_base_mod <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + (dos + I(dos^2) | year),
                            data = dat,
                            REML = TRUE)

summary(wesa_base_mod)


## ----reduce dat to complete cases-------------------------------------------------
# Reduce dataset to our variables of interest, and only keep 
# complete cases
dat2 <- dat[,c("predicted_wesa", "log_wesa", "year", "year_c", "dos", "elev_range", "total_precip", "mean_temp", "flow", "u", "v", "n_s")]
dat2 <- dat2[complete.cases(dat2),]


## ----wesa ~ canham + n_s model, warning=FALSE-------------------------------------
wesa_full_mod <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + scale(elev_range) + scale(total_precip) + scale(mean_temp) + scale(flow) + scale(u) + scale(v) + n_s + (dos + I(dos^2) | year) + (scale(flow) | n_s),
                            data = dat2,
                            REML = TRUE)
summary(wesa_full_mod)


## ----selection--------------------------------------------------------------------
lmerTest::step(wesa_full_mod, 
               direction = "backward", 
               reduce.random = F,
               keep=c("year_c", "dos", "I(dos^2)"))


## ----summary n_s best fit, warning=FALSE------------------------------------------
wesa_best_fit <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + scale(elev_range) + scale(v) + (dos + I(dos^2) | year) + (scale(flow) | n_s),
                                data = dat2,
                                REML = TRUE)

summary(wesa_best_fit)


## ----add predicted n-s values to dat2 ------------------------------
# Add predicted values to dat2
dat2$predicted_log_wesa <- fitted(wesa_best_fit)
dat2$resids <- resid(wesa_best_fit)


## ----wesa n-s plot: observed vs predicted-----------------------------------------
ggplot(dat2, aes(x = predicted_log_wesa, y = log_wesa)) +
  geom_point() + 
  ggtitle("Observed vs. Predicted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----wesa n-s plot: heteroskedasticity--------------------------------------------
ggplot(dat2, aes(x = predicted_log_wesa, y = resids)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----wesa n-s plot: qq plot-------------------------------------------------------
ggplot(dat2, aes(sample = resids)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()



## ----mech vs time -------------------------------------


## ----aggregate by NS -----------------------------------------------
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
anova(base_model, mech_model, temp_model)



## ----wesa vs year-----------------------------------------------------------------
ggplot(dat3, aes(x = year_n, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Year vs log(WESA)",
       x = "Year", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----wesa vs dos------------------------------------------------------------------
ggplot(dat3, aes(x = dos, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Day of Season vs log(WESA)",
       x = "DOS", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----wesa vs mean temp------------------------------------------------------------
ggplot(dat3, aes(x = mean_temp, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Mean temperature (C°) vs log(WESA)",
       x = "C°", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----wesa vs total_precip---------------------------------------------------------
ggplot(dat3, aes(x = total_precip, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Total precipitation (mm) vs log(WESA)",
       x = "mm", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----wesa vs flow-----------------------------------------------------------------
ggplot(dat3, aes(x = flow, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Flow vs log(WESA)",
       x = "Flow", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()

## ----wesa vs u--------------------------------------------------------------------
ggplot(dat3, aes(x = u, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "u (westerly winds) vs log(WESA)",
       x = "u (westerly winds)", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()

## ----wesa vs v--------------------------------------------------------------------
ggplot(dat3, aes(x = v, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "v (southerly winds) vs log(WESA)",
       x = "v (southerly winds)", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()

## ----wesa vs windspd--------------------------------------------------------------
ggplot(dat3, aes(x = windspd, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Wind speed vs log(WESA)",
       x = "Wind speed (km/h)", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----wesa vs tide-----------------------------------------------------------------
ggplot(dat3, aes(x = elev_range, y = log_wesa, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Tidal amplitude vs log(WESA)",
       x = "Tidal amplitude (m)", 
       y = "log(WESA)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----full wesa model--------------------------------------------------------------
full_wesa_model <- lmerTest::lmer("log_wesa ~ scale(total_precip) + scale(mean_temp) + n_s*scale(flow) + n_s*scale(elev_range) + n_s*scale(u) + n_s*scale(v) + n_s*scale(windspd) + dos + I(dos^2) + year_c + (dos + I(dos^2) | year)",
           data = dat3)

summary(full_wesa_model)


## ----wesa stepwise----------------------------------------------------------------
lmerTest::step(full_wesa_model, data = dat3, reduce.random = F, keep = c("year_c","dos", "I(dos^2)"))


## ----final wesa model-------------------------------------------------------------
s <- lmerTest::step(full_wesa_model, data = dat3, reduce.random = F, keep = c("year_c","dos", "I(dos^2)"))
final_wesa_model <- lme4::lmer(lmerTest::get_model(s)@call$formula, data = dat3)


## ----wesa model forest plot-------------------------------------------------------
sjPlot::plot_model(final_wesa_model, type = "std") +
  ggtitle("Final WESA model - standardized fixed effect sizes") +
  scale_x_discrete(labels = rev(c("n_s [S]", "elev_range", 
                              "u", "v", "windspd", "dos", "dos^2", 
                              "year", "n_s : elev_range",
                              "n_s : u", "n_s : v", "n_s : windspd")))


## ----wesa random effects forest plot----------------------------------------------
sjPlot::plot_model(final_wesa_model, type = "re")


## ----wesa make temp plot data-----------------------------------------------------
temp_plot <- broom.mixed::augment(final_wesa_model, dat3)
temp_plot <- janitor::clean_names(temp_plot)


## ----wesa temp plot: observed vs predicted----------------------------------------
ggplot(temp_plot, aes(x = log_wesa, y = fitted)) +
  geom_point() + 
  ggtitle("Observed vs. Fitted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----wesa temp plot: heteroskedasticity-------------------------------------------
ggplot(temp_plot, aes(x = fitted, y = resid)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----wesa temp plot: qq plot------------------------------------------------------
ggplot(temp_plot, aes(sample = resid)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----wesa slope coeff-------------------------------------------------------------
sjPlot::plot_model(final_wesa_model, type = "slope") +
  ggtitle("Coefficient slopes vs Response")


## ----wesa vars p1-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggtitle("Full dataset variables vs. Residuals") + 
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 1) +
  theme_minimal()


## ----wesa vars p2-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 2) +
  theme_minimal()


## ----wesa vars p3-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 3) +
  theme_minimal()


## ----dunl vs year-----------------------------------------------------------------
ggplot(dat3, aes(x = year_n, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Year vs log(DUNL)",
       x = "Year", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs dos------------------------------------------------------------------
ggplot(dat3, aes(x = dos, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Day of Season vs log(DUNL)",
       x = "DOS", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs mean temp------------------------------------------------------------
ggplot(dat3, aes(x = mean_temp, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Mean temperature (C°) vs log(DUNL)",
       x = "C°", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs total_precip---------------------------------------------------------
ggplot(dat3, aes(x = total_precip, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Total precipitation (mm) vs log(DUNL)",
       x = "mm", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs flow-----------------------------------------------------------------
ggplot(dat3, aes(x = flow, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Flow vs log(DUNL)",
       x = "Flow", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()

## ----dunl vs u--------------------------------------------------------------------
ggplot(dat3, aes(x = u, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "u (westerly winds) vs log(DUNL)",
       x = "u (westerly winds)", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs v--------------------------------------------------------------------
ggplot(dat3, aes(x = v, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "v (southerly winds) vs log(DUNL)",
       x = "v (southerly winds)", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs windspd--------------------------------------------------------------
ggplot(dat3, aes(x = windspd, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Wind speed vs log(DUNL)",
       x = "Wind speed (km/h)", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----dunl vs tide-----------------------------------------------------------------
ggplot(dat3, aes(x = elev_range, y = log_dunl, color = n_s)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Tidal amplitude vs log(DUNL)",
       x = "Tidal amplitude (m)", 
       y = "log(DUNL)",
       color = "Station") + 
  cowplot::theme_cowplot()


## ----full dunl model--------------------------------------------------------------
full_dunl_model <- lmerTest::lmer("log_dunl ~ scale(total_precip) + scale(mean_temp) + n_s*scale(flow) + n_s*scale(elev_range) + n_s*scale(u) + n_s*scale(v) + n_s*scale(windspd) + dos + I(dos^2) + year_c + (dos + I(dos^2) | year)",
           data = dat3)

summary(full_dunl_model)


## ----dunl stepwise----------------------------------------------------------------
lmerTest::step(full_dunl_model, data = dat3, reduce.random = F, keep = c("year_c","dos", "I(dos^2)"))


## ----final dunl model-------------------------------------------------------------
s <- lmerTest::step(full_dunl_model, data = dat3, reduce.random = F, keep = c("year_c","dos", "I(dos^2)"))
final_dunl_model <- lme4::lmer(lmerTest::get_model(s)@call$formula, data = dat3)


## ----dunl model forest plot-------------------------------------------------------
sjPlot::plot_model(final_dunl_model, type = "std") +
  ggtitle("Final DUNL model - standardized fixed effect sizes") +
  scale_x_discrete(labels = rev(c("n_s [S]", "flow", "elev_range", 
                              "u", "v", "windspd", "dos", "dos^2",
                              "year", "n_s : elev_range",
                              "n_s : u", "n_s : v", "n_s : windspd")))


## ----dunl random effects forest plot----------------------------------------------
sjPlot::plot_model(final_dunl_model, type = "re")


## ----dunl make temp plot data-----------------------------------------------------
temp_plot <- broom.mixed::augment(final_dunl_model, dat3)
temp_plot <- janitor::clean_names(temp_plot)


## ----dunl temp plot: observed vs predicted----------------------------------------
ggplot(temp_plot, aes(x = log_wesa, y = fitted)) +
  geom_point() + 
  ggtitle("Observed vs. Fitted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----dunl temp plot: heteroskedasticity-------------------------------------------
ggplot(temp_plot, aes(x = fitted, y = resid)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----dunl temp plot: qq plot------------------------------------------------------
ggplot(temp_plot, aes(sample = resid)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----dunl slope coeff-------------------------------------------------------------
sjPlot::plot_model(final_dunl_model, type = "slope") +
  ggtitle("Coefficient slopes vs Response")


## ----dunl vars p1-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggtitle("Variables vs. Residuals") + 
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 1) +
  theme_minimal()


## ----dunl vars p2-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 2) +
  theme_minimal()


## ----dunl vars p3-----------------------------------------------------------------
temp_plot %>%
  dplyr::select(year:resid, -fitted) %>%
  tidyr::gather(-resid, key = "var", value = "value") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(n.breaks = 3) +
  ggforce::facet_wrap_paginate(~ var, ncol = 3, nrow = 3, scales = "free", page = 3) +
  theme_minimal()

