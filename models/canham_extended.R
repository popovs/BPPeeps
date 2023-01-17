## ----canham extended setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE, 
                      message = FALSE)

# This RMD is NOT intended as a standalone script, but rather to be sourced
# from models.Rmd. The data used in this Rmd file is generated in models.Rmd
# and binomial_sr.Rmd.


## ----wesa ~ base model------------------------------------------------------------------------
# First, add predicted wesa ratio generated above to the dat dataset
dat <- merge(dat, yrs[,c("year", "julian_day", "predicted_ratio")], 
             by = c("year", "julian_day"))

# Scale and log-transform variables of interest
dat$predicted_wesa <- round(dat$final_count * dat$predicted_ratio, 0)
dat$predicted_dunl <- dat$final_count - dat$predicted_wesa
dat$log_wesa <- log(dat$predicted_wesa + 1)
dat$log_dunl <- log(dat$predicted_dunl + 1)
dat$year_c <- scale(as.numeric(dat$year))

# Base model
# Response variable: log-transformed predicted WESA count (log_wesa)
# Predictor variables:
#   - Scale-transformed survey year (year_c)
#   - Scale-transformed Julian date, aka Day of Season (dos)
wesa_base_mod <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + (dos + I(dos^2) | year),
                            data = dat,
                            REML = TRUE)

summary(wesa_base_mod)


## ----reduce dat to complete cases-------------------------------------------------------------
# Reduce dataset to our variables of interest, and only keep 
# complete cases
dat2 <- dat[,c("predicted_wesa", "log_wesa", "year", "year_c", "dos", "elev_range", "total_precip", "mean_temp", "flow", "u", "v", "n_s")]
dat2 <- dat2[complete.cases(dat2),]


## ----wesa ~ canham + n_s model, warning=FALSE-------------------------------------------------
wesa_full_mod <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + scale(elev_range) + scale(total_precip) + scale(mean_temp) + scale(flow) + scale(u) + scale(v) + n_s + (dos + I(dos^2) | year) + (scale(flow) | n_s),
                            data = dat2,
                            REML = TRUE)
summary(wesa_full_mod)


## ----selection--------------------------------------------------------------------------------
lmerTest::step(wesa_full_mod, 
               direction = "backward", 
               reduce.random = F,
               keep=c("year_c", "dos", "I(dos^2)"))


## ----summary n_s best fit, warning=FALSE------------------------------------------------------
wesa_best_fit <- lmerTest::lmer(log_wesa ~ year_c + dos + I(dos^2) + scale(elev_range) + scale(v) + (dos + I(dos^2) | year) + (scale(flow) | n_s),
                                data = dat2,
                                REML = TRUE)

summary(wesa_best_fit)


## ----add predicted n-s values to dat2, include=FALSE------------------------------------------
# Add predicted values to dat2
dat2$predicted_log_wesa <- fitted(wesa_best_fit)
dat2$resids <- resid(wesa_best_fit)


## ----wesa n-s plot: observed vs predicted-----------------------------------------------------
ggplot(dat2, aes(x = predicted_log_wesa, y = log_wesa)) +
  geom_point() + 
  ggtitle("Observed vs. Predicted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----wesa n-s plot: heteroskedasticity--------------------------------------------------------
ggplot(dat2, aes(x = predicted_log_wesa, y = resids)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----wesa n-s plot: qq plot-------------------------------------------------------------------
ggplot(dat2, aes(sample = resids)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()

