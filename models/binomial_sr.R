## ----binomial setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE, 
                      message = FALSE)

library(ggplot2)

# This RMD is NOT intended as a standalone script, but rather to be sourced
# from models.Rmd. The data used in this Rmd file is generated in models.Rmd.


## ----sr summary-----------------------------------------------------------------------
summary(sr)


## ----sr glmm fitting------------------------------------------------------------------
# First, create our response variable: 
# WESA:DUNL proportion from our daily_percent_ratio table
y <- cbind(sr$wesa, sr$dunl)

# And build our 5 models 
# Response variable: ratio of WESA to DUNL
# Predictor variables:
# - dos: day of season (recentered/scaled ordinal date)
# - year: year (as a factor)
lme1 <- lme4::glmer(y ~ dos + I(dos^2) + (dos + I(dos^2)|year),
                    family = binomial, 
                    data = sr)
lme2 <- lme4::glmer(y  ~ dos + I(dos^2) + (1|year), 
                    family = binomial, 
                    data = sr)
lme3 <- lme4::glmer(y ~ dos + (dos|year), 
                    family = binomial, 
                    data = sr)
lme4 <- lme4::glmer(y ~ dos + (1|year), 
                    family = binomial, 
                    data = sr)
lme5 <- lme4::glmer(y ~ 1 + (1|year), 
                    family = binomial, 
                    data = sr)

## Compare AIC values 
anova(lme1, lme2, lme3, lme4, lme5)


## ----best fit sr----------------------------------------------------------------------
sr$resids <- residuals(lme1)

# Add residuals into model to examine coefficients
lme4::glmer(y ~ dos + I(dos^2) + (dos + I(dos^2)|year) + (1|resids),
            family = binomial, 
            data = sr)

sr_glmm <- lme1

# Add predicted values to sr
sr$predicted_ratio <- fitted(sr_glmm)

# Remove all the other lmes
rm(lme1, lme2, lme3, lme4, lme5)


## ----summary best fit sr--------------------------------------------------------------
summary(sr_glmm)


## ----sr plot: observed vs predicted---------------------------------------------------
ggplot(sr, aes(x = p_wesa, y = predicted_ratio)) +
  geom_point() + 
  ggtitle("Observed vs. Predicted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----sr plot: heteroskedasticity------------------------------------------------------
ggplot(sr, aes(x = predicted_ratio, y = resids)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----sr plot: qq plot-----------------------------------------------------------------
ggplot(sr, aes(sample = resids)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----sr year effects, include = FALSE-------------------------------------------------
year_effects <- lme4::ranef(sr_glmm)$year
year_effects <- janitor::clean_names(year_effects)


## ----sr plot: year intercept plot-----------------------------------------------------
ggplot(year_effects, aes(sample = intercept)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Year effects", 
          subtitle = "Intercept") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----sr plot: year slope plot---------------------------------------------------------
ggplot(year_effects, aes(sample = dos)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Year effects", 
          subtitle = "Slope") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----sr plot: year slope quadratic plot-----------------------------------------------
ggplot(year_effects, aes(sample = i_dos_2)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Year effects", 
          subtitle = "Slope quadratic") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----predict wesa ratio, include = FALSE----------------------------------------------
# Create yrs df, which is a df containing all possible
# ordinal_day-year combinations
yrs <- expand.grid(ordinal_day = seq(min(dt$ordinal_day), max(dt$ordinal_day), 1),
                   year = as.factor(unique(lubridate::year(dt$survey_date))))
# Add day of season variable
yrs$dos <- (yrs$ordinal_day - mean(sr$ordinal_day))/sd(sr$ordinal_day)

# Extract sr_glmm model coefficients
coefs <- coef(sr_glmm)$year
names(coefs) <- c("intercept", "dos_param", "dos2_param")
coefs$year <- rownames(coefs)

# Merge yrs and coefs together
yrs <- merge(yrs, coefs, by = "year", all.x = TRUE)

# Use mean coefficient values for any years where coefs NA
yrs[["intercept"]][is.na(yrs$intercept)] <- mean(coefs$intercept)
yrs[["dos_param"]][is.na(yrs$dos_param)] <- mean(coefs$dos_param)
yrs[["dos2_param"]][is.na(yrs$dos2_param)] <- mean(coefs$dos2_param)

# Now calculate odds & predict ratios for all survey dates
yrs$odds <- yrs$intercept + yrs$dos_param * yrs$dos + yrs$dos2_param * yrs$dos^2
yrs$predicted_ratio <- exp(yrs$odds) / (1 + exp(yrs$odds))


## ----add predicted ratios to dat, include=FALSE---------------------------------------
# Add predicted wesa ratio from binomial model to dat
dat <- merge(dat, yrs[,c("year", "ordinal_day", "predicted_ratio")], 
             by = c("year", "ordinal_day"))

# Scale and log-transform variables of interest
dat$predicted_wesa <- round(dat$final_count * dat$predicted_ratio, 0)
dat$predicted_dunl <- dat$final_count - dat$predicted_wesa
dat$log_wesa <- log(dat$predicted_wesa + 1)
dat$log_dunl <- log(dat$predicted_dunl + 1)
dat$year_c <- scale(as.numeric(dat$year))


## ----add predicted ratios to dt, include=FALSE----------------------------------------
# Now merge predicted wesa ratio generated above to the dt dataset
dt <- merge(dt, yrs[,c("year", "ordinal_day", "predicted_ratio")], 
            by = c("year", "ordinal_day"))

# Scale and log-transform variables of interest
dt$predicted_wesa <- round(dt$total_count * dt$predicted_ratio, 0)
dt$predicted_dunl <- dt$total_count - dt$predicted_wesa
dt$log_wesa <- log(dt$predicted_wesa + 1)
dt$log_dunl <- log(dt$predicted_dunl + 1)
dt$year_n <- as.numeric(dt$year)
dt$year_c <- scale(as.numeric(dt$year))


## ----plot predicted wesa ratios, fig.height = 8, fig.cap="Yearly calculated seasonal species ratios. In years where species ratio subsample surveys were not conducted (1991, 1992, 1994, 1995, 1998), the mean species ratio curve across all years was applied."----
ggplot() +
  geom_line(data = yrs, aes(x = ordinal_day, y = predicted_ratio, group = year)) +
  geom_point(data = sr, aes(x = ordinal_day, y = (p_wesa/100), group = year), color = 'blue') +
  facet_wrap(~ year, ncol = 3)


## ----cleanup, include = FALSE---------------------------------------------------------
rm(yrs, coefs, year_effects, y)

