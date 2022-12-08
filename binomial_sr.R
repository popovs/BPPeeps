## ----binomial setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE, 
                      message = FALSE)

library(ggplot2)

# This RMD is NOT intended as a standalone script, but rather to be sourced
# from models.Rmd. The data used in this Rmd file is generated in models.Rmd.


## ----sr summary-------------------------------------------------------------------------------
summary(sr)


## ----sr glmm fitting--------------------------------------------------------------------------
# First, create our response variable: 
# WESA:DUNL proportion from our daily_percent_ratio table
y <- cbind(sr$wesa, sr$dunl)

# And build our 5 models 
# Response variable: ratio of WESA to DUNL
# Predictor variables:
# - dos: day of season (recentered/scaled julian date)
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


## ----best fit sr------------------------------------------------------------------------------
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


## ----summary best fit sr----------------------------------------------------------------------
summary(sr_glmm)


## ----sr plot: observed vs predicted-----------------------------------------------------------
ggplot(sr, aes(x = p_wesa, y = predicted_ratio)) +
  geom_point() + 
  ggtitle("Observed vs. Predicted values") +
  xlab("Observed") + 
  ylab("Predicted") +
  theme_minimal()


## ----sr plot: heteroskedasticity--------------------------------------------------------------
ggplot(sr, aes(x = predicted_ratio, y = resids)) +
  geom_point() + 
  ggtitle("Heteroskedasticity",
          subtitle = "Fitted values vs. Residuals") +
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal()


## ----sr plot: qq plot-------------------------------------------------------------------------
ggplot(sr, aes(sample = resids)) +
  stat_qq() + 
  stat_qq_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggtitle("Quantile-Quantile") +
  xlab("Theoretical") + 
  ylab("Sample") +
  theme_minimal()


## ----sr year effects, include = FALSE---------------------------------------------------------
year_effects <- lme4::ranef(sr_glmm)$year
year_effects <- janitor::clean_names(year_effects)


## ----sr plot: year intercept plot-------------------------------------------------------------
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


## ----sr plot: year slope plot-----------------------------------------------------------------
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


## ----sr plot: year slope quadratic plot-------------------------------------------------------
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


## ----predict wesa ratio, include = FALSE------------------------------------------------------
# Create yrs df, which is a df containing all possible
# julian_day-year combinations
yrs <- expand.grid(julian_day = seq(min(dat$julian_day), max(dat$julian_day), 1),
                   year = as.factor(unique(lubridate::year(dat$survey_date))))
# Add day of season variable
yrs$dos <- (yrs$julian_day - mean(sr$julian_day))/sd(sr$julian_day)

# Extract sr_glmm model coefficients
coefs <- coef(sr_glmm)$year
names(coefs) <- c("intercept", "jd_parm", "jd2_parm")
coefs$year <- unique(yrs$year)

# Merge yrs and coefs together
yrs <- merge(yrs, coefs, by = "year", all.x = TRUE)

# Previously, within Canham paper, the mean coefficients across all
# years were calculated for years without species ratios. However,
# in this case that was not necessary, as there are no missing years
# in the species ratio/count data.

# Now calculate odds & predict ratios for all survey dates
yrs$odds <- yrs$intercept + yrs$jd_parm * yrs$dos + yrs$jd2_parm * yrs$dos^2
yrs$predicted_ratio <- exp(yrs$odds) / (1 + exp(yrs$odds))


## ----plot predicted wesa ratios, fig.height = 8-----------------------------------------------
ggplot() +
  geom_line(data = yrs, aes(x = julian_day, y = predicted_ratio, group = year)) +
  geom_point(data = sr, aes(x = julian_day, y = (p_wesa/100), group = year), color = 'blue') +
  facet_wrap(~ year, ncol = 3)

