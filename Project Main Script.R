library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(psych)
library(car)
install.packages("corrplot")
library(corrplot)

###############################################################################
# Section 0: Importing and cleaning the datasets
###############################################################################

#Clearing the workspace
rm(list = ls())


Houses_Sold <- read_excel("data/Annual Rate for New Single Family Houses Sold.xlsx", skip = 7)

Houses_Sold <- Houses_Sold %>%
   mutate(
     Date = parse_date_time(Period, orders = "b-Y"),
     Houses_Sold = Value
   ) %>%
   select(Date, Houses_Sold)                           # keep only these two cols

 CCI <- read_excel("data/Composite Consumer Confidence NAA.xlsx", sheet = "Monthly", col_names = TRUE, skip = 0)
 colnames(CCI) <- c("Date", "CCI")

 Fund_Rate <- read_excel("data/Federal Fund Effective Rate Monthly.xlsx",
                                                   sheet = "Monthly", col_types = c("date", "numeric"))
 colnames(Fund_Rate) <- c("Date", "Fund Rate")

 HPI <- read_excel("data/FHFA Monthly Housing Price Index USA.xlsx", range = "A4:U412")
 names(HPI) <- gsub("[\r\n]", "", names(HPI))
 HPI <- HPI %>% select(Month, `USA(NSA)`)
 colnames(HPI) <- c("Date","HPI")

 Unemployment <- read_excel("data/Monthly Unemployment Rate.xlsx",
                            sheet = "Monthly", col_types = c("date",
                                                             "numeric"))
 colnames(Unemployment) <- c("Date","Unemployment")

 Houses_Built <- read_excel("data/New Privately Owned Housing Units Completed Monthly.xlsx",
                            sheet = "Monthly")
 colnames(Houses_Built) <- c("Date", "Houses Built")


 Mortgage <- read_excel("data/Weekly 30 Year US Mortgage Rate.xlsx",
                        sheet = "Weekly, Ending Thursday")
 Mortgage_monthly <- Mortgage %>%
   mutate(Date = floor_date(observation_date, "month")) %>%
   group_by(Date) %>%
   summarize(monthly_mortgage_rate = mean(`MORTGAGE30US`, na.rm = TRUE))


master_df <- Houses_Sold


# Merge all other datasets by Date
master_df <- master_df %>%
  left_join(Fund_Rate, by = "Date") %>%
  left_join(Houses_Built, by = "Date") %>%
  left_join(HPI, by = "Date") %>%
  left_join(Mortgage_monthly, by = "Date") %>%
  left_join(Unemployment, by = "Date") %>%
  left_join(CCI, by = "Date")

names(master_df) <- c("Date","Houses_Sold", "Fund_Rate", "Houses_Built", "HPI", "Mortgage_Monthly", "Unemployment", "CCI")

###############################################################################
# Section I: Exploratory Data Analysis
###############################################################################

#Summary Statistics
summary(master_df)
describe(master_df)

#Histograms for each variable
numeric_vars <- names(master_df)[sapply(master_df, is.numeric)]

for (var in numeric_vars) {
  p <- ggplot(master_df, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    ggtitle(paste("Histogram of", var)) +
    theme_minimal(base_size = 16) +   # increases all text sizes
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text  = element_text(size = 14)
    )
  print(p)
}

#Time series for each variable
for (var in numeric_vars) {
  p <- ggplot(master_df, aes(x = Date, y = !!sym(var))) +
    geom_line(color = "steelblue", linewidth = 1) +
    labs(title = paste("Trend of", var, "over Time"),
         x = "Date",
         y = var) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text  = element_text(size = 14)
    )
  print(p)
}


#Scatterplot Matrix
pairs(master_df[numeric_vars], main = "Scatterplot Matrix")

#Scatterplot for each variable
predictors <- names(master_df)[sapply(master_df, is.numeric) & names(master_df) != "Houses_Sold"]

# Loop to create scatterplots
for (var in predictors) {
  p <- ggplot(master_df, aes(x = .data[[var]], y = Houses_Sold)) +
    geom_point(color = "steelblue", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
    labs(
      title = paste("Scatterplot of Houses_Sold vs", var),
      x = var,
      y = "Houses_Sold"
    ) +
    theme_minimal(base_size = 15)
  
  print(p)
}


#Correlation Matrix
cor_matrix <- cor(master_df[numeric_vars], use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

################################################################################
#Section II: Modeling
################################################################################


############################################################
# 1. Preliminary Checks
############################################################

# Make sure Date is of class Date (not POSIXct)
master_df$Date <- as.Date(master_df$Date)

# Make sure data are ordered in time and create a time index t = 1, 2, ..., T
master_df <- master_df %>%
  arrange(Date) %>%
  mutate(t = 1:n())


############################################################
# 2. Detrend all series using a linear time trend
############################################################

# For each variable, we regress it on the time index t and keep the residuals.
# These residuals are the "detrended" versions (deviations from the linear trend).

master_df <- master_df %>%
  mutate(
    Houses_Sold_dt      = resid(lm(Houses_Sold      ~ t)),
    Fund_Rate_dt        = resid(lm(Fund_Rate        ~ t)),
    Houses_Built_dt     = resid(lm(Houses_Built     ~ t)),
    HPI_dt              = resid(lm(HPI              ~ t)),
    Mortgage_Monthly_dt = resid(lm(Mortgage_Monthly ~ t)),
    Unemployment_dt     = resid(lm(Unemployment     ~ t)),
    CCI_dt              = resid(lm(CCI              ~ t))
  )


############################################################
# 3. Visualize original series vs. linear trend and detrended series
############################################################

# 3.1 Fit an explicit linear trend for Houses_Sold (for plotting)
trend_fit <- lm(Houses_Sold ~ t, data = master_df)
master_df$Houses_Sold_trend <- fitted(trend_fit)

# 3.2 Original series with fitted linear trend
ggplot(master_df, aes(x = Date)) +
  geom_line(aes(y = Houses_Sold)) +
  geom_line(aes(y = Houses_Sold_trend), linetype = "dashed") +
  labs(title = "Houses_Sold with linear time trend",
       x = "Date",
       y = "Number of houses sold") +
  theme_minimal()

# 3.3 Detrended series (residuals from the trend regression)
ggplot(master_df, aes(x = Date, y = Houses_Sold_dt)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Houses_Sold after detrending (deviations from trend)",
       x = "Date",
       y = "Residual relative to the trend") +
  theme_minimal()


############################################################
# 4. Static OLS model using detrended data
############################################################

# OLS regression of detrended housing sales on detrended macro variables
model_det <- lm(
  Houses_Sold_dt ~ Fund_Rate_dt + Houses_Built_dt + HPI_dt +
    Mortgage_Monthly_dt + Unemployment_dt + CCI_dt,
  data = master_df
)

# Summary of coefficients, R-squared, etc.
summary(model_det)

# ---------------------------------------------------------
# Extra regressors for OLS model variations:
#  - Covid_dummy: COVID-19 period dummy
#  - HPI_dt_sq: quadratic term in house price index
#  - Mortgage_CCI_int: interaction between mortgage payment and confidence
# ---------------------------------------------------------

master_df <- master_df %>%
  mutate(
    Covid_dummy      = ifelse(Date >= as.Date("2020-03-01"), 1, 0),
    HPI_dt_sq        = HPI_dt^2,
    Mortgage_CCI_int = Mortgage_Monthly_dt * CCI_dt
  )

# ---------------------------------------------------------
# OLS model with quadratic term in HPI and COVID dummy
# ---------------------------------------------------------

model_quad <- lm(
  Houses_Sold_dt ~ Fund_Rate_dt + Houses_Built_dt + HPI_dt + 
    HPI_dt_sq + Mortgage_Monthly_dt + Unemployment_dt +
    CCI_dt + Covid_dummy,
  data = master_df
)

summary(model_quad)

# ---------------------------------------------------------
# OLS model with interaction (Mortgage × CCI) and COVID dummy
# ---------------------------------------------------------

model_int <- lm(
  Houses_Sold_dt ~ Fund_Rate_dt + Houses_Built_dt + HPI_dt +
    Mortgage_Monthly_dt + CCI_dt + Mortgage_CCI_int +
    Unemployment_dt + Covid_dummy,
  data = master_df
)

summary(model_int)

# ---------------------------------------------------------
# Quick comparison: adjusted R-squared across OLS models
#  - model_det : baseline linear model
#  - model_quad: quadratic + dummy
#  - model_int : interaction + dummy
# ---------------------------------------------------------

adjR2_compare <- c(
  baseline_adjR2 = summary(model_det)$adj.r.squared,
  quad_adjR2     = summary(model_quad)$adj.r.squared,
  int_adjR2      = summary(model_int)$adj.r.squared
)

adjR2_compare

# Standard diagnostic plots for the OLS model (residuals vs fitted, QQ-plot, etc.)
par(mfrow = c(2, 2))
plot(model_det)
par(mfrow = c(1, 1))

# Extract residuals from the detrended OLS model
resid_det <- resid(model_det)

# Autocorrelation and partial autocorrelation of OLS residuals
acf(resid_det, main = "ACF of residuals from detrended OLS model")
pacf(resid_det, main = "PACF of residuals from detrended OLS model")

# Ljung–Box test for residual autocorrelation (up to lag 12)
Box.test(resid_det, lag = 12, type = "Ljung-Box")
# -> Strong rejection here motivates moving to a dynamic (ARIMAX) model.


############################################################
# 5. Build ARIMAX models with detrended series
############################################################

# 5.1 Dependent variable: detrended housing sales
y <- master_df$Houses_Sold_dt

# 5.2 Matrix of detrended exogenous regressors
X <- master_df %>%
  dplyr::select(
    Fund_Rate_dt, Houses_Built_dt, HPI_dt,
    Mortgage_Monthly_dt,Unemployment_dt, CCI_dt
  ) %>%
  as.matrix()

############################################################
# 5A. ARIMAX(1,0,0): AR(1) with detrended exogenous regressors
############################################################

fit_arimax_1 <- arima(
  x    = y,
  order = c(1, 0, 0),   # AR(1), no differencing, no MA part
  xreg = X
)

fit_arimax_1   # prints AR(1) coefficient and beta estimates

# Residuals from ARIMAX(1,0,0)
resid_arimax_1 <- residuals(fit_arimax_1)

# ACF and PACF of ARIMAX(1,0,0) residuals
acf(resid_arimax_1, main = "ACF of residuals from ARIMAX(1,0,0)")
pacf(resid_arimax_1, main = "PACF of residuals from ARIMAX(1,0,0)")

# Ljung–Box test: some autocorrelation is still present
Box.test(resid_arimax_1, lag = 12, type = "Ljung-Box")


############################################################
# 5B. ARIMAX(2,0,0): AR(2) with detrended exogenous regressors
############################################################

fit_arimax_2 <- arima(
  x    = y,
  order = c(2, 0, 0),   # AR(2), no differencing, no MA part
  xreg = X
)

fit_arimax_2   # preferred dynamic model

# Residuals from ARIMAX(2,0,0)
resid_arimax_2 <- residuals(fit_arimax_2)

# Residual diagnostics for ARIMAX(2,0,0)
acf(resid_arimax_2, main = "ACF of residuals from ARIMAX(2,0,0)")
pacf(resid_arimax_2, main = "PACF of residuals from ARIMAX(2,0,0)")

# Ljung–Box test: now we do NOT reject no autocorrelation
Box.test(resid_arimax_2, lag = 12, type = "Ljung-Box")

# Compare information criteria between ARIMAX(1,0,0) and ARIMAX(2,0,0)
fit_arimax_1$aic
fit_arimax_2$aic
# -> Lower AIC and better residual diagnostics justify choosing ARIMAX(2,0,0)

# Extract the estimated coefficients from the ARIMAX(2,0,0) model
beta_hat <- coef(fit_arimax_2)

# Variance–covariance matrix of the estimated coefficients
V <- fit_arimax_2$var.coef

# Compute standard errors as the square root of the diagonal elements of V
se <- sqrt(diag(V))

# Calculate (approximate) t-statistics for each coefficient
t_stat <- beta_hat / se

# Compute two-sided p-values using the standard normal approximation
p_val <- 2 * (1 - pnorm(abs(t_stat)))

# Build a tidy summary table with coefficient estimates and test statistics
arimax2_table <- data.frame(
  Variable   = names(beta_hat),
  Estimate   = round(beta_hat, 3),
  Std_Error  = round(se, 3),
  t_value    = round(t_stat, 2),
  p_value    = signif(p_val, 3)
)

arimax2_table
