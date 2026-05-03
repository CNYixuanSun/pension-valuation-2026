# ============================================================
# Phase 1 & 2: Prepare data, fit Lee-Carter, forecast, combine
# ============================================================

library(tidyverse)
library(StMoMo)

# ---------------------------
# 1. Read and clean Mx (mortality rates) to create mhat_clean
# ---------------------------
mx_raw <- read.table("Mx_LUX_1x1.txt", header = TRUE, na.strings = ".")

mx_clean <- mx_raw %>%
  filter(!grepl("\\+", Age)) %>%
  mutate(Age = as.integer(Age)) %>%
  select(Year, Age, mx = Total) %>%
  filter(!is.na(mx), Year >= 1980)

# Reshape to wide (years as rows, ages as columns)
mx_wide <- mx_clean %>%
  pivot_wider(id_cols = Year, names_from = Age, values_from = mx, names_sort = TRUE) %>%
  column_to_rownames(var = "Year") %>%
  as.matrix()

# Check maximum age (should be 109 or 110)
max_obs_age <- max(as.numeric(colnames(mx_wide)))
cat("Maximum observed age:", max_obs_age, "\n")

# Define closing function (Kannisto, threshold 80) - same as earlier
close_mortality <- function(mx, max_age = 110) {
  ages_all <- as.numeric(names(mx))
  max_obs <- max(ages_all)
  if (max_obs >= max_age) return(mx)
  threshold <- 80
  idx_fit <- ages_all >= threshold
  ages_fit <- ages_all[idx_fit]
  mx_fit <- mx[idx_fit]
  mx_fit <- pmax(pmin(mx_fit, 1 - 1e-10), 1e-10)
  logit_fit <- log(mx_fit / (1 - mx_fit))
  fit <- lm(logit_fit ~ ages_fit)
  ages_new <- seq(max_obs + 1, max_age)
  logit_pred <- predict(fit, newdata = data.frame(ages_fit = ages_new))
  mx_pred <- exp(logit_pred) / (1 + exp(logit_pred))
  names(mx_pred) <- ages_new
  combined <- c(mx, mx_pred)
  combined[order(as.numeric(names(combined)))]
}

# Clean function to cap and ensure monotonicity
clean_mortality <- function(mx, max_rate = 0.95) {
  mx[is.na(mx)] <- 1e-6
  mx <- pmin(mx, max_rate)
  ages <- as.numeric(names(mx))
  for (i in 2:length(mx)) {
    age <- ages[i]
    if (!is.na(age) && age >= 40 && !is.na(mx[i]) && !is.na(mx[i-1])) {
      if (mx[i] < mx[i-1]) mx[i] <- mx[i-1] * 1.01
    }
  }
  pmin(mx, max_rate)
}

# Apply closing and cleaning to each year (row) of mx_wide
ultimate_age <- 110
closed_rows <- list()
for (yr in rownames(mx_wide)) {
  mx_year <- mx_wide[yr, ]
  names(mx_year) <- colnames(mx_wide)
  mx_year <- mx_year[!is.na(mx_year)]
  closed_rows[[yr]] <- close_mortality(mx_year, max_age = ultimate_age)
}

# Convert to matrix (years as rows, ages as columns)
all_ages <- sort(unique(unlist(lapply(closed_rows, names))))
mhat_closed <- matrix(NA, nrow = length(closed_rows), ncol = length(all_ages))
rownames(mhat_closed) <- names(closed_rows)
colnames(mhat_closed) <- all_ages
for (yr in names(closed_rows)) {
  mhat_closed[yr, names(closed_rows[[yr]])] <- closed_rows[[yr]]
}
mhat_closed <- mhat_closed[, as.character(sort(as.numeric(colnames(mhat_closed))))]

# Clean
cleaned_rows <- list()
for (yr in rownames(mhat_closed)) {
  cleaned_rows[[yr]] <- clean_mortality(mhat_closed[yr, ], max_rate = 0.95)
}
mhat_clean <- do.call(rbind, cleaned_rows)
colnames(mhat_clean) <- names(cleaned_rows[[1]])
rownames(mhat_clean) <- rownames(mhat_closed)

# Ensure we only keep ages 0:110 and years 1980:2024
ages_keep <- as.character(0:110)
years_keep <- as.character(1980:2024)
mhat_clean <- mhat_clean[years_keep, ages_keep]
dim(mhat_clean)  # 45 years, 111 ages

# ---------------------------
# 2. Read Deaths and Exposures, subset to ages 0-79, years >=1980
# ---------------------------
deaths_raw <- read.table("Deaths_LUX_1x1.txt", header = TRUE, na.strings = ".")
deaths_clean <- deaths_raw %>%
  filter(!grepl("\\+", Age)) %>%
  mutate(Age = as.integer(Age)) %>%
  select(Year, Age, Deaths = Total) %>%
  filter(!is.na(Deaths), Year >= 1980)

exposures_raw <- read.table("Exposures_LUX_1x1.txt", header = TRUE, na.strings = ".")
exposures_clean <- exposures_raw %>%
  filter(!grepl("\\+", Age)) %>%
  mutate(Age = as.integer(Age)) %>%
  select(Year, Age, Exposure = Total) %>%
  filter(!is.na(Exposure), Year >= 1980)

# Wide format
deaths_wide <- deaths_clean %>%
  pivot_wider(id_cols = Year, names_from = Age, values_from = Deaths, names_sort = TRUE) %>%
  column_to_rownames(var = "Year") %>%
  as.matrix()

exposures_wide <- exposures_clean %>%
  pivot_wider(id_cols = Year, names_from = Age, values_from = Exposure, names_sort = TRUE) %>%
  column_to_rownames(var = "Year") %>%
  as.matrix()

# Transpose: ages rows, years columns
Dxt <- t(deaths_wide)
Ext <- t(exposures_wide)
Ext[Ext == 0] <- 1e-6

# Subset ages 0-79
max_age_fit <- 79
age_indices <- which(as.numeric(rownames(Dxt)) <= max_age_fit)
Dxt_sub <- Dxt[age_indices, ]
Ext_sub <- Ext[age_indices, ]

# ---------------------------
# 3. Fit Lee-Carter on ages 0-79
# ---------------------------
LC_fit_sub <- fit(lc(), Dxt = Dxt_sub, Ext = Ext_sub,
                  ages = as.numeric(rownames(Dxt_sub)),
                  years = as.numeric(colnames(Dxt_sub)))

# ---------------------------
# 4. Forecast kt and mortality rates for ages 0-79
# ---------------------------
kt_obs <- as.numeric(LC_fit_sub$kt)
years_obs <- LC_fit_sub$years
drift <- mean(diff(kt_obs))
last_kt <- tail(kt_obs, 1)
h <- 50
future_years <- max(years_obs) + 1:h
kt_forecast_manual <- last_kt + drift * (1:h)

ax <- LC_fit_sub$ax
bx <- LC_fit_sub$bx

forecast_rates_0_79 <- matrix(NA, nrow = length(ax), ncol = h)
rownames(forecast_rates_0_79) <- rownames(LC_fit_sub$Dxt)
colnames(forecast_rates_0_79) <- future_years
for (j in 1:h) {
  forecast_rates_0_79[, j] <- exp(ax + bx * kt_forecast_manual[j])
}

# ---------------------------
# 5. Old-age rates (80-110) from last historical year (static)
# ---------------------------
last_hist_year <- max(years_obs)   # e.g., 2024
mx_old <- mhat_clean[as.character(last_hist_year), as.character(80:110)]

old_age_matrix <- matrix(rep(mx_old, times = h), 
                         nrow = length(80:110), 
                         ncol = h,
                         byrow = FALSE)
rownames(old_age_matrix) <- 80:110
colnames(old_age_matrix) <- future_years

# ---------------------------
# 6. Combine historical and forecast for all ages 0-110
# ---------------------------
# Historical (observed) rates for ages 0-79
hist_rates_0_79 <- LC_fit_sub$Dxt / LC_fit_sub$Ext
colnames(hist_rates_0_79) <- as.character(years_obs)

rates_0_79_full <- cbind(hist_rates_0_79, forecast_rates_0_79)

# Historical old-age rates from mhat_clean (for years 1980-2024)
old_historical <- t(mhat_clean[as.character(years_obs), as.character(80:110)])
colnames(old_historical) <- years_obs

old_rates_full <- cbind(old_historical, old_age_matrix)

# Combine by rows (ages)
mhat_forecast_full <- rbind(rates_0_79_full, old_rates_full)

# Final matrix: ages 0-110 as rows, years 1980-2074 as columns
dim(mhat_forecast_full)   # should be 111 ages × 95 years

# Optional: check that all values are within [0,1]
summary(c(mhat_forecast_full))


# Example
cohort_LE <- LE_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961)
cat("Cohort life expectancy at age 65 (born 1961):", round(cohort_LE, 2), "years\n")

# For annuity EPV, we need the EIOPA risk-free curve. 
# If you have a file, load it. Otherwise, create a dummy curve for testing:
rfr_test <- data.frame(T = 0:100, i = rep(0.02, 101))   # flat 2%
# Replace with your actual EIOPA curve when available.
cohort_EPV <- LA_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961, amount = 10000, rfr = rfr_test)
cat("EPV of whole life annuity (€10,000):", round(cohort_EPV, 2), "EUR\n")

library(readxl)

rfr_raw <- read_excel("RFR_spot_with_VA.xlsx", sheet = 1, skip = 2) # Adjust skip if needed
# Pick your "Country XYZ" column (e.g., Euro area, Germany, or Luxembourg proxy)
country_col <- "EUR_31_03_2026_SWP_LLP_20_EXT_40_UFR_3.30" 
rfr <- data.frame(
  T = 1:150,
  i = as.numeric(rfr_raw[[country_col]][1:150]) # Spot rates for years 1-150
)
rfr <- rbind(data.frame(T=0, i=rfr$i[1]), rfr) # Add T=0 if needed

# Identify forecast years
forecast_years <- as.numeric(colnames(mhat_forecast_full))
future_mask <- forecast_years > 2024

# Base valuation
base_EPV <- LA_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961, amount = 10000, rfr = rfr)

# Longevity shock (mortality × 0.8)
mhat_long <- mhat_forecast_full
mhat_long[, future_mask] <- mhat_long[, future_mask] * 0.8
long_EPV <- LA_cohort(mhat_long, Age = 65, BirthYear = 1961, amount = 10000, rfr = rfr)

# Mortality shock (mortality × 1.15)
mhat_mort <- mhat_forecast_full
mhat_mort[, future_mask] <- mhat_mort[, future_mask] * 1.15
mort_EPV <- LA_cohort(mhat_mort, Age = 65, BirthYear = 1961, amount = 10000, rfr = rfr)

cat("Base EPV:", round(base_EPV, 2), "\n")
cat("Longevity Shock EPV:", round(long_EPV, 2), "\n")
cat("Mortality Shock EPV:", round(mort_EPV, 2), "\n")

# ============================================================
# Generate Historical Mortality Heatmap
# ============================================================
library(ggplot2)
library(reshape2)

# Convert mhat_clean to long format for ggplot
mhat_long <- as.data.frame(mhat_clean) %>%
  mutate(Year = rownames(mhat_clean)) %>%
  pivot_longer(cols = -Year, names_to = "Age", values_to = "Mortality_Rate") %>%
  mutate(Year = as.integer(Year), Age = as.integer(Age))

# Create heatmap
p_hist_heatmap <- ggplot(mhat_long, aes(x = Year, y = Age, fill = Mortality_Rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue", trans = "log",
                      name = "Mortality Rate\n(log scale)") +
  labs(title = "Historical Mortality Rates in Luxembourg (1980-2024)",
       subtitle = "Both sexes, ages 0-110",
       x = "Calendar Year", y = "Age") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        panel.grid = element_blank())

print(p_hist_heatmap)
ggsave("Phase1_Historical_Mortality_Heatmap.png", p_hist_heatmap, 
       width = 10, height = 7, dpi = 300)

# ============================================================
# Generate Comprehensive Lee-Carter Parameter Plot (All 3 Parameters)
# ============================================================
library(ggplot2)
library(patchwork)
library(dplyr)

# Extract parameters safely from StMoMo fit object
ax_vec <- as.numeric(LC_fit_sub$ax)
bx_vec <- as.numeric(LC_fit_sub$bx)
kt_obs <- as.numeric(LC_fit_sub$kt)
years_obs <- LC_fit_sub$years

# Explicit age vector (0 to 79, matching your LC calibration)
ages_fit <- as.numeric(rownames(LC_fit_sub$Dxt))

# ============================================================
# Panel A: α_x (Age-Specific Intercept)
# ============================================================
p_alpha <- ggplot(data.frame(Age = ages_fit, Alpha = ax_vec), 
                  aes(x = Age, y = Alpha)) +
  geom_line(color = "#2E7D32", linewidth = 1.2) +
  geom_point(color = "#2E7D32", size = 1.5, alpha = 0.6) +
  labs(title = expression("Panel A: Age-Specific Intercept (" * alpha[x] * ")"),
       subtitle = "Average log-mortality by age",
       x = "Age", 
       y = expression(alpha[x])) +  # Use expression() for consistency
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
        panel.grid.minor = element_blank())

# ============================================================
# Panel B: β_x (Age-Specific Sensitivity)
# ============================================================
p_beta <- ggplot(data.frame(Age = ages_fit, Beta = bx_vec), 
                 aes(x = Age, y = Beta)) +
  geom_line(color = "#1565C0", linewidth = 1.2) +
  geom_point(color = "#1565C0", size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = expression("Panel B: Age-Specific Sensitivity (" * beta[x] * ")"),
       subtitle = "Sensitivity to period mortality improvements",
       x = "Age", 
       y = expression(beta[x])) +  # Use expression() for consistency
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
        panel.grid.minor = element_blank())

# ============================================================
# Panel C: κ_t (Time Index - Historical + Forecast)
# ============================================================
drift <- mean(diff(kt_obs))
last_kt <- kt_obs[length(kt_obs)]
h <- 50
future_years <- max(years_obs) + 1:h
kt_forecast <- last_kt + drift * seq_len(h)

kt_hist_df <- data.frame(Year = years_obs, Kt = kt_obs, Period = "Historical")
kt_forecast_df <- data.frame(Year = future_years, Kt = kt_forecast, Period = "Forecast")
kt_combined <- rbind(kt_hist_df, kt_forecast_df)

p_kt <- ggplot(kt_combined, aes(x = Year, y = Kt, color = Period)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = max(years_obs) + 0.5, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  scale_color_manual(values = c("Historical" = "#2E7D32", "Forecast" = "#C62828")) +
  labs(
    # FIX: Use expression() to ensure Kappa-t renders correctly
    title = expression("Panel C: Time Index (" * kappa[t] * ")"),
    subtitle = "Historical estimates (1980-2024) + Deterministic forecast (2025-2074)",
    x = "Calendar Year",
    y = expression(kappa[t]),  # FIX: Ensure y-axis label also uses expression()
    color = "Period"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# ============================================================
# Combine & Export
# ============================================================
p_combined <- (p_alpha | p_beta) / p_kt +
  plot_annotation(
    title = "Lee-Carter Model Parameters for Luxembourg (1980-2024)",
    subtitle = "Ages 0-79 calibration | StMoMo package",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                  plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"))
  )

print(p_combined)
ggsave("Phase1_LC_Kt_Series.png", p_combined, width = 12, height = 10, dpi = 300, bg = "white")

# Export summary for appendix
lc_params_summary <- data.frame(
  Parameter = c("αₓ (intercept)", "βₓ (sensitivity)", "κₜ drift"),
  Min = c(min(ax_vec), min(bx_vec), min(kt_obs)),
  Max = c(max(ax_vec), max(bx_vec), max(kt_obs)),
  Mean = c(mean(ax_vec), mean(bx_vec), mean(kt_obs)),
  Age_Year_Range = c("0-79", "0-79", "1980-2024")
)
write.csv(lc_params_summary, "Phase1_LC_Parameters_Summary.csv", row.names = FALSE)