# ============================================================
# Step 5: Run Cohort Valuations Across Portfolio Ages
# ============================================================

library(tidyverse)
library(readxl)

# 1. Load and format the EIOPA RFR curve
library(readxl)
rfr_raw <- read_excel("RFR_spot_with_VA.xlsx", sheet = 1, skip = 10, col_names = FALSE)

rfr <- data.frame(
  T = as.integer(rfr_raw[[1]]),
  i = as.numeric(rfr_raw[[2]])
)

# Safe format conversion
if (max(rfr$i, na.rm = TRUE) > 1) rfr$i <- rfr$i / 100
rfr <- rbind(data.frame(T = 0, i = rfr$i[1]), rfr)

cat("RFR loaded. T=1 rate:", rfr$i[2], "\n")

# Define forecast years & future mask (required for stress shocks)
forecast_years <- as.numeric(colnames(mhat_forecast_full))
future_mask <- forecast_years > 2024

# 2. Define portfolio ages
portfolio_ages <- c(30, 40, 50, 60, 70, 80, 90)

# 3. Create results data frame
cohort_results <- data.frame(
  Age = portfolio_ages,
  BirthYear = 2026 - portfolio_ages,
  Cohort_LE = NA_real_,
  EPV_10000 = NA_real_
)

# 4. Loop through each age to calculate LE and EPV
for (i in seq_along(portfolio_ages)) {
  age <- portfolio_ages[i]
  birth_year <- 2026 - age
  
  # Calculate cohort life expectancy
  le <- LE_cohort(mhat_forecast_full, Age = age, BirthYear = birth_year, max_age = 110)
  cohort_results$Cohort_LE[i] <- le
  
  # Calculate EPV of whole life annuity (€10,000 annual payment)
  epv <- LA_cohort(mhat_forecast_full, Age = age, BirthYear = birth_year, 
                   amount = 10000, rfr = rfr, max_age = 110)
  cohort_results$EPV_10000[i] <- epv
  
  cat(sprintf("Age %2d (born %4d): LE = %5.2f years, EPV = €%10.2f\n", 
              age, birth_year, le, epv))
}

# 5. View results
print(cohort_results)

# 6. Create visualizations (CRO prefers graphs!)

# Plot 1: Cohort Life Expectancy by Age
p1 <- ggplot(cohort_results, aes(x = Age, y = Cohort_LE)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = sprintf("%.1f", Cohort_LE)), vjust = -0.5, size = 3.5) +
  labs(title = "Cohort Life Expectancy by Current Age",
       subtitle = "Based on Lee-Carter forecasts (2026 valuation)",
       x = "Current Age",
       y = "Life Expectancy (years)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

# Plot 2: EPV of Whole Life Annuity by Age
p2 <- ggplot(cohort_results, aes(x = Age, y = EPV_10000 / 1000)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(aes(label = sprintf("%.1f", EPV_10000 / 1000)), vjust = -0.5, size = 3.5) +
  labs(title = "EPV of Whole Life Annuity (€10,000 annual payment)",
       subtitle = "Present value in thousands of EUR",
       x = "Current Age",
       y = "EPV (€ thousands)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

# Plot 3: Combined view with dual axes
cohort_long <- cohort_results %>%
  pivot_longer(cols = c(Cohort_LE, EPV_10000), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(Metric = case_when(
    Metric == "Cohort_LE" ~ "Life Expectancy (years)",
    Metric == "EPV_10000" ~ "EPV (€ thousands)",
    TRUE ~ Metric
  ),
  Value = ifelse(Metric == "EPV (€ thousands)", Value / 1000, Value))

p3 <- ggplot(cohort_long, aes(x = Age, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Cohort Valuations Across Portfolio Ages",
       subtitle = "Life expectancy and annuity EPV by age",
       x = "Current Age",
       y = "Value",
       color = "Metric") +
  scale_color_manual(values = c("Life Expectancy (years)" = "steelblue",
                                "EPV (€ thousands)" = "darkgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Display plots
print(p1)
print(p2)
print(p3)

# 7. Save results to CSV for reporting
write_csv(cohort_results, "cohort_valuations_portfolio_ages.csv")

# Save plots
ggsave("cohort_LE_by_age.png", p1, width = 8, height = 6, dpi = 300)
ggsave("cohort_EPV_by_age.png", p2, width = 8, height = 6, dpi = 300)
ggsave("cohort_combined_valuations.png", p3, width = 10, height = 6, dpi = 300)

# 8. Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat(sprintf("Average cohort LE across ages: %.2f years\n", mean(cohort_results$Cohort_LE)))
cat(sprintf("Average EPV across ages: €%.2f\n", mean(cohort_results$EPV_10000)))
cat(sprintf("Age with highest LE: %d years (%.2f years)\n", 
            cohort_results$Age[which.max(cohort_results$Cohort_LE)],
            max(cohort_results$Cohort_LE)))
cat(sprintf("Age with highest EPV: %d years (€%.2f)\n", 
            cohort_results$Age[which.max(cohort_results$EPV_10000)],
            max(cohort_results$EPV_10000)))

# ============================================================
# FIXED: Safe LA_cohort + Stress Scenario Valuations
# ============================================================

# 1. Patched LA_cohort (Fixes the max(years) NA error)
LA_cohort <- function(mhat, Age, BirthYear, amount, rfr, max_age = 110) {
  ages <- as.numeric(rownames(mhat))
  years <- as.numeric(colnames(mhat))
  start_year <- BirthYear + Age
  end_year <- BirthYear + max_age
  cohort_years <- start_year:end_year
  cohort_ages <- Age:max_age
  mu <- numeric(length(cohort_ages))
  
  # SAFER FALLBACK: Use last column name directly instead of max(years)
  last_col_name <- tail(colnames(mhat), 1)
  
  for (i in seq_along(cohort_ages)) {
    yr <- as.character(cohort_years[i])
    ag <- as.character(cohort_ages[i])
    if (yr %in% colnames(mhat) && ag %in% rownames(mhat)) {
      mu[i] <- mhat[ag, yr]
    } else {
      mu[i] <- mhat[ag, last_col_name]
    }
  }
  
  surv <- c(1, cumprod(exp(-mu)))
  n <- length(cohort_ages) - 1
  v <- c(1, rep(NA, n))
  for (T in 1:n) {
    idx <- which(rfr$T == T)
    if (length(idx) == 0) idx <- which.min(abs(rfr$T - T))
    i_T <- rfr$i[idx[1]]
    v[T+1] <- 1 / (1 + i_T)^T
  }
  return(amount * sum(surv[1:(n+1)] * v))
}

# 2. Define forecast mask explicitly
forecast_years <- as.numeric(colnames(mhat_forecast_full))
future_mask <- forecast_years > 2024

# 3. Create shocked matrices (preserves column names safely)
mhat_long <- mhat_forecast_full
mhat_long[, as.character(forecast_years[future_mask])] <- 
  mhat_long[, as.character(forecast_years[future_mask])] * 0.8

mhat_mort <- mhat_forecast_full
mhat_mort[, as.character(forecast_years[future_mask])] <- 
  mhat_mort[, as.character(forecast_years[future_mask])] * 1.15

# 4. Run valuations
age_val <- 65
birth_year_val <- 2026 - age_val

base_epv_age <- LA_cohort(mhat_forecast_full, Age = age_val, BirthYear = birth_year_val, 
                          amount = 10000, rfr = rfr, max_age = 110)
long_epv_age <- LA_cohort(mhat_long, Age = age_val, BirthYear = birth_year_val, 
                          amount = 10000, rfr = rfr, max_age = 110)
mort_epv_age <- LA_cohort(mhat_mort, Age = age_val, BirthYear = birth_year_val, 
                          amount = 10000, rfr = rfr, max_age = 110)

# 5. Create data frame & plot
stress_summary <- data.frame(
  Scenario = c("Base", "Longevity Shock", "Mortality Shock"),
  EPV_EUR = c(base_epv_age, long_epv_age, mort_epv_age),
  Change_Pct = c(0, 
                 (long_epv_age - base_epv_age) / base_epv_age * 100,
                 (mort_epv_age - base_epv_age) / base_epv_age * 100)
)

p_stress_impact <- ggplot(stress_summary, aes(x = Scenario, y = EPV_EUR / 1000, fill = Scenario)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("€%.0fK\n(%+.1f%%)", EPV_EUR / 1000, Change_Pct)), 
            vjust = -2, size = 3.8, fontface = "bold") +
  expand_limits(y = 275) +
  scale_fill_manual(values = c("Base" = "steelblue", "Longevity Shock" = "darkgreen", "Mortality Shock" = "darkred")) +
  labs(title = sprintf("Impact of Stress Scenarios on Annuity EPV (Age %d)", age_val),
       subtitle = "€10,000 annual payment, retirement age 65",
       x = "Scenario", y = "EPV (Thousands EUR)", fill = "Scenario") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        panel.grid.minor = element_blank(), legend.position = "none")

print(p_stress_impact)
ggsave("Phase2_Stress_Impact_BarChart.png", p_stress_impact, width = 8, height = 6, dpi = 300)

# ============================================================
# Re-run Phase 2 Stress Scenarios with Corrected RFR
# ============================================================
# Base valuation
base_EPV <- LA_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961,
                      amount = 10000, rfr = rfr)

# Longevity shock (mortality × 0.8)
mhat_long <- mhat_forecast_full
mhat_long[, as.character(forecast_years[future_mask])] <- 
  mhat_long[, as.character(forecast_years[future_mask])] * 0.8
long_EPV <- LA_cohort(mhat_long, Age = 65, BirthYear = 1961,
                      amount = 10000, rfr = rfr)

# Mortality shock (mortality × 1.15)
mhat_mort <- mhat_forecast_full
mhat_mort[, as.character(forecast_years[future_mask])] <- 
  mhat_mort[, as.character(forecast_years[future_mask])] * 1.15
mort_EPV <- LA_cohort(mhat_mort, Age = 65, BirthYear = 1961,
                      amount = 10000, rfr = rfr)

# IR +50bps
rfr_plus50 <- rfr
rfr_plus50$i <- pmin(rfr$i + 0.0050, 0.10)
ir_plus_EPV <- LA_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961,
                         amount = 10000, rfr = rfr_plus50)

# IR -50bps
rfr_minus50 <- rfr
rfr_minus50$i <- pmax(rfr$i - 0.0050, 0.0001)
ir_minus_EPV <- LA_cohort(mhat_forecast_full, Age = 65, BirthYear = 1961,
                          amount = 10000, rfr = rfr_minus50)

# Print summary table for report
cat("\n=== UPDATED STRESS SCENARIO IMPACTS (Age 65, €10k annuity) ===\n")
stress_table <- data.frame(
  Scenario = c("Base", "Longevity Shock", "Mortality Shock", "IR +50bps", "IR -50bps"),
  EPV_EUR = c(base_EPV, long_EPV, mort_EPV, ir_plus_EPV, ir_minus_EPV),
  Change_Pct = c(0, 
                 (long_EPV - base_EPV) / base_EPV * 100,
                 (mort_EPV - base_EPV) / base_EPV * 100,
                 (ir_plus_EPV - base_EPV) / base_EPV * 100,
                 (ir_minus_EPV - base_EPV) / base_EPV * 100)
)
print(stress_table, row.names = FALSE)

# Save for LaTeX table insertion
write_csv(stress_table, "Phase2_Stress_Results_Updated.csv")