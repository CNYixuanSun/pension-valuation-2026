# ============================================================
# Phase 3 - Part 2: Build Full Mortality Matrices per Scenario
# ============================================================
library(tidyverse)

# 1. Load/Verify required objects from previous phases
# kt_sim already be in my environment from Part 1
# If not: kt_sim <- readRDS("kt_sim_100_scenarios.rds")

ax_vec <- as.numeric(LC_fit_sub$ax)  # Ages 0-79
bx_vec <- as.numeric(LC_fit_sub$bx)  # Ages 0-79
years_obs <- LC_fit_sub$years
h <- 50
future_years <- as.character(max(years_obs) + 1:h)  # "2025" to "2074"

# Historical observed rates for ages 0-79
hist_rates_0_79 <- LC_fit_sub$Dxt / LC_fit_sub$Ext
colnames(hist_rates_0_79) <- as.character(years_obs)

# Static old-age rates from last historical year (2024)
last_year <- as.character(max(years_obs))
old_age_rates_2024 <- as.numeric(mhat_clean[last_year, as.character(80:110)])

# Historical old-age rates (1980-2024)
old_hist_rates <- t(mhat_clean[as.character(years_obs), as.character(80:110)])

# 2. Generate 100 full scenario matrices
n_scenarios <- nrow(kt_sim)
mhat_scenarios <- vector("list", n_scenarios)

for (s in 1:n_scenarios) {
  # Forecast mx for ages 0-79: exp(ax + bx * kt)
  # Outer product efficiently computes ax + bx*kt for all ages × years
  mx_forecast_0_79 <- exp(ax_vec + bx_vec %o% kt_sim[s, ])
  colnames(mx_forecast_0_79) <- future_years
  rownames(mx_forecast_0_79) <- as.character(0:79)
  
  # Combine historical + forecast for ages 0-79
  rates_0_79 <- cbind(hist_rates_0_79, mx_forecast_0_79)
  
  # Static old-age rates for future years (80-110)
  old_age_forecast <- matrix(rep(old_age_rates_2024, h),
                             nrow = 31, ncol = h, byrow = FALSE)
  colnames(old_age_forecast) <- future_years
  rownames(old_age_forecast) <- as.character(80:110)
  
  # Combine historical + forecast for ages 80-110
  old_rates <- cbind(old_hist_rates, old_age_forecast)
  
  # Full matrix: ages 0-110 (rows) × years 1980-2074 (cols)
  mhat_scenarios[[s]] <- rbind(rates_0_79, old_rates)
}

# 3. Quick Validation & Visualization (Age 65 projection)
cat("cenario 1 dimensions:", dim(mhat_scenarios[[1]]), "\n")
cat("Age range:", head(rownames(mhat_scenarios[[1]])), "...", tail(rownames(mhat_scenarios[[1]])), "\n")
cat("Year range:", head(colnames(mhat_scenarios[[1]])), "...", tail(colnames(mhat_scenarios[[1]])), "\n")

# Compare deterministic base vs stochastic scenarios at age 65
age_65_df <- data.frame(Year = future_years, Base = mhat_forecast_full["65", future_years])
for(s in 1:n_scenarios) {
  age_65_df[[paste0("S", s)]] <- mhat_scenarios[[s]]["65", future_years]
}

# 1. Clean & validate the long-format data
age_65_long <- age_65_long %>%
  mutate(Year = as.numeric(Year))  # Convert to numeric for proper line connection

# Quick sanity check to ensure base forecast wasn't accidentally filtered out
base_data <- filter(age_65_long, Type == "Deterministic Base")
if (nrow(base_data) == 0) {
  stop("Base forecast data is empty! Check that 'Base' column exists in age_65_df.")
}

# 2. Plot with explicit group=1 for the deterministic line
p_scen_val <- ggplot() +
  # Stochastic fan
  geom_line(data = filter(age_65_long, Type == "Stochastic Scenario"),
            aes(x = Year, y = Rate, group = Scenario),
            color = "blue", alpha = 0.25, linewidth = 0.3) +
  # Deterministic base (group=1 forces ggplot to connect all points)
  geom_line(data = base_data,
            aes(x = Year, y = Rate, group = 1),
            color = "red", linewidth = 1.2) +
  labs(title = "Phase 3 Part 2: Mortality Projections at Age 65",
       subtitle = "100 Stochastic Paths vs Base Deterministic Forecast",
       x = "Calendar Year",
       y = "Force of Mortality (μx)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid = element_line(color = "gray90"))

print(p_scen_val)
ggsave("Phase3_Part2_validation_age65.png", p_scen_val, width = 8, height = 5, dpi = 300)

# Save the list of matrices for Part 3
saveRDS(mhat_scenarios, file = "mhat_scenarios_100.RDS")
