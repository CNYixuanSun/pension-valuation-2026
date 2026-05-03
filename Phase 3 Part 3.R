# ============================================================
# Phase 3 - Part 3: Valuation of Stochastic Scenarios
# ============================================================
library(ggplot2)
library(dplyr)

# Define the participant profile for this analysis
val_age <- 65
val_birth_year <- 1961  # Since valuation year is 2026
val_amount <- 10000

# 1. Calculate Base (Deterministic) Values
# ----------------------------------------------------------
cat("Calculating Base Deterministic Values...\n")
base_LE <- LE_cohort(mhat_forecast_full, Age = val_age, BirthYear = val_birth_year)
base_EPV <- LA_cohort(mhat_forecast_full, Age = val_age, BirthYear = val_birth_year, 
                      amount = val_amount, rfr = rfr)

cat(sprintf("Base LE (Age %d): %.2f years\n", val_age, base_LE))
cat(sprintf("Base EPV (Age %d): €%.2f\n", val_age, base_EPV))

# 2. Loop through 100 Scenarios
# ----------------------------------------------------------
cat("Running valuations for 100 stochastic scenarios...\n")

# Initialize vectors to store results
sim_LE <- numeric(n_scenarios)
sim_EPV <- numeric(n_scenarios)

# Loop through each matrix in the list
for (s in 1:n_scenarios) {
  
  # Extract the s-th mortality matrix
  mat_s <- mhat_scenarios[[s]]
  
  # Calculate Cohort LE
  sim_LE[s] <- LE_cohort(mat_s, Age = val_age, BirthYear = val_birth_year)
  
  # Calculate Cohort EPV
  sim_EPV[s] <- LA_cohort(mat_s, Age = val_age, BirthYear = val_birth_year, 
                          amount = val_amount, rfr = rfr)
}

# 3. Organize Results into a Data Frame
# ----------------------------------------------------------
results_df <- data.frame(
  Scenario = 1:n_scenarios,
  LE = sim_LE,
  EPV = sim_EPV
)

# 4. Visualization: Distributions
# ----------------------------------------------------------

# --- Plot 1: Life Expectancy Distribution ---
p_le_hist <- ggplot(results_df, aes(x = LE)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", alpha = 0.6, color = "white") +
  geom_density(color = "darkblue", linewidth = 0.5) +
  geom_vline(xintercept = base_LE, color = "red", linewidth = 0.2, linetype = "dashed") +
  labs(title = "Distribution of Simulated Cohort Life Expectancy",
       subtitle = sprintf("Age %d, Born %d (n=100)", val_age, val_birth_year),
       x = "Life Expectancy (Years)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")

# --- Plot 2: EPV Distribution ---
p_epv_hist <- ggplot(results_df, aes(x = EPV)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "darkgreen", alpha = 0.6, color = "white") +
  geom_density(color = "darkgreen", linewidth = 0.8) +
  geom_vline(xintercept = base_EPV, color = "red", linewidth = 0.2, linetype = "dashed") +
  labs(title = "Distribution of Simulated Annuity EPV",
       subtitle = sprintf("Age %d, Born %d (n=100)", val_age, val_birth_year),
       x = "Expected Present Value (EUR)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")

# Add annotations to plots for the Base value
p_le_hist <- p_le_hist + annotate("text", x = base_LE, y = Inf, 
                                  label = sprintf("Base: %.2f", base_LE), 
                                  vjust = 2, color = "red")
p_epv_hist <- p_epv_hist + annotate("text", x = base_EPV, y = Inf, 
                                    label = sprintf("Base: €%.0f", base_EPV), 
                                    vjust = 2, color = "red")

# Display and Save Plots
print(p_le_hist)
ggsave("Phase3_Part3_LE_Distribution.png", p_le_hist, width = 7, height = 5, dpi = 300)

print(p_epv_hist)
ggsave("Phase3_Part3_EPV_Distribution.png", p_epv_hist, width = 7, height = 5, dpi = 300)

# 5. Summary Statistics
# ----------------------------------------------------------
cat("\n=== STOCHASTIC RESULTS SUMMARY ===\n")
cat(sprintf("Mean LE: %.2f years (Std Dev: %.2f)\n", mean(results_df$LE), sd(results_df$LE)))
cat(sprintf("Mean EPV: €%.2f (Std Dev: €%.2f)\n", mean(results_df$EPV), sd(results_df$EPV)))

cat("\nQuantiles for LE:\n")
print(quantile(results_df$LE, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)))

cat("\nQuantiles for EPV:\n")
print(quantile(results_df$EPV, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)))

# Save results to CSV
write_csv(results_df, "Phase3_Stochastic_Results.csv")