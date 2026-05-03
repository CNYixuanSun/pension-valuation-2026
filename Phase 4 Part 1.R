# ============================================================
# Phase 4 - Part 1: Portfolio Setup & Core Valuation Function
# ============================================================
library(tidyverse)

# 1. Define Portfolio Exposures (from assignment brief)
portfolio_exposures <- data.frame(
  Age    = c(30, 40, 50, 60, 70, 80, 90),
  Green  = c(500, 1200, 2000, 1800, 1500, 300, 0),
  Silver = c(300, 850, 1400, 1800, 1650, 550, 50),
  Gold   = c(100, 500, 800, 1800, 1800, 800, 100)
)

# 2. Core Valuation Function
value_portfolio <- function(mhat, rfr, exposures_df, portfolio_name, 
                            retirement_age = 65, val_year = 2026) {
  
  # Ensure RFR rates are in decimal form
  if (is.character(rfr$i) || max(rfr$i, na.rm = TRUE) > 1) {
    rfr$i <- as.numeric(gsub("%", "", rfr$i)) / 100
  }
  
  exp_vec <- exposures_df[[portfolio_name]]
  age_vec <- exposures_df$Age
  max_matrix_year <- max(as.numeric(colnames(mhat)))
  max_age <- max(as.numeric(rownames(mhat)))
  
  total_liability <- 0
  
  for (i in seq_along(age_vec)) {
    xc <- age_vec[i]
    Ec <- exp_vec[i]
    if (Ec == 0) next
    
    # Maximum projection horizon: min(age limit, matrix year limit)
    max_T <- min(max_age - xc, max_matrix_year - val_year)
    T_vec <- 0:max_T
    
    # 1. Extract cohort mortality rates along the diagonal (age, year) = (xc+T, val_year+T)
    mu_T <- numeric(length(T_vec))
    for (t_idx in seq_along(T_vec)) {
      T <- T_vec[t_idx]
      age_at_T <- xc + T
      year_at_T <- val_year + T
      
      ag_str <- as.character(age_at_T)
      yr_str <- as.character(min(year_at_T, max_matrix_year)) # fallback to last matrix year
      
      if (ag_str %in% rownames(mhat) && yr_str %in% colnames(mhat)) {
        mu_T[t_idx] <- mhat[ag_str, yr_str]
      } else {
        mu_T[t_idx] <- 0 # safety fallback
      }
    }
    
    # 2. T-year survival probabilities: 0_p_x = 1, T_p_x = prod(exp(-mu))
    T_px <- c(1, cumprod(exp(-mu_T[-1])))
    
    # 3. Discount factors: v(T) = 1 / (1 + i_T)^T
    v_T <- numeric(length(T_vec))
    for (t_idx in seq_along(T_vec)) {
      T <- T_vec[t_idx]
      # Find closest maturity in RFR curve
      idx <- which.min(abs(rfr$T - T))
      i_T <- rfr$i[idx]
      v_T[t_idx] <- ifelse(T == 0, 1, 1 / (1 + i_T)^T)
    }
    
    # 4. Retirement indicator: I(xc + T >= retirement_age)
    pay_indicator <- as.numeric((xc + T_vec) >= retirement_age)
    
    # 5. Liability for this age class
    liab_xc <- Ec * sum(pay_indicator * T_px * v_T)
    total_liability <- total_liability + liab_xc
  }
  
  return(total_liability)
}

# Quick test to ensure function works with existing objects
cat("Testing function with Base Deterministic Matrix...\n")
test_green <- value_portfolio(mhat_forecast_full, rfr, portfolio_exposures, "Green")
cat(sprintf("Green Portfolio Base Liability: €%.2f\n", test_green))

# ============================================================
# Phase 4 - Part 2: Base Deterministic Portfolio Valuations
# ============================================================
library(tidyverse)
library(ggplot2)

# 1. Run base valuations for all three portfolios
cat("Calculating Base Deterministic Liabilities for all portfolios...\n")

base_liabilities <- sapply(c("Green", "Silver", "Gold"), function(port) {
  value_portfolio(
    mhat = mhat_forecast_full,
    rfr = rfr,
    exposures_df = portfolio_exposures,
    portfolio_name = port,
    retirement_age = 65,
    val_year = 2026
  )
})

# 2. Create a tidy results data frame
base_results_df <- data.frame(
  Portfolio = names(base_liabilities),
  Base_Liability_EUR = as.numeric(base_liabilities),
  Base_Liability_Millions = base_liabilities / 1e6,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Base_Liability_EUR = round(Base_Liability_EUR, 2),
    Base_Liability_Millions = round(Base_Liability_Millions, 2)
  )

# 3. Display results
cat("\n=== BASE DETERMINISTIC PORTFOLIO LIABILITIES (2026) ===\n")
print(base_results_df, row.names = FALSE)

# 4. CRO-Preferred Visualization: Bar Chart with Annotations
p_base <- ggplot(base_results_df, aes(x = Portfolio, y = Base_Liability_Millions, fill = Portfolio)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("€%.2fM", Base_Liability_Millions)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "Base Deterministic Portfolio Liabilities",
    subtitle = "Present value of lifelong pension obligations (2026 valuation)",
    x = "Portfolio",
    y = "Liability (Millions EUR)",
    fill = "Portfolio"
  ) +
  scale_fill_manual(values = c("Green" = "#4CAF50", "Silver" = "#9E9E9E", "Gold" = "#FFC107")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(p_base)
ggsave("Phase4_Part2_Base_Liabilities.png", p_base, width = 7, height = 5, dpi = 300)

# 5. Save results for reporting
write_csv(base_results_df, "Phase4_Part2_Base_Results.csv")

# 6. Quick sanity check: compare portfolio compositions
cat("\n=== PORTFOLIO COMPOSITION SUMMARY ===\n")
portfolio_exposures %>%
  mutate(Total_Participants = Green + Silver + Gold) %>%
  select(Age, Total_Participants) %>%
  print(row.names = FALSE)

# ============================================================
# Phase 4 - Part 3: Stochastic Portfolio Valuations (100 Scenarios)
# ============================================================
library(tidyverse)

cat("Running stochastic valuations for 100 scenarios...\n")

# 1. Pre-allocate matrix for speed (avoids slow row-by-row binding)
liab_matrix <- matrix(NA, nrow = 100, ncol = 3)
colnames(liab_matrix) <- c("Green", "Silver", "Gold")

# 2. Loop through scenarios
for (s in 1:100) {
  mat_s <- mhat_scenarios[[s]]
  
  liab_matrix[s, "Green"]  <- value_portfolio(mat_s, rfr, portfolio_exposures, "Green")
  liab_matrix[s, "Silver"] <- value_portfolio(mat_s, rfr, portfolio_exposures, "Silver")
  liab_matrix[s, "Gold"]   <- value_portfolio(mat_s, rfr, portfolio_exposures, "Gold")
  
  # Progress indicator
  if (s %% 25 == 0) cat(sprintf("completed %d scenarios...\n", s))
}

# 3. Convert to tidy long format
stoch_df <- as.data.frame(liab_matrix) %>%
  mutate(Scenario = row_number()) %>%
  pivot_longer(cols = c("Green", "Silver", "Gold"), 
               names_to = "Portfolio", 
               values_to = "Liability_EUR")

# 4. Quick sanity check: compare stochastic mean vs deterministic base
comparison <- stoch_df %>%
  group_by(Portfolio) %>%
  summarise(
    Stoch_Mean = mean(Liability_EUR),
    Stoch_SD   = sd(Liability_EUR),
    .groups    = "drop"
  ) %>%
  left_join(base_results_df, by = "Portfolio") %>%
  mutate(Diff_Pct = ((Stoch_Mean - Base_Liability_EUR) / Base_Liability_EUR) * 100) %>%
  select(Portfolio, Stoch_Mean, Stoch_SD, Base_Liability_EUR, Diff_Pct) %>%
  mutate(across(c(Stoch_Mean, Stoch_SD, Base_Liability_EUR, Diff_Pct), ~round(.x, 2)))

cat("\n=== STOCHASTIC vs BASE COMPARISON ===\n")
print(comparison, row.names = FALSE)

# 5. Save results for Part 4 (visualization & risk metrics)
saveRDS(stoch_df, "Phase4_Part3_Stochastic_Results.rds")
write_csv(stoch_df, "Phase4_Part3_Stochastic_Results.csv")
