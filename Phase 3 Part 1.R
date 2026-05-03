# ============================================================
# Phase 3 - Part 1: Stochastic Simulation of kt (Random Walk with Drift)
# ============================================================
library(ggplot2)
library(dplyr)

set.seed(42) # Reproducibility
n_scenarios <- 100
h <- 50      # Forecast horizon (2025 to 2074)

# 1. Extract fitted parameters from Phase 1/2
kt_obs   <- as.numeric(LC_fit_sub$kt)
years_obs <- LC_fit_sub$years
last_kt  <- kt_obs[length(kt_obs)]

# 2. Estimate drift & residual volatility
drift <- mean(diff(kt_obs))
sigma <- sd(diff(kt_obs))

cat(sprintf("kt Drift: %.4f | Sigma: %.4f\n", drift, sigma))

# 3. Simulate kt paths (vectorized for speed)
# Each row = 1 scenario, Each column = 1 future year
kt_sim <- matrix(rnorm(n_scenarios * h), nrow = n_scenarios, ncol = h)
kt_sim <- t(apply(kt_sim, 1, cumsum))          # Cumulative shocks
kt_sim <- last_kt + drift * (1:h) + sigma * kt_sim # RW with drift
colnames(kt_sim) <- as.character(max(years_obs) + 1:h)

# 4. Quick visualization (first 15 paths + mean path)
kt_df <- as.data.frame(t(kt_sim)) %>%
  mutate(Year = max(years_obs) + 1:h) %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "kt")

# Calculate mean path for reference
mean_path <- kt_df %>%
  group_by(Year) %>%
  summarise(kt_mean = mean(kt), .groups = "drop")

# Fixed ggplot: explicitly scope data & aes per layer to avoid inheritance conflicts
p_kt <- ggplot() +
  geom_line(data = kt_df, 
            aes(x = Year, y = kt, group = Scenario, color = "Individual Paths"), 
            alpha = 0.15, linewidth = 0.4) +
  geom_line(data = mean_path, 
            aes(x = Year, y = kt_mean, color = "Mean Path"), 
            linewidth = 1.2, linetype = "dashed") +
  labs(title = "Phase 3: Simulated kt Paths (n=100)",
       subtitle = "Random Walk with Drift | 2025–2074",
       x = "Calendar Year", y = "kt(t)", color = "Legend") +
  scale_color_manual(values = c("Individual Paths" = "blue", 
                                "Mean Path" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

print(p_kt)
ggsave("Phase3_Part1_kt_scenarios.png", p_kt, width = 8, height = 5, dpi = 300)

# Save kt matrix for Phase 3 Part 2
saveRDS(kt_sim, file = "kt_sim_100_scenarios.rds")

