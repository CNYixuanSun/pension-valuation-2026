library(tidyverse)  # for dplyr, tidyr

setwd("C:/Users/sun00/Desktop/ALIM Project")
getwd()


# Read the raw data
mx_raw <- read.table("Mx_LUX_1x1.txt", 
                     header = TRUE, 
                     na.strings = ".")

# Step 1: Clean the data
mx_clean <- mx_raw %>%
  # Keep only rows where Age is numeric (i.e., not "110+" or "Open")
  filter(!grepl("\\+", Age)) %>%
  # Convert Age to integer (it should already be, but ensure)
  mutate(Age = as.integer(Age)) %>%
  # Select only Year, Age, and Total (the death rate for both sexes)
  select(Year, Age, mx = Total) %>%
  # Remove any missing death rates (if any)
  filter(!is.na(mx))

# Step 2: Reshape from long to wide format (years as rows, ages as columns)
mx_wide <- mx_clean %>%
  pivot_wider(id_cols = Year,
              names_from = Age,
              values_from = mx,
              names_sort = TRUE) %>%
  # Convert Year column to row names
  column_to_rownames(var = "Year")

# Step 3: Convert to matrix (as required for mhat)
mhat <- as.matrix(mx_wide)

# Keep only years >= 1980
years_keep <- rownames(mhat)[as.numeric(rownames(mhat)) >= 1980]
mhat <- mhat[years_keep, ]

# Check the result
dim(mhat)        # e.g., 65 years × 111 ages
head(mhat[, 1:5])  # first 5 ages for first few years

# Step 4: Close the mortality
# ----------------------------------------------
# Function to close mortality rates for one year
# ----------------------------------------------
close_mortality <- function(mx, max_age = 110) {
  # mx: named vector of mortality rates (ages as names, observed up to 109)
  # max_age: ultimate age to extrapolate to (e.g., 120)
  
  ages_all <- as.numeric(names(mx))
  max_obs <- max(ages_all)  # should be 109 for Luxembourg
  
  # If already beyond max_age, just return (not needed)
  if (max_obs >= max_age) {
    return(mx)
  }
  
  # Use ages from 90 to max_obs to fit the Kannisto model
  # (threshold fixed at 90 – you can change if needed)
  threshold <- 80
  idx_fit <- ages_all >= threshold
  ages_fit <- ages_all[idx_fit]
  mx_fit <- mx[idx_fit]
  
  # Avoid logit extremes (0 or 1)
  mx_fit <- pmax(pmin(mx_fit, 1 - 1e-10), 1e-10)
  
  # Kannisto: logit(mx) = a + b * age
  logit_fit <- log(mx_fit / (1 - mx_fit))
  fit <- lm(logit_fit ~ ages_fit)
  
  # Predict for new ages: from max_obs+1 to max_age
  ages_new <- seq(max_obs + 1, max_age)
  logit_pred <- predict(fit, newdata = data.frame(ages_fit = ages_new))
  mx_pred <- exp(logit_pred) / (1 + exp(logit_pred))
  names(mx_pred) <- ages_new
  
  # Combine observed (all original) + extrapolated
  combined <- c(mx, mx_pred)
  # Sort by age
  combined <- combined[order(as.numeric(names(combined)))]
  
  return(combined)
}

clean_mortality <- function(mx, max_rate = 0.95) {
  # mx: named numeric vector (ages as names)
  # Replace NA with a small rate (1e-6)
  mx[is.na(mx)] <- 1e-6
  
  # Cap at max_rate
  mx <- pmin(mx, max_rate)
  
  # Ensure monotonic increase after age 40
  ages <- as.numeric(names(mx))
  for (i in 2:length(mx)) {
    age <- ages[i]
    if (!is.na(age) && age >= 40 && !is.na(mx[i]) && !is.na(mx[i-1])) {
      if (mx[i] < mx[i-1]) {
        mx[i] <- mx[i-1] * 1.01  # increase slightly
      }
    }
  }
  # Final cap again (in case multiplication exceeded)
  mx <- pmin(mx, max_rate)
  return(mx)
}


# ----------------------------------------------
# Apply my close mortality function to all years in the mhat matrix
# ----------------------------------------------
ultimate_age <- 110
closed_rows <- list()

for (yr in rownames(mhat)) {
  mx_year <- mhat[yr, ]
  names(mx_year) <- colnames(mhat)
  mx_year <- mx_year[!is.na(mx_year)]  # remove any NA if present
  closed_rows[[yr]] <- close_mortality(mx_year, max_age = ultimate_age)
}

# Convert to matrix
all_ages <- sort(unique(unlist(lapply(closed_rows, names))))
mhat_closed <- matrix(NA, nrow = length(closed_rows), ncol = length(all_ages))
rownames(mhat_closed) <- names(closed_rows)
colnames(mhat_closed) <- all_ages

for (yr in names(closed_rows)) {
  mhat_closed[yr, names(closed_rows[[yr]])] <- closed_rows[[yr]]
}

# Ensure columns are sorted numerically (0,1,2,...,120)
mhat_closed <- mhat_closed[, as.character(sort(as.numeric(colnames(mhat_closed))))]

# Apply to each year (row)
cleaned_rows <- list()
for (yr in rownames(mhat_closed)) {
  cleaned_rows[[yr]] <- clean_mortality(mhat_closed[yr, ], max_rate = 0.95)
}

# Convert list back to matrix
mhat_clean <- do.call(rbind, cleaned_rows)
colnames(mhat_clean) <- names(cleaned_rows[[1]])  # ages are the same across years
rownames(mhat_clean) <- rownames(mhat_closed)

# Check the problematic year again
mhat_clean["2020", as.character(100:110)]

# Check result
dim(mhat_clean)   # should be [1] 45 111  (0 to 110 inclusive = 111 ages)
head(mhat_clean[1, 90:101])   # look at ages 90-101 for first year
tail(mhat_clean[1, ])         # last 5 ages (105 to 110) – should be extrapolated
