# Calculate period life expectancy for a particular age and year
# mhat: Matrix of (closed) death rates/forces of mortality (years, ages)
# Age: Age at which to calculate period LE
# Year: Year at which to calculate period LE

LE_period <- function(mhat, Age, Year) {
  
  # Extract years and ages from row and col names
  years <- as.integer(rownames(mhat))
  ages <- as.integer(colnames(mhat))
  
  # Extract forces of mortality in year 'Year' at ages 'Age' and beyond
  # First, get the row index for the given Year
  if (!(Year %in% years)) stop("Year not found in mhat")
  mx <- mhat[as.character(Year), ]  # vector of mx for all ages
  mx <- mx[!is.na(mx)]
  
  # Keep ages from 'Age' to the maximum age
  age_indices <- which(ages >= Age)
  mx_age <- mx[age_indices]
  ages_use <- ages[age_indices]
  
  # First term in formula: (1 - exp(-mu_x)) / mu_x
  mu_x <- mx_age[1]
  term1 <- (1 - exp(-mu_x)) / mu_x
  
  # For loop to calculate summation term in formula
  term2 <- 0
  n <- length(mx_age) - 1  # remaining ages after the first
  
  for (k in 1:n) {
    # Survival probability up to age (Age + k - 1)
    surv <- exp(-sum(mx_age[1:k]))
    mu_k <- mx_age[k+1]
    term2 <- term2 + surv * (1 - exp(-mu_k)) / mu_k
  }
  
  # Period life expectancy = term1 + term2
  LE <- term1 + term2
  return(LE)
}

# Calculate EPV of a whole life annuity
# mhat: Matrix of (closed) death rates/forces of mortality (years, ages)
# Age: Age at which to value whole life annuity
# Year: Year at which to value whole life annuity
# amount: Annual pay-out of the whole life annuity (in advance)
# rfr: Data frame with columns 'T' (time to maturity in years) and 'i' (interest rate)

LA_period <- function(mhat, Age, Year, amount, rfr) {
  
  # Extract years and ages from row and col names
  years <- as.integer(rownames(mhat))
  ages <- as.integer(colnames(mhat))
  
  # Extract mortality rates for the given Year
  if (!(Year %in% years)) stop("Year not found in mhat")
  mx <- mhat[as.character(Year), ]
  mx <- mx[!is.na(mx)]
  
  # One-year survival probabilities for ages Age, Age+1, ...
  age_start <- which(ages >= Age)
  mx_sub <- mx[age_start]
  pxt <- exp(-mx_sub)   # 1-year survival probability at each age
  
  # T-year survival probabilities at age Age (cumulative product)
  Tpxt <- cumprod(pxt)
  
  # Maximum remaining years (up to the last age in mhat)
  n <- length(pxt) - 1   # number of future payments beyond first
  
  # Discount factors: need to match each future year T (0,1,2,...,n)
  # The rfr data frame should have column 'T' (integer years) and 'i' (interest rate)
  # For each T, v(T) = 1 / (1 + i(T))
  # If your rfr gives spot rates, you may need to extract i for each maturity.
  # Assume rfr has rows for T = 0,1,2,... (T=0 corresponds to discount factor 1)
  
  # Vector of discount factors: v[1] for T=0? Actually first payment is at time 0 (age Age).
  # We'll create v for T = 0,1,2,...,n
  # For T=0, v = 1
  v <- c(1, rep(NA, n))
  for (T in 1:n) {
    # Find interest rate for maturity T in rfr
    idx <- which(rfr$T == T)
    if (length(idx) == 0) {
      # If not found, interpolate or use last available
      idx <- which.min(abs(rfr$T - T))
      warning(paste("Interest rate for T =", T, "not found. Using nearest:", rfr$T[idx]))
    }
    i_T <- rfr$i[idx]
    v[T+1] <- 1 / (1 + i_T)^T   # cumulative discount for T years
  }
  
  # EPV = amount * (1 + sum_{T=1}^{n} Tpxt[T] * v[T+1])
  # Since first payment is at T=0 (certain), then for each future T, survival probability Tpxt[T] and discount v[T+1]
  # Tpxt[1] is survival to age Age+1 (first future payment)
  EPV <- amount * (1 + sum(Tpxt[1:n] * v[2:(n+1)]))
  
  return(EPV)
}