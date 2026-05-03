# Cohort life expectancy (max_age = 110)
LE_cohort <- function(mhat, Age, BirthYear, max_age = 110) {
  ages <- as.numeric(rownames(mhat))
  years <- as.numeric(colnames(mhat))
  start_year <- BirthYear + Age
  end_year <- BirthYear + max_age
  cohort_years <- start_year:end_year
  cohort_ages <- Age:max_age
  
  mu <- numeric(length(cohort_ages))
  for (i in seq_along(cohort_ages)) {
    yr <- as.character(cohort_years[i])
    ag <- as.character(cohort_ages[i])
    if (yr %in% colnames(mhat) && ag %in% rownames(mhat)) {
      mu[i] <- mhat[ag, yr]
    } else {
      mu[i] <- mhat[ag, as.character(max(years))]  # use last available year
    }
  }
  term1 <- (1 - exp(-mu[1])) / mu[1]
  surv <- cumprod(exp(-mu))
  n <- length(mu)
  term2 <- sum(surv[1:(n-1)] * (1 - exp(-mu[-1])) / mu[-1])
  return(term1 + term2)
}

# Cohort annuity EPV (annuity-due, annual payment in advance)
LA_cohort <- function(mhat, Age, BirthYear, amount, rfr, max_age = 110) {
  ages <- as.numeric(rownames(mhat))
  years <- as.numeric(colnames(mhat))
  start_year <- BirthYear + Age
  end_year <- BirthYear + max_age
  cohort_years <- start_year:end_year
  cohort_ages <- Age:max_age
  
  mu <- numeric(length(cohort_ages))
  for (i in seq_along(cohort_ages)) {
    yr <- as.character(cohort_years[i])
    ag <- as.character(cohort_ages[i])
    if (yr %in% colnames(mhat) && ag %in% rownames(mhat)) {
      mu[i] <- mhat[ag, yr]
    } else {
      mu[i] <- mhat[ag, as.character(max(years))]
    }
  }
  surv <- c(1, cumprod(exp(-mu)))   # survival probabilities at integer times
  n <- length(cohort_ages) - 1      # number of future payments after time 0
  
  # Discount factors: assume rfr has columns T (maturity) and i (spot rate)
  v <- c(1, rep(NA, n))
  for (T in 1:n) {
    idx <- which(rfr$T == T)
    if (length(idx) == 0) {
      # fallback: linear interpolation (or use nearest)
      idx <- which.min(abs(rfr$T - T))
      warning(paste("Interest rate for T =", T, "not found; using nearest maturity", rfr$T[idx]))
    }
    i_T <- rfr$i[idx[1]]
    v[T+1] <- 1 / (1 + i_T)^T
  }
  EPV <- amount * sum(surv[1:(n+1)] * v)
  return(EPV)
}