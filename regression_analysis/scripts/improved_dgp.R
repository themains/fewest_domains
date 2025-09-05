# Improved Data Generating Process for HT Testing
# Addresses sparsity and adult domain distribution issues

library(MASS)  # For mvrnorm

# Generate more realistic browsing data with better properties for HT
generate_improved_data <- function(n_users = 200, n_domains = 300, 
                                 adult_fraction = 0.15, seed = 2024) {
  
  set.seed(seed)
  
  cat("=== GENERATING IMPROVED REGRESSION DATA ===\n")
  
  # 1. USER CHARACTERISTICS (more realistic correlations)
  # Create correlated user characteristics
  user_means <- c(0, 0, 0, 0)  # age, income, tech_savvy, privacy_concern
  user_cov <- matrix(c(
    1.0, 0.3, -0.2, 0.1,   # age: positively corr w/ income, negatively w/ tech
    0.3, 1.0, 0.1, -0.1,   # income: positively corr w/ age, slightly w/ tech
    -0.2, 0.1, 1.0, -0.3,  # tech_savvy: negatively corr w/ age, privacy
    0.1, -0.1, -0.3, 1.0   # privacy_concern: negatively corr w/ tech
  ), nrow = 4)
  
  user_chars <- mvrnorm(n_users, user_means, user_cov)
  colnames(user_chars) <- c("age_std", "income_std", "tech_savvy_std", "privacy_concern_std")
  
  # Add some additional characteristics
  education <- rnorm(n_users, mean = 0, sd = 1)
  online_hours_std <- rnorm(n_users, mean = 0, sd = 1)  
  mobile_user <- rbinom(n_users, 1, 0.6)
  
  X_matrix <- cbind(
    intercept = 1,
    age_std = user_chars[, "age_std"],
    income_std = user_chars[, "income_std"],
    education = education,
    tech_savvy_std = user_chars[, "tech_savvy_std"],
    privacy_concern_std = user_chars[, "privacy_concern_std"],
    online_hours_std = online_hours_std,
    mobile_user = mobile_user
  )
  
  # 2. DOMAIN TYPES (more balanced distribution)
  n_adult <- round(n_domains * adult_fraction)
  n_news <- round(n_domains * 0.20)      # News sites
  n_social <- round(n_domains * 0.15)    # Social media
  n_shopping <- round(n_domains * 0.15)  # Shopping
  n_tech <- round(n_domains * 0.10)      # Tech sites
  n_other <- n_domains - (n_adult + n_news + n_social + n_shopping + n_tech)
  
  domain_types <- c(
    rep("adult", n_adult),
    rep("news", n_news), 
    rep("social", n_social),
    rep("shopping", n_shopping),
    rep("tech", n_tech),
    rep("other", n_other)
  )
  
  # 3. DOMAIN POPULARITY (more realistic power law but less extreme)
  # Create base popularity with moderate power law
  domain_popularity <- exp(rnorm(n_domains, mean = 2, sd = 1.2))
  domain_popularity <- domain_popularity / sum(domain_popularity)
  
  # Adjust popularity by domain type (more realistic patterns)
  type_multipliers <- c(
    "adult" = 0.8,      # Adult sites: moderately less popular on average
    "news" = 1.2,       # News: more popular
    "social" = 1.5,     # Social: very popular  
    "shopping" = 1.1,   # Shopping: slightly more popular
    "tech" = 0.9,       # Tech: slightly less popular
    "other" = 1.0       # Other: baseline
  )
  
  for (i in 1:n_domains) {
    domain_popularity[i] <- domain_popularity[i] * type_multipliers[domain_types[i]]
  }
  
  # Normalize again
  domain_popularity <- domain_popularity / sum(domain_popularity)
  
  # 4. USER-DOMAIN AFFINITIES (less extreme correlations)
  # Create user preferences that correlate with characteristics but not too strongly
  user_domain_affinities <- matrix(1, nrow = n_users, ncol = n_domains)
  
  for (i in 1:n_users) {
    for (j in 1:n_domains) {
      
      affinity <- 1  # Base affinity
      
      # Type-specific affinities (moderate correlations)
      if (domain_types[j] == "adult") {
        # Adult content: moderately correlated with age (younger prefer more)
        age_effect <- -0.3 * user_chars[i, "age_std"] 
        privacy_effect <- -0.2 * user_chars[i, "privacy_concern_std"]
        affinity <- exp(age_effect + privacy_effect)
        
      } else if (domain_types[j] == "tech") {
        # Tech sites: correlated with tech savviness and income
        tech_effect <- 0.4 * user_chars[i, "tech_savvy_std"]
        income_effect <- 0.2 * user_chars[i, "income_std"]
        affinity <- exp(tech_effect + income_effect)
        
      } else if (domain_types[j] == "news") {
        # News: correlated with age and education
        age_effect <- 0.2 * user_chars[i, "age_std"]
        education_effect <- 0.3 * education[i]
        affinity <- exp(age_effect + education_effect)
        
      } else if (domain_types[j] == "social") {
        # Social media: younger users, less privacy concern
        age_effect <- -0.2 * user_chars[i, "age_std"]
        privacy_effect <- -0.1 * user_chars[i, "privacy_concern_std"]
        mobile_effect <- 0.3 * mobile_user[i]
        affinity <- exp(age_effect + privacy_effect + mobile_effect)
        
      } else if (domain_types[j] == "shopping") {
        # Shopping: correlated with income
        income_effect <- 0.3 * user_chars[i, "income_std"]
        affinity <- exp(income_effect)
      }
      
      user_domain_affinities[i, j] <- affinity
    }
  }
  
  # 5. GENERATE VISIT COUNTS (less sparse)
  cij <- matrix(0, nrow = n_users, ncol = n_domains)
  
  for (i in 1:n_users) {
    # Each user gets more visits (less sparsity)
    total_visits <- rpois(1, lambda = 80) + 20  # 20-150 visits per user
    
    # Visit probabilities combine popularity and personal affinity
    visit_probs <- domain_popularity * user_domain_affinities[i, ]
    visit_probs <- visit_probs / sum(visit_probs)
    
    # Allocate visits across domains (multinomial)
    if (total_visits > 0) {
      visits <- rmultinom(1, size = total_visits, prob = visit_probs)
      cij[i, ] <- as.vector(visits)
    }
  }
  
  # 6. CALCULATE TRUE ADULT CONTENT PROPORTIONS
  adult_domains <- which(domain_types == "adult")
  y_true <- numeric(n_users)
  
  for (i in 1:n_users) {
    total_visits <- sum(cij[i, ])
    if (total_visits > 0) {
      adult_visits <- sum(cij[i, adult_domains])
      y_true[i] <- adult_visits / total_visits
    }
  }
  
  # 7. DATA QUALITY STATISTICS
  sparsity <- sum(cij == 0) / (n_users * n_domains)
  avg_domains_per_user <- mean(rowSums(cij > 0))
  adult_content_rate <- mean(y_true)
  
  # Correlation with user characteristics
  age_cor <- cor(y_true, user_chars[, "age_std"])
  tech_cor <- cor(y_true, user_chars[, "tech_savvy_std"])
  privacy_cor <- cor(y_true, user_chars[, "privacy_concern_std"])
  
  cat("Data characteristics:\n")
  cat("- Adult content domains:", length(adult_domains), "/", n_domains, "\n")
  cat("- Average adult content rate:", round(adult_content_rate, 4), "\n")
  cat("- Age-adult correlation:", round(age_cor, 3), "\n") 
  cat("- Tech-adult correlation:", round(tech_cor, 3), "\n")
  cat("- Privacy-adult correlation:", round(privacy_cor, 3), "\n")
  cat("- Matrix sparsity:", round(100 * sparsity, 1), "% zeros\n")
  cat("- Average domains per user:", round(avg_domains_per_user, 1), "\n")
  
  # 8. VERIFY REGRESSION RELATIONSHIP EXISTS
  lm_check <- lm(y_true ~ X_matrix[, -1])
  r_squared <- summary(lm_check)$r.squared
  cat("- Regression R²:", round(r_squared, 3), "\n")
  
  return(list(
    cij = cij,
    X_matrix = X_matrix,
    y_true = y_true,
    domain_types = domain_types,
    adult_domains = adult_domains,
    characteristics = list(
      sparsity = sparsity,
      avg_domains_per_user = avg_domains_per_user,
      adult_content_rate = adult_content_rate,
      age_adult_cor = age_cor,
      tech_adult_cor = tech_cor,
      privacy_adult_cor = privacy_cor,
      regression_r2 = r_squared
    )
  ))
}

# Test the improved DGP
test_improved_dgp <- function() {
  
  cat("🔬 TESTING IMPROVED DATA GENERATING PROCESS\n")
  cat(rep("=", 70), "\n")
  
  # Generate data with different sparsity levels
  configs <- list(
    list(n_users = 150, n_domains = 200, adult_fraction = 0.12, name = "Moderate"),
    list(n_users = 200, n_domains = 300, adult_fraction = 0.15, name = "Standard"), 
    list(n_users = 250, n_domains = 400, adult_fraction = 0.18, name = "Large")
  )
  
  for (config in configs) {
    cat("\n", rep("-", 40), "\n")
    cat("CONFIGURATION:", config$name, "\n")
    cat("Users:", config$n_users, "| Domains:", config$n_domains, 
        "| Adult %:", round(100 * config$adult_fraction), "%\n")
    cat(rep("-", 40), "\n")
    
    data <- generate_improved_data(
      n_users = config$n_users, 
      n_domains = config$n_domains,
      adult_fraction = config$adult_fraction,
      seed = 2024
    )
    
    # Quick quality check
    chars <- data$characteristics
    
    cat("\nQUALITY ASSESSMENT:\n")
    if (chars$sparsity < 0.85) {
      cat("✅ Sparsity: GOOD (", round(100 * chars$sparsity, 1), "%)\n")
    } else {
      cat("⚠️ Sparsity: HIGH (", round(100 * chars$sparsity, 1), "%)\n")
    }
    
    if (abs(chars$age_adult_cor) < 0.6) {
      cat("✅ Age correlation: MODERATE (", round(chars$age_adult_cor, 3), ")\n")
    } else {
      cat("❌ Age correlation: TOO HIGH (", round(chars$age_adult_cor, 3), ")\n")
    }
    
    if (chars$regression_r2 > 0.05) {
      cat("✅ Regression signal: DETECTABLE (R² =", round(chars$regression_r2, 3), ")\n")
    } else {
      cat("⚠️ Regression signal: WEAK (R² =", round(chars$regression_r2, 3), ")\n")  
    }
    
    if (chars$avg_domains_per_user > 15) {
      cat("✅ User diversity: GOOD (", round(chars$avg_domains_per_user, 1), " domains/user)\n")
    } else {
      cat("⚠️ User diversity: LIMITED (", round(chars$avg_domains_per_user, 1), " domains/user)\n")
    }
  }
  
  cat("\n✅ IMPROVED DGP TESTING COMPLETE!\n")
  cat("Choose configuration with best balance of realism and HT stability.\n")
  
  # Return the standard configuration for further use
  return(generate_improved_data(n_users = 200, n_domains = 300, adult_fraction = 0.15, seed = 2024))
}

# Run the test
if (!interactive()) {
  cat("🚀 TESTING IMPROVED DATA GENERATING PROCESS\n\n")
  test_data <- test_improved_dgp()
}