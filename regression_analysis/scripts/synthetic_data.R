# Synthetic Data Generation for Regression Analysis
# Creates realistic scenarios with known ground truth for testing

# Generate synthetic browsing data with known regression relationships
generate_regression_data <- function(n_respondents = 100, n_domains = 200, seed = 42) {
  set.seed(seed)
  
  cat("=== GENERATING REGRESSION DATA ===\n")
  cat("Respondents:", n_respondents, "| Domains:", n_domains, "\n")
  
  # Step 1: Generate respondent characteristics (X variables)
  X <- data.frame(
    # Demographics  
    age = rnorm(n_respondents, mean = 40, sd = 15),
    income = exp(rnorm(n_respondents, mean = 10.5, sd = 0.8)), # log-normal
    education = sample(1:4, n_respondents, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    
    # Behavioral traits
    tech_savvy = rbeta(n_respondents, 2, 2),
    privacy_concern = rbeta(n_respondents, 3, 2),
    
    # Usage patterns  
    online_hours = rgamma(n_respondents, shape = 3, rate = 0.5),
    mobile_user = rbinom(n_respondents, 1, 0.7)
  )
  
  # Standardize continuous variables
  X$age_std <- scale(X$age)[,1]
  X$income_std <- scale(log(X$income))[,1] 
  X$tech_savvy_std <- scale(X$tech_savvy)[,1]
  X$privacy_concern_std <- scale(X$privacy_concern)[,1]
  X$online_hours_std <- scale(X$online_hours)[,1]
  
  # Step 2: Generate domain characteristics
  domain_types <- sample(c("news", "social", "shopping", "entertainment", "adult", "tech"), 
                        n_domains, replace = TRUE, 
                        prob = c(0.15, 0.2, 0.2, 0.25, 0.1, 0.1))
  
  domain_popularity <- rexp(n_domains, rate = 0.1) # Heavy-tailed popularity
  domain_tech_level <- rbeta(n_domains, 2, 3)      # Most domains low-tech
  
  # Step 3: Generate browsing patterns (cij) influenced by X
  cij <- matrix(0, n_respondents, n_domains)
  
  for (i in 1:n_respondents) {
    # Base browsing intensity
    total_visits <- max(100, 500 + 200 * X$online_hours_std[i] + rnorm(1, 0, 50))
    
    # Domain preferences based on characteristics
    domain_affinity <- numeric(n_domains)
    
    for (j in 1:n_domains) {
      # Base popularity effect
      affinity <- log(domain_popularity[j] + 1)
      
      # Tech-savvy users prefer tech domains and less popular sites
      if (domain_types[j] == "tech") {
        affinity <- affinity + 2 * X$tech_savvy_std[i]
      }
      if (X$tech_savvy_std[i] > 1) { # High tech users explore more
        affinity <- affinity - 0.5 * log(domain_popularity[j] + 1) 
      }
      
      # Privacy-concerned users avoid social/adult content
      if (domain_types[j] %in% c("social", "adult")) {
        affinity <- affinity - 1.5 * X$privacy_concern_std[i]
      }
      
      # Age effects
      if (domain_types[j] == "social" && X$age_std[i] < -1) { # Young users
        affinity <- affinity + 1
      }
      if (domain_types[j] == "news" && X$age_std[i] > 1) { # Older users
        affinity <- affinity + 1
      }
      
      # Income effects on shopping
      if (domain_types[j] == "shopping") {
        affinity <- affinity + 0.5 * X$income_std[i]
      }
      
      domain_affinity[j] <- affinity
    }
    
    # Convert to probabilities and generate visits
    probs <- exp(domain_affinity) / sum(exp(domain_affinity))
    
    # Each person visits subset of domains
    n_domains_visited <- min(n_domains, max(10, round(20 + 15 * X$tech_savvy_std[i])))
    visited_domains <- sample(n_domains, n_domains_visited, prob = probs)
    
    # Generate visits to selected domains
    visit_probs <- probs[visited_domains]
    visit_probs <- visit_probs / sum(visit_probs)
    
    visits <- rmultinom(1, total_visits, visit_probs)
    cij[i, visited_domains] <- as.numeric(visits)
  }
  
  # Step 4: Generate true outcome proportions (adult content)
  y_true <- numeric(n_respondents)
  
  for (i in 1:n_respondents) {
    adult_domains <- which(domain_types == "adult")
    adult_visits <- sum(cij[i, adult_domains])
    total_visits <- sum(cij[i, ])
    
    if (total_visits > 0) {
      y_true[i] <- adult_visits / total_visits
    } else {
      y_true[i] <- 0
    }
  }
  
  # Step 5: Define true regression relationship
  # y = f(X, true_browsing_pattern) + noise
  
  # True coefficients (what we want to recover)
  beta_true <- c(
    intercept = 0.05,           # Baseline adult content rate
    age = -0.001,               # Older users less adult content  
    income = 0.002,             # Higher income slightly more
    education = -0.005,         # More educated less adult content
    tech_savvy = 0.02,          # Tech users find more varied content
    privacy_concern = -0.015,   # Privacy-concerned avoid adult sites
    online_hours = 0.003,       # More hours = more adult content
    mobile_user = 0.01          # Mobile users different patterns
  )
  
  # Construct design matrix
  X_matrix <- model.matrix(~ age_std + income_std + education + tech_savvy_std + 
                          privacy_concern_std + online_hours_std + mobile_user, 
                          data = X)
  
  # Generate regression outcome
  y_regression <- as.numeric(X_matrix %*% beta_true) + rnorm(n_respondents, 0, 0.02)
  y_regression <- pmax(0, pmin(1, y_regression)) # Bound between 0-1
  
  # Data characteristics
  cat("Data characteristics:\n")
  cat("- Adult content domains:", sum(domain_types == "adult"), "/", n_domains, "\n")
  cat("- Average adult content rate (observed):", round(mean(y_true), 4), "\n")
  cat("- Average adult content rate (regression):", round(mean(y_regression), 4), "\n")
  cat("- Correlation (observed, regression):", round(cor(y_true, y_regression), 3), "\n")
  
  nonzero_visits <- sum(cij > 0)
  sparsity <- 1 - nonzero_visits / (n_respondents * n_domains)
  cat("- Matrix sparsity:", round(100 * sparsity, 1), "% zeros\n")
  
  avg_domains_per_person <- mean(rowSums(cij > 0))
  cat("- Average domains per person:", round(avg_domains_per_person, 1), "\n")
  
  return(list(
    # Core data
    cij = cij,
    X = X,
    X_matrix = X_matrix,
    
    # Outcomes
    y_true = y_true,           # From actual browsing patterns
    y_regression = y_regression, # From regression model
    
    # Ground truth
    beta_true = beta_true,
    
    # Domain info
    domain_types = domain_types,
    domain_popularity = domain_popularity,
    
    # Sample info
    n_respondents = n_respondents,
    n_domains = n_domains,
    
    # Characteristics
    characteristics = list(
      sparsity = sparsity,
      avg_domains_per_person = avg_domains_per_person,
      adult_rate_observed = mean(y_true),
      adult_rate_regression = mean(y_regression)
    )
  ))
}

# Test the synthetic data generation
if (!interactive()) {
  cat("🧪 TESTING SYNTHETIC DATA GENERATION\n")
  cat("Creating realistic browsing data with known regression relationships...\n\n")
  
  data <- generate_regression_data()
  
  cat("\n✅ SYNTHETIC DATA GENERATION COMPLETE!\n")
  cat("Ready for regression bias analysis.\n")
}