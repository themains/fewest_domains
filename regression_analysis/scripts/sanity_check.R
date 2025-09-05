# Sanity Check: Simple Test of Regression Bias
# Let's create a very controlled scenario to understand what's happening

source("synthetic_data.R")

# Create simple test data with known patterns
create_simple_test_data <- function(n_users = 100, n_domains = 50, seed = 12345) {
  set.seed(seed)
  
  # Simple user characteristics
  age <- rnorm(n_users, mean = 0, sd = 1)
  tech_savvy <- rnorm(n_users, mean = 0, sd = 1)
  
  X_matrix <- cbind(intercept = 1, age = age, tech_savvy = tech_savvy)
  
  # Simple domain usage pattern
  # Older users prefer domains 1-20, younger users prefer domains 21-40, domains 41-50 are random
  cij <- matrix(0, nrow = n_users, ncol = n_domains)
  
  for (i in 1:n_users) {
    n_visits <- rpois(1, lambda = 20) + 5  # Each user visits 5-40 domains
    
    # Age-based preference
    if (age[i] > 0) {  # Older users
      # Prefer domains 1-20
      preferred_domains <- sample(1:20, size = min(n_visits, 20), replace = TRUE)
      random_domains <- sample(21:n_domains, size = max(0, n_visits - 20), replace = TRUE)
      visited_domains <- c(preferred_domains, random_domains)
    } else {  # Younger users  
      # Prefer domains 21-40
      preferred_domains <- sample(21:40, size = min(n_visits, 20), replace = TRUE)
      random_domains <- sample(c(1:20, 41:n_domains), size = max(0, n_visits - 20), replace = TRUE)
      visited_domains <- c(preferred_domains, random_domains)
    }
    
    # Assign visit counts
    for (domain in visited_domains) {
      cij[i, domain] <- cij[i, domain] + rpois(1, lambda = 3) + 1
    }
  }
  
  # Define "adult" domains - let's say domains 5-15 are adult content
  adult_domains <- 5:15
  
  # Calculate true adult content proportion
  y_true <- numeric(n_users)
  for (i in 1:n_users) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  cat("SIMPLE TEST DATA CREATED:\n")
  cat("- Users:", n_users, "\n")
  cat("- Domains:", n_domains, "\n") 
  cat("- Adult domains:", length(adult_domains), "(domains", min(adult_domains), "-", max(adult_domains), ")\n")
  cat("- Average adult content rate:", round(mean(y_true), 3), "\n")
  cat("- Age-adult correlation:", round(cor(age, y_true), 3), "\n")
  cat("- Tech-adult correlation:", round(cor(tech_savvy, y_true), 3), "\n")
  
  return(list(
    cij = cij,
    X_matrix = X_matrix,
    y_true = y_true,
    adult_domains = adult_domains,
    age = age,
    tech_savvy = tech_savvy
  ))
}

# Simple domain selection methods
simple_popular_method <- function(cij, n_select) {
  domain_popularity <- colSums(cij)
  selected <- head(order(domain_popularity, decreasing = TRUE), n_select)
  return(selected)
}

simple_random_method <- function(cij, n_select) {
  selected <- sample(1:ncol(cij), n_select)
  return(selected)
}

simple_balanced_method <- function(cij, X_matrix, n_select) {
  # Try to select domains that are used across different age groups
  n_domains <- ncol(cij)
  age_values <- X_matrix[, "age"]
  
  # Split users into young and old
  young_users <- which(age_values <= 0)
  old_users <- which(age_values > 0)
  
  domain_scores <- numeric(n_domains)
  
  for (j in 1:n_domains) {
    young_usage <- sum(cij[young_users, j] > 0)
    old_usage <- sum(cij[old_users, j] > 0)
    
    # Score domains that are used by both groups
    total_usage <- young_usage + old_usage
    balance_score <- min(young_usage, old_usage) / max(young_usage, old_usage, 1)
    
    domain_scores[j] <- total_usage * (1 + balance_score)
  }
  
  selected <- head(order(domain_scores, decreasing = TRUE), n_select)
  return(selected)
}

# Test simple methods
test_simple_methods <- function() {
  
  cat("🔬 SANITY CHECK: Simple Controlled Test\n")
  cat(rep("=", 60), "\n")
  
  # Create simple test data
  data <- create_simple_test_data(n_users = 100, n_domains = 50, seed = 12345)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- data$adult_domains
  age <- data$age
  tech_savvy <- data$tech_savvy
  
  # Ground truth regression
  lm_true <- lm(y_true ~ age + tech_savvy)
  beta_true <- coef(lm_true)
  
  cat("\nGROUND TRUTH REGRESSION:\n")
  print(round(beta_true, 4))
  
  # Test different selection strategies
  methods <- list(
    "All_Domains" = function() 1:ncol(cij),
    "Popular_15" = function() simple_popular_method(cij, 15),
    "Random_15" = function() simple_random_method(cij, 15),
    "Balanced_15" = function() simple_balanced_method(cij, X_matrix, 15)
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 40), "\n")
    cat("METHOD:", method_name, "\n")
    
    # Apply method
    selected_domains <- methods[[method_name]]()
    
    cat("Selected", length(selected_domains), "domains\n")
    
    # Calculate estimated proportions
    y_estimated <- numeric(nrow(cij))
    
    for (i in 1:nrow(cij)) {
      visits_selected <- sum(cij[i, selected_domains])
      
      if (visits_selected > 0) {
        adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
        y_estimated[i] <- adult_visits_selected / visits_selected
      } else {
        y_estimated[i] <- 0
      }
    }
    
    # Show basic statistics
    cat("Mean estimated adult content:", round(mean(y_estimated), 3), "\n")
    cat("Mean true adult content:", round(mean(y_true), 3), "\n")
    cat("Correlation (estimated, true):", round(cor(y_estimated, y_true), 3), "\n")
    
    # Calculate row-level errors
    row_errors <- y_estimated - y_true
    cat("Mean absolute row error:", round(mean(abs(row_errors)), 4), "\n")
    cat("Max absolute row error:", round(max(abs(row_errors)), 4), "\n")
    
    # Error correlations with user characteristics
    age_error_cor <- cor(row_errors, age)
    tech_error_cor <- cor(row_errors, tech_savvy)
    
    cat("Error-age correlation:", round(age_error_cor, 4), "\n")
    cat("Error-tech correlation:", round(tech_error_cor, 4), "\n")
    
    # Regression on estimated data
    lm_estimated <- lm(y_estimated ~ age + tech_savvy)
    beta_estimated <- coef(lm_estimated)
    
    # Calculate bias
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * bias / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    
    cat("\nREGRESSION RESULTS:\n")
    cat("True age coefficient:", round(beta_true[2], 4), "\n")
    cat("Estimated age coefficient:", round(beta_estimated[2], 4), "\n")
    cat("Age coefficient bias:", round(pct_bias[2], 1), "%\n")
    
    cat("True tech coefficient:", round(beta_true[3], 4), "\n")
    cat("Estimated tech coefficient:", round(beta_estimated[3], 4), "\n")
    cat("Tech coefficient bias:", round(pct_bias[3], 1), "%\n")
    
    max_bias <- max(abs(pct_bias))
    cat("Max coefficient bias:", round(max_bias, 1), "%\n")
    
    # Assessment
    if (max_bias < 15) {
      status <- "✅ GOOD"
    } else if (max_bias < 50) {
      status <- "⚠️ MODERATE"
    } else {
      status <- "❌ POOR"
    }
    
    cat("Assessment:", status, "\n")
    
    results[[method_name]] <- list(
      domains = length(selected_domains),
      mean_row_error = mean(abs(row_errors)),
      max_row_error = max(abs(row_errors)),
      age_error_cor = age_error_cor,
      tech_error_cor = tech_error_cor,
      max_bias = max_bias,
      status = status
    )
  }
  
  # Summary
  cat("\n", rep("=", 60), "\n")
  cat("SANITY CHECK SUMMARY\n")
  cat(rep("=", 60), "\n")
  
  cat(sprintf("%-12s %8s %12s %12s %10s\n", 
              "Method", "Domains", "MeanRowErr", "MaxCoefBias%", "Status"))
  cat(rep("-", 60), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-12s %8d %12.4f %12.1f %10s\n",
                name, r$domains, r$mean_row_error, r$max_bias, r$status))
  }
  
  cat("\n💡 KEY INSIGHTS:\n")
  cat("- Are row errors really that small?\n")
  cat("- Do error correlations make sense?\n") 
  cat("- Is the bias amplification mechanism working as expected?\n")
  
  # Show which domains each method selects
  cat("\n📋 DOMAIN SELECTION DETAILS:\n")
  for (method_name in names(methods)) {
    if (method_name == "All_Domains") next
    
    selected <- methods[[method_name]]()
    adult_selected <- intersect(selected, adult_domains)
    
    cat(method_name, ": selected", length(selected), "domains\n")
    cat("  Adult domains selected:", length(adult_selected), "/", length(adult_domains), 
        "(", round(100 * length(adult_selected) / length(adult_domains), 1), "%)\n")
    cat("  Adult domains:", paste(adult_selected, collapse = ", "), "\n")
  }
  
  return(results)
}

# Run sanity check
if (!interactive()) {
  cat("🚀 RUNNING SANITY CHECK\n\n") 
  results <- test_simple_methods()
  
  cat("\n✅ SANITY CHECK COMPLETE!\n")
  cat("This should help us understand if the dramatic bias is real or an artifact.\n")
}