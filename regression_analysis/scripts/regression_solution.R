# Regression-Valid Domain Selection Solution
# A separate approach that preserves coefficient validity

source("synthetic_data.R")
source("main_methods_interface.R")

# Method 1: Balanced Representative Sampling
balanced_representative_method <- function(cij, X_matrix, target_se, verbose = TRUE) {
  
  if (verbose) {
    cat("=== BALANCED REPRESENTATIVE METHOD ===\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Step 1: Find domains with minimal correlation to user characteristics
  user_chars <- c("age_std", "tech_savvy_std", "privacy_concern_std")
  available_chars <- intersect(user_chars, colnames(X_matrix))
  
  domain_bias_scores <- numeric(m)
  
  for (j in 1:m) {
    domain_usage <- (cij[, j] > 0) * 1
    
    if (sum(domain_usage) < 10) {
      domain_bias_scores[j] <- 1  # High bias score = avoid
      next
    }
    
    correlations <- numeric(0)
    for (char in available_chars) {
      cor_val <- abs(cor(domain_usage, X_matrix[, char], use = "complete.obs"))
      if (!is.na(cor_val)) correlations <- c(correlations, cor_val)
    }
    
    domain_bias_scores[j] <- mean(correlations, na.rm = TRUE)
  }
  
  # Step 2: Select domains with low bias scores and good coverage
  domain_coverage <- colSums(cij > 0) / n
  domain_utility <- domain_coverage * (1 - domain_bias_scores)
  
  # Start with top unbiased domains
  selected_domains <- head(order(domain_utility, decreasing = TRUE), 30)
  
  # Step 3: Add domains to meet precision constraints
  for (iteration in 1:100) {
    all_satisfied <- TRUE
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next
      
      visit_props <- cij[i, ] / sum(cij[i, ])
      coverage <- sum(visit_props[selected_domains])
      eff_sample <- coverage * length(selected_domains)
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        break
      }
    }
    
    if (all_satisfied) break
    
    # Add domain with best utility (low bias + helps precision)
    candidates <- setdiff(1:m, selected_domains)
    if (length(candidates) == 0) break
    
    best_candidate <- candidates[which.max(domain_utility[candidates])]
    selected_domains <- c(selected_domains, best_candidate)
  }
  
  if (verbose) {
    avg_bias <- mean(domain_bias_scores[selected_domains], na.rm = TRUE)
    cat("Selected", length(selected_domains), "domains\n")
    cat("Average domain-user correlation:", round(avg_bias, 3), "\n")
  }
  
  return(list(
    method = "Balanced_Representative",
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Method 2: Stratified Balanced Sampling  
stratified_balanced_method <- function(cij, X_matrix, target_se, verbose = TRUE) {
  
  if (verbose) {
    cat("=== STRATIFIED BALANCED METHOD ===\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Create balanced strata using age (main driver of browsing differences)
  age_col <- which(grepl("age", colnames(X_matrix)))[1]
  if (length(age_col) > 0) {
    age_values <- X_matrix[, age_col]
    strata <- cut(age_values, breaks = 5, labels = FALSE)
  } else {
    strata <- sample(1:5, n, replace = TRUE)
  }
  
  selected_domains <- c()
  
  # Select representative domains from each stratum
  for (s in 1:5) {
    stratum_users <- which(strata == s)
    if (length(stratum_users) == 0) next
    
    stratum_cij <- cij[stratum_users, , drop = FALSE]
    
    # Find commonly used domains in this stratum
    stratum_usage <- colSums(stratum_cij > 0)
    stratum_volume <- colSums(stratum_cij)
    
    # Score domains by usage in this stratum
    stratum_scores <- stratum_usage * log(stratum_volume + 1)
    
    # Select top domains from this stratum
    n_select <- max(8, round(40 / 5 * length(stratum_users) / n))
    stratum_domains <- head(order(stratum_scores, decreasing = TRUE), n_select)
    
    selected_domains <- c(selected_domains, stratum_domains)
  }
  
  # Add global popular domains for coverage
  global_usage <- colSums(cij > 0)
  top_global <- head(order(global_usage, decreasing = TRUE), 15)
  selected_domains <- c(selected_domains, top_global)
  
  selected_domains <- unique(selected_domains)
  
  # Ensure precision constraints
  for (iteration in 1:50) {
    all_satisfied <- TRUE
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next
      
      visit_props <- cij[i, ] / sum(cij[i, ])
      coverage <- sum(visit_props[selected_domains])
      eff_sample <- coverage * length(selected_domains)
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        
        # Add domain that helps this user
        available <- which(cij[i, ] > 0)
        unselected <- setdiff(available, selected_domains)
        
        if (length(unselected) > 0) {
          best_domain <- unselected[which.max(cij[i, unselected])]
          selected_domains <- c(selected_domains, best_domain)
        }
        break
      }
    }
    
    if (all_satisfied) break
  }
  
  if (verbose) {
    cat("Selected", length(selected_domains), "domains\n")
  }
  
  return(list(
    method = "Stratified_Balanced", 
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Method 3: Coverage-First Approach
coverage_first_method <- function(cij, X_matrix, target_se, verbose = TRUE) {
  
  if (verbose) {
    cat("=== COVERAGE-FIRST METHOD ===\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Start with domains that provide broad coverage across all users
  user_coverage <- rowSums(cij > 0)  # How many domains each user visits
  
  # Find domains that are used by users with different characteristics
  selected_domains <- c()
  
  # Step 1: Find domains with good coverage across user types
  for (j in 1:m) {
    domain_users <- which(cij[, j] > 0)
    if (length(domain_users) < 20) next  # Skip rarely used domains
    
    # Check if this domain is used across different user types
    # (simple heuristic: used by both high and low values of key characteristics)
    user_chars <- c("age_std", "tech_savvy_std") 
    available_chars <- intersect(user_chars, colnames(X_matrix))
    
    diverse_usage <- TRUE
    for (char in available_chars) {
      char_values <- X_matrix[domain_users, char]
      if (length(char_values) > 10) {
        # Check if domain is used across the range of this characteristic
        range_coverage <- (max(char_values) - min(char_values)) / 
                         (max(X_matrix[, char]) - min(X_matrix[, char]))
        if (range_coverage < 0.5) {
          diverse_usage <- FALSE
          break
        }
      }
    }
    
    if (diverse_usage) {
      selected_domains <- c(selected_domains, j)
    }
  }
  
  # If we don't have enough diverse domains, add popular ones
  if (length(selected_domains) < 30) {
    domain_popularity <- colSums(cij > 0)
    top_popular <- head(order(domain_popularity, decreasing = TRUE), 50)
    selected_domains <- unique(c(selected_domains, top_popular))
  }
  
  # Step 2: Ensure precision constraints
  for (iteration in 1:100) {
    all_satisfied <- TRUE
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next
      
      visit_props <- cij[i, ] / sum(cij[i, ])
      coverage <- sum(visit_props[selected_domains])
      eff_sample <- coverage * length(selected_domains)
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        
        # Add domain that helps this user
        available <- which(cij[i, ] > 0)
        unselected <- setdiff(available, selected_domains)
        
        if (length(unselected) > 0) {
          # Add the most popular unselected domain for this user
          best_domain <- unselected[which.max(cij[i, unselected])]
          selected_domains <- c(selected_domains, best_domain)
        }
        break
      }
    }
    
    if (all_satisfied) break
  }
  
  if (verbose) {
    cat("Selected", length(selected_domains), "domains\n")
  }
  
  return(list(
    method = "Coverage_First",
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Comprehensive test of regression-valid methods
test_regression_valid_methods <- function() {
  
  cat("🎯 TESTING REGRESSION-VALID DOMAIN SELECTION METHODS\n")
  cat(rep("=", 80), "\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 300, n_domains = 400, seed = 2025)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  cat("Test setup:\n")
  cat("- Respondents:", nrow(cij), "\n")
  cat("- Domains:", ncol(cij), "\n") 
  cat("- Adult domains:", length(adult_domains), "\n\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Test methods
  methods <- list(
    "Balanced_Rep" = function() balanced_representative_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Stratified_Bal" = function() stratified_balanced_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Coverage_First" = function() coverage_first_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Naive_Baseline" = function() list(selected_domains = naive_method_simple(cij, 0.1), method = "Naive"),
    "Optimal_Baseline" = function() list(selected_domains = optimal_method_simple(cij, 0.1), method = "Optimal")
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 60), "\n")
    cat("TESTING:", method_name, "\n")
    
    # Apply method
    result <- methods[[method_name]]()
    selected_domains <- result$selected_domains
    
    cat("Selected", length(selected_domains), "domains (", 
        round(100 * length(selected_domains) / ncol(cij), 1), "% of total)\n")
    
    # Estimate proportions
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
    
    # Regression
    lm_estimated <- lm(y_estimated ~ X_matrix[,-1])
    beta_estimated <- coef(lm_estimated)
    
    # Bias metrics
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * abs(bias) / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    
    max_bias <- max(pct_bias)
    mean_bias <- mean(pct_bias)
    
    # Error correlation test
    selection_errors <- y_estimated - y_true
    error_lm <- lm(selection_errors ~ X_matrix[,-1])
    f_test <- summary(error_lm)$fstatistic
    
    if (!is.null(f_test)) {
      f_pvalue <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)
    } else {
      f_pvalue <- 1
    }
    
    # Assessment
    excellent <- max_bias < 20 && f_pvalue > 0.05
    good <- max_bias < 50 && f_pvalue > 0.01
    
    if (excellent) {
      status <- "✅ EXCELLENT"
    } else if (good) {
      status <- "👍 GOOD"
    } else {
      status <- "❌ POOR"
    }
    
    cat("Max coefficient bias:", round(max_bias, 1), "%\n")
    cat("Mean coefficient bias:", round(mean_bias, 1), "%\n")
    cat("Error correlation p-value:", round(f_pvalue, 4), "\n")
    cat("Assessment:", status, "\n")
    
    results[[method_name]] <- list(
      domains = length(selected_domains),
      efficiency_pct = 100 * length(selected_domains) / ncol(cij),
      max_bias = max_bias,
      mean_bias = mean_bias,
      error_p = f_pvalue,
      status = status,
      excellent = excellent,
      good = good
    )
  }
  
  # Summary table
  cat("\n", rep("=", 80), "\n")
  cat("REGRESSION-VALID METHODS COMPARISON\n")
  cat(rep("=", 80), "\n")
  
  cat(sprintf("%-18s %8s %10s %10s %10s %12s\n", 
              "Method", "Domains", "Effic%", "MaxBias%", "MeanBias%", "Status"))
  cat(rep("-", 80), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-18s %8d %10.1f %10.1f %10.1f %12s\n",
                name, r$domains, r$efficiency_pct, r$max_bias, r$mean_bias, r$status))
  }
  
  # Key insights
  cat("\n🔍 KEY INSIGHTS:\n")
  
  valid_methods <- names(results)[sapply(results, function(x) x$excellent || x$good)]
  if (length(valid_methods) > 0) {
    cat("✅ Regression-valid methods:", paste(valid_methods, collapse = ", "), "\n")
    
    # Find most efficient valid method
    valid_results <- results[valid_methods]
    best_method <- names(valid_results)[which.min(sapply(valid_results, function(x) x$domains))]
    best_domains <- valid_results[[best_method]]$domains
    best_efficiency <- valid_results[[best_method]]$efficiency_pct
    
    cat("🏆 Most efficient valid method:", best_method, 
        "(", best_domains, "domains,", round(best_efficiency, 1), "% of total)\n")
    
    # Compare to baselines
    if ("Optimal_Baseline" %in% names(results)) {
      optimal_domains <- results$Optimal_Baseline$domains
      efficiency_ratio <- best_domains / optimal_domains
      cat("📊 Efficiency cost vs Optimal:", round(efficiency_ratio, 1), "x more domains\n")
    }
    
  } else {
    cat("❌ No regression-valid methods found - need further development\n")
  }
  
  return(results)
}

# Run the test
if (!interactive()) {
  cat("🚀 TESTING REGRESSION-VALID DOMAIN SELECTION SOLUTION\n\n")
  results <- test_regression_valid_methods()
  
  cat("\n✅ REGRESSION-VALID SOLUTION TESTING COMPLETE!\n")
  cat("Now you have methods specifically designed for regression analysis.\n")
}