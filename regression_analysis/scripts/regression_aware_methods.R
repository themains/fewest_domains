# Regression-Aware Domain Selection Methods
# Methods that optimize for both precision AND regression validity

source("synthetic_data.R")

# Stratified Domain Selection - ensures representativeness across user characteristics
stratified_regression_method <- function(cij, X_matrix, target_se, n_strata = 5, verbose = TRUE) {
  
  if (verbose) {
    cat("=== STRATIFIED REGRESSION-AWARE METHOD ===\n")
    cat("Target SE:", target_se, "| Strata:", n_strata, "\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Step 1: Create user strata based on key characteristics
  # Use first PC of X for stratification (captures main user heterogeneity)
  X_numeric <- X_matrix[, sapply(X_matrix, is.numeric)]
  if (ncol(X_numeric) > 1) {
    pc1 <- prcomp(X_numeric, scale = TRUE)$x[, 1]
  } else {
    pc1 <- X_numeric[, 1]
  }
  
  # Create balanced strata
  stratum_breaks <- quantile(pc1, probs = seq(0, 1, length.out = n_strata + 1))
  stratum_breaks[1] <- min(pc1) - 0.001  # Ensure all points included
  stratum_breaks[length(stratum_breaks)] <- max(pc1) + 0.001
  
  user_strata <- cut(pc1, breaks = stratum_breaks, include.lowest = TRUE, labels = FALSE)
  
  if (verbose) {
    strata_counts <- table(user_strata)
    cat("User strata sizes:", paste(strata_counts, collapse = ", "), "\n")
  }
  
  # Step 2: For each stratum, select domains that are representative
  selected_domains <- c()
  
  for (s in 1:n_strata) {
    stratum_users <- which(user_strata == s)
    stratum_cij <- cij[stratum_users, , drop = FALSE]
    
    if (length(stratum_users) == 0) next
    
    # Domain importance within this stratum
    stratum_domain_use <- colSums(stratum_cij > 0)  # How many users in stratum use each domain
    stratum_domain_visits <- colSums(stratum_cij)   # Total visits in stratum
    
    # Select domains that are well-represented in this stratum
    domain_scores <- stratum_domain_use * log(stratum_domain_visits + 1)
    
    # Select top domains for this stratum (ensure representation)
    n_domains_stratum <- max(5, round(20 / n_strata * length(stratum_users) / (n / n_strata)))
    stratum_domains <- head(order(domain_scores, decreasing = TRUE), n_domains_stratum)
    
    selected_domains <- c(selected_domains, stratum_domains)
  }
  
  # Step 3: Add globally important domains (core domains everyone needs)
  global_importance <- colSums(cij > 0)  # Used by how many people
  core_domains <- head(order(global_importance, decreasing = TRUE), 10)
  selected_domains <- c(selected_domains, core_domains)
  
  # Remove duplicates
  selected_domains <- unique(selected_domains)
  
  # Step 4: Check precision constraints and add more if needed
  max_iterations <- 50
  iteration <- 0
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    
    # Check if all users meet precision constraint
    all_satisfied <- TRUE
    unsatisfied_users <- c()
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next  # Skip users with no visits
      
      visit_props <- cij[i, ] / sum(cij[i, ])
      coverage <- sum(visit_props[selected_domains])
      eff_sample <- coverage * length(selected_domains)
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        unsatisfied_users <- c(unsatisfied_users, i)
      }
    }
    
    if (all_satisfied) break
    
    # Add domains that help unsatisfied users while maintaining balance
    if (length(unsatisfied_users) > 0) {
      # Find domains that help unsatisfied users across different strata
      candidate_domains <- setdiff(1:m, selected_domains)
      
      if (length(candidate_domains) == 0) break
      
      best_domain <- NA
      best_score <- 0
      
      for (domain in candidate_domains) {
        # Score based on helping unsatisfied users across strata
        score <- 0
        
        for (s in 1:n_strata) {
          stratum_unsatisfied <- intersect(unsatisfied_users, which(user_strata == s))
          if (length(stratum_unsatisfied) > 0) {
            help_in_stratum <- sum(cij[stratum_unsatisfied, domain] > 0)
            score <- score + help_in_stratum / max(1, length(stratum_unsatisfied))
          }
        }
        
        if (score > best_score) {
          best_score <- score
          best_domain <- domain
        }
      }
      
      if (!is.na(best_domain)) {
        selected_domains <- c(selected_domains, best_domain)
      } else {
        break
      }
    }
  }
  
  if (verbose) {
    cat("Selected", length(selected_domains), "domains after", iteration, "iterations\n")
  }
  
  return(list(
    method = "Stratified Regression-Aware",
    selected_domains = selected_domains,
    n_domains = length(selected_domains),
    strata_info = list(
      user_strata = user_strata,
      n_strata = n_strata
    )
  ))
}

# Balanced sampling method - explicitly balances user characteristics
balanced_regression_method <- function(cij, X_matrix, target_se, balance_vars = NULL, verbose = TRUE) {
  
  if (verbose) {
    cat("=== BALANCED REGRESSION-AWARE METHOD ===\n")
    cat("Target SE:", target_se, "\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Step 1: Identify key balance variables if not specified
  if (is.null(balance_vars)) {
    # Use variables that are likely to correlate with browsing patterns
    balance_vars <- c("age_std", "income_std", "tech_savvy_std", "privacy_concern_std")
    balance_vars <- intersect(balance_vars, colnames(X_matrix))
  }
  
  if (verbose) {
    cat("Balancing on variables:", paste(balance_vars, collapse = ", "), "\n")
  }
  
  # Step 2: Start with core domains (highest coverage)
  domain_coverage <- colSums(cij > 0) / n
  core_domains <- head(order(domain_coverage, decreasing = TRUE), 5)
  selected_domains <- core_domains
  
  # Step 3: Iteratively add domains while maintaining balance
  max_iterations <- 100
  
  for (iteration in 1:max_iterations) {
    # Check precision constraints
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
    
    # Find best domain to add that maintains balance
    candidate_domains <- setdiff(1:m, selected_domains)
    if (length(candidate_domains) == 0) break
    
    best_domain <- NA
    best_score <- -Inf
    
    for (domain in candidate_domains) {
      # Test adding this domain
      test_selection <- c(selected_domains, domain)
      
      # Calculate balance score
      balance_score <- calculate_balance_score(cij, X_matrix, test_selection, balance_vars)
      
      # Calculate utility (how much it helps with precision)
      utility_score <- 0
      for (i in 1:n) {
        if (sum(cij[i, ]) == 0) next
        if (cij[i, domain] > 0) {
          visit_props <- cij[i, ] / sum(cij[i, ])
          current_coverage <- sum(visit_props[selected_domains])
          new_coverage <- sum(visit_props[test_selection])
          utility_score <- utility_score + (new_coverage - current_coverage)
        }
      }
      
      # Combined score: balance + utility
      combined_score <- balance_score + 0.1 * utility_score  # Weight utility lower
      
      if (combined_score > best_score) {
        best_score <- combined_score
        best_domain <- domain
      }
    }
    
    if (!is.na(best_domain)) {
      selected_domains <- c(selected_domains, best_domain)
    } else {
      break
    }
    
    if (verbose && length(selected_domains) %% 20 == 0) {
      cat("Selected", length(selected_domains), "domains, iteration", iteration, "\n")
    }
  }
  
  if (verbose) {
    cat("Final selection:", length(selected_domains), "domains\n")
  }
  
  return(list(
    method = "Balanced Regression-Aware",
    selected_domains = selected_domains,
    n_domains = length(selected_domains),
    balance_vars = balance_vars
  ))
}

# Helper function to calculate balance score
calculate_balance_score <- function(cij, X_matrix, selected_domains, balance_vars) {
  # Calculate how "balanced" the selected domains are across user characteristics
  
  n <- nrow(cij)
  balance_score <- 0
  
  for (var in balance_vars) {
    if (!var %in% colnames(X_matrix)) next
    
    user_var <- X_matrix[, var]
    
    # For each user, calculate their representation in selected domains
    representation <- numeric(n)
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) {
        representation[i] <- 0
        next
      }
      
      total_visits <- sum(cij[i, ])
      selected_visits <- sum(cij[i, selected_domains])
      representation[i] <- selected_visits / total_visits
    }
    
    # Balance score: correlation between user characteristic and representation should be ~ 0
    if (length(unique(representation)) > 1 && length(unique(user_var)) > 1) {
      correlation <- abs(cor(user_var, representation, use = "complete.obs"))
      balance_score <- balance_score - correlation  # Lower correlation = higher balance score
    }
  }
  
  return(balance_score)
}

# Test regression-aware methods
test_regression_aware_methods <- function() {
  
  cat("🚀 TESTING REGRESSION-AWARE METHODS\n")
  cat(rep("=", 70), "\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 200, n_domains = 300, seed = 789)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  # Ground truth regression
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Test new methods
  methods <- list(
    "Stratified" = function() stratified_regression_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Balanced" = function() balanced_regression_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Optimal_Old" = function() list(selected_domains = optimal_method_simple(cij, 0.1), method = "Optimal_Old")
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 40), "\n")
    cat("METHOD:", method_name, "\n")
    
    # Apply method
    method_result <- methods[[method_name]]()
    selected_domains <- method_result$selected_domains
    
    cat("Selected", length(selected_domains), "domains\n")
    
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
    
    # Regression analysis
    lm_estimated <- lm(y_estimated ~ X_matrix[,-1])
    beta_estimated <- coef(lm_estimated)
    
    # Calculate metrics
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * bias / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    
    # Error correlation test
    selection_errors <- y_estimated - y_true
    error_lm <- lm(selection_errors ~ X_matrix[,-1])
    f_test <- summary(error_lm)$fstatistic
    
    if (!is.null(f_test)) {
      f_pvalue <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)
    } else {
      f_pvalue <- 1
    }
    
    max_abs_bias <- max(abs(pct_bias))
    
    cat("Max coefficient bias:", round(max_abs_bias, 1), "%\n")
    cat("Error correlation p-value:", round(f_pvalue, 4), "\n")
    
    # Assessment
    excellent <- max_abs_bias < 10 && f_pvalue > 0.05
    acceptable <- max_abs_bias < 25 && f_pvalue > 0.01
    
    if (excellent) {
      status <- "✅ EXCELLENT"
    } else if (acceptable) {
      status <- "👍 ACCEPTABLE" 
    } else {
      status <- "❌ PROBLEMATIC"
    }
    
    cat("Assessment:", status, "\n")
    
    results[[method_name]] <- list(
      domains_selected = length(selected_domains),
      efficiency = 100 * length(selected_domains) / ncol(cij),
      max_bias_pct = max_abs_bias,
      error_corr_p = f_pvalue,
      status = status,
      excellent = excellent,
      acceptable = acceptable
    )
  }
  
  # Summary
  cat("\n", rep("=", 70), "\n")
  cat("REGRESSION-AWARE METHODS COMPARISON\n")
  cat(rep("=", 70), "\n")
  
  cat(sprintf("%-15s %10s %12s %12s %15s\n", 
              "Method", "Domains", "Efficiency%", "MaxBias%", "Status"))
  cat(rep("-", 70), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-15s %10d %12.1f %12.1f %15s\n",
                name, r$domains_selected, r$efficiency, r$max_bias_pct, r$status))
  }
  
  return(results)
}

# Run tests
if (!interactive()) {
  source("main_methods_interface.R")  # For optimal_method_simple
  results <- test_regression_aware_methods()
  
  cat("\n🎯 NEXT: Compare efficiency vs regression validity trade-offs\n")
}