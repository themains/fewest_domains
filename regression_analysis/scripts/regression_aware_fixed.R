# Fixed Regression-Aware Methods
# Methods that optimize for both precision AND regression validity

source("synthetic_data.R")
source("main_methods_interface.R")

# Simplified stratified method
stratified_regression_method <- function(cij, X_matrix, target_se, n_strata = 3, verbose = TRUE) {
  
  if (verbose) {
    cat("=== STRATIFIED REGRESSION-AWARE METHOD ===\n")
    cat("Target SE:", target_se, "| Strata:", n_strata, "\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Step 1: Create user strata based on age (simple stratification)
  age_col <- which(grepl("age", colnames(X_matrix)))
  if (length(age_col) > 0) {
    age_values <- X_matrix[, age_col[1]]
    age_quantiles <- quantile(age_values, probs = seq(0, 1, length.out = n_strata + 1))
    user_strata <- cut(age_values, breaks = age_quantiles, include.lowest = TRUE, labels = FALSE)
  } else {
    # Fallback: random strata
    user_strata <- sample(1:n_strata, n, replace = TRUE)
  }
  
  if (verbose) {
    strata_counts <- table(user_strata)
    cat("User strata sizes:", paste(strata_counts, collapse = ", "), "\n")
  }
  
  # Step 2: Select representative domains from each stratum
  selected_domains <- c()
  
  for (s in 1:n_strata) {
    stratum_users <- which(user_strata == s)
    if (length(stratum_users) == 0) next
    
    stratum_cij <- cij[stratum_users, , drop = FALSE]
    
    # Find important domains in this stratum
    domain_usage <- colSums(stratum_cij > 0)  # Number of users
    domain_visits <- colSums(stratum_cij)     # Total visits
    
    # Score domains by usage and visits
    domain_scores <- domain_usage * log(domain_visits + 1)
    
    # Select top domains from this stratum
    n_select <- max(3, round(15 / n_strata))
    top_domains <- head(order(domain_scores, decreasing = TRUE), n_select)
    
    selected_domains <- c(selected_domains, top_domains)
  }
  
  # Step 3: Add core domains everyone uses
  global_usage <- colSums(cij > 0)
  core_domains <- head(order(global_usage, decreasing = TRUE), 10)
  selected_domains <- c(selected_domains, core_domains)
  
  # Remove duplicates
  selected_domains <- unique(selected_domains)
  
  # Step 4: Ensure precision constraints are met
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
          # Add highest-visit domain for this user
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
    method = "Stratified",
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Representative sampling method - ensures balanced representation
representative_method <- function(cij, X_matrix, target_se, verbose = TRUE) {
  
  if (verbose) {
    cat("=== REPRESENTATIVE SAMPLING METHOD ===\n")
    cat("Target SE:", target_se, "\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Step 1: Start with most representative domains
  # Use domains that have usage patterns uncorrelated with user characteristics
  
  tech_col <- which(grepl("tech", colnames(X_matrix)))
  privacy_col <- which(grepl("privacy", colnames(X_matrix)))
  age_col <- which(grepl("age", colnames(X_matrix)))
  
  selected_domains <- c()
  
  # Find domains with low correlation to user characteristics
  domain_correlations <- numeric(m)
  
  for (j in 1:m) {
    domain_usage <- (cij[, j] > 0) * 1  # Binary usage
    
    if (sum(domain_usage) < 5) {  # Skip rarely used domains
      domain_correlations[j] <- 1  # High correlation = avoid
      next
    }
    
    correlations <- numeric(0)
    
    # Check correlation with key user characteristics
    if (length(tech_col) > 0) {
      correlations <- c(correlations, abs(cor(domain_usage, X_matrix[, tech_col[1]], use = "complete.obs")))
    }
    if (length(privacy_col) > 0) {
      correlations <- c(correlations, abs(cor(domain_usage, X_matrix[, privacy_col[1]], use = "complete.obs")))
    }
    if (length(age_col) > 0) {
      correlations <- c(correlations, abs(cor(domain_usage, X_matrix[, age_col[1]], use = "complete.obs")))
    }
    
    domain_correlations[j] <- mean(correlations, na.rm = TRUE)
  }
  
  # Step 2: Select domains with low user-characteristic correlation first
  # But also consider popularity for coverage
  domain_popularity <- colSums(cij > 0)
  
  # Combined score: low correlation + high popularity
  domain_scores <- (1 - domain_correlations) * log(domain_popularity + 1)
  
  # Select initial set of representative domains
  initial_domains <- head(order(domain_scores, decreasing = TRUE), 20)
  selected_domains <- initial_domains
  
  # Step 3: Add domains as needed to meet precision constraints
  for (iteration in 1:100) {
    all_satisfied <- TRUE
    unsatisfied_users <- c()
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next
      
      visit_props <- cij[i, ] / sum(cij[i, ])
      coverage <- sum(visit_props[selected_domains])
      eff_sample <- coverage * length(selected_domains)
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        unsatisfied_users <- c(unsatisfied_users, i)
      }
    }
    
    if (all_satisfied) break
    
    # Add domain that helps most unsatisfied users with lowest bias
    candidate_domains <- setdiff(1:m, selected_domains)
    
    if (length(candidate_domains) == 0) break
    
    best_domain <- NA
    best_score <- -Inf
    
    for (domain in candidate_domains) {
      # How many unsatisfied users does this domain help?
      help_count <- sum(cij[unsatisfied_users, domain] > 0)
      
      # What's the bias risk (correlation with user characteristics)?
      bias_risk <- domain_correlations[domain]
      
      # Combined score
      score <- help_count - 5 * bias_risk  # Penalize bias risk
      
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
  
  if (verbose) {
    cat("Selected", length(selected_domains), "domains\n")
    avg_correlation <- mean(domain_correlations[selected_domains], na.rm = TRUE)
    cat("Average domain-user correlation:", round(avg_correlation, 3), "\n")
  }
  
  return(list(
    method = "Representative",
    selected_domains = selected_domains,
    n_domains = length(selected_domains),
    avg_correlation = mean(domain_correlations[selected_domains], na.rm = TRUE)
  ))
}

# Test all methods including new ones
comprehensive_regression_test <- function() {
  
  cat("🎯 COMPREHENSIVE REGRESSION VALIDITY TEST\n")
  cat(rep("=", 75), "\n")
  
  # Generate challenging test data
  data <- generate_regression_data(n_respondents = 250, n_domains = 400, seed = 2024)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  cat("Test setup: N =", nrow(cij), ", M =", ncol(cij), ", Adult domains =", length(adult_domains), "\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("\nGround truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Test methods
  methods <- list(
    "Optimal" = function() list(selected_domains = optimal_method_simple(cij, 0.1)),
    "Naive" = function() list(selected_domains = naive_method_simple(cij, 0.1)),
    "Stratified" = function() stratified_regression_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Representative" = function() representative_method(cij, X_matrix, 0.1, verbose = FALSE),
    "Random_150" = function() list(selected_domains = sample(1:ncol(cij), 150))
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 40), "\n")
    cat("TESTING:", method_name, "\n")
    
    # Apply method
    result <- methods[[method_name]]()
    selected_domains <- result$selected_domains
    
    cat("Domains:", length(selected_domains), "/", ncol(cij), 
        "(", round(100 * length(selected_domains) / ncol(cij), 1), "%)\n")
    
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
    
    # Metrics
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * abs(bias) / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    
    max_bias <- max(pct_bias)
    mean_bias <- mean(pct_bias)
    
    # Error correlation
    selection_errors <- y_estimated - y_true
    error_lm <- lm(selection_errors ~ X_matrix[,-1])
    f_test <- summary(error_lm)$fstatistic
    
    if (!is.null(f_test)) {
      f_pvalue <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)
    } else {
      f_pvalue <- 1
    }
    
    # Status
    excellent <- max_bias < 15 && f_pvalue > 0.05
    good <- max_bias < 30 && f_pvalue > 0.01
    
    if (excellent) {
      status <- "✅ EXCELLENT"
    } else if (good) {
      status <- "👍 GOOD"
    } else {
      status <- "❌ POOR"
    }
    
    cat("Max bias:", round(max_bias, 1), "% | Mean bias:", round(mean_bias, 1), "%\n")
    cat("Error correlation p-value:", round(f_pvalue, 4), "\n")
    cat("Status:", status, "\n")
    
    results[[method_name]] <- list(
      domains = length(selected_domains),
      efficiency = 100 * length(selected_domains) / ncol(cij),
      max_bias = max_bias,
      mean_bias = mean_bias,
      error_p = f_pvalue,
      status = status,
      excellent = excellent,
      good = good
    )
  }
  
  # Summary table
  cat("\n", rep("=", 75), "\n")
  cat("COMPREHENSIVE RESULTS\n")
  cat(rep("=", 75), "\n")
  
  cat(sprintf("%-15s %8s %10s %10s %10s %12s\n", 
              "Method", "Domains", "Effic%", "MaxBias%", "MeanBias%", "Status"))
  cat(rep("-", 75), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-15s %8d %10.1f %10.1f %10.1f %12s\n",
                name, r$domains, r$efficiency, r$max_bias, r$mean_bias, r$status))
  }
  
  # Insights
  cat("\n🔍 KEY INSIGHTS:\n")
  
  good_methods <- names(results)[sapply(results, function(x) x$excellent || x$good)]
  bad_methods <- names(results)[sapply(results, function(x) !x$excellent && !x$good)]
  
  if (length(good_methods) > 0) {
    cat("✅ Regression-valid methods:", paste(good_methods, collapse = ", "), "\n")
  }
  
  if (length(bad_methods) > 0) {
    cat("❌ Biased methods:", paste(bad_methods, collapse = ", "), "\n")
  }
  
  # Efficiency vs validity trade-off
  valid_results <- results[good_methods]
  if (length(valid_results) > 0) {
    best_efficiency <- names(valid_results)[which.min(sapply(valid_results, function(x) x$domains))]
    cat("🏆 Most efficient valid method:", best_efficiency, 
        "(", valid_results[[best_efficiency]]$domains, "domains )\n")
  }
  
  return(results)
}

# Run comprehensive test
if (!interactive()) {
  cat("🚀 STARTING COMPREHENSIVE REGRESSION-AWARE EVALUATION\n\n")
  results <- comprehensive_regression_test()
  
  cat("\n✅ EVALUATION COMPLETE!\n")
  cat("Regression-aware methods developed and tested.\n")
}