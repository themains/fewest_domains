# Fast Multi-Objective Domain Selection
# Directly incorporates regression bias constraints into optimization

source("synthetic_data.R")
source("main_methods_interface.R")

# Fast greedy method with regression bias penalty
greedy_regression_aware <- function(cij, X_matrix, target_se, bias_penalty = 5, verbose = TRUE) {
  
  if (verbose) {
    cat("=== GREEDY REGRESSION-AWARE METHOD ===\n")
    cat("Target SE:", target_se, "| Bias penalty:", bias_penalty, "\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Identify adult domains (simple proxy for now)
  adult_domains <- sample(1:m, max(1, round(0.1 * m)))
  
  # Calculate true proportions for bias reference
  y_true <- numeric(n)
  for (i in 1:n) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  # Key user characteristics for bias calculation
  user_chars <- c("age_std", "tech_savvy_std", "privacy_concern_std")
  available_chars <- intersect(user_chars, colnames(X_matrix))
  
  selected_domains <- c()
  
  # Greedy selection with combined scoring
  for (iteration in 1:100) {
    
    # Check if precision constraints are met
    all_satisfied <- TRUE
    unsatisfied_users <- c()
    
    for (i in 1:n) {
      if (sum(cij[i, ]) == 0) next
      
      if (length(selected_domains) == 0) {
        eff_sample <- 0
      } else {
        visit_props <- cij[i, ] / sum(cij[i, ])
        coverage <- sum(visit_props[selected_domains])
        eff_sample <- coverage * length(selected_domains)
      }
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        unsatisfied_users <- c(unsatisfied_users, i)
      }
    }
    
    if (all_satisfied && length(selected_domains) > 10) {
      # Check if regression bias is acceptable
      y_estimated <- numeric(n)
      
      for (i in 1:n) {
        visits_selected <- sum(cij[i, selected_domains])
        
        if (visits_selected > 0) {
          adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
          y_estimated[i] <- adult_visits_selected / visits_selected
        } else {
          y_estimated[i] <- 0
        }
      }
      
      # Calculate bias score (correlation between errors and user characteristics)
      errors <- y_estimated - y_true
      total_error_correlation <- 0
      
      for (char in available_chars) {
        tryCatch({
          cor_val <- abs(cor(errors, X_matrix[, char], use = "complete.obs"))
          if (!is.na(cor_val)) {
            total_error_correlation <- total_error_correlation + cor_val
          }
        }, error = function(e) {})
      }
      
      avg_error_correlation <- total_error_correlation / max(1, length(available_chars))
      
      # If bias is low enough, stop
      if (avg_error_correlation < 0.15) {
        if (verbose) {
          cat("GOOD SOLUTION: domains =", length(selected_domains), 
              ", avg error correlation =", round(avg_error_correlation, 3), "\n")
        }
        break
      }
    }
    
    # Find best domain to add
    candidates <- setdiff(1:m, selected_domains)
    if (length(candidates) == 0) break
    
    best_score <- -Inf
    best_domain <- NA
    
    for (domain_j in candidates) {
      test_selection <- c(selected_domains, domain_j)
      
      # Score 1: Precision benefit
      precision_score <- 0
      for (i in unsatisfied_users) {
        if (cij[i, domain_j] > 0) {
          visit_props <- cij[i, ] / sum(cij[i, ])
          
          current_coverage <- if (length(selected_domains) == 0) 0 else sum(visit_props[selected_domains])
          current_eff <- current_coverage * length(selected_domains)
          
          new_coverage <- current_coverage + visit_props[domain_j]
          new_eff <- new_coverage * length(test_selection)
          
          improvement <- new_eff - current_eff
          precision_score <- precision_score + improvement
        }
      }
      
      # Score 2: Bias penalty (how much adding this domain increases error correlations)
      bias_penalty_score <- 0
      
      # Calculate what errors would be with this domain added
      y_test <- numeric(n)
      for (i in 1:n) {
        visits_selected <- sum(cij[i, test_selection])
        
        if (visits_selected > 0) {
          adult_visits_selected <- sum(cij[i, intersect(test_selection, adult_domains)])
          y_test[i] <- adult_visits_selected / visits_selected
        } else {
          y_test[i] <- 0
        }
      }
      
      test_errors <- y_test - y_true
      
      for (char in available_chars) {
        tryCatch({
          cor_val <- abs(cor(test_errors, X_matrix[, char], use = "complete.obs"))
          if (!is.na(cor_val)) {
            bias_penalty_score <- bias_penalty_score - cor_val  # Negative because high correlation is bad
          }
        }, error = function(e) {})
      }
      
      # Combined score
      combined_score <- precision_score + bias_penalty * bias_penalty_score
      
      if (combined_score > best_score) {
        best_score <- combined_score
        best_domain <- domain_j
      }
    }
    
    if (is.na(best_domain)) break
    selected_domains <- c(selected_domains, best_domain)
    
    if (verbose && length(selected_domains) %% 25 == 0) {
      cat("Iteration", iteration, ": selected", length(selected_domains), "domains\n")
    }
  }
  
  if (verbose) {
    cat("Final selection:", length(selected_domains), "domains\n")
  }
  
  return(list(
    method = "Greedy_Regression_Aware",
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Hybrid approach: start with good coverage, then optimize for bias
hybrid_coverage_bias <- function(cij, X_matrix, target_se, verbose = TRUE) {
  
  if (verbose) {
    cat("=== HYBRID COVERAGE-BIAS METHOD ===\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  # Adult domains for testing
  adult_domains <- sample(1:m, max(1, round(0.1 * m)))
  
  # True proportions
  y_true <- numeric(n)
  for (i in 1:n) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  # Phase 1: Get good coverage with popular domains
  domain_popularity <- colSums(cij > 0)
  selected_domains <- head(order(domain_popularity, decreasing = TRUE), 50)
  
  # Phase 2: Add domains to meet precision constraints
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
  
  # Phase 3: Try to reduce bias by swapping domains
  user_chars <- c("age_std", "tech_savvy_std", "privacy_concern_std") 
  available_chars <- intersect(user_chars, colnames(X_matrix))
  
  for (swap_iteration in 1:20) {
    
    # Calculate current bias
    y_current <- numeric(n)
    for (i in 1:n) {
      visits_selected <- sum(cij[i, selected_domains])
      
      if (visits_selected > 0) {
        adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
        y_current[i] <- adult_visits_selected / visits_selected
      } else {
        y_current[i] <- 0
      }
    }
    
    current_errors <- y_current - y_true
    current_bias_score <- 0
    
    for (char in available_chars) {
      tryCatch({
        cor_val <- abs(cor(current_errors, X_matrix[, char], use = "complete.obs"))
        if (!is.na(cor_val)) {
          current_bias_score <- current_bias_score + cor_val
        }
      }, error = function(e) {})
    }
    
    # Try swapping one domain for a better one
    best_improvement <- 0
    best_swap <- NULL
    
    for (remove_domain in sample(selected_domains, min(20, length(selected_domains)))) {
      candidates <- setdiff(1:m, selected_domains)
      
      for (add_domain in sample(candidates, min(20, length(candidates)))) {
        test_selection <- c(setdiff(selected_domains, remove_domain), add_domain)
        
        # Check if precision constraints still met
        precision_ok <- TRUE
        for (i in 1:n) {
          if (sum(cij[i, ]) == 0) next
          
          visit_props <- cij[i, ] / sum(cij[i, ])
          coverage <- sum(visit_props[test_selection])
          eff_sample <- coverage * length(test_selection)
          
          if (eff_sample < required_eff_sample) {
            precision_ok <- FALSE
            break
          }
        }
        
        if (!precision_ok) next
        
        # Calculate bias with this swap
        y_test <- numeric(n)
        for (i in 1:n) {
          visits_selected <- sum(cij[i, test_selection])
          
          if (visits_selected > 0) {
            adult_visits_selected <- sum(cij[i, intersect(test_selection, adult_domains)])
            y_test[i] <- adult_visits_selected / visits_selected
          } else {
            y_test[i] <- 0
          }
        }
        
        test_errors <- y_test - y_true
        test_bias_score <- 0
        
        for (char in available_chars) {
          tryCatch({
            cor_val <- abs(cor(test_errors, X_matrix[, char], use = "complete.obs"))
            if (!is.na(cor_val)) {
              test_bias_score <- test_bias_score + cor_val
            }
          }, error = function(e) {})
        }
        
        improvement <- current_bias_score - test_bias_score
        if (improvement > best_improvement) {
          best_improvement <- improvement
          best_swap <- list(remove = remove_domain, add = add_domain)
        }
      }
    }
    
    # Make the best swap if it improves bias
    if (best_improvement > 0.01) {
      selected_domains <- c(setdiff(selected_domains, best_swap$remove), best_swap$add)
      if (verbose) {
        cat("Swap iteration", swap_iteration, ": improved bias by", round(best_improvement, 3), "\n")
      }
    } else {
      break
    }
  }
  
  if (verbose) {
    cat("Final hybrid selection:", length(selected_domains), "domains\n")
  }
  
  return(list(
    method = "Hybrid_Coverage_Bias",
    selected_domains = selected_domains,
    n_domains = length(selected_domains)
  ))
}

# Test the fast multi-objective methods
test_fast_multi_objective <- function() {
  
  cat("⚡ TESTING FAST MULTI-OBJECTIVE METHODS\n")
  cat(rep("=", 70), "\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 200, n_domains = 300, seed = 2025)
  
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
    "Greedy_RA_1" = function() greedy_regression_aware(cij, X_matrix, 0.1, bias_penalty = 1, verbose = FALSE),
    "Greedy_RA_5" = function() greedy_regression_aware(cij, X_matrix, 0.1, bias_penalty = 5, verbose = FALSE),
    "Hybrid_CB" = function() hybrid_coverage_bias(cij, X_matrix, 0.1, verbose = FALSE),
    "Optimal_Baseline" = function() list(selected_domains = optimal_method_simple(cij, 0.1), method = "Optimal"),
    "Naive_Baseline" = function() list(selected_domains = naive_method_simple(cij, 0.1), method = "Naive")
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 50), "\n")
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
    
    # Regression analysis
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
    
    # Assessment
    excellent <- max_bias < 20 && f_pvalue > 0.05
    good <- max_bias < 35 && f_pvalue > 0.01
    
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
  cat("\n", rep("=", 70), "\n")
  cat("FAST MULTI-OBJECTIVE RESULTS\n")
  cat(rep("=", 70), "\n")
  
  cat(sprintf("%-15s %8s %10s %10s %10s %12s\n", 
              "Method", "Domains", "Effic%", "MaxBias%", "MeanBias%", "Status"))
  cat(rep("-", 70), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-15s %8d %10.1f %10.1f %10.1f %12s\n",
                name, r$domains, r$efficiency_pct, r$max_bias, r$mean_bias, r$status))
  }
  
  # Key insights
  cat("\n🔍 KEY FINDINGS:\n")
  
  valid_methods <- names(results)[sapply(results, function(x) x$excellent || x$good)]
  if (length(valid_methods) > 0) {
    cat("✅ Regression-valid methods:", paste(valid_methods, collapse = ", "), "\n")
    
    # Find most efficient valid method
    valid_results <- results[valid_methods]
    best_method <- names(valid_results)[which.min(sapply(valid_results, function(x) x$domains))]
    best_efficiency <- valid_results[[best_method]]$efficiency_pct
    
    cat("🏆 Most efficient valid method:", best_method, 
        "(", round(best_efficiency, 1), "% of domains)\n")
    
    # Compare to baselines
    if ("Optimal_Baseline" %in% names(results)) {
      optimal_efficiency <- results$Optimal_Baseline$efficiency_pct
      efficiency_cost <- best_efficiency / optimal_efficiency
      cat("📊 Efficiency cost vs Optimal:", round(efficiency_cost, 1), "x more domains\n")
    }
    
  } else {
    cat("❌ No regression-valid methods found - all methods introduce significant bias\n")
  }
  
  return(results)
}

# Run the test
if (!interactive()) {
  cat("🚀 TESTING FAST MULTI-OBJECTIVE OPTIMIZATION\n\n")
  results <- test_fast_multi_objective()
  
  cat("\n✅ FAST MULTI-OBJECTIVE TESTING COMPLETE!\n")
  cat("These methods directly incorporate regression bias into the objective function.\n")
}