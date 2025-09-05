# Interface to Main Domain Selection Methods
# Adapts the main methods for regression bias testing

# Simple implementations of main methods for testing
naive_method_simple <- function(cij, target_se) {
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  all_selected <- c()
  
  for (i in 1:n) {
    visit_props <- cij[i, ] / sum(cij[i, ])
    available_domains <- which(cij[i, ] > 0)
    
    if (length(available_domains) == 0) next
    
    selected <- c()
    current_eff_sample <- 0
    
    sorted_domains <- available_domains[order(visit_props[available_domains], decreasing = TRUE)]
    
    for (domain in sorted_domains) {
      selected <- c(selected, domain)
      coverage <- sum(visit_props[selected])
      current_eff_sample <- coverage * length(selected)
      
      if (current_eff_sample >= required_eff_sample) break
    }
    
    all_selected <- c(all_selected, selected)
  }
  
  return(unique(all_selected))
}

optimal_method_simple <- function(cij, target_se) {
  n <- nrow(cij)
  m <- ncol(cij)
  required_eff_sample <- 0.25 / (target_se^2)
  
  visit_props <- t(apply(cij, 1, function(x) x / sum(x)))
  selected_domains <- c()
  
  repeat {
    all_satisfied <- TRUE
    unsatisfied <- c()
    
    for (i in 1:n) {
      if (length(selected_domains) == 0) {
        eff_sample <- 0
      } else {
        coverage <- sum(visit_props[i, selected_domains])
        eff_sample <- coverage * length(selected_domains)
      }
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        unsatisfied <- c(unsatisfied, i)
      }
    }
    
    if (all_satisfied) break
    if (length(selected_domains) >= m) break
    
    best_benefit <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      benefit <- 0
      for (i in unsatisfied) {
        current_coverage <- if (length(selected_domains) == 0) 0 else sum(visit_props[i, selected_domains])
        current_eff <- current_coverage * length(selected_domains)
        
        new_coverage <- current_coverage + visit_props[i, j]
        new_eff <- new_coverage * (length(selected_domains) + 1)
        
        improvement <- new_eff - current_eff
        deficit <- required_eff_sample - current_eff
        benefit <- benefit + improvement * (1 + deficit/required_eff_sample)
      }
      
      if (benefit > best_benefit) {
        best_benefit <- benefit
        best_domain <- j
      }
    }
    
    if (is.na(best_domain)) break
    selected_domains <- c(selected_domains, best_domain)
  }
  
  return(selected_domains)
}

# Test main methods for regression bias
test_main_methods_regression <- function() {
  
  cat("🔥 TESTING MAIN METHODS FOR REGRESSION BIAS\n")
  cat(rep("=", 70), "\n")
  
  source("synthetic_data.R")
  
  # Generate challenging test data
  data <- generate_regression_data(n_respondents = 300, n_domains = 500, seed = 456)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  cat("Test Setup:\n")
  cat("- Respondents:", nrow(cij), "\n")
  cat("- Total domains:", ncol(cij), "\n")
  cat("- Adult domains:", length(adult_domains), "\n")
  cat("- Sparsity:", round(100 * data$characteristics$sparsity, 1), "%\n\n")
  
  # Ground truth regression
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("GROUND TRUTH COEFFICIENTS:\n")
  print(round(beta_true, 4))
  
  # Test methods
  methods <- list(
    "Naive" = function() naive_method_simple(cij, 0.1),
    "Optimal" = function() optimal_method_simple(cij, 0.1),
    "Random_100" = function() sample(1:ncol(cij), 100),
    "Popular_100" = function() head(order(colSums(cij), decreasing = TRUE), 100)
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 50), "\n")
    cat("METHOD:", method_name, "\n")
    
    # Apply selection
    selected_domains <- methods[[method_name]]()
    
    cat("Selected", length(selected_domains), "/", ncol(cij), "domains\n")
    
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
    
    # Run regression
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
    problematic <- f_pvalue < 0.05 || max_abs_bias > 20
    
    cat("Max |coefficient bias|:", round(max_abs_bias, 1), "%\n")
    cat("Error-covariate correlation p:", round(f_pvalue, 6), "\n")
    cat("Assessment:", ifelse(problematic, "❌ PROBLEMATIC", "✅ ACCEPTABLE"), "\n")
    
    results[[method_name]] <- list(
      domains_selected = length(selected_domains),
      efficiency = 100 * length(selected_domains) / ncol(cij),
      max_bias_pct = max_abs_bias,
      error_corr_p = f_pvalue,
      problematic = problematic,
      beta_bias = bias,
      pct_bias = pct_bias
    )
  }
  
  # Summary table
  cat("\n", rep("=", 70), "\n")
  cat("MAIN METHODS REGRESSION BIAS SUMMARY\n")
  cat(rep("=", 70), "\n")
  
  cat(sprintf("%-12s %10s %12s %15s %12s\n", 
              "Method", "Domains", "Efficiency%", "MaxBias%", "Status"))
  cat(rep("-", 65), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    status <- ifelse(r$problematic, "❌ BIAS", "✅ OK")
    cat(sprintf("%-12s %10d %12.1f %15.1f %12s\n",
                name, r$domains_selected, r$efficiency, r$max_bias_pct, status))
  }
  
  # Key insights
  cat("\n💡 KEY INSIGHTS:\n")
  
  naive_ok <- !results$Naive$problematic
  optimal_ok <- !results$Optimal$problematic
  
  if (!optimal_ok) {
    cat("⚠️  OPTIMAL method introduces significant regression bias!\n")
    cat("   - This confirms our hypothesis about popular domain selection\n")
  }
  
  if (!naive_ok) {
    cat("⚠️  NAIVE method also problematic - needs improvement\n")
  }
  
  if (optimal_ok && naive_ok) {
    cat("✅ Current methods appear acceptable for regression analysis\n")
  } else {
    cat("🔧 NEED: Regression-aware domain selection methods\n")
  }
  
  return(results)
}

# Run the test
if (!interactive()) {
  results <- test_main_methods_regression()
}