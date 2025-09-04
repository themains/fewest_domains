# Comparison of Naive vs Optimal Domain Selection
# Problem: Find fewest domains to code across all respondents to achieve SE < target

# Generate realistic browsing data with heavy skew
generate_browsing_data <- function(n_respondents, n_domains, seed = 42) {
  set.seed(seed)
  
  # Create shared popular domains (Zipf distribution)
  domain_popularity <- (1:n_domains)^(-1.2)  # Heavy tail
  domain_popularity <- domain_popularity / sum(domain_popularity)
  
  # Generate visit data
  cij <- matrix(0, n_respondents, n_domains)
  
  for (i in 1:n_respondents) {
    # Each person has 80% visits to top domains + 20% idiosyncratic
    total_visits <- sample(800:1200, 1)  # Variable activity levels
    
    # Popular domains (shared across users)
    popular_visits <- rmultinom(1, round(0.8 * total_visits), domain_popularity^2)
    
    # Idiosyncratic tail (personalized long tail)
    personal_probs <- domain_popularity * runif(n_domains, 0.1, 2.0)
    personal_probs <- personal_probs / sum(personal_probs)
    idio_visits <- rmultinom(1, total_visits - sum(popular_visits), personal_probs)
    
    cij[i, ] <- as.numeric(popular_visits + idio_visits)
  }
  
  return(cij)
}

# Naive approach: weighted random sampling per respondent
naive_solution <- function(cij, target_se, n_trials = 100) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Calculate required sample size per respondent
  # SE = sqrt(p*(1-p)/n) <= target_se, worst case p=0.5
  min_sample_size <- ceiling(0.25 / target_se^2)
  
  cat("Naive approach: sampling", min_sample_size, "domains per respondent\n")
  
  # For each respondent, sample domains proportional to visit frequency
  all_domains_used <- c()
  respondent_domains <- list()
  
  for (i in 1:n) {
    visit_probs <- cij[i, ] / sum(cij[i, ])
    
    # Sample without replacement, weighted by visits
    selected <- sample(1:m, min_sample_size, prob = visit_probs, replace = FALSE)
    respondent_domains[[i]] <- selected
    all_domains_used <- c(all_domains_used, selected)
  }
  
  unique_domains <- unique(all_domains_used)
  
  return(list(
    method = "Naive (per-respondent sampling)",
    total_unique_domains = length(unique_domains),
    unique_domains = unique_domains,
    respondent_domains = respondent_domains,
    domains_per_respondent = min_sample_size
  ))
}

# Optimal approach: minimize total unique domains across all respondents
optimal_solution <- function(cij, target_se) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Required effective sample size per respondent
  min_sample_size <- 0.25 / target_se^2
  
  cat("Optimal approach: ensuring", min_sample_size, "effective domains per respondent\n")
  
  # Convert visits to proportions (frequency weights)
  visit_props <- t(apply(cij, 1, function(x) x / sum(x)))
  
  # Greedy selection: pick domains that help most respondents simultaneously  
  selected_domains <- c()
  
  # Key insight: we need each respondent to have effective sample >= min_sample_size
  # Effective sample = sum(visit_prop[i,j]) for selected domains j
  # We want the minimum total domains that satisfy this for ALL respondents
  
  repeat {
    # Check if current selection satisfies all respondents
    all_satisfied <- TRUE
    for (i in 1:n) {
      if (length(selected_domains) == 0) {
        eff_sample <- 0
      } else {
        eff_sample <- sum(visit_props[i, selected_domains])
      }
      se <- sqrt(0.25 / eff_sample) if eff_sample > 0 else Inf
      if (se > target_se) {
        all_satisfied <- FALSE
        break
      }
    }
    
    if (all_satisfied) break
    if (length(selected_domains) >= m) break
    
    # Find domain that helps the most "unsatisfied" respondents
    best_score <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      # Count how many currently unsatisfied respondents this would help
      score <- 0
      for (i in 1:n) {
        current_eff <- if (length(selected_domains) == 0) 0 else sum(visit_props[i, selected_domains])
        current_se <- sqrt(0.25 / current_eff) if current_eff > 0 else Inf
        
        # If respondent i is not satisfied
        if (current_se > target_se) {
          new_eff <- current_eff + visit_props[i, j]
          new_se <- sqrt(0.25 / new_eff)
          # Score based on SE improvement and visit frequency
          improvement <- max(0, current_se - new_se)  
          score <- score + improvement * visit_props[i, j]
        }
      }
      
      if (score > best_score) {
        best_score <- score
        best_domain <- j
      }
    }
    
    if (is.na(best_domain)) {
      cat("Warning: No beneficial domain found\n")
      break
    }
    
    selected_domains <- c(selected_domains, best_domain)
    
    if (length(selected_domains) %% 10 == 0) {
      # Count unsatisfied respondents
      unsatisfied <- 0
      for (i in 1:n) {
        eff_sample <- sum(visit_props[i, selected_domains])
        se <- sqrt(0.25 / eff_sample)
        if (se > target_se) unsatisfied <- unsatisfied + 1
      }
      cat("Selected", length(selected_domains), "domains,", unsatisfied, "respondents still unsatisfied\n")
    }
  }
  
  return(list(
    method = "Optimal (shared domain selection)",
    total_unique_domains = length(selected_domains),
    unique_domains = selected_domains,
    selected_domains = selected_domains
  ))
}

# Validation function
validate_solution <- function(cij, selected_domains, target_se) {
  n <- nrow(cij)
  visit_props <- t(apply(cij, 1, function(x) x / sum(x)))
  
  results <- data.frame(
    respondent = 1:n,
    coverage = numeric(n),
    effective_sample = numeric(n),
    standard_error = numeric(n),
    meets_target = logical(n)
  )
  
  for (i in 1:n) {
    # Coverage is sum of visit proportions to selected domains
    coverage <- sum(visit_props[i, selected_domains])
    
    # Effective sample size (frequency-weighted)
    eff_sample <- coverage * length(selected_domains)
    
    # Standard error (worst case p=0.5)
    se <- sqrt(0.25 / eff_sample)
    
    results$coverage[i] <- coverage
    results$effective_sample[i] <- eff_sample
    results$standard_error[i] <- se
    results$meets_target[i] <- se <= target_se
  }
  
  return(results)
}

# Main comparison
cat("=== DOMAIN SELECTION COMPARISON ===\n\n")

# Generate realistic data
n_respondents <- 20
n_domains <- 500
target_se <- 0.1

cat("Generating browsing data:", n_respondents, "respondents,", n_domains, "domains\n")
cij <- generate_browsing_data(n_respondents, n_domains)

# Analyze data characteristics
cat("\nData characteristics:\n")
total_visits_per_domain <- colSums(cij)
cat("- Top 10 domains capture", 
    round(100 * sum(head(sort(total_visits_per_domain, decreasing = TRUE), 10)) / sum(total_visits_per_domain), 1),
    "% of all visits\n")
cat("- Top 50 domains capture", 
    round(100 * sum(head(sort(total_visits_per_domain, decreasing = TRUE), 50)) / sum(total_visits_per_domain), 1),
    "% of all visits\n")

# Run both approaches
cat("\n" , rep("=", 50), "\n")
cat("NAIVE APPROACH\n")
cat(rep("=", 50), "\n")
naive_result <- naive_solution(cij, target_se)

cat("\n", rep("=", 50), "\n") 
cat("OPTIMAL APPROACH\n")
cat(rep("=", 50), "\n")
optimal_result <- optimal_solution(cij, target_se)

# Compare results
cat("\n", rep("=", 60), "\n")
cat("COMPARISON RESULTS\n")
cat(rep("=", 60), "\n")

cat("Naive approach:\n")
cat("- Total unique domains needed:", naive_result$total_unique_domains, "\n")
cat("- Domains per respondent:", naive_result$domains_per_respondent, "\n")
cat("- Total domain-coding cost:", naive_result$total_unique_domains, "\n")

cat("\nOptimal approach:\n") 
cat("- Total unique domains needed:", optimal_result$total_unique_domains, "\n")
cat("- Total domain-coding cost:", optimal_result$total_unique_domains, "\n")

improvement <- naive_result$total_unique_domains - optimal_result$total_unique_domains
improvement_pct <- round(100 * improvement / naive_result$total_unique_domains, 1)
cat("\nImprovement:\n")
cat("- Domains saved:", improvement, "\n")
cat("- Percentage reduction:", improvement_pct, "%\n")

# Validate both solutions
cat("\n", rep("=", 50), "\n")
cat("VALIDATION\n") 
cat(rep("=", 50), "\n")

naive_validation <- validate_solution(cij, naive_result$unique_domains, target_se)
optimal_validation <- validate_solution(cij, optimal_result$unique_domains, target_se)

cat("Naive approach validation:\n")
cat("- Respondents meeting target SE:", sum(naive_validation$meets_target), "/", n_respondents, "\n")
cat("- Average SE:", round(mean(naive_validation$standard_error), 4), "\n")
cat("- Max SE:", round(max(naive_validation$standard_error), 4), "\n")

cat("\nOptimal approach validation:\n")
cat("- Respondents meeting target SE:", sum(optimal_validation$meets_target), "/", n_respondents, "\n") 
cat("- Average SE:", round(mean(optimal_validation$standard_error), 4), "\n")
cat("- Max SE:", round(max(optimal_validation$standard_error), 4), "\n")

# Show domain overlap analysis
if (naive_result$total_unique_domains > 0 && optimal_result$total_unique_domains > 0) {
  overlap <- length(intersect(naive_result$unique_domains, optimal_result$unique_domains))
  cat("\nDomain selection overlap:", overlap, "domains\n")
  cat("Overlap percentage:", round(100 * overlap / min(naive_result$total_unique_domains, optimal_result$total_unique_domains), 1), "%\n")
}