# Naive Domain Selection Method
# Per-respondent weighted random sampling baseline

source("scripts/data_generation.R")
source("scripts/validation.R")

naive_domain_selection <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Calculate required sample size per respondent
  # SE = sqrt(p*(1-p)/n) <= target_se, worst case p=0.5
  min_sample_size <- ceiling(0.25 / target_se^2)
  
  if (verbose) {
    cat("=== NAIVE METHOD ===\n")
    cat("Target standard error:", target_se, "\n")
    cat("Required domains per respondent:", min_sample_size, "\n")
  }
  
  # For each respondent, sample domains proportional to visit frequency
  all_domains_used <- c()
  respondent_domains <- list()
  
  for (i in 1:n) {
    # Skip respondents with no visits (shouldn't happen with realistic data)
    if (sum(cij[i, ]) == 0) {
      respondent_domains[[i]] <- integer(0)
      next
    }
    
    visit_probs <- cij[i, ] / sum(cij[i, ])
    
    # Sample without replacement, weighted by visit frequency
    # This ensures each respondent gets domains proportional to their usage
    available_domains <- which(cij[i, ] > 0)  # Only domains they actually visit
    
    if (length(available_domains) >= min_sample_size) {
      selected <- sample(available_domains, min_sample_size, 
                        prob = visit_probs[available_domains], replace = FALSE)
    } else {
      # If person visits fewer domains than needed, take all visited + random others
      selected <- available_domains
      remaining_needed <- min_sample_size - length(available_domains)
      if (remaining_needed > 0) {
        unvisited <- setdiff(1:m, available_domains)
        if (length(unvisited) >= remaining_needed) {
          additional <- sample(unvisited, remaining_needed, replace = FALSE)
          selected <- c(selected, additional)
        } else {
          selected <- c(selected, unvisited)  # Take all available
        }
      }
    }
    
    respondent_domains[[i]] <- selected
    all_domains_used <- c(all_domains_used, selected)
  }
  
  unique_domains <- unique(all_domains_used)
  
  if (verbose) {
    cat("Domains sampled per respondent:", min_sample_size, "\n")
    cat("Total unique domains needed:", length(unique_domains), "\n")
    
    # Show overlap statistics
    overlap_counts <- table(all_domains_used)
    cat("Domain usage overlap:\n")
    cat("- Domains used by 1 person:", sum(overlap_counts == 1), "\n")
    cat("- Domains used by 2+ people:", sum(overlap_counts >= 2), "\n")
    cat("- Most shared domain used by:", max(overlap_counts), "people\n")
  }
  
  return(list(
    method = "Naive (per-respondent sampling)",
    total_unique_domains = length(unique_domains),
    unique_domains = unique_domains,
    respondent_domains = respondent_domains,
    domains_per_respondent = min_sample_size,
    target_se = target_se
  ))
}

# Validation function for naive method
validate_naive_solution <- function(cij, solution, verbose = TRUE) {
  n <- nrow(cij)
  
  results <- data.frame(
    respondent = 1:n,
    sampled_domains = numeric(n),
    coverage_pct = numeric(n),
    effective_sample = numeric(n),
    standard_error = numeric(n),
    meets_target = logical(n)
  )
  
  for (i in 1:n) {
    selected_domains <- solution$respondent_domains[[i]]
    
    # Calculate coverage (proportion of visits captured)
    total_visits <- sum(cij[i, ])
    captured_visits <- sum(cij[i, selected_domains])
    coverage_pct <- if (total_visits > 0) 100 * captured_visits / total_visits else 0
    
    # Effective sample size for proportion estimation
    # In stratified sampling, this is approximately the number of sampled domains
    # weighted by their visit frequency
    if (length(selected_domains) > 0 && total_visits > 0) {
      weights <- cij[i, selected_domains] / total_visits
      eff_sample <- sum(weights) * length(selected_domains)
    } else {
      eff_sample <- 0
    }
    
    # Standard error (worst case p=0.5)
    se <- if (eff_sample > 0) sqrt(0.25 / eff_sample) else Inf
    
    results$sampled_domains[i] <- length(selected_domains)
    results$coverage_pct[i] <- coverage_pct
    results$effective_sample[i] <- eff_sample
    results$standard_error[i] <- se
    results$meets_target[i] <- se <= solution$target_se
  }
  
  if (verbose) {
    cat("\n=== NAIVE METHOD VALIDATION ===\n")
    cat("Respondents meeting target SE:", sum(results$meets_target), "/", n, "\n")
    cat("Average coverage:", round(mean(results$coverage_pct), 1), "%\n")
    cat("Average standard error:", round(mean(results$standard_error), 4), "\n")
    cat("Max standard error:", round(max(results$standard_error), 4), "\n")
    
    if (any(!results$meets_target)) {
      failed <- which(!results$meets_target)
      cat("Failed respondents:", paste(failed, collapse = ", "), "\n")
    }
  }
  
  return(results)
}

# Test the method
if (!interactive()) {
  cat("=== TESTING NAIVE METHOD ===\n")
  
  # Generate test data
  data <- generate_realistic_browsing_data(15, 200)
  cij <- data$cij
  
  # Test with different SE targets
  target_se <- 0.1
  
  # Run naive method
  solution <- naive_domain_selection(cij, target_se)
  
  # Validate
  validation <- validate_naive_solution(cij, solution)
  
  cat("\nSample of validation results:\n")
  print(head(validation))
}