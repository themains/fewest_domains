library(lpSolveAPI)

# Optimal Domain Selection Solution
# Solves: minimize total domains while keeping standard error <= s for each respondent

solve_domain_selection <- function(cij, s, use_heuristic = FALSE, time_limit = 300) {
  n <- nrow(cij)  # respondents
  m <- ncol(cij)  # domains
  
  # Calculate minimum effective domains needed per respondent
  # From constraint: sigma_i = sqrt(0.25/d_i) <= s
  min_eff_domains <- 0.25 / s^2
  
  # Normalize visit weights per respondent
  weights <- t(apply(cij, 1, function(x) x / sum(x)))
  
  if (!use_heuristic && n * m <= 50000) {
    # ILP Solution for smaller problems
    cat("Using ILP solver...\n")
    
    # Create LP model
    lp_model <- make.lp(0, m)
    
    # Objective: minimize sum of selected domains
    set.objfn(lp_model, rep(1, m))
    set.type(lp_model, 1:m, "binary")
    
    # Add constraints for each respondent
    for (i in 1:n) {
      # Constraint: sum(x_j * w_ij) >= min_eff_domains
      add.constraint(lp_model, weights[i, ], ">=", min_eff_domains)
    }
    
    # Set time limit
    set.timeout(lp_model, time_limit)
    
    # Solve
    status <- solve(lp_model)
    
    if (status == 0) {
      solution <- get.variables(lp_model)
      selected_domains <- which(solution > 0.5)
      return(list(
        domains = selected_domains,
        method = "ILP",
        status = "optimal",
        total_domains = length(selected_domains)
      ))
    } else {
      cat("ILP failed, falling back to heuristic...\n")
      use_heuristic <- TRUE
    }
  }
  
  if (use_heuristic) {
    # Greedy Heuristic Solution
    cat("Using greedy heuristic...\n")
    
    selected_domains <- c()
    remaining_need <- rep(min_eff_domains, n)
    
    while (any(remaining_need > 1e-6)) {
      # Calculate coverage efficiency for each unselected domain
      efficiency <- sapply(1:m, function(j) {
        if (j %in% selected_domains) return(0)
        
        coverage <- sapply(1:n, function(i) {
          min(remaining_need[i], weights[i, j])
        })
        sum(coverage)
      })
      
      # Select domain with highest efficiency
      best_domain <- which.max(efficiency)
      selected_domains <- c(selected_domains, best_domain)
      
      # Update remaining needs
      for (i in 1:n) {
        covered <- min(remaining_need[i], weights[i, best_domain])
        remaining_need[i] <- remaining_need[i] - covered
      }
    }
    
    return(list(
      domains = selected_domains,
      method = "Heuristic",
      status = "feasible",
      total_domains = length(selected_domains)
    ))
  }
}

# Validation function
validate_solution <- function(cij, selected_domains, s) {
  n <- nrow(cij)
  weights <- t(apply(cij, 1, function(x) x / sum(x)))
  
  # Calculate effective domains and standard errors
  results <- data.frame(
    respondent = 1:n,
    effective_domains = numeric(n),
    standard_error = numeric(n),
    meets_constraint = logical(n)
  )
  
  for (i in 1:n) {
    eff_domains <- sum(weights[i, selected_domains])
    std_err <- sqrt(0.25 / eff_domains)
    
    results$effective_domains[i] <- eff_domains
    results$standard_error[i] <- std_err
    results$meets_constraint[i] <- std_err <= s
  }
  
  return(results)
}

# Example usage
set.seed(42)
n <- 10
m <- 100
s <- 0.05

# Generate realistic browsing data (power law distribution)
cij <- matrix(0, n, m)
for (i in 1:n) {
  # Heavy-tailed visitation pattern
  probs <- (1:m)^(-1.5)
  probs <- probs / sum(probs)
  cij[i, ] <- rmultinom(1, 1000, probs)
}

# Solve the problem
solution <- solve_domain_selection(cij, s)

# Validate results
validation <- validate_solution(cij, solution$domains, s)

# Output results
cat("\n=== SOLUTION RESULTS ===\n")
cat("Method:", solution$method, "\n")
cat("Status:", solution$status, "\n")
cat("Total domains selected:", solution$total_domains, "out of", m, "\n")
cat("Selected domains:", paste(solution$domains, collapse = ", "), "\n")

cat("\n=== VALIDATION ===\n")
print(validation)
cat("All constraints satisfied:", all(validation$meets_constraint), "\n")
cat("Max standard error:", round(max(validation$standard_error), 6), "\n")
cat("Target standard error:", s, "\n")