# Realistic Browsing Data Generation
# Captures key properties: extreme skew, shared popular domains, idiosyncratic long tails

generate_realistic_browsing_data <- function(n_respondents, n_domains, seed = 42) {
  set.seed(seed)
  
  cat("Generating realistic browsing data with properties:\n")
  cat("- Heavy skew: top domains visited by almost everyone\n")
  cat("- Shared core: ~10-20 domains account for 80%+ of all visits\n")
  cat("- Long tail: each person has unique low-frequency domains\n")
  cat("- Frequency concentration: people visit few domains very often\n\n")
  
  # Step 1: Create the "universal" top domains that almost everyone visits
  # These follow a very steep power law - top 1% of domains get most traffic
  n_core_domains <- max(10, ceiling(0.02 * n_domains))  # Top 2% are "core"
  
  # Core domain base popularity (very steep decay)
  core_popularity <- (1:n_core_domains)^(-2.5)
  core_popularity <- core_popularity / sum(core_popularity)
  
  # Step 2: Generate visit matrix
  cij <- matrix(0, n_respondents, n_domains)
  
  for (i in 1:n_respondents) {
    total_visits <- sample(500:2000, 1)  # Variable activity levels
    
    # Most visits (70-90%) go to core domains, with person-specific preferences
    core_fraction <- runif(1, 0.70, 0.90)
    core_visits <- round(core_fraction * total_visits)
    
    # Personal preferences for core domains (everyone visits them, but with different intensities)
    personal_core_prefs <- core_popularity * runif(n_core_domains, 0.3, 3.0)
    personal_core_prefs <- personal_core_prefs / sum(personal_core_prefs)
    
    core_visit_counts <- rmultinom(1, core_visits, personal_core_prefs)
    cij[i, 1:n_core_domains] <- as.numeric(core_visit_counts)
    
    # Remaining visits go to personal "long tail" domains
    remaining_visits <- total_visits - core_visits
    if (remaining_visits > 0) {
      # Each person has their own tail distribution
      # Most people visit different tail domains (low overlap)
      tail_domains <- (n_core_domains + 1):n_domains
      n_tail <- length(tail_domains)
      
      # Person-specific tail preferences (random with decay)
      tail_popularity <- (1:n_tail)^(-1.0) * runif(n_tail, 0.01, 1.0)
      tail_popularity <- tail_popularity / sum(tail_popularity)
      
      # Only visit a subset of tail domains (sparse patterns)
      n_tail_visited <- sample(1:min(50, n_tail), 1)
      visited_tail <- sample(tail_domains, n_tail_visited, prob = tail_popularity[1:n_tail])
      
      # Distribute remaining visits among selected tail domains
      if (n_tail_visited > 1) {
        tail_visit_probs <- tail_popularity[visited_tail - n_core_domains]
        tail_visit_probs <- tail_visit_probs / sum(tail_visit_probs)
        tail_visits <- rmultinom(1, remaining_visits, tail_visit_probs)
        cij[i, visited_tail] <- as.numeric(tail_visits)
      } else {
        cij[i, visited_tail] <- remaining_visits
      }
    }
  }
  
  # Add some noise to make it more realistic
  cij <- cij + matrix(rpois(n_respondents * n_domains, 0.1), n_respondents, n_domains)
  
  # Analysis of generated data
  total_visits_per_domain <- colSums(cij)
  total_visits_per_person <- rowSums(cij)
  
  cat("Generated data characteristics:\n")
  cat("- Total domains with any visits:", sum(total_visits_per_domain > 0), "/", n_domains, "\n")
  cat("- Average visits per person:", round(mean(total_visits_per_person)), "\n")
  
  # Core domain analysis
  top_10_visits <- sum(head(sort(total_visits_per_domain, decreasing = TRUE), 10))
  top_50_visits <- sum(head(sort(total_visits_per_domain, decreasing = TRUE), 50))
  total_all_visits <- sum(total_visits_per_domain)
  
  cat("- Top 10 domains capture:", round(100 * top_10_visits / total_all_visits, 1), "% of visits\n")
  cat("- Top 50 domains capture:", round(100 * top_50_visits / total_all_visits, 1), "% of visits\n")
  
  # Overlap analysis
  domain_user_counts <- colSums(cij > 0)  # How many people visit each domain
  top_domains_overlap <- head(order(domain_user_counts, decreasing = TRUE), 20)
  avg_overlap <- mean(domain_user_counts[top_domains_overlap])
  cat("- Average users per top-20 domain:", round(avg_overlap, 1), "/", n_respondents, 
      "(", round(100 * avg_overlap / n_respondents, 1), "% overlap)\n")
  
  # Concentration analysis
  person_domain_counts <- rowSums(cij > 0)  # How many domains each person visits
  cat("- Average domains per person:", round(mean(person_domain_counts), 1), "\n")
  
  # Heavy usage analysis
  heavy_visit_threshold <- quantile(as.numeric(cij[cij > 0]), 0.9)
  heavy_visits_per_person <- rowSums(cij >= heavy_visit_threshold)
  cat("- Average heavily-used domains per person:", round(mean(heavy_visits_per_person), 1), "\n")
  
  return(list(
    cij = cij,
    n_respondents = n_respondents,
    n_domains = n_domains,
    core_domains = 1:n_core_domains,
    characteristics = list(
      top_10_pct = round(100 * top_10_visits / total_all_visits, 1),
      top_50_pct = round(100 * top_50_visits / total_all_visits, 1),
      avg_domains_per_person = round(mean(person_domain_counts), 1),
      avg_overlap_top20 = round(100 * avg_overlap / n_respondents, 1)
    )
  ))
}

# Test the data generation
if (!interactive()) {
  cat("=== TESTING DATA GENERATION ===\n")
  data <- generate_realistic_browsing_data(20, 500)
  
  cat("\nFirst few rows of visit matrix:\n")
  print(data$cij[1:5, 1:10])
  
  cat("\nTop 10 most popular domains (total visits):\n")
  top_domains <- head(order(colSums(data$cij), decreasing = TRUE), 10)
  for (i in 1:10) {
    domain_idx <- top_domains[i]
    visits <- colSums(data$cij)[domain_idx]
    users <- sum(data$cij[, domain_idx] > 0)
    cat("Domain", domain_idx, ":", visits, "visits from", users, "users\n")
  }
}