library(lpSolveAPI)
set.seed(31415)
# Define the parameters
n <- 40  # Number of respondents
m <- 20000  # Number of domains
epsilon <- 0.1  # Maximum standard error

# Generate fake data
cij <- matrix(rpois(n * m, lambda = 10), ncol = m)  # Visitation matrix
tj <- sample(c(0, 1), m, replace = TRUE)  # Trait vector

# Create the LP model
lp_model <- make.lp(0, m)

# Set the objective function (minimize 0)
set.objfn(lp_model, rep(0, m))

# Add constraints for each respondent
for (i in 1:n) {
  constraint <- rep(0, m)
  constraint[(i - 1) * m + 1:(i * m)] <- 1
  add.constraint(lp_model, constraint, ">=", 1)
}

# Calculate the standard error constraint
se_constraint <- sqrt(colSums(cij^2)) / sqrt(sum(cij^2 * tj))
se_constraint_extended <- rep(se_constraint, each = n)

# Add constraint for standard error
add.constraint(lp_model, se_constraint_extended, "<=", epsilon)

# Set variable type (binary)
set.type(lp_model, 1:m, "binary")

# Solve the LP problem
solve(lp_model)

# Check if the LP problem is feasible
if (get.status(lp_model) != 0) {
  stop("No feasible solution found.")
}

# Get the solution
solution <- get.variables(lp_model)

# Get the selected domains for each respondent
Di <- lapply(1:n, function(i) which(solution[(i - 1) * m + 1:(i * m)] > 0))

# Calculate the standard error for each respondent
se <- sapply(Di, function(d) {
  if (length(d) > 0) {
    sqrt(sum(tj[d]) / length(d))
  } else {
    NaN
  }
})

# Output the selected domains and standard error for each respondent
for (i in 1:n) {
  cat("Respondent", i, ": Selected Domains =", Di[[i]], "Standard Error =", se[i], "\n")
}
