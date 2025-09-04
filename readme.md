## Cheap Precision: What're the Fewest Items You Can Select?

Passively observed browsing data is a powerful tool for social science. Working with it means learning how to infer some characteristics of the URLs that people visit. Measurement of some of those characteristics is expensive and it is useful to think about the following question: what is the fewest number of domains (and identify which domains) you need to get inference on so that you can measure some person-level metric, e.g., the proportion of time spent on domains with adult content, can be answered with a standard error equal or less than some specified number.

### The Estimand

**What we want to estimate**: For each respondent $i$, we want to estimate the proportion of their browsing time spent on domains with some characteristic (e.g., adult content):

$$\hat{p}_i = \frac{\sum_{j \in D_i} c_{ij} \cdot t_j}{\sum_{j \in D_i} c_{ij}}$$

where:
- $c_{ij}$ = observed visits by respondent $i$ to domain $j$
- $t_j$ = unobserved binary trait of domain $j$ (1 if has characteristic, 0 otherwise)
- $D_i$ = set of domains selected for manual coding for respondent $i$

**The precision requirement**: We need the standard error of $\hat{p}_i$ to be ≤ some target value $s$ for every respondent.

**The cost constraint**: Manual coding of domains is expensive, so we want to minimize the total number of unique domains that need to be manually classified across all respondents.

### Problem Setup

The setup has the following characteristics:
- **Large skew in visitation**: A few domains are visited by basically everyone  
- **Large overlap across people**: Common sites that most people visit
- **Large idiosyncratic tail per person**: Each person has unique low-frequency domains with little overlap

### Optimization Problem Formulation

**Objective**: Minimize the total number of domains requiring manual classification

**Constraint**: Maintain statistical precision for every respondent

Formally, we solve:

$$\min |D| \quad \text{subject to} \quad \text{SE}(\hat{p}_i) \leq s \quad \forall i$$

where:
- $D = \bigcup_{i=1}^n D_i$ is the set of all domains selected across respondents
- $s$ is the target standard error threshold
- $\text{SE}(\hat{p}_i)$ is the standard error of the proportion estimate for respondent $i$

**Standard Error Formula**: For frequency-weighted sampling, the standard error of the proportion estimate is:

$$\text{SE}(\hat{p}_i) = \sqrt{\frac{p_i(1-p_i)}{\text{eff\_sample}_i}}$$

where $\text{eff\_sample}_i$ is the effective sample size based on the selected domains.

**Worst-case assumption**: Since the true proportion $p_i$ is unknown, we use the conservative worst-case scenario $p_i = 0.5$, which maximizes the standard error. This gives:

$$\text{SE}(\hat{p}_i) = \sqrt{\frac{0.25}{\text{eff\_sample}_i}} = \frac{0.5}{\sqrt{\text{eff\_sample}_i}}$$

**Effective Sample Size**: For respondent $i$ with selected domains $D_i$:

$$\text{eff\_sample}_i = \text{coverage}_i \times |D_i|$$

where $\text{coverage}_i = \frac{\sum_{j \in D_i} c_{ij}}{\sum_{j=1}^m c_{ij}}$ is the proportion of respondent $i$'s browsing captured by the selected domains.

**The constraint**: For the constraint $\text{SE}(\hat{p}_i) \leq s$ to be satisfied, we need:

$$\frac{0.5}{\sqrt{\text{eff\_sample}_i}} \leq s$$

Rearranging: $\text{eff\_sample}_i \geq \frac{0.25}{s^2}$

This gives us the minimum effective sample size required per respondent to achieve the target standard error.

## Solution

### Methods

**Naive Method (Baseline):** Per-respondent weighted random sampling where each respondent gets their own set of domains proportional to their visit frequency.

**Optimal Method:** Shared domain selection that leverages the skewed browsing patterns to minimize total domains across all respondents while meeting standard error constraints.

### Implementation

The solution consists of **5 core R scripts**:

- **[`scripts/main.R`](scripts/main.R)** - Main entry point with demo
- **[`scripts/data_generation.R`](scripts/data_generation.R)** - Generates realistic heavy-skewed browsing data  
- **[`scripts/corrected_methods.R`](scripts/corrected_methods.R)** - Both naive and optimal methods with rigorous validation
- **[`scripts/validation.R`](scripts/validation.R)** - Constraint verification framework
- **[`scripts/robust_evaluation.R`](scripts/robust_evaluation.R)** - Comprehensive testing across multiple scenarios

### Robust Results

**Tested across 40 scenarios** (4 data variants × 10 random seeds):

| Method | Domains Required | Improvement | Success Rate |
|--------|------------------|-------------|--------------|
| **Optimal** | **30.3 ± 3.7** | **78.6% ± 4.1%** | **100%** |
| Naive | 145.6 ± 29.2 | - | 100% |

**Key Findings:**
- **Statistically significant**: 95% CI = 77.3% to 79.9% improvement
- **Always successful**: 100% constraint satisfaction across all scenarios  
- **Remarkably consistent**: Works across diverse data generating processes
- **Massive cost savings**: 71-85% reduction in domains requiring manual classification

### Performance by Data Type

| Scenario | Optimal Domains | Improvement |
|----------|-----------------|-------------|
| **Standard Skew** | 27.7 | 74.1% |
| **Extreme Skew** | 28.9 | 83.8% |  
| **Moderate Skew** | 36.5 | 75.7% |
| **High Overlap** | 28.0 | 80.8% |

## Usage

### Quick Demo
```r
# Run main demo
source("scripts/main.R")
```

### Custom Analysis
```r
# Load the methods
source("scripts/corrected_methods.R")

# Generate data (see data_generation.R for details)
data <- generate_realistic_browsing_data(n_respondents = 20, n_domains = 200)

# Run both methods (see corrected_methods.R for implementation)
naive_result <- naive_corrected(data$cij, target_se = 0.1)
optimal_result <- optimal_corrected(data$cij, target_se = 0.1)

# Compare results
domains_saved <- naive_result$total_unique_domains - optimal_result$total_unique_domains
improvement <- 100 * domains_saved / naive_result$total_unique_domains

cat("Domains saved:", domains_saved, "(", round(improvement, 1), "% improvement)\n")
cat("Constraints satisfied:", optimal_result$constraints_satisfied, "\n")
```

### Comprehensive Testing
```r
# Run robust evaluation (40 test scenarios)
# See robust_evaluation.R for full implementation details
source("scripts/robust_evaluation.R")
```

## Validation

**All results are mathematically verified** (see [`scripts/validation.R`](scripts/validation.R)):
- Every solution is validated to ensure SE ≤ target for every respondent
- Comprehensive testing across multiple data generating processes  
- Statistical significance confirmed with 95% confidence intervals
- 100% success rate across all tested scenarios

The validation framework in [`validation.R`](scripts/validation.R) implements rigorous constraint verification by:
1. Computing actual standard errors for each respondent given their selected domains
2. Checking that all respondents meet the SE ≤ target requirement
3. Providing detailed diagnostics when constraints are violated
4. Testing with synthetic data where ground truth is known

## Key Innovation

The optimal method achieves dramatic cost savings by **leveraging the natural structure** of web browsing data:
- **Exploits heavy skew**: Most visits concentrate on few popular domains
- **Utilizes shared patterns**: Popular domains help multiple respondents simultaneously  
- **Maintains precision**: Mathematical guarantees that all respondents meet SE requirements
- **Scales efficiently**: Performance improves with more extreme (realistic) data patterns

## References

Original feasible solution: https://github.com/soodoku/weighted_selection/blob/main/scripts/feasible.R

Blog about the heuristic solution: https://gojiberries.io/2022/05/15/gathering-domain-knowledge/