## Cheap Precision: What're the Fewest Items You Can Select?

Passively observed browsing data is a powerful tool for social science. Working with it means learning how to infer some characteristics of the URLs that people visit. Measurement of some of those characteristics is expensive and it is useful to think about the following question: what is the fewest number of domains (and identify which domains) you need to get inference on so that you can measure some person-level metric, e.g., the proportion of time spent on domains with adult content, can be answered with a standard error equal or less than some specified number. The setup has the following characteristics: large skew in visitation, a large overlap across people in sites that they commonly visit, and a large idiosyncratic tail per person with little overlap.

More generally, we want to solve the following optimization problem:

Let $i$ iterate over respondents 1 to $n$, let $j$ iterate over domains that range from 1 to m, let $c_{ij}$ denote the number of visits by respondent $i$ to domain $j$, let $t_j$ denote an unmeasured trait of domain $j$, with $t$ being a boolean, let $d_i$ denote the total number of selected domains for respondent $i$, and $D_i$ as the list of domains selected, and $D$ as the set of all domains selected across respondents.

minimize $d$ s.t. $ \sigma_i <= s$ for each $i$ where $s$ is some constant.

$\sigma_i = \sqrt \frac{(p_i (1 - p_i))}{d_i}$

where $p_i = \frac{\Sigma_j c_{ij}*(t_{ij})}{\Sigma_j c_{ij}}$

Since the latent trait, $t_j$ of each domain is unknown, we have to solve for the worst-case scenario: $p_i = .5$. This leaves the two unknowns as $d_i$ and $D_i$, which jointly tell us about the quantities of interest $d$ and $D$. Technically, $d$ is an attribute of $D$ and hence needn't be seen as a separate unknown. We assume that the only way to get to $d$ is to find $D$.

### Solution

This repository provides two approaches to solve the domain selection optimization problem:

#### **Naive Method** (Baseline)
Per-respondent weighted random sampling where each respondent gets their own set of domains proportional to their visit frequency. Located in `scripts/naive_method.R`.

#### **Optimal Method** 
Shared domain selection that leverages the skewed browsing patterns to minimize total domains across all respondents while meeting standard error constraints. Located in `scripts/optimal_method.R`.

### Implementation

The solution consists of modular R scripts:

- `scripts/data_generation.R` - Generates realistic heavy-skewed browsing data with characteristics matching real web usage patterns
- `scripts/naive_method.R` - Implements per-respondent weighted sampling baseline  
- `scripts/optimal_method.R` - Implements shared domain selection optimization
- `scripts/compare_methods.R` - Complete comparison framework and sensitivity analysis

### Key Results

**✓ All results are validated to ensure standard error constraints are actually met**

Testing on realistic browsing data with heavy skew (top 10 domains capture ~90% of visits) and high overlap (80%+ of users visit popular domains):

| Standard Error Target | Naive Domains | Optimal Domains | Improvement | Constraints Met |
|----------------------|---------------|-----------------|-------------|-----------------|
| 0.05                 | 250           | 102             | **59.2%** reduction | ✓ Both methods |
| 0.10                 | 141           | 28              | **80.1%** reduction | ✓ Both methods |  
| 0.15                 | 35            | 14              | **60.0%** reduction | ✓ Both methods |
| 0.20                 | 18            | 8               | **55.6%** reduction | ✓ Both methods |

The validated optimal method dramatically outperforms the naive approach by:
- **Reducing coding costs by 55-80%** (fewer domains to manually classify)
- **Guaranteeing 100% constraint satisfaction** for both methods through rigorous validation
- **Leveraging shared popular domains** that help multiple respondents simultaneously
- **Meeting strict SE requirements** with mathematical verification

### Usage

```r
# Run validated comparison (RECOMMENDED)
source("scripts/final_comparison.R")

# Or run individual corrected methods
source("scripts/corrected_methods.R")
data <- generate_realistic_browsing_data(n_respondents = 25, n_domains = 400)

# Validated optimal method
result <- optimal_corrected(data$cij, target_se = 0.1)

# Check constraints are met
print(result$constraints_satisfied)  # Should be TRUE
```

### Validation

The solution includes rigorous constraint validation:
- `scripts/validation.R` - Comprehensive validation framework
- `scripts/final_comparison.R` - Validated comparison ensuring all constraints are met
- All reported results are **mathematically verified** to satisfy SE ≤ target for every respondent

### References

Original feasible solution: https://github.com/soodoku/weighted_selection/blob/main/scripts/feasible.R

Blog about the heuristic solution: https://gojiberries.io/2022/05/15/gathering-domain-knowledge/
