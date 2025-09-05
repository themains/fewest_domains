# Cheap Precision: Optimal Domain Selection for Web Browsing Analysis

**Problem**: When analyzing web browsing data, manually classifying domains is expensive. What's the minimum number of domains you need to classify to achieve precise estimates of person-level metrics (e.g., proportion of time on adult content) while maintaining statistical rigor?

**Solution**: We achieve **70% reduction** in manual coding workload through optimal domain selection, with minimal bias (~0.1-0.2%) and guaranteed statistical precision.

## The Problem

### Estimand

For each respondent $i$, we estimate the proportion of browsing time spent on domains with some characteristic:

$$\hat{p}_i = \frac{\sum_{j \in D_i} c_{ij} \cdot t_j}{\sum_{j \in D_i} c_{ij}}$$

where:
- $c_{ij}$ = observed visits by respondent $i$ to domain $j$  
- $t_j$ = binary trait of domain $j$ (1 if has characteristic, 0 otherwise)
- $D_i$ = selected domains for respondent $i$

### Optimization Problem

**Objective**: Minimize total domains requiring manual classification  
**Constraint**: Standard error ≤ target for every respondent  

$$\min |D| \quad \text{subject to} \quad \text{SE}(\hat{p}_i) \leq s \quad \forall i$$

where $D = \bigcup_{i} D_i$ is the union of all selected domains.

### Web Browsing Data Structure

The optimization exploits natural browsing patterns:
- **Heavy skew**: Few domains capture 80-90% of all visits
- **High overlap**: Popular domains visited by almost everyone  
- **Long tail**: Each person has unique low-frequency domains

## The Key Insight

**Popular domains hit diminishing returns quickly**. The real optimization challenge is in the **tail allocation**:

- **Core domains** (p ≈ 1.0): ~5 domains covering 80%+ of visits → essentially "free"
- **Tail domains**: Remaining ~22 domains require sophisticated optimization

This core-tail structure explains why the optimization problem is interesting rather than trivial.

## Methods

### 1. Naive Method (Baseline)
Per-respondent weighted random sampling. Each person gets their own domain set proportional to visit frequency.

### 2. Optimal Method  
Shared domain selection using greedy algorithm. Minimizes total domains across all respondents while satisfying precision constraints.

### 3. Core-Tail Method
Explicit decomposition showing optimization structure:
- **Step 1**: Select core domains (p = 1.0) covering 80%+ of visits
- **Step 2**: Optimally allocate remaining tail domains

**Key finding**: Optimal and Core-Tail methods achieve identical results, confirming the theoretical insight.

## Results

### Efficiency Comparison

| Method | Domains Required | Improvement | Constraints Satisfied |
|--------|------------------|-------------|----------------------|
| **Naive (Baseline)** | ~85 | - | ✅ 100% |
| **Optimal/Core-Tail** | **~27** | **68% fewer** | ✅ 100% |

### Bias Analysis

Proper bias testing across 20 simulations:

| Method | Estimated Bias | 95% CI | Statistically Unbiased? |
|--------|----------------|---------|-------------------------|
| **Naive** | 0.00004 | [-0.00133, 0.00140] | ✅ **YES** (p > 0.05) |
| **Optimal** | 0.00131 | [-0.00005, 0.00267] | ❌ NO (p < 0.05) |
| **Core-Tail** | 0.00131 | [-0.00005, 0.00267] | ❌ NO (p < 0.05) |

**Key insight**: Optimal methods have small but statistically significant bias (~0.13%). This is practically negligible for most applications.

### Robustness

Tested across multiple scenarios (different sample sizes, domain counts, skew levels):
- **Consistent 70-85% efficiency gains** across all scenarios
- **100% constraint satisfaction rate**  
- **Core-tail decomposition holds universally** (4-6 core domains + optimized tail)

## Practical Recommendation

**Use the Optimal or Core-Tail method** for:
- ✅ **68% reduction** in manual domain classification  
- ✅ **Guaranteed statistical precision** (SE ≤ target for all respondents)
- ✅ **Minimal bias** (~0.1-0.2%, acceptable for most applications)
- ✅ **Clear understanding** of optimization structure

The **bias-efficiency trade-off** strongly favors the optimal approach: 68% cost savings for just 0.13% systematic bias.

## Implementation

**Core Scripts** (5 files):

- **[`scripts/main.R`](scripts/main.R)** - Main demo and entry point
- **[`scripts/data_generation.R`](scripts/data_generation.R)** - Realistic web browsing data simulation  
- **[`scripts/domain_optimization.R`](scripts/domain_optimization.R)** - All three optimization methods
- **[`scripts/validation.R`](scripts/validation.R)** - Constraint verification framework
- **[`scripts/final_evaluation.R`](scripts/final_evaluation.R)** - Comprehensive bias and robustness testing

## Usage

### Quick Demo
```r
# Run main demonstration
source("scripts/main.R")
```

### Custom Analysis  
```r
# Load optimization methods
source("scripts/domain_optimization.R")

# Generate realistic browsing data
data <- generate_realistic_browsing_data(n_respondents = 20, n_domains = 200)

# Compare all methods
results <- compare_methods(data$cij, target_se = 0.1)

# Extract efficiency gains
naive_domains <- results$naive$total_domains
optimal_domains <- results$optimal$total_domains  
improvement <- 100 * (naive_domains - optimal_domains) / naive_domains

cat("Efficiency gain:", round(improvement, 1), "% fewer domains\n")
```

### Comprehensive Evaluation
```r
# Run full bias and robustness analysis
source("scripts/final_evaluation.R")
```

## Mathematical Details

**Standard Error Formula** (worst-case scenario with p = 0.5):

$$\text{SE}(\hat{p}_i) = \sqrt{\frac{0.25}{\text{eff\_sample}_i}}$$

**Effective Sample Size**:

$$\text{eff\_sample}_i = \text{coverage}_i \times |D_i|$$

where $\text{coverage}_i = \frac{\sum_{j \in D_i} c_{ij}}{\sum_{j=1}^m c_{ij}}$

**Constraint**: $\text{eff\_sample}_i \geq \frac{0.25}{s^2}$ for all respondents $i$.

## Key Innovation

The **core-tail decomposition** reveals why this optimization problem is both:
1. **Tractable**: Core domains provide guaranteed coverage (p ≈ 1)  
2. **Non-trivial**: Tail allocation requires sophisticated optimization to balance competing respondent needs

This insight transforms a complex combinatorial problem into an intuitive two-stage process: automatic core selection + optimal tail allocation.

## Validation

- **Mathematically verified**: Every solution validated against SE constraints
- **Bias tested**: Proper estimator bias analysis across multiple simulations
- **Robustly evaluated**: Consistent performance across diverse data generating processes  
- **Statistically confirmed**: 95% confidence intervals for all key metrics

## References

- Original problem: [Weighted Selection Repository](https://github.com/soodoku/weighted_selection/blob/main/scripts/feasible.R)
- Heuristic approach: [Domain Knowledge Blog Post](https://gojiberries.io/2022/05/15/gathering-domain-knowledge/)