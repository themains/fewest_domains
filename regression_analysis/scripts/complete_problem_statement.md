# Complete Problem Statement: Regression-Aware Domain Selection

## The Core Problem

**We want to minimize the number of domains we need to manually classify while preserving both statistical precision and regression validity for downstream analysis.**

## The Setting

### What We Have
- **Web browsing data**: Each of `n` users visits some subset of `m` total domains
- **Visit counts**: User `i` visits domain `j` exactly `cᵢⱼ` times (could be zero)
- **User characteristics**: Each user has covariates `Xᵢ = [age, income, tech_savviness, ...]`
- **Target outcome**: We want to estimate each user's proportion of visits to "adult content" domains

### What We Want to Do
1. **Classify domains**: Manually label each domain as "adult" or "not adult" (expensive!)
2. **Estimate proportions**: For each user, calculate `ŷᵢ = (adult visits) / (total visits)` using only classified domains
3. **Run regressions**: Use these proportions in downstream analysis like `ŷᵢ ~ Xᵢ` to understand relationships

### The Optimization Challenge
**We want to classify as few domains as possible while ensuring both:**
- **Precision**: Each user's proportion estimate `ŷᵢ` has standard error ≤ some threshold
- **Regression validity**: Coefficients from `ŷᵢ ~ Xᵢ` are not systematically biased

## Data Generating Process Assumptions

### Assumption 1: User Browsing Patterns
**Users visit domains based on their characteristics and preferences.**

- Older users prefer different domains than younger users
- Tech-savvy users visit different domains than non-tech-savvy users  
- Privacy-conscious users avoid certain domains
- But there's also random variation - not everything is predictable from demographics

### Assumption 2: Adult Content Distribution
**Adult content is not randomly distributed across domains or users.**

- Some domains are primarily adult content, others have none
- Users vary in how much adult content they consume
- Adult content consumption correlates with user characteristics (age, values, etc.)
- **Crucially**: The adult content rate `yᵢ = Σⱼ∈Adult cᵢⱼ / Σⱼ cᵢⱼ` has a systematic relationship with user characteristics `Xᵢ`

### Assumption 3: Domain Selection Creates Systematic Patterns
**When we select only a subset of domains to classify, we inadvertently create systematic errors.**

- If we select popular domains → we oversample domains used by mainstream users
- If we select domains used by older users → younger users' proportions become systematically wrong
- If we select domains correlated with tech-savviness → tech coefficient gets biased

### Assumption 4: Error Structure
**The selection errors follow a specific pattern:**
- `True proportion`: `yᵢ = Σⱼ∈Adult cᵢⱼ / Σⱼ cᵢⱼ` (using all domains)  
- `Estimated proportion`: `ŷᵢ = Σⱼ∈(Adult∩Selected) cᵢⱼ / Σⱼ∈Selected cᵢⱼ` (using only selected domains)
- `Selection error`: `εᵢ = ŷᵢ - yᵢ`

**Key assumption**: These errors `εᵢ` are **not random** - they systematically correlate with user characteristics `Xᵢ` depending on which domains we select.

## The Complete Optimization Problem

### Objective
**Minimize the number of domains we need to manually classify**

### Constraints
1. **Precision Constraint**: Every user's proportion estimate must be sufficiently precise
   - For each user `i`: Standard error of `ŷᵢ` ≤ target threshold  
   - This ensures our estimates are not too noisy

2. **Regression Bias Constraint**: Downstream regression coefficients must not be systematically biased
   - When we regress `ŷᵢ ~ Xᵢ`, the coefficients should be close to what we'd get regressing `yᵢ ~ Xᵢ`
   - This ensures our causal/associational inferences remain valid

### Mathematical Formulation
```
minimize |D|  (number of domains to classify)

subject to:
  SE(ŷᵢ) ≤ s  for all users i           [precision constraint]
  |β̂ⱼ(D) - β̂ⱼ(M)| ≤ τⱼ  for all coefficients j  [regression bias constraint]

where:
  D = selected domains to classify
  M = all domains  
  β̂ⱼ(D) = j-th coefficient from regressing ŷᵢ(D) ~ Xᵢ
  β̂ⱼ(M) = j-th coefficient from regressing yᵢ(M) ~ Xᵢ
```

## Why This Is Hard

### Computational Difficulty
- **Nonlinear objective**: Regression coefficients change unpredictably as we add/remove domains
- **Constraint interaction**: Precision and bias constraints often conflict
- **Combinatorial explosion**: With 400 domains, there are 2^400 possible selections

### Statistical Difficulty  
- **Bias amplification**: Small systematic errors in individual estimates create large biases in regression coefficients
- **Correlation patterns**: Popular domains tend to be used by certain demographic groups, creating inevitable correlations
- **Coverage-bias tradeoff**: Domains that provide good coverage often introduce systematic bias

## Real-World Context

### When This Matters
- **Policy research**: "Does privacy education reduce risky online behavior?" 
  - If domain selection biases age coefficients, policy recommendations could be wrong
- **Market research**: "Which demographics prefer adult content?"
  - Biased coefficients lead to wrong targeting decisions
- **Academic research**: "How do digital divides affect online behavior?"
  - Selection bias could confound causal inferences

### When Precision-Only Optimization Fails
**The original domain optimization (minimize domains subject to precision) can achieve 70% efficiency gains but:**
- Creates 200-800% bias in regression coefficients
- Makes age, income, tech-savviness coefficients systematically wrong
- Invalidates downstream causal/policy analysis

### The Fundamental Tradeoff
**Efficiency vs. Validity**
- High efficiency (classify 20% of domains) → Large regression bias  
- High validity (classify 90% of domains) → Defeats the optimization purpose
- The sweet spot (if it exists) requires sophisticated methods

## Solution Approaches We've Tried

### 1. Stratified Methods
- Divide users into groups (young/old, tech-savvy/not)
- Select representative domains from each group
- **Result**: Still biased, expensive

### 2. Multi-Objective Optimization
- Directly optimize for both precision and regression validity
- Use weighted scoring: efficiency gain - bias penalty
- **Result**: Gets stuck in local optima, still biased

### 3. Error Correlation Minimization  
- Select domains that minimize correlation between errors and user characteristics
- **Result**: Theoretical improvement but practically still fails

## The Bottom Line

**This problem reveals a fundamental tension between statistical efficiency and causal validity.** 

Domain selection that optimizes for precision (the original goal) systematically creates the correlation patterns that bias regression coefficients. Methods that avoid this bias require classifying most domains anyway, defeating the optimization purpose.

This suggests we may need different approaches for different research goals:
- **Descriptive research**: Use precision-optimized methods
- **Causal/policy research**: Accept lower efficiency to preserve coefficient validity
- **Exploratory research**: Use bias correction techniques post-hoc