# Formal Optimization Problem with Regression Constraints
# Mathematical formalization of multi-objective domain selection

# Original Problem (Precision-Only)
cat("📐 MATHEMATICAL FORMALIZATION OF DOMAIN SELECTION OPTIMIZATION\n")
cat(rep("=", 80), "\n")

cat("1. ORIGINAL PRECISION-ONLY PROBLEM:\n")
cat(rep("-", 40), "\n")
cat("minimize |D|\n")
cat("subject to: SE(ŷᵢ) ≤ s  ∀i ∈ {1,...,n}\n\n")

cat("Where:\n")
cat("- D ⊆ {1,...,m} is the set of selected domains\n")
cat("- |D| is the number of domains to manually code\n") 
cat("- SE(ŷᵢ) is the standard error of proportion estimate for user i\n")
cat("- s is the target standard error threshold\n\n")

cat("Precision constraint in detail:\n")
cat("SE(ŷᵢ) = √(pᵢ(1-pᵢ)/nᵢᵉᶠᶠ) ≤ s\n")
cat("where nᵢᵉᶠᶠ = coverage(i,D) × |D| is effective sample size\n")
cat("and coverage(i,D) = Σⱼ∈D (cᵢⱼ/Σₖcᵢₖ) is visit proportion coverage\n\n")

# Multi-Objective Problem 
cat("2. MULTI-OBJECTIVE PROBLEM (Precision + Regression Validity):\n")
cat(rep("-", 40), "\n")
cat("minimize |D|\n")
cat("subject to:\n")
cat("  (a) SE(ŷᵢ) ≤ s  ∀i ∈ {1,...,n}                    [precision constraint]\n")
cat("  (b) |β̂ⱼ(D) - β̂ⱼ(M)| ≤ τⱼ  ∀j ∈ {1,...,p}          [regression bias constraint]\n\n")

cat("Where:\n")
cat("- β̂ⱼ(D) is the j-th regression coefficient using domain subset D\n")
cat("- β̂ⱼ(M) is the j-th coefficient using all domains M = {1,...,m}\n")
cat("- τⱼ is the maximum allowed bias for coefficient j\n\n")

# Alternative formulations
cat("3. ALTERNATIVE FORMULATIONS:\n")
cat(rep("-", 40), "\n")

cat("FORMULATION A: Hard Constraint on Coefficient Bias\n")
cat("minimize |D|\n")
cat("subject to:\n")
cat("  SE(ŷᵢ) ≤ s  ∀i\n") 
cat("  |β̂ⱼ(D) - β̂ⱼ(M)| ≤ τⱼ  ∀j\n\n")

cat("FORMULATION B: Weighted Objective (Pareto)\n")
cat("minimize λ|D| + (1-λ) Σⱼ wⱼ|β̂ⱼ(D) - β̂ⱼ(M)|\n")
cat("subject to: SE(ŷᵢ) ≤ s  ∀i\n")
cat("where λ ∈ [0,1] trades off efficiency vs regression validity\n")
cat("and wⱼ are coefficient-specific importance weights\n\n")

cat("FORMULATION C: Error Correlation Constraint\n")
cat("minimize |D|\n")
cat("subject to:\n")
cat("  SE(ŷᵢ) ≤ s  ∀i\n")
cat("  |Corr(εᵢ(D), xᵢⱼ)| ≤ ρⱼ  ∀j ∈ {1,...,p}           [error correlation constraint]\n")
cat("where εᵢ(D) = ŷᵢ(D) - yᵢ are selection errors\n")
cat("and xᵢⱼ is the j-th covariate for user i\n\n")

# Implementation challenges
cat("4. COMPUTATIONAL CHALLENGES:\n")
cat(rep("-", 40), "\n")
cat("- Regression coefficients β̂ⱼ(D) are nonlinear in domain selection D\n")
cat("- Error correlations Corr(ε(D), X) are also nonlinear in D\n")
cat("- Problem becomes a mixed-integer nonlinear program (MINLP)\n")
cat("- Need approximation methods or heuristics\n\n")

# Approximation strategies
cat("5. APPROXIMATION STRATEGIES:\n")
cat(rep("-", 40), "\n")

cat("STRATEGY 1: Greedy Approximation\n")
cat("At each step, select domain j* that maximizes:\n")
cat("  Score(j) = α × Precision_Gain(j) - β × Bias_Penalty(j)\n")
cat("where:\n")
cat("  Precision_Gain(j) = Σᵢ [Improvement in SE(ŷᵢ) from adding j]\n")
cat("  Bias_Penalty(j) = Σₖ |Corr(ε(D∪{j}), xₖ)| - |Corr(ε(D), xₖ)|\n\n")

cat("STRATEGY 2: Local Search\n")
cat("1. Start with precision-optimal solution D₀\n")
cat("2. Iteratively swap domains to reduce regression bias\n")
cat("3. Accept swaps that maintain precision constraints\n")
cat("4. Stop when no improving swap exists\n\n")

cat("STRATEGY 3: Decomposition\n")
cat("1. Solve precision-only problem → D_prec\n")
cat("2. Solve bias-minimization problem on D_prec:\n")
cat("   minimize Σⱼ |β̂ⱼ(D) - β̂ⱼ(M)|\n")
cat("   subject to: D ⊆ D_prec, SE constraints still satisfied\n\n")

# Formal algorithm
cat("6. FORMAL GREEDY ALGORITHM:\n")
cat(rep("-", 40), "\n")
cat("Input: cᵢⱼ (visit matrix), X (covariates), s (SE threshold), α, β (weights)\n")
cat("Output: D (selected domains)\n\n")

cat("Algorithm:\n")
cat("1. Initialize D = ∅\n")
cat("2. While precision constraints not satisfied:\n")
cat("   a) For each candidate domain j ∉ D:\n")
cat("      i) Calculate precision gain: Δ_prec(j)\n")
cat("      ii) Calculate bias penalty: Δ_bias(j)\n")  
cat("      iii) Calculate score: Score(j) = α × Δ_prec(j) - β × Δ_bias(j)\n")
cat("   b) Select j* = argmax Score(j)\n")
cat("   c) Add j* to D\n")
cat("3. [Optional] Local improvement phase:\n")
cat("   While can find beneficial swap (i,o) where i ∈ D, o ∉ D:\n")
cat("     If precision maintained and bias reduced: D ← (D \\ {i}) ∪ {o}\n")
cat("4. Return D\n\n")

# Measurement details
cat("7. DETAILED MEASUREMENTS:\n")
cat(rep("-", 40), "\n")

cat("Precision Gain for domain j:\n")
cat("Δ_prec(j) = Σᵢ max(0, nᵢᵉᶠᶠ(D∪{j}) - nᵢᵉᶠᶠ(D)) × I(SE(ŷᵢ,D) > s)\n")
cat("where I(·) is indicator function for unsatisfied users\n\n")

cat("Bias Penalty for domain j:\n")
cat("Δ_bias(j) = Σₖ |Corr(ε(D∪{j}), xₖ)| - |Corr(ε(D), xₖ)|\n")
cat("where ε(D) = [ŷᵢ(D) - yᵢ]ᵢ₌₁ⁿ are selection errors\n\n")

cat("Regression Coefficient Bias:\n")
cat("For each coefficient k: Bias_k(D) = |β̂ₖ(D) - β̂ₖ(M)|\n")
cat("where β̂ₖ(D) = (X'X)⁻¹X'ŷ(D) and ŷ(D) uses only domains in D\n\n")

# Connection to causal inference
cat("8. CONNECTION TO CAUSAL INFERENCE:\n") 
cat(rep("-", 40), "\n")
cat("This problem is related to:\n")
cat("- Sample selection problems in econometrics\n")
cat("- Matching methods with complex treatment assignment\n")
cat("- Survey sampling with auxiliary information\n")
cat("- Active learning with fairness constraints\n\n")

cat("The key insight: Domain selection creates a 'treatment assignment'\n")
cat("where treatment = 'being included in the sample for user i'\n")
cat("If treatment correlates with covariates → selection bias\n")

cat("\n✅ OPTIMIZATION FORMALIZATION COMPLETE!\n")
cat("This shows how to formally incorporate regression constraints into domain selection.\n")