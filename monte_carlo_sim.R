# Billionaire Tax Act: Monte Carlo Simulation for 
# Date: March  2026
# Purpose: Simulate plausible outcomes for income taxes paid. 
# =============================================================================
# PART 0: Load Packages
# =============================================================================
library(dplyr)
library(ggplot2)
library(ggthemes)

# Set seed
set.seed(2026)

# Set working directory (update as needed)
setwd("~/Documents/GitHub/wealth_tax")

################################################################################
# Monte Carlo Simulation: Billionaire Income Tax Contributions
# 
# Purpose: Estimate the aggregate CA income tax liability of 212 billionaires
#          under the assumption that they are NOT necessarily the top 212
#          earners in the $10M+ AGI bracket, but are instead randomly
#          distributed across the top K positions (K = 212, ..., 2000).
#
# Approach:
#   1. Fit a Pareto distribution to the upper tail of the CA AGI distribution
#      using published FTB data (TY 2023).
#   2. Generate a Pareto-implied income schedule for all 4,729 filers in the
#      $10M+ bracket.
#   3. For each value of K (212, 250, 300, ..., 2000):
#        - Draw 212 positions uniformly at random from ranks 1:K
#        - Sum the Pareto-implied tax liabilities at those positions
#        - Repeat 100,000 times
#   4. Report mean, median, 5th/95th percentiles for each K.
#   5. Scale from TY 2023 to FY 2024-25.
#
# Authors: Rauh, Jaros, Kearney, Doran, Cosso
# Date: March 2026
################################################################################

library(ggplot2)

set.seed(42)

# =============================================================================
# 1. DATA AND PARETO FIT
# =============================================================================

# FTB published cumulative filer counts above AGI thresholds (TY 2023)
thresholds <- c(1e6, 2e6, 3e6, 4e6, 5e6, 10e6)
filer_counts <- c(131400, 43900, 24700, 16700, 12200, 4729)

# OLS on log-linearized Pareto survival function: ln(N) = ln(A) - alpha * ln(y)
fit <- lm(log(filer_counts) ~ log(thresholds))
alpha <- -coef(fit)[2]
ln_A  <- coef(fit)[1]
A     <- exp(ln_A)
r_sq  <- summary(fit)$r.squared

cat(sprintf("Pareto fit: alpha = %.4f, R-squared = %.4f\n", alpha, r_sq))
cat(sprintf("Paper reports: alpha = 1.44, R-squared = 0.999\n\n"))

# =============================================================================
# 2. GENERATE PARETO-IMPLIED INCOME SCHEDULE
# =============================================================================

# Total filers in $10M+ bracket
n_filers <- 4729

# Bracket parameters
bracket_tax_ty23 <- 11.1  # $B, total tax liability in bracket (TY 2023)

# For a Pareto distribution with survival function N(y) = A * y^(-alpha),
# the income at rank r (r=1 is highest) is:
#   y_r = (A / r)^(1/alpha)
#
# The share of total bracket income attributable to rank r is:
#   y_r / sum(y_i for i=1..n)
#
# Under Pareto, tax liability is approximately proportional to income
# (all filers are in the top 13.3% bracket), so:
#   tax_r / total_bracket_tax = y_r / sum(y_i)

ranks <- 1:n_filers
incomes <- (A / ranks)^(1 / alpha)

# Normalize to get each filer's share of total bracket tax
income_shares <- incomes / sum(incomes)
tax_by_rank <- income_shares * bracket_tax_ty23  # $B per filer

# Verification: top 212 share
top_212_share <- sum(income_shares[1:212])
top_212_tax   <- sum(tax_by_rank[1:212])
cat(sprintf("Verification:\n"))
cat(sprintf("  Top 212 share of bracket income: %.1f%%\n", top_212_share * 100))
cat(sprintf("  Top 212 tax liability (TY 2023): $%.2fB\n", top_212_tax))
cat(sprintf("  Paper formula S(212,4729) check:  %.1f%%\n", (212/4729)^((alpha-1)/alpha) * 100))
cat(sprintf("\n"))

# =============================================================================
# 3. SCALING FACTOR: TY 2023 → FY 2024-25
# =============================================================================

# Total PIT in TY 2023: bracket was 11.4% of total
total_pit_ty23 <- bracket_tax_ty23 / 0.114  # ~$97.4B
total_pit_fy25 <- 130.0  # $B, FY 2024-25
scale_factor   <- total_pit_fy25 / total_pit_ty23

cat(sprintf("Scaling: TY2023 total PIT = $%.1fB, FY24-25 = $%.0fB, factor = %.4f\n\n",
            total_pit_ty23, total_pit_fy25, scale_factor))

# =============================================================================
# 4. MONTE CARLO SIMULATION
# =============================================================================

n_billionaires <- 212
n_sims         <- 100000

# Sweep K from 212 to 2000
K_values <- c(212, seq(250, 2000, by = 50))

# Pre-allocate results
results <- data.frame(
  K        = integer(),
  mean     = numeric(),
  median   = numeric(),
  p05      = numeric(),
  p10      = numeric(),
  p25      = numeric(),
  p75      = numeric(),
  p90      = numeric(),
  p95      = numeric(),
  sd       = numeric()
)

cat("Running Monte Carlo simulations...\n")

for (K in K_values) {
  
  # For each simulation: draw 212 ranks from 1:K without replacement,
  # sum their tax liabilities
  sim_totals <- replicate(n_sims, {
    drawn_ranks <- sample(1:K, size = n_billionaires, replace = FALSE)
    sum(tax_by_rank[drawn_ranks])
  })
  
  results <- rbind(results, data.frame(
    K      = K,
    mean   = mean(sim_totals),
    median = median(sim_totals),
    p05    = quantile(sim_totals, 0.05),
    p10    = quantile(sim_totals, 0.10),
    p25    = quantile(sim_totals, 0.25),
    p75    = quantile(sim_totals, 0.75),
    p90    = quantile(sim_totals, 0.90),
    p95    = quantile(sim_totals, 0.95),
    sd     = sd(sim_totals)
  ))
  
  if (K %% 500 == 0 || K == 212) {
    cat(sprintf("  K = %4d: mean = $%.2fB, median = $%.2fB, [p05 = $%.2fB, p95 = $%.2fB]\n",
                K, mean(sim_totals), median(sim_totals),
                quantile(sim_totals, 0.05), quantile(sim_totals, 0.95)))
  }
}

cat("\nDone.\n\n")

# =============================================================================
# 5. SCALE TO FY 2024-25
# =============================================================================

results$mean_fy25   <- results$mean   * scale_factor
results$median_fy25 <- results$median * scale_factor
results$p05_fy25    <- results$p05    * scale_factor
results$p10_fy25    <- results$p10    * scale_factor
results$p25_fy25    <- results$p25    * scale_factor
results$p75_fy25    <- results$p75    * scale_factor
results$p90_fy25    <- results$p90    * scale_factor
results$p95_fy25    <- results$p95    * scale_factor

# =============================================================================
# 6. SUMMARY TABLE
# =============================================================================

cat("=================================================================\n")
cat("RESULTS: Aggregate Tax Liability of 212 Billionaires (FY 2024-25)\n")
cat("=================================================================\n")
cat(sprintf("%-6s %10s %10s %10s %10s\n", "K", "Mean($B)", "Median($B)", "P05($B)", "P95($B)"))
cat(paste(rep("-", 50), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  cat(sprintf("%-6d %10.2f %10.2f %10.2f %10.2f\n",
              results$K[i],
              results$mean_fy25[i],
              results$median_fy25[i],
              results$p05_fy25[i],
              results$p95_fy25[i]))
}

# Print the upper bound (K=212, deterministic)
cat(sprintf("\nUpper bound (K=212, deterministic): $%.2fB (FY 24-25)\n",
            top_212_tax * scale_factor))

# =============================================================================
# 7. PLOT
# =============================================================================

p <- ggplot(results, aes(x = K)) +
  # Confidence band: 5th to 95th percentile
  geom_ribbon(aes(ymin = p05_fy25, ymax = p95_fy25), 
              fill = "steelblue", alpha = 0.2) +
  # Interquartile range
  geom_ribbon(aes(ymin = p25_fy25, ymax = p75_fy25), 
              fill = "steelblue", alpha = 0.3) +
  # Mean line
  geom_line(aes(y = mean_fy25), color = "steelblue", linewidth = 1.2) +
  # Median line
  geom_line(aes(y = median_fy25), color = "darkblue", linewidth = 0.8, 
            linetype = "dashed") +
  # Reference: upper bound (top 212 = top 212)
  geom_hline(yintercept = top_212_tax * scale_factor, 
             color = "red", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 1800, y = top_212_tax * scale_factor + 0.15,
           label = sprintf("Upper bound: $%.1fB", top_212_tax * scale_factor),
           color = "red", size = 3.5, hjust = 1) +
  labs(
    title = "Aggregate Income Tax Liability of 212 Billionaires",
    subtitle = "Monte Carlo simulation: 212 drawn from top K of 4,729 filers (100,000 draws per K)",
    x = "Pool size K (212 = billionaires are top earners; larger K = more dispersed)",
    y = "Aggregate tax liability ($B, FY 2024-25)",
    caption = "Solid blue = mean; dashed = median; light band = 5th-95th percentile; dark band = IQR"
  ) +
  scale_x_continuous(breaks = seq(200, 2000, by = 200)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "B")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("billionaire_tax_monte_carlo.pdf", p, width = 10, height = 6.5)
ggsave("billionaire_tax_monte_carlo.png", p, width = 10, height = 6.5, dpi = 300)

cat("\nPlots saved: billionaire_tax_monte_carlo.pdf / .png\n")

# =============================================================================
# 8. EXPORT RESULTS
# =============================================================================

write.csv(results, "monte_carlo_results.csv", row.names = FALSE)
cat("Results saved: monte_carlo_results.csv\n")

# =============================================================================
# 9. KEY TAKEAWAYS FOR PAPER
# =============================================================================

cat("\n=================================================================\n")
cat("KEY NUMBERS FOR SECTION 5.1.1\n")
cat("=================================================================\n")
cat(sprintf("Upper bound (Pareto, K=212):     $%.2fB (FY 24-25)\n",
            top_212_tax * scale_factor))

# Find the K where mean ≈ Splinter lower bound ($4.9B)
splinter_target <- 4.9
closest_idx <- which.min(abs(results$mean_fy25 - splinter_target))
cat(sprintf("Splinter lower bound ($4.9B) ≈ K = %d (mean = $%.2fB)\n",
            results$K[closest_idx], results$mean_fy25[closest_idx]))

# Report a few key K values
for (K_report in c(500, 750, 1000, 1500, 2000)) {
  row <- results[results$K == K_report, ]
  if (nrow(row) > 0) {
    cat(sprintf("K = %4d: mean = $%.2fB, 90%% CI = [$%.2fB, $%.2fB]\n",
                K_report, row$mean_fy25, row$p05_fy25, row$p95_fy25))
  }
}

