# Billionaire Tax Act: NPV Distribution of Outcomes
# Date: February 2026
# Purpose: Simulate NPV across plausible parameter ranges, plot distribution
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

#Working directory for home computer
setwd("/Users/ben/Documents/GitHub/wealth_tax")

# =============================================================================
# PART I: Define Parameters
# =============================================================================

# Fixed
baseline_revenue <- 107.04  # $B, no-departure wealth tax revenue

# Number of simulations
n_sims <- 100000

# =============================================================================
# PART II: Generate Random Parameter Draws
# =============================================================================

# WT: Wealth tax revenue ($44B to $66B, centered at $48B)
#   Triangular-ish: uniform draw across plausible range
wt <- runif(n_sims, min = 44, max = 66)

# C: Annual income tax contribution ($4.9B to $5.8B)
#   Uniform across Splinter lower bound to Pareto upper bound
c_income <- runif(n_sims, min = 4.9, max = 5.8)

# r: Real discount rate (3% to 7%)
#   Uniform across standard range
r_discount <- runif(n_sims, min = 0.015, max = 0.07)

# =============================================================================
# PART III: Compute NPV for Each Draw
# =============================================================================

# Implied departure fraction
f <- 1 - (wt / baseline_revenue)

# Annual lost income tax
annual_loss <- f * c_income

# PV of lost income tax stream (inflation-linked perpetuity)
pv_lost <- annual_loss / r_discount

# Net present value
npv <- wt - pv_lost

# Assemble results
results <- data.frame(
  wt = wt,
  c_income = c_income,
  r_discount = r_discount,
  f = f,
  annual_loss = annual_loss,
  pv_lost = pv_lost,
  npv = npv
)

# =============================================================================
# PART IV: Summary Statistics
# =============================================================================

cat("=== NPV Distribution Summary ===\n")
cat(sprintf("  Simulations:        %d\n", n_sims))
cat(sprintf("  Mean NPV:           $%.1fB\n", mean(results$npv)))
cat(sprintf("  Median NPV:         $%.1fB\n", median(results$npv)))
cat(sprintf("  Std Dev:            $%.1fB\n", sd(results$npv)))
cat(sprintf("  Min NPV:            $%.1fB\n", min(results$npv)))
cat(sprintf("  Max NPV:            $%.1fB\n", max(results$npv)))
cat(sprintf("  Pct Negative NPV:   %.1f%%\n", 100 * mean(results$npv < 0)))
cat(sprintf("  Pct Below -$10B:    %.1f%%\n", 100 * mean(results$npv < -10)))
cat(sprintf("  Pct Below -$25B:    %.1f%%\n", 100 * mean(results$npv < -25)))

# =============================================================================
# PART V: Plot Distribution of NPV Outcomes
# =============================================================================

pct_negative <- round(100 * mean(results$npv < 0), 1)

p <- ggplot(results, aes(x = npv)) +
  geom_histogram(aes(fill = npv < 0), bins = 80, alpha = 0.85, color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "#B3173C", "FALSE" = "#2C5F8A"),
    labels = c("TRUE" = "Negative NPV", "FALSE" = "Positive NPV"),
    name = NULL
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text", x = -5, y = Inf, vjust = 2, hjust = 1,
           label = paste0(pct_negative, "% of draws\nyield negative NPV"),
           size = 4, fontface = "bold", color = "#B3173C") +
  labs(
    title = "Distribution of Net Present Value: Billionaire Tax Act",
    subtitle = paste0("100,000 draws: WT ~ U[$44B, $66B], C ~ U[$4.9B, $5.8B], r ~ U[3%, 7%]"),
    x = "Net Present Value ($B)",
    y = "Count"
  ) +
  theme_few() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

ggsave("npv_distribution.png", p, width = 10, height = 6, dpi = 300)
ggsave("npv_distribution.pdf", p, width = 10, height = 6)

cat("\nPlots saved: npv_distribution.png, npv_distribution.pdf\n")


#