library(pwr)

# Using the pwr package to calculate power for comparing two proportions
effect_size <- ES.h(p1 = 0.35, p2 = 0.4)  # hypothetical proportions of a race in two types of counties

# Calculate power given effect size, sample size, significance level, and ratio of group sizes
power_analysis <- pwr.p.test(h = effect_size, n = 3235, sig.level = 0.01, power = NULL, alternative = "two.sided")

print(power_analysis)
