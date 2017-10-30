b0 <- 0.8
bD <- -1
bA <- 0.1
A <- 0

kappa0 <- exp(-(b0 + bA * A) / bD)
kappa0

exp(b0 + (bD + 1) * log(kappa0) + bA * A)

A <- 1

kappa1 <- exp(-(b0 + bA * A) / bD)
kappa1

(kappa1 - kappa0) / kappa0 * 100

(exp(b0 + (bD + 1) * log(kappa0) + bA * A) - kappa0) / kappa0 * 100
(exp(bA * A) - 1) * 100
