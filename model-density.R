model_bayesian <- model(
"model {
  bIntercept ~ dnorm(0, 5^-2)
  log_sAnnual ~ dnorm(0, 5^-2)
  log_sLek ~ dnorm(0, 5^-2)
  log_sLekAnnual ~ dnorm(0, 5^-2)

  log(sAnnual) <- log_sAnnual
  log(sLek) <- log_sLek
  log(sLekAnnual) <- log_sLekAnnual

  for(i in 1:nAnnual) {
    bAnnual[i] ~ dnorm(0, sAnnual^-2)
  }
  for(i in 1:nLek) {
    bLek[i] ~ dnorm(0, sLek^-2)
    for(j in 1:nAnnual) {
      bLekAnnual[i,j] ~ dnorm(0, sLekAnnual^-2)
    }
  }

  for(i in 1:nObs) {
    log(eMales[i]) = bIntercept + bLek[Lek[i]] + bAnnual[Annual[i]] + bLekAnnual[Lek[i],Annual[i]]
    Males[i] ~ dpois(eMales[i])
  }
}",
  gen_inits = function(data) {list()},
  new_expr = "
  for(i in 1:length(Males)) {
    log(prediction[i]) <- bIntercept + bAnnual[Annual[i]] + bLek[Lek[i]] + bLekAnnual[Lek[i],Annual[i]]
  }
  fit <- prediction
  residual <- (Males - fit) / sqrt(fit + exp(log_bPhi) * fit^2)
",
  random_effects = list(bAnnual = "Annual", bLek = "Lek", bLekAnnual = c("Lek", "Annual")),
  select_data = list("Males" = 1L, Annual = factor(1), Lek = factor(1)),
  nthin = 500L
)
