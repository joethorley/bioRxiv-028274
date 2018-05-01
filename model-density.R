model_bayesian <- model(
"data {

  int nAnnual;
  int nLek;
  int nObs;

  int Annual[nObs];
  int Lek[nObs];

  int Males[nObs];
}

parameters {
  real bIntercept;
  vector[nAnnual] bAnnual;
  vector[nLek] bLek;

  real log_sAnnual;
  real log_sLek;
  real log_bPhi;
}

transformed parameters{
  real sAnnual = exp(log_sAnnual);
  real sLek = exp(log_sLek);
  real bPhi = exp(log_bPhi);
}

model {
  vector[nObs] log_eMales;

  bIntercept ~ normal(0, 5);
  log_sAnnual ~ normal(0, 5);
  log_sLek ~ normal(0, 5);
  log_bPhi ~ normal(0, 5);

  bAnnual ~ normal(0, sAnnual);
  bLek ~ normal(0, sLek);

  for(i in 1:nObs) {
    log_eMales[i] = bIntercept + bLek[Lek[i]] + bAnnual[Annual[i]];
    Males[i] ~ neg_binomial_2_log(log_eMales[i], 1/bPhi);
  }
}",
  gen_inits = function(data) {list()},
  new_expr = "
  for(i in 1:length(Males)) {
    log(prediction[i]) <- bIntercept + bAnnual[Annual[i]] + bLek[Lek[i]]
  }
  fit <- prediction
  residual <- (Males - fit) / sqrt(fit + exp(log_bPhi) * fit^2)
",
  random_effects = list(bAnnual = "Annual", bLek = "Lek"),
  select_data = list("Males" = 1L, Annual = factor(1), Lek = factor(1)),
  nthin = 10L
)
