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
  matrix[nLek, nAnnual] bLekAnnual;

  real log_sAnnual;
  real log_sLek;
  real log_sLekAnnual;
  real log_bPhi;
}

transformed parameters{
  real sAnnual = exp(log_sAnnual);
  real sLek = exp(log_sLek);
  real sLekAnnual = exp(log_sLekAnnual);
}

model {
  vector[nObs] eMales;

  bIntercept ~ normal(0, 5);
  log_sAnnual ~ normal(0, 5);
  log_sLek ~ normal(0, 5);
  log_sLekAnnual ~ normal(0, 5);

  bAnnual ~ normal(0, sAnnual);
  bLek ~ normal(0, sLek);

  for(j in 1:nAnnual) {
    bLekAnnual[,j] ~ normal(0, sLekAnnual);
  }

  for(i in 1:nObs) {
    eMales[i] = exp(bIntercept + bLek[Lek[i]] + bAnnual[Annual[i]] + bLekAnnual[Lek[i],Annual[i]]);
    Males[i] ~ poisson(eMales[i]);
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
  nthin = 100L
)
