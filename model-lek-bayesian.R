source("model-lek.R")

model %<>% update_model("data {

  int nAnnual;
  int nLek;
  int nObs;

  int Annual[nObs];
  int Lek[nObs];

  int Males[nObs];
  real Area[nObs];
  real PDO[nObs];
  }

  parameters {
  real bIntercept;
  real bArea;
  real bPDO;
  vector[nAnnual] bAnnual;
  vector[nLek] bLek;

  real<lower=0> sAnnual;
  real<lower=0> sLek;
  real<lower=0, upper=5> bPhi;
  }

  model {
  vector[nObs] log_eMales;

  bIntercept ~ normal(0, 5);
  bArea ~ normal(0, 5);
  bPDO ~ normal(0, 5);
  sAnnual ~ normal(0, 5);
  sLek ~ normal(0, 5);

  bAnnual ~ normal(0, sAnnual);
  bLek ~ normal(0, sLek);

  for(i in 1:nObs) {
    log_eMales[i] = bIntercept + bArea * Area[i] + bPDO * PDO[i] + bLek[Lek[i]] + bAnnual[Annual[i]];
    Males[i] ~ neg_binomial_2_log(log_eMales[i], 1/bPhi);
  }
  }",
  gen_inits = function(data) {list()},
  nthin = 10L
)
