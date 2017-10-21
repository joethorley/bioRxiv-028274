source("model-lek.R")

model <- model(
"data {

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

  real log_sAnnual;
  real log_sLek;
  real<lower=0,upper=5> bPhi;
  }

  transformed parameters {
  real sAnnual = exp(log_sAnnual);
  real sLek = exp(log_sLek);
  }

  model {
  vector[nObs] eMales;

  bIntercept ~ normal(0, 5);
  bArea ~ normal(0, 5);
  bPDO ~ normal(0, 5);
  log_sAnnual ~ normal(0, 5);
  log_sLek ~ normal(0, 5);

  bAnnual ~ normal(0, sAnnual);
  bLek ~ normal(0, sLek);

  for(i in 1:nObs) {
    eMales[i] = bIntercept + bArea * Area[i] + bPDO * PDO[i] + bLek[Lek[i]] + bAnnual[Annual[i]];
    Males[i] ~ neg_binomial_2_log(eMales[i], bPhi);
  }
}",
new_expr = "
for(i in 1:length(Males)) {
  log(prediction[i]) <- bIntercept + bArea * Area[i] + bPDO * PDO[i] + bAnnual[Annual[i]] + bLek[Lek[i]]
}
  fit <- prediction
  residual <- (Males - fit) / sqrt(fit + (fit * exp(log_sDispersion))^2)
",
random_effects = list(bAnnual = "Annual", bLek = "Lek"),
select_data = list("Males" = 1L, "PDO*" = 1, "Area*" = 1,
                   Annual = factor(1), Lek = factor(1), Dispersion = factor(1)),
nthin = 10L
)
