model  <- model("
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(Males);
DATA_VECTOR(Area);
DATA_VECTOR(PDO);
DATA_FACTOR(Annual);
DATA_INTEGER(nAnnual);
DATA_FACTOR(Lek);
DATA_INTEGER(nLek);
DATA_INTEGER(nObs);

PARAMETER(bIntercept);
PARAMETER(bArea);
PARAMETER(bPDO);
PARAMETER_VECTOR(bAnnual);
PARAMETER_VECTOR(bLek);

PARAMETER(log_sAnnual);
PARAMETER(log_sLek);
PARAMETER(log_bPhi);

Type sAnnual = exp(log_sAnnual);
Type sLek = exp(log_sLek);
Type bPhi = exp(log_bPhi);

vector<Type> eMales = Males;

int i;

Type nll = 0.0;

for(i = 0; i < nAnnual; i++){
  nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
}

for(i = 0; i < nLek; i++){
  nll -= dnorm(bLek(i), Type(0), sLek, true);
}

for(i = 0; i < nObs; i++){
  eMales(i) = exp(bIntercept + bArea * Area(i) + bPDO * PDO(i) + bLek(Lek(i)) + bAnnual(Annual(i)));
  nll -= dnbinom2(Males(i), eMales(i), eMales(i) + bPhi * pow(eMales(i),2), true);
}
return nll;
}",
gen_inits = function(data) {
  inits <- list()
  inits$bIntercept = 2.5
  inits$bPDO = 0
  inits$bArea = 0
  inits$log_sAnnual = -1
  inits$log_sLek = 0
  inits$log_bPhi = -1
  inits
},
new_expr = "
for(i in 1:length(Males)) {
  log(prediction[i]) <- bIntercept + bArea * Area[i] + bPDO * PDO[i] + bAnnual[Annual[i]] + bLek[Lek[i]]
}
  fit <- prediction
  residual <- (Males - fit) / sqrt(fit + exp(log_bPhi) * fit^2)
",
random_effects = list(bAnnual = "Annual", bLek = "Lek"),
select_data = list("Males" = 1L, "PDO*" = 1, "Area*" = 1,
                   Annual = factor(1), Lek = factor(1)),
  drops = list("bPDO", "bArea")
)

model_bayesian <- update_model(model,
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
  bArea ~ normal(0, 5);
  bPDO ~ normal(0, 5);
  log_sAnnual ~ normal(0, 5);
  log_sLek ~ normal(0, 5);
  log_bPhi ~ normal(0, 5);

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
