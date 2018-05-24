model  <- model(
  "#include <TMB.hpp>

  template<class Type>
  Type objective_function<Type>::operator() () {

  DATA_VECTOR(Males);
  DATA_VECTOR(Males1);
  DATA_VECTOR(PDO);
  DATA_VECTOR(Area);
  DATA_FACTOR(Group);
  DATA_FACTOR(Annual);
  DATA_INTEGER(nGroup);
  DATA_INTEGER(nAnnual);
  DATA_INTEGER(nObs);

  PARAMETER(bIntercept);
  PARAMETER(bDensity);
  PARAMETER(bPDO);
  PARAMETER(bArea);
  PARAMETER_VECTOR(bGroup);
  PARAMETER_VECTOR(bAnnual);
  PARAMETER(log_sGroup);
  PARAMETER(log_sAnnual);
  PARAMETER(log_sProcess);

  Type sGroup = exp(log_sGroup);
  Type sAnnual = exp(log_sAnnual);
  Type sProcess = exp(log_sProcess);

  vector<Type> log_eMales = Males;

  int i;

  Type nll = 0.0;

  for(i = 0; i < nAnnual; i++){
  nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
  }

  for(i = 0; i < nGroup; i++) {
    nll -= dnorm(bGroup(i), Type(0), sGroup, true);
  }

  for(i = 0; i < nObs; i++) {
    log_eMales(i) = bIntercept + (bDensity + 1 + bGroup(Group(i))) * log(Males1(i)) + bArea * Area(i) + bPDO * PDO(i) + bAnnual(Annual(i));

  nll -= dnorm(log(Males(i)), log_eMales(i), sProcess, true);
  }
  ADREPORT(log_eMales);

  return nll;
  }",
  new_expr = "
for(i in 1:length(Males)) {
prediction[i] <- exp(bIntercept + (bDensity + 1 + bGroup[Group[i]]) * log(Males1[i]) + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]])
kappa[i] <- exp(-(bIntercept + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]])  / (bDensity + bGroup[Group[i]]))
fit[i] <- prediction[i]
residual[i] <- (log(Males[i]) - log(fit[i])) / exp(log_sProcess)
}
",
gen_inits = function(data) {
  inits <- list()
  inits$bIntercept <- 0
  inits$bDensity <- 0
  inits$bPDO <- 0
  inits$bArea <- 0
  inits$bGroup <- rep(0, data$nGroup)
  inits$bAnnual <- rep(0, data$nAnnual)
  inits$log_sGroup <- 0
  inits$log_sAnnual <- 0
  inits$log_sProcess <- 0

  inits
},
random_effects = list(bGroup = "Group", bAnnual = "Annual"),
select_data = list("Males" = c(1, 100), "Males1" = c(1, 100),
                   "PDO*" = 1, "Area*" = 1,
                   Annual = factor(1), Group = factor(1)),
drops = list("bPDO", "bArea")
)

model_bayesian  <- update_model(model,
                                " data {
  int nGroup;
  int nAnnual;
  int nObs;

  real Males[nObs];
  real Males1[nObs];
  real Area[nObs];
  real PDO[nObs];
  int Group[nObs];
  int Annual[nObs];
}

parameters {
  real bIntercept;
  real bDensity;
  real bPDO;
  real bArea;

  vector[nAnnual] bAnnual;
  vector[nGroup] bGroup;

  real log_sGroup;
  real log_sAnnual;
  real log_sProcess;
}

transformed parameters {
  real sGroup = exp(log_sGroup);
  real sAnnual = exp(log_sAnnual);
  real sProcess = exp(log_sProcess);
}

  model {

  vector[nObs] log_eMales;

  bArea ~ normal(0, 5);
  bDensity ~ normal(0, 5);
  bIntercept ~ normal(0, 5);
  bPDO ~ normal(0, 5);

  log_sAnnual ~ normal(0, 5);
  log_sGroup ~ normal(0, 5);
  log_sProcess ~ normal(0, 5);

  bAnnual ~ normal(0, sAnnual);
  bGroup ~ normal(0, sGroup);

  for(i in 1:nObs) {
  log_eMales[i] = bIntercept + (bDensity + 1 + bGroup[Group[i]]) * log(Males1[i]) + bArea * Area[i] + bPDO * PDO[i] + bAnnual[Annual[i]];
  Males[i] ~ lognormal(log_eMales[i], sProcess);
}
}",
derived = character(0),
gen_inits = function(data) {
  inits <- list()
  inits
},
nthin = 50L)
