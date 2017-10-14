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

PARAMETER(bIntercept);
PARAMETER(bArea);
PARAMETER(bPDO);
PARAMETER_VECTOR(bAnnual);
PARAMETER_VECTOR(bLek);
PARAMETER_VECTOR(bDispersion);

PARAMETER(log_sAnnual);
PARAMETER(log_sLek);
PARAMETER(log_sDispersion);

Type sAnnual = exp(log_sAnnual);
Type sLek = exp(log_sLek);
Type sDispersion = exp(log_sDispersion);

vector<Type> eMales = Males;
vector<Type> eNormDispersion = pnorm(bDispersion, Type(0), Type(1));
vector<Type> eDispersion = qgamma(eNormDispersion, pow(sDispersion, -2), pow(sDispersion, 2));

int nObs = Males.size();

int i;

Type nll = 0.0;

for(i = 0; i < nAnnual; i++){
  nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
}

for(i = 0; i < nLek; i++){
  nll -= dnorm(bLek(i), Type(0), sLek, true);
}

for(i = 0; i < nObs; i++){
  nll -= dnorm(bDispersion(i), Type(0), Type(1), true);
  eMales(i) = exp(bIntercept + bArea * Area(i) + bPDO * PDO(i) + bLek(Lek(i)) + bAnnual(Annual(i)));
  nll -= dpois(Males(i), eMales(i) * eDispersion(i), true);
}
return nll;
}",
gen_inits = function(data) {
  inits <- list()
  inits$bIntercept = 0
  inits$bPDO = 0
  inits$bArea = 0
  inits$log_sAnnual = 0
  inits$log_sLek = 0
  inits$log_sDispersion = 0
  inits
},
new_expr = "
for(i in 1:length(Males)) {
  log(prediction[i]) <- bIntercept + bArea * Area[i] + bPDO * PDO[i] + bAnnual[Annual[i]] + bLek[Lek[i]]
}
  fit <- prediction
  residual <- (Males - fit) / sqrt(fit + (fit * exp(log_sDispersion))^2)
",
random_effects = list(bAnnual = "Annual", bLek = "Lek", bDispersion = "Dispersion"),
select_data = list("Males" = 1L, "PDO*" = 1, "Area*" = 1,
                   Annual = factor(1), Lek = factor(1), Dispersion = factor(1)),
  drops = list("bPDO", "bArea")
)
