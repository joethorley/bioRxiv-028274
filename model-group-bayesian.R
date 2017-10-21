model  <- model(
" data {
  int nGroup;
  int nAnnual;

  matrix[nGroup,nAnnual] Males;
  matrix[nGroup,nAnnual] PDO;
  matrix[nGroup,nAnnual] Area;

  int FirstAnnual[nGroup];
}

parameters {
  real bIntercept;
  real bDensity;
  real bPDO;
  real bArea;
  real bInitialIntercept;

  vector[nAnnual] bAnnual;
  vector[nGroup] bGroup;
  vector[nGroup] bInitial;

  matrix[nGroup,nAnnual] bProcess;

  real<lower=0> sGroup;
  real<lower=0> sInitial;
  real<lower=0> sAnnual;
  real<lower=0> sProcess;
  real<lower=0> sObservation;
}

model {

  matrix[nGroup,nAnnual] log_eMales;

  bIntercept ~ normal(0, 2);
  bDensity ~ normal(0, 1);
  bPDO ~ normal(0, 1);
  bArea ~ normal(0, 1);
  bInitialIntercept ~ normal(0, 5);

  sGroup ~ normal(0, 1);
  sInitial ~ normal(0, 1);
  sAnnual ~ normal(0, 1);
  sProcess ~ normal(0, 1);
  sObservation ~ normal(0, 1);

  bAnnual ~ normal(0, sAnnual);
  bGroup ~ normal(0, sGroup);
  bInitial ~ normal(0, sInitial);

  for(i in 1:nGroup) {
     bProcess[i,FirstAnnual[i]] ~ normal(0, sProcess);
    log_eMales[i,FirstAnnual[i]] = bIntercept + (bDensity + 1 + bGroup[i]) * bInitial[i] + bArea * Area[i,FirstAnnual[i]] + bPDO * PDO[i,FirstAnnual[i]] + bAnnual[FirstAnnual[i]] + bProcess[i,FirstAnnual[i]];
    Males[i,FirstAnnual[i]] ~ lognormal(log_eMales[i,FirstAnnual[i]], sObservation);
    for(j in (FirstAnnual[i]+1):nAnnual) {
      bProcess[i,j] ~ normal(0, sProcess);
      log_eMales[i,j] = bIntercept + (bDensity + 1 + bGroup[i]) * log_eMales[i,j-1] + bArea * Area[i,j] + bPDO * PDO[i,j] + bAnnual[j] + bProcess[i,j];
      Males[i,j] ~ lognormal(log_eMales[i,j], sObservation);
    }
  }
}",
new_expr = "
for(i in 1:length(Males)) {
prediction[i] <- bIntercept + (bDensity + bGroup[Group[i]]) * log(Males[i]) + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]]
kappa[i] <- exp(-(bIntercept + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]])  / (bDensity + bGroup[Group[i]]))
}",
modify_data = function(data) {

  arrayize <- function(x) {
    x %<>%
      dplyr::data_frame(Group = data$Group, Annual = data$Annual) %>%
      reshape2::acast(Group ~ Annual, value.var = ".")
    x
  }
  data[c("Males", "PDO", "Area")] %<>%
    lapply(arrayize)

  data$FirstAnnual <- apply(data$Males, 1, function(x) {min(which(!is.na(x)))})

  data$Males[is.na(data$Males)] <- 1L
  data$PDO[is.na(data$PDO)] <- 0
  data$Area[is.na(data$Area)] <- 0

  data
},
gen_inits = function(data) {
  inits <- list()

  inits$bDensity <- 0
  inits$bIntercept <- 0.75
  inits$bInitialIntercept <- 3
  inits$sAnnual <- 0.1
  inits$sGroup <- 0.01
  inits$sInitial <- 0.5
  inits$sObservation <- 0.1
  inits$sProcess <- 0.1
  inits
},
random_effects = list(bInitial = "Group", bGroup = "Group", bAnnual = "Annual", bProcess = c("Group", "Annual")),
select_data = list("Males" = 1, "PDO*" = 1, "Area*" = 1,
                   Annual = factor(1), Group = factor(1)),
nthin = 10L
)

# if(FirstAnnual[i] > 1) {
#   for(j in 1:(FirstAnnual[i]-1))
#     bProcess[i,j] ~ normal(0, sProcess);
# }

