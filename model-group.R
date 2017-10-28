model  <- model(
  "#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

  DATA_MATRIX(Males);
  DATA_MATRIX(PDO);
  DATA_MATRIX(Area);
  DATA_IVECTOR(FirstAnnual);
  DATA_INTEGER(nGroup);
  DATA_INTEGER(nAnnual);

  PARAMETER(bIntercept);
  PARAMETER(bDensity);
  PARAMETER(bPDO);
  PARAMETER(bArea);
  PARAMETER(bInitialIntercept);
  PARAMETER_VECTOR(bInitial);
  PARAMETER_VECTOR(bGroup);
  PARAMETER_VECTOR(bAnnual);
  PARAMETER_MATRIX(bProcess);
  PARAMETER(log_sGroup);
  PARAMETER(log_sInitial);
  PARAMETER(log_sAnnual);
  PARAMETER(log_sProcess);
  PARAMETER(log_sObservation);

  Type sGroup = exp(log_sGroup);
  Type sInitial = exp(log_sInitial);
  Type sAnnual = exp(log_sAnnual);
  Type sProcess = exp(log_sProcess);
  Type sObservation = exp(log_sObservation);

  matrix<Type> log_eMales = Males;

  int i,j;

  Type nll = 0.0;

  for(i = 0; i < nAnnual; i++){
    nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
  }

  for(i = 0; i < nGroup; i++) {
    for(j = 0; j < nAnnual; j++){
      nll -= dnorm(bProcess(i,j), Type(0), sProcess, true);
    }
  }

  for(i = 0; i < nGroup; i++) {
    nll -= dnorm(bInitial(i), bInitialIntercept, sInitial, true);
    nll -= dnorm(bGroup(i), Type(0), sGroup, true);

    log_eMales(i,FirstAnnual(i)-1) = bIntercept + (bDensity + 1 + bGroup(i)) * bInitial(i) + bArea * Area(i,FirstAnnual(i)-1) + bPDO * PDO(i,FirstAnnual(i)-1) + bAnnual(FirstAnnual(i)-1) + bProcess(i,FirstAnnual(i)-1);

    nll -= dnorm(log(Males(i,FirstAnnual(i)-1)), log_eMales(i,FirstAnnual(i)-1), sObservation, true);

    for(j = FirstAnnual(i); j < nAnnual; j++) {
      log_eMales(i,j) = bIntercept + (bDensity + 1 + bGroup(i)) * log_eMales(i,j-1) + bArea * Area(i,j) + bPDO * PDO(i,j) + bAnnual(j) + bProcess(i,j);

      nll -= dnorm(log(Males(i,j)), log_eMales(i,j), sObservation, true);
    }
  }
  ADREPORT(log_eMales);

  return nll;
}",
new_expr = "
  for(i in 1:length(Males)) {
    prediction[i] <- exp(bIntercept + (bDensity + 1 + bGroup[Group[i]]) * log(Males[i]) + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]])
    kappa[i] <- exp(-(bIntercept + bPDO * PDO[i] + bArea * Area[i] + bAnnual[Annual[i]])  / (bDensity + bGroup[Group[i]]))
    fit[i] <- log_eMales[Group[i], Annual[i]]
    residual[i] <- (log(Males[i]) - fit[i]) / exp(log_sObservation)
  }
",
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

    data
  },
  gen_inits = function(data) {
    inits <- list()
    inits$bIntercept <- 0
    inits$bDensity <- 0
    inits$bPDO <- 0
    inits$bArea <- 0
    inits$bInitialIntercept <- 0
    inits$bGroup <- rep(0, data$nGroup)
    inits$bAnnual <- rep(0, data$nAnnual)
    inits$bProcess <- matrix(0, nrow = data$nGroup, ncol = data$nAnnual)
    inits$bInitial <- rep(0, data$nGroup)
    inits$log_sInitial <- 0
    inits$log_sGroup <- 0
    inits$log_sAnnual <- 0
    inits$log_sProcess <- 0
    inits$log_sObservation <- 0

    inits
  },
  derived = "log_eMales",
  random_effects = list(bInitial = "Group", bGroup = "Group", bAnnual = "Annual", bProcess = c("Group", "Annual")),
  select_data = list("Males" = 1, "PDO*" = 1, "Area*" = 1,
                     Annual = factor(1), Group = factor(1)),
  drops = list("bPDO", "bArea")
)
