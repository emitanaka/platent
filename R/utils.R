use_multicore <- function(ncores) {
  if (!is.null(ncores)) {
    ncores <- ifelse(ncores > parallel::detectCores() - 1,
                     parallel::detectCores() - 1,
                     ncores
    )
    if (ncores > 1) doMC::registerDoMC(ncores)
  }
}

#' Parse formula to extract the design matrices for fixed and random effects
#'
#' @param formula the model formula
#' @param data a data frame that contain variables in `formula`
#'
#' @return the X and Z matrices
parse_formula <- function(formula, data = NULL) {

  tmp <- deparse(formula)
  parsed <- as.formula(gsub("id", "as.factor", gsub("fa", "as.factor", tmp)))
  designMatrix <- model.matrix(parsed, data)
  Z <- designMatrix[, grep(":", colnames(designMatrix))]
  X <- designMatrix[, -grep(":", colnames(designMatrix))]

  return(X, Z)
}







