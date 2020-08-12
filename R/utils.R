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
#' @param formula the model formula, e.g., y ~ v1 + id(v2):fa(v3)
#' @param data a data frame that contain variables in `formula`
#'
#' @return the X and Z matrices
parse_formula <- function(formula = y ~ v1 + v4 + id(v2):fa(v3), data = NULL) {

  cov_str <- c("id", "fa")
  model_terms <- terms(formula, specials = cov_str, keep.order = TRUE)
  var_labels <- as.character(attr(model_terms, "variables")[-1])
  term_labels <- colnames(attr(model_terms, "factors"))
  spl_locs <- unlist(attr(model_terms, "specials"))
  resp_loc <- attr(model_terms, "response")

  # separate variables
  response <- var_labels[resp_loc]
  fixed_var <- intersect(var_labels, term_labels)
  fixed <- paste0(fixed_var, collapse = "+")
  random_var <- gsub(paste0(c("\\)", paste(cov_str, "\\(", sep = "")),
                        collapse = "|"), "", var_labels[spl_locs])
  random <- paste0(random_var, collapse = ":")

  fixed_formula <- as.formula(paste(response, fixed, sep = "~"))
  X <- model.matrix(fixed_formula, data)

  random_formula <- as.formula(paste(paste(response, random, sep = "~"), "-1"))
  Z <- model.matrix(random_formula, data)

  return(list(X = X, Z = Z))

}
