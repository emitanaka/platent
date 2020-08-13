ofal_control <- function(ncores = NULL,
                         eps = 1e-3,
                         max_iter = 100,
                         max_tuning = 0.5,
                         trace = FALSE,
                         seed = NULL,
                         ...) {

  use_multicore(ncores)

  list(eps = eps, max_iter = max_iter, trace = trace, max_tuning = max_tuning,
       seed = seed, ...)
}
