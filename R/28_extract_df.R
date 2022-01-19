#' Extract results from `dynamics()` function as a dataframe
#'
#' @description #unnest results from dynamics() and turn them into a dataframe that you can bind together
#' @param x Outputs from a call to dynamics(), which are a named list
#'
#' @return a dataframe with population trajectories, initial depletion, and bycatch levels that can be printed as a table.
#' @export
#'
#' @examples
#' x <- dynamics(S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 17, InitDepl = 0.6, ConstantCatch = NA, ConstantF = rep(0.01, times = 100), z = 2.39, nyears = 100, nages = 25, lambdaMax = 1.04)
#' extract_df(x)
#' 
extract_df <- function(x){
  y <- data.frame(x$trajectories)
  colnames(y) <- paste0("Yr_",1:ncol(y))
  rownames(y) <- 1:nrow(y)
  
  y$sim <- 1:nrow(y)
  
  y$Bycatch_Catch <- x$ConstantBycatch['Catch']
  y$Bycatch_CV <- x$ConstantBycatch['CV']
  
  y$Bycatch_Rate <- x$ConstantRateBycatch['Rate']
  y$Bycatch_Rate_CV <- x$ConstantRateBycatch['CV']
  
  y$InitDepl <- x$InitDepl
  
  return(y)
}