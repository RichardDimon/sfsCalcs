#' Down project (multi-core) a stack of site frequency spectra using an approach outlined by https://doi.org/10.1534/genetics.166.1.351 
#'
#' @param SFS_stack - a stack of site frequecy spectra in list format [Required]
#' @param m - value for down-projection [Required]
#' @param ncpu - value for number of cores to use [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS_stack_mc <- function(SFS_stack,m, ncpu, unlim_m) {

 require(parallel)
  if (is.null(ncpu)) {
    ncpu <- detectCores() - 2
  }
  n_stack <- length(SFS_stack)
  sfs_mat_list = list()
  for (i in 1:n_stack) {
    sfs_mat_list[[i]] <- SFS_stack[[i]][[2]]
  }
  call_projection <- function(sfss, m) {
    i_proj_sfs <- project_SFS(sfss, m, unlim_m)
    return(i_proj_sfs)
  }
  results <- mclapply(sfs_mat_list, call_projection, m = m, 
                      mc.cores = ncpu)
  
  totallengthstack <- length(results[[n_stack]])
  
  for (i in 1:n_stack) {
    i_proj_sfs <- results[[i]]
    
    if (length(i_proj_sfs)<totallengthstack){
      i_proj_sfs <- c(i_proj_sfs, rep(0, totallengthstack-length(i_proj_sfs))) #this makes all the stacks the same length to add together
    }
    
    if (i == 1) {
      c_proj_sfs <- i_proj_sfs
    }
    if (i > 1) {
      c_proj_sfs <- c_proj_sfs + i_proj_sfs
    }
  }
  return(c_proj_sfs)
   
}

