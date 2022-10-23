#' Down-project a stack of joint site frequency spectra. 
#'
#' @param JSFS_stack  - a set of site frequency spectra, in list format.  [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_JSFS_stack_mc <- function(JSFS_stack,m, ncpu = NULL) {

   require(parallel)

   if (is.null(ncpu)) {
      ncpu <- detectCores() - 2
   }

   n_stack <- length(JSFS_stack)

   jsfs_mat_list = list()

   for (i in 1:n_stack) {
      jsfs_mat_list[[ i ]] <- JSFS_stack[[i]][[2]]
   }

   call_projection <- function(jsfss, m) {
      i_proj_sfs <- project_JSFS(jsfss, m)
      return(i_proj_sfs)
   }

   results <- mclapply(jsfs_mat_list, call_projection, m=20, mc.cores=ncpu)

   for (i in 1:n_stack) {
      i_proj_sfs <- results[[ i ]]
      if (i==1) { c_proj_sfs <- i_proj_sfs}
      if (i>1) { c_proj_sfs <- c_proj_sfs + i_proj_sfs}
   }

   return(c_proj_sfs)
}

