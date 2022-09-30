#' Down-project a stack of joint site frequency spectra. 
#'
#' @param JSFS_stack  - a set of site frequency spectra, in list format.  [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_JSFS_stack <- function(JSFS_stack,m) {

   n_stack <- length(JSFS_stack)
   for (i in 1:n_stack) {
      i_proj_sfs <- project_JSFS(JSFS_stack[[i]][[2]], m)
      if (i==1) { c_proj_sfs <- i_proj_sfs}
      if (i>1) { c_proj_sfs <- c_proj_sfs + i_proj_sfs}
   }

   return(c_proj_sfs)
}

