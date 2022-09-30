#' Down project a stack of site frequency spectra using an approach outlined by https://doi.org/10.1534/genetics.166.1.351 
#'
#' @param SFS_stack - a stack of site frequecy spectra in list format [Required]
#' @param m - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export


project_SFS_stack <- function(SFS_stack,m) {

   n_stack <- length(SFS_stack)
   for (i in 1:n_stack) {
      i_proj_sfs <- project_SFS(SFS_stack[[i]][[2]], m)
      if (i==1) { c_proj_sfs <- i_proj_sfs}
      if (i>1) { c_proj_sfs <- c_proj_sfs + i_proj_sfs}
   }

   return(c_proj_sfs)
}

