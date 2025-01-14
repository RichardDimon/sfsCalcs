#' Calculate and down-project a site frequency spectrum from a genotype matrix
#'
#' @param gt_MAC  - genotype matrix with samples in rows. [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS_from_MAC <- function(gt_MAC, gt_N, m, ncpu, unlim_m=TRUE) {

   sfs_stack <- get_SFS_stack_MAC(gt_MAC, gt_N)
   proj_sfs  <- project_SFS_stack_mc(sfs_stack, m, ncpu, unlim_m=unlim_m)
   cat("m", length(proj_sfs), "")
   return(proj_sfs)

}

