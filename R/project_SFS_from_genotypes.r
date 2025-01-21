#' Calculate and down-project a site frequency spectrum from a genotype matrix
#'
#' @param gt_SNP  - genotype matrix with samples in rows. [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS_from_genotypes <- function(gt_SNP, m, ncpu, unlim_m=FALSE) {

   sfs_stack <- get_SFS_stack(gt_SNP)
  proj_sfs <- project_SFS_stack_mc(SFS_stack=sfs_stack, m=m, ncpu=ncpu, unlim_m=unlim_m)
  cat("sfs", length(proj_sfs), "")
  return(proj_sfs)
}

