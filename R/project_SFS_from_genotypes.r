#' Calculate and down-project a site frequency spectrum from a genotype matrix
#'
#' @param gt_SNP  - genotype matrix with samples in rows. [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS_from_genotypes <- function(gt_SNP, m, n_min=1) {

   sfs_stack <- get_SFS_stack(gt_SNP,n_min)
   proj_sfs  <- project_SFS_stack(sfs_stack,m)
   return(proj_sfs)

}

