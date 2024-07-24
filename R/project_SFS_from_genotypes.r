#' Calculate and down-project a site frequency spectrum from a genotype matrix
#'
#' @param gt_SNP  - genotype matrix with samples in rows. [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS_from_genotypes <- function(gt_SNP, m, ncpu) {

   sfs_stack <- get_SFS_stack(gt_SNP)
  
  if (m=="auto"){
      m <- length(sfs_stack[[1]]$sfs)-1
  }
  
  if (m > length(sfs_stack[[1]]$sfs)-1){
      cat("Warning: m is larger than the length og smallest SFS. Consider changing value or setting m to auto")
  }
  
  proj_sfs <- project_SFS_stack_mc(sfs_stack, m, ncpu)
  cat("m", length(proj_sfs), "")
  return(proj_sfs)
}

