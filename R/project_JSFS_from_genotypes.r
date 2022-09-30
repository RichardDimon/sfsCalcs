#' Calculate and down project a joint site frequency spectrum from two gentoype matrices, assuming loci in same order. Data can be missing. 
#'
#' @param gt_SNP_1  - genotype matrix for a set of individuals (in rows) [Required]
#' @param gt_SNP_2  - genotype matrix for a set of individuals (in rows) [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_JSFS_from_genotypes <- function(gt_SNP_1, gt_SNP_2, m, n_min=1) {

   jsfs_stack <- get_JSFS_stack(gt_SNP_1, gt_SNP_2, n_min)
   proj_jsfs  <- project_JSFS_stack(jsfs_stack,m)
   return(proj_jsfs)

}

