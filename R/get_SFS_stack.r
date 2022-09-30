#' Calculate site frequency spectrum from a gentoype matrix. Data can be missing, and will return a stack of spectra for sites with different numbers of missing genotypes. 
#'
#' @param gt_SNP  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
   get_SFS_stack  <- function(gt_SNP, n_min=1) {
      n_loci    <- ncol(gt_SNP) 
      n_calls   <- colSums(!is.na(gt_SNP))
      tab_calls <- names(table(n_calls))
      n_stack   <- length(tab_calls)

      SFS_stack = vector(mode = "list", length = n_stack)
      for (i in 1:n_stack) {
         i_calls <- which( tab_calls[i] == n_calls)
         i_gt    <- gt_SNP[, i_calls]
         i_SFS   <- get_SFSm(i_gt)
         SFS_stack[[ i ]] <- list(n_value = tab_calls[i], sfs=i_SFS)
      }

      return(SFS_stack)

   }



