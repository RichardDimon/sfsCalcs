#' Calculate site frequency spectrum from a gentoype matrix. Data can be missing, and will return a stack of spectra for sites with different numbers of missing genotypes. 
#'
#' @param gt_MAC  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a stack of site frequency spectra 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
   get_SFS_stack_MAC  <- function(gt_MAC, gt_N) {
      n_loci    <- ncol(gt_MAC) 
      n_calls   <- colSums(gt_N)
      tab_calls <- names(table(n_calls))
      n_stack   <- length(tab_calls)

      SFS_stack = vector(mode = "list", length = n_stack)
      for (i in 1:n_stack) {
         i_calls  <- which( tab_calls[i] == n_calls)
         i_gt_MAC <- as.matrix(gt_MAC[, i_calls], ncol = length(i_calls))
         i_gt_N <- as.matrix(gt_N[, i_calls], ncol = length(i_calls))
         i_SFS   <- get_SFS_MAC(i_gt_MAC, i_gt_N)
         SFS_stack[[ i ]] <- list(n_value = tab_calls[i], sfs=i_SFS)
      }

      return(SFS_stack)

   }



