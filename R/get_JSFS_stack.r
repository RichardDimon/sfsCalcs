#' Calculate joint site frequency spectrum from two gentoype matrices, assuming loci in same order. Data can be missing. Generates a stack of spectra for sites with equal missingness profiles.  
#'
#' @param gt_SNP_1  - genotype matrix for a set of individuals (in rows) [Required]
#' @param gt_SNP_2  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a stack of joint site frequency spectra 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

   get_JSFS_stack  <- function(gt_SNP_1, gt_SNP_2) {

      mv1 <- colSums(is.na(gt_SNP_1))
      mv2 <- colSums(is.na(gt_SNP_2))

      n1 <- nrow(gt_SNP_1)
      n2 <- nrow(gt_SNP_2)

      mc <- unique(as.data.frame(cbind(mv1,mv2)))

      JSFS_stack = vector(mode = "list", length = nrow(mc))
      for (i in 1:nrow(mc)) {

         i_ind_gt_1 <- which( mv1 == mc[i,1])
         i_ind_gt_2 <- which( mv2 == mc[i,2])

         i_gt_1    <- gt_SNP_1[, i_ind_gt_1]
         i_gt_2    <- gt_SNP_2[, i_ind_gt_2]

         i_JSFS   <- get_JSFSm(i_gt_1, i_gt_2)
         JSFS_stack[[ i ]] <- list(nvalues = mc[i,], sfs=i_JSFS)
      }

      return(JSFS_stack)

   }




