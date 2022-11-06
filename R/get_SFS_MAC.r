#' Calculate site frequency spectrum from a gentoype matrix. Data can be missing, but must be same number missing from each locus. 
#'
#' @param gt_MAC  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
   get_SFS_MAC  <- function(gt_MAC, gt_N){
      if (length(unique(colSums(gt_N))) != 1) {
         cat("Warning, genotypes supplied with unequal number of missing sites\n")
      } 
      SFS  <- mat.or.vec(1, (sum(gt_N[,1]))*2+1)
      for ( i in 1:ncol(gt_MAC) ) {
         pc      <- sum(gt_MAC[ , i],na.rm=TRUE) 
         ipc     <- pc + 1
         SFS[ipc] <- SFS[ipc] + 1
      }

      SFS <- matrix(SFS, nrow=length(SFS))
      rownames(SFS) <- as.character(0:(length(SFS)-1))
      return(SFS)

   }


