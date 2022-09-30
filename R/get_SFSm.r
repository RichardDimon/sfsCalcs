#' Calculate site frequency spectrum from a gentoype matrix. Data can be missing, but must be same number missing from each locus. 
#'
#' @param gt_SNP  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
   get_SFSm  <- function(gt_SNP){
      mv <- colSums(is.na(gt_SNP))
      miss = mv[1]
      if (any(mv != miss)) {
         cat("Warning, genotypes supplied with unequal number of missing sites\n")
      } 
      SFS  <- mat.or.vec(1, (nrow(gt_SNP)-miss)*2+1)
      for ( i in 1:ncol(gt_SNP) ) {
         pc      <- sum(gt_SNP[ , i],na.rm=TRUE) 
         ipc     <- pc + 1
         SFS[ipc] <- SFS[ipc] + 1
      }

      SFS <- matrix(SFS, nrow=length(SFS))
      rownames(SFS) <- as.character(0:(length(SFS)-1))
      return(SFS)

   }


