#' Calculate joint site frequency spectrum from two gentoype matrices, assuming loci in same order. Data can be missing, but must be same number missing from each locus. 
#'
#' @param gt_SNP_1  - genotype matrix for a set of individuals (in rows) [Required]
#' @param gt_SNP_2  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

   get_JSFSm <- function(gt_SNP_1, gt_SNP_2){

      mv1 <- colSums(is.na(gt_SNP_1))
      miss1 = mv1[1]
      mv2 <- colSums(is.na(gt_SNP_2))
      miss2 = mv2[1]

      if (any(mv1 != miss1) | any(mv2 != miss2)) {
         cat("Warning, genotypes supplied with unequal number of missing sites\n")
      } 

      n1 <- nrow(gt_SNP_1)
      n2 <- nrow(gt_SNP_2)

      JSFS   <- mat.or.vec(n1*2+1, n2*2+1)

      for ( j in 1:ncol(gt_SNP_1) ) {

          p1c <- sum(gt_SNP_1[ , j],na.rm=TRUE)+1
          p2c <- sum(gt_SNP_2[ , j],na.rm=TRUE)+1

          JSFS[ p1c, p2c ] <- JSFS[ p1c, p2c ] + 1
      }

      rownames(JSFS) <- as.character(0:(nrow(JSFS)-1))
      colnames(JSFS) <- as.character(0:(ncol(JSFS)-1))

      return(JSFS)
   }



