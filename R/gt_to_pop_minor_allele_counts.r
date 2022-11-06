#' Returns a genotype matrix in counts of the minor allele.  
#'
#' @param gt_SNP_1  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a genotype matrix with counts of minor allele.  
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

gt_to_pop_minor_allele_counts <- function(gtr, pop) {

   gtmin <- gt_to_minor_alleles(gtr) 
   pnames <- unique(pop)

   macm <- mat.or.vec(length(pnames), ncol(gtmin))
   nm   <- mat.or.vec(length(pnames), ncol(gtmin))

   c <- 1
   for ( p in pnames )  {

      ipop <- which( pop == p )
      mac  <- colSums(gtmin[ipop,],na.rm=TRUE)
      n    <- colSums(!is.na(gtmin[ipop,]))
      macm[c, ] <- mac
      nm[c, ]   <- n
      c <- c+1
   }

   rownames(macm) <- pnames
   colnames(macm) <- colnames(gtr)
   rownames(nm) <- pnames
   colnames(nm) <- colnames(gtr)

   return(list(MAC=macm, N=nm))

}

