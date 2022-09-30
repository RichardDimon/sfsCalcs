#' Returns a genotype matrix in counts of the minor allele.  
#'
#' @param gt_SNP_1  - genotype matrix for a set of individuals (in rows) [Required]
#' @return a genotype matrix with counts of minor allele.  
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

gt_to_minor_alleles <- function(gtr) {

   n_calls        <- colSums(!is.na(gtr) )
   n_ref_alleles  <- colSums(gtr,na.rm=TRUE)
 
   i_ref_major    <- which( (n_ref_alleles / (n_calls*2)) > 0.5 )

   gt_minor <- gtr
   for (i in i_ref_major) {
      gt_minor[ gtr[,i] == 2, i] <- 0 
      gt_minor[ gtr[,i] == 0, i] <- 2 
   }

   return(gt_minor)

}

