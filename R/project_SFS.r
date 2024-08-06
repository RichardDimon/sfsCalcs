#' Down project a site frequency spectrum using an approach outlined by https://doi.org/10.1534/genetics.166.1.351 
#'
#' @param sfs  - site frequency spectrum [Required]
#' @param m  - value for down-projection [Required]
#' @return a joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_SFS <- function (sfs, m, unlim_m) {
  C_n <- matrix(sfs, nrow = nrow(sfs))
  n <- length(C_n) - 1
  C_m <- matrix(rep(0, m + 1), nrow = (m + 1))
  rownames(C_m) <- as.character(0:(length(C_m) - 1))

if (unlim_m == TRUE) {
  cat("unlimited m ")
  C_m[1:nrow(sfs)] <- sfs
 
} else if (unlim_m == FALSE) {
  
   if (nrow(sfs) < m) {
    cat("length of sfs (",nrow(sfs),") is smaller than m (",m,"). not including in stack \n")
    #C_m[, 1][1:nrow(sfs)] <- sfs
    C_m <- 0
  } else{
    
  for (i in 0:(m)) {
    C_m_i <- 0
    for (j in i:(n - m + i)) {
      pm <- (choose(m, i) * choose((n - m), (j - i)))/choose(n, j)
      pm_Cn <- pm * C_n[j + 1, 1]
      C_m_i = C_m_i + pm_Cn
    }
    C_m[i + 1, 1] <- C_m_i
  }
  }
  }

  return(C_m)
}


