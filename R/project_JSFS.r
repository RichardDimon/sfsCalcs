#' Performs a down-projection of the joint site frequency spectrum  
#'
#' @param jsfs  - a joint site frequency spectrum [Required]
#' @param m     - value for down projection [Required]
#' @return a down-projected joint site frequency spectrum 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

project_JSFS <- function(jsfs, m, renorm=FALSE) {

      # collapse rows
      nr <- nrow(jsfs)
      nc <- ncol(jsfs)

      tjsfs <- mat.or.vec(m+1, nc) 

      for (i in 1:nc) {
         tsfs <- as.matrix(jsfs[,i],ncol=1)
         psfs <- project_SFS(tsfs, m)
         tjsfs[,i] <- psfs
      }


      ttjsfs <- mat.or.vec(m+1, m+1) 
      for (j in 1:nrow(tjsfs)) {
         tsfs <- as.matrix(tjsfs[j,],ncol=1)
         psfs <- project_SFS(tsfs, m)
         ttjsfs[j,] <- psfs
      }

           
      rownames(ttjsfs) <- as.character(0:m)
      colnames(ttjsfs) <- as.character(0:m)
      return(ttjsfs)

}


