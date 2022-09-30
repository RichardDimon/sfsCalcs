source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/get_SFSm.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/gt_to_minor_alleles.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/get_SFS_stack.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/get_JSFS_stack.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_SFS.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_JSFS.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_SFS_stack.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_JSFS_stack.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_SFS_from_genotypes.r')
source('/home/jgb/pkgs/sfs/dev/sfsCalcs/R/project_JSFS_from_genotypes.r')

# conda activate renv
gtr <- matrix(floor(runif(256*256)*3),nrow=256)
gtr <- matrix( sample(floor(  c( runif(256*30)*3, runif(256*226)*0.1))) ,nrow=256)

gtm <- gtr
gtm[1:10,1:5] <- NA

gt <- gt_to_minor_alleles(gtm)


sfs_50  <- project_SFS_from_genotypes(gt, 50)
jsfs_20 <- project_JSFS_from_genotypes(gt, gt[1:30,], 20) 




sfs <- get_SFSm(gt_SNP=gt)
sfsm1 <- get_SFS_stack(gt_SNP=gt)

sfsm  <- get_SFS_stack(gt_SNP=gtm)

project_SFS(sfsm[[1]][[2]],20)
c_proj_sfs <- project_SFS_stack(sfsm,20)

project_SFS_200 <- project_SFS_from_genotypes(gtm, 200)
project_SFS_20 <- project_SFS_from_genotypes(gtm, 20)


source('/home/jgb/pkgs/sfs/comp/R/get_JSFSm.r')
source('/home/jgb/pkgs/sfs/comp/R/project_JSFS_from_n_to_m.r')
jsfs <- get_JSFSm(gt[1:128,], gt[129:256,])
source("/home/jgb/pkgs/sfs/comp/R/project_JSFS_to_m.r")
pjsfs <- project_JSFS_to_m(jsfs, 20)



# project_SFS_from_genotypes(gt, m) *
## - get_SFS_stack(gt,n_min)        *
###- get_SFSm(gt)                   *
## - project_SFS_stack(sfs_stack,m) *
###- project_SFS(sfs, m)            *

# project_JSFS_from_genotypes(gt, ip1, ip2, m)
## - get_JSFS_stack(gt1, gt2,n_min)                  * 
###- get_JSFSm(gt1, gt2)                             *
## - project_JSFS_stack(jsfs_stack,m)
###- project_JSFS(jsfs, m)                     *


gtSNP1 <- gtm[1:128,]
gtSNP2 <- gtm[129:256,]

jsfs_stack <- get_JSFS_stack(gtSNP1, gtSNP2,n_min)
jsfsm <- project_JSFS_stack(jsfs_stack,20)

