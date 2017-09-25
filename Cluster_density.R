rm(list = ls())
setwd("~/THESIS/Quality_control/data/")
load("6_akumulasi_mcs_with_scc.rda")
#filter djfma dan jason


ymd_MC = list()
H_mc = list()
len_mc = c()
HMcB = list()
M_mc = list()
lonb = list()
latb = list()


for(i in 1:6){
  ymd_MC[[i]] = substr(hasil_akumulasi[[i]][[2]],1,8)
  M_mc[[i]] = substr(hasil_akumulasi[[i]][[2]],5,6)
  H_mc[[i]]  = substr(hasil_akumulasi[[i]][[2]],9,10)
  len_mc[i] = length(substr(hasil_akumulasi[[i]][[2]],9,10))
  lonb[[i]] = hasil_akumulasi[[i]][[3]]
  latb[[i]] = hasil_akumulasi[[i]][[4]]
}


library(akima)
library(MASS)
library(ggplot2)
library(viridis)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
# x11(width =15 ,height = 15)
# par(mfrow=c(3,2))
for(i in 1){
  denn = get_density(lonb[[i]],latb[[i]],n = 100)
  coors = data.frame(lonb[[i]],latb[[i]])
  library(fields)
  dist.in.km.matrix <- rdist.earth(coors,miles = F,R=6371)  #matrix
  fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
  clusters <- cutree(fit,h = 70)
  
  # x11()
  # plot(coors$lonb..1.., coors$latb..1.., col = clusters, pch = 20)
  
  
  fld <- interp(x = lonb[[i]], y = latb[[i]], z = clusters,duplicate = T)
  
  
  # par(mar=c(0,0,0,-1))
  # par(xpd=NA)
  cco = colorRampPalette(c("red","white","green","grey","white")) 
  
  source("../code/Filled.contour2.R")
  library(maps)
  mcs_name = c("MCCs","PECSs","MBCCSs","MBECSs","SMBCCSs","SMBECSs")
  # png(filename = sprintf("../png/%s.png",mcs_name[i]),height = 700, width = 1200)
  # 
  fld$z[fld$z >= 500] =NA
  my.filled.contour(x =fld$x,y=fld$y,(fld$z)*10000,
                    col = c(cco(17),rep("white",13),"white","white"),
                    # colorRampPalette(c("white","lightgreen",colors))(50)
                    ,axes = T, 
                    # levels = seq(0,200000000,length=40),
                    plot.axes = {axis(1,cex.axis = 3);axis(2,cex.axis = 3);
                      grid(col="black",lwd=2);title(main = sprintf("%s",mcs_name[i]),cex.main = 4);
                      points(lonb[[1]],latb[[1]],pch = 3,cex = 0.7);
                      # world(col="red",add=T,lwd=3);
                      map("world", fill=F, col="black", bg=NULL,
                          xlim=c(min(fld$x),max(fld$x)),
                          ylim=c(min(fld$y), max(fld$y)),
                          resolution = 0.0001,
                          add=T,interior = F);
                      # contour(x = (fld$x),y = (fld$y),z = (fld$z),nlevels = 7,add = T)
                    })
  # dev.off()
  
}

