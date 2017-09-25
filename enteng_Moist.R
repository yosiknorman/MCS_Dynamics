library(matlab)
library(fields)
library(maps)
rm(list = ls())


load("../data/data_moist.Rda")
lon = data_moist$lon
lat = data_moist$lat
aveq = data_moist$aveq
avequ = data_moist$avequ
aveqv = data_moist$aveqv
qum = data_moist$qum
qvm = data_moist$qvm
qu = data_moist$qu
dtm = data_moist$dtm
i_m = data_moist$i_m


source("../code/plot_arah.R")
mo_bulan = function(pilihbulan){
  cco = colorRampPalette(c("pink","orange","white","green","darkgreen","darkgrey","darkgreen"))
  var = aveq[[pilihbulan]]
  var[is.na(var)] = 0
  filled.contour(lon,lat,var,col = cco(100),axes = T,
                 levels = seq(-0.0002635031,0.0004784743,length=90),
                 las=0,
                 plot.axes = { grid(col = "red");
                   axis(1,cex.axis = 1);
                   axis(2,cex.axis = 1);
                   title(main = "Monthly Moisture flux convergence");
                   plot_arah(skipx = 3,skipy = 3,lon = lon,lat = lat ,
                             zx = avequ[[pilihbulan]],
                             zy = aveqv[[pilihbulan]]);
                   map("world", fill=F, col="brown",
                       bg=NULL, xlim=c(90,150), ylim=c(-15, 15), 
                       resolution = 0.0000001, add = TRUE)
                 }
  )
  
}

