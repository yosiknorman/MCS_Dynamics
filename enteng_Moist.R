rm(list = ls())
setwd("~/THESIS/Quality_control/data/")
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
data_dinamis = list(lon,lat,aveq,avequ,avequ)
# save(file = "../data/data_dinamis.Rda" ,data_dinamis)

source("../code/plot_arah.R")
rnglev = list()
for(i in 1:11){
  rnglev[[i]] =   list(range(aveq[[i]][!is.na(aveq[[i]])]),
                       range(aveq[[i+1]][!is.na(aveq[[i+1]])]))
}
rnglev = unlist(rnglev)
rnglev = range(rnglev)

mo_bulan = function(pilihbulan){
  cco = colorRampPalette(c("darkred","white","darkblue","darkblue"))
  var = aveq[[pilihbulan]]
  var[is.na(var)] = 0
  # x11()
  filled.contour(lon,lat,var*10000,col = cco(40),axes = T,
                 levels = seq((rnglev[1]*10000),(rnglev[2]*10000)+0.7,length=39),
                 # levels = seq(-6,4,length=3),
                 # nlevels = 89,
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
mo_bulan(9)
