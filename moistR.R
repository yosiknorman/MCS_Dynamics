library(ncdf4)
library(maps)
library(fields)
library(matlab)

rm(list = ls())
setwd("~/THESIS/Quality_control/data/")
f = list.files(pattern = ".nc")
#  <<<<<<<<<<<< BACA DATA Specific Humidity >>>>>>>>>>>
q_nc <- nc_open(f[1])
q = ncvar_get(q_nc,"lowq")
#  <<<<<<<<<<<< BACA DATA Meridional Wind >>>>>>>>>>>
qu_nc <- nc_open(f[2])
qu = ncvar_get(qu_nc,"lowqu")
#  <<<<<<<<<<<< BACA DATA Zonal Wind >>>>>>>>>>>
qv_nc <- nc_open(f[3])
qv = ncvar_get(qv_nc,"lowqv")
#  <<<<<<<<<<<< BACA DATA LINTANG DAN BUJUR >>>>>>>>>>>
lon = ncvar_get(qu_nc,"lon")
lat = ncvar_get(qu_nc,"lat")
#  <<<<<<<<<<<< BACA DATA WAKTU >>>>>>>>>>>
dt = seq(ISOdate(2010,1,1,0), ISOdate(2014,12,31,18), by="hours")
dt = dt[seq(1, length(dt),by=6)]
y = substr(dt,1,4)
m = substr(dt,6,7)
d = substr(dt,9,10)
h = substr(dt,12,13)
dt = sprintf("%s%s%s%s",y,m,d,h)
dt[length(dt)]
#<<<<<<<<<<<<<<<<<<<<<< COMPOSITE MONTHLY >>>>>>>>>>>>>>>>>>>>>>
i_m = list()
qum = list()
qvm = list()
qm = list()
aveq = list()
aveqv = list()
avequ = list()
dtm = list()

for(i in 1:12){
  i_m[[i]] = which(as.numeric(m) == i)
  dtm[[i]] = dt[ i_m[[i]] ]
  qum[[i]] = qu[,, i_m[[i]] ]
  qvm[[i]] = qv[,, i_m[[i]] ]
  qm[[i]] = q[,, i_m[[i]] ]
  aveq[[i]] = apply(qm[[i]],c(1,2),FUN = mean)
  aveqv[[i]] = apply(qvm[[i]],c(1,2),FUN = mean)
  avequ[[i]] = apply(qum[[i]],c(1,2),FUN = mean)
}
data_moist = list(aveq,avequ,aveqv,qum,qvm,qu,dtm,i_m,lon,lat)
names(data_moist) = c("aveq","avequ","aveqv","qum","qvm","qu","dtm","i_m","lon","lat")

save(data_moist,file = "../data/data_moist.Rda")
source("../code/plot_arah.R")
mo_bulan = function(pilihbulan){
  cco = colorRampPalette(c("pink","orange","white","green","darkgreen","darkgrey","darkgreen"))
  var = aveq[[pilihbulan]]
  var[is.na(var)] = 0
  # x11()
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
