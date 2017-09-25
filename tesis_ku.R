library(raster)
library(mgcv)
library(sp)
library(maps)
# <<<<<<<<<<<<<<<<<< Making Polygon >>>>>>>>>>>>>>>>>>
# <<<<<<<<<<<<<<< Create by Yosik Norman >>>>>>>>>>>>>
setwd("~/THESIS/Quality_control/")

rm(list = ls())
E1 = c(3.557283,92.460938 , 3.908099,93.032227 , 4.434044,93.515625 , 5.615986,
       93.427734 , 4.872048,94.394531 , 5.441022,95.185547 , 5.615986,95.712891 , 
       6.620957,96.811523 , 7.406048,97.163086 , 7.536764,97.69043 , 7.275292,
       98.261719 , 6.708254,98.657227 , 5.965754,98.525391 , 5.353521,98.745117 , 
       5.047171,99.536133 , 5.965754,100.50293 , 7.493196,101.030273 , 8.754795,
       101.118164 , 9.75237,101.118164 , 10.617418,101.206055 , 10.617418,103.754883 , 
       10.401378,105.424805 , 10.401378,107.182617 , 9.968851,109.248047 , 8.667918,
       110.258789 , 7.318882,112.060547 , 6.664608,112.104492 , 5.353521,111.313477 , 
       4.915833,110.126953 , 3.645,108.588867 , 2.547988,108.457031 , 2.504085,109.907227 , 
       3.030812,110.830078 , 3.337954,112.104492 , 3.294082,113.24707 , 3.337954,114.609375 , 
       4.65308,115.268555 , 5.834616,115.004883 , 6.446318,116.015625 , 6.315299,118.168945 ,
       4.872048,119.970703 , 3.601142,119.311523 , 2.152814,118.696289 , 0.747049,117.553711 , 
       1.230374,114.213867 , 0.087891,113.686523 , -0.747049,114.609375 , -1.933227,114.301758 , 
       -3.118576,112.368164 , -1.362176,109.291992 , 0.043945,108.896484 , 1.318243,108.764648 , 
       1.493971,107.270508 , 0.131836,106.831055 , 0.131836,105.908203 , -0.79099,105.117188 , 
       -1.669686,105.117188 , -0.878872,103.623047 , -1.098565,101.99707 , -1.845384,101.689453 , 
       -2.723583,101.469727 , -3.995781,101.601563 , -4.521666,101.074219 , -5.003394,99.536133 , 
       -5.659719,96.108398 , -5.878332,94.350586 , -6.446318,93.603516, -5.922045,92.373047)

lon1 = E1[seq(2,length(E1),by=2)]
lat1 = E1[seq(1,length(E1),by=2)]

pol = function(dataE){
  coords1 = matrix(c(dataE), 
                   ncol = 2, byrow = TRUE)
  coords1 = cbind(coords1[,2],coords1[,1])
  P1 = Polygon(coords1)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  return(Ps1)
}

gab = pol(E1)
# <<<<<<<<<<<<<<<<<<<<<<<<< If you have Many polygons >>>>>>>>>>>>>>>>>>>>>>>>
# use > gab = union(polE1,polE2)


# <<<<<<<<<<<<<<<<< Load Data MCS >>>>>>>>>>>>>>>>>
load("data/6_akumulasi_mcs_with_scc.rda")
lon = list()
lat = list()
xymcs = list()
inside.poly = list()
outside.poly = list()
iip = list()
lonE = list()
latE = list()
op = list()
lonO = list()
latO = list()

th = list()
sth = 2010:2014
stahun =list()
bl = list()
sbl = c("01","02","03","04","05","06","07","08","09","10","11","12")

blth = list()
iblth = list()

for(i in 1:6){
  lon[[i]] = hasil_akumulasi[[i]][[3]]
  lat[[i]] = hasil_akumulasi[[i]][[4]]
  xymcs[[i]] = SpatialPoints(data.frame(lon[[i]],lat[[i]]))
  xymcs[[i]]@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  inside.poly[[i]] <- !is.na(over(xymcs[[i]],gab))
  outside.poly[[i]] <- is.na(over(xymcs[[i]],gab))
  iip[[i]] = which(inside.poly[[i]] == TRUE)
  op[[i]] = which(outside.poly[[i]] == TRUE)
  lonE[[i]] = lon[[i]][ iip[[i]] ]
  latE[[i]] = lat[[i]][ iip[[i]] ]
  lonO[[i]] = lon[[i]][ op[[i]] ]
  latO[[i]] = lat[[i]][ op[[i]] ]
  
  # <<<<<<<<<<< Pilah Tahun >>>>>>>>>>
  th[[i]] = list()
  blth[[i]] = list()
  stahun[[i]] = list()
  iblth[[i]] = list()

  for(j in 1:5){
    th[[i]][[j]] = which(as.numeric(substr(hasil_akumulasi[[i]][[2]],1,4)) == sth[j])
    stahun[[i]][[j]] = hasil_akumulasi[[i]][[2]][ th[[i]][[j]]  ]
    blth[[i]][[j]] = list()
    iblth[[i]][[j]] = list()
    # <<<<<<<<<<< Pilah Bulan Berdasarkan Tahun >>>>>>>>>>
    for(k in 1:12){
      blth[[i]][[j]][[k]] = stahun[[i]][[j]][ substr(stahun[[i]][[j]],5,6) == sbl[k] ]
      iblth[[i]][[j]][[k]] = which(substr(stahun[[i]][[j]],5,6) == sbl[k])
    }
    names(iblth[[i]][[j]]) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  }
  names(iblth[[i]]) = as.character(2010:2014) 
  
  # <<<<<<<<<<< Pilah Bulan >>>>>>>>>>
  bl[[i]] = list()
  for(j in 1:12){
    bl[[i]][[j]] = which(as.numeric(substr(hasil_akumulasi[[i]][[2]],5,6)) == sbl[j])
  }
}

# <<<<<<<<<<<<<<<<<<< Load Data Moisture >>>>>>>>>>>>>>>>>
load("data/data_moist.Rda")



x11(width = 10,height=7)
plot(gab,axes=TRUE,xlim = c(90,150),ylim=c(-10,10))
image(data_moist$lon,data_moist$lat,data_moist$aveq[[1]],add=T)
map("world", fill=F, col="brown", bg=NULL, xlim=c(90,150), ylim=c(-12, 12),
    mar=c(0,0,0,0),resolution = 0.0000001,add = TRUE)
points(lonE[[i]][1:4],latE[[i]][1:4])
plot(gab,add = T)
# points(lonO[[i]],latO[[i]],col="blue")
grid(col = "red")

box()



# length(lonE[[i]])
# length(lonO[[i]])


