plot_arah = function(skipx,skipy,lon,lat,zx,zy){
  lat1=lat[seq(1,length(lat),by=skipy)]
  iy = seq(1,length(lat),by=skipy)
  lon1=lon[seq(1,length(lon),by=skipx)]
  ix = seq(1,length(lon),by=skipx)
  xy = meshgrid(lon1,lat1)
  
  # dim(xy$x)
  ss = cbind(as.numeric(xy$x),as.numeric(xy$y))
  uz = zx[ix,iy]
  vz = zy[ix,iy]
  arrow.plot(ss[,1], ss[,2], 
             as.numeric(uz), 
             as.numeric(vz), length=0.08,arrow.ex = 0.1,xpd = T,angle=20)
  
}
