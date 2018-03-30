
gcj02_wgs84_matrix<-function(x){
  x_PI <- 3.14159265358979324 * 3000.0 / 180.0
  PI<-3.1415926535897932384626;
  a<-6378245.0;
  ee<-0.00669342162296594323;

  lng<-x[,1]
  lat<-x[,2]

transformlat<-function(lng, lat) {
  ret <- -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng));
  ret <-ret+(20.0 * sin(6.0 * lng * PI) + 20.0 * sin(2.0 * lng * PI)) * 2.0 / 3.0;
  ret <-ret+(20.0 * sin(lat * PI) + 40.0 * sin(lat / 3.0 * PI)) * 2.0 / 3.0;
  ret <-ret+(160.0 * sin(lat / 12.0 * PI) + 320 * sin(lat * PI / 30.0)) * 2.0 / 3.0;
  return(ret)
}

transformlng<-function(lng, lat) {
  ret<-300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng));
  ret <-ret+(20.0 * sin(6.0 * lng * PI) + 20.0 * sin(2.0 * lng * PI)) * 2.0 / 3.0;
  ret <-ret+(20.0 * sin(lng * PI) + 40.0 * sin(lng / 3.0 * PI)) * 2.0 / 3.0;
  ret <-ret+(150.0 * sin(lng / 12.0 * PI) + 300.0 * sin(lng / 30.0 * PI)) * 2.0 / 3.0;
  return(ret)
}

gcj02_wgs84_lng<-function(lng, lat) {
    dlat <- transformlat(lng - 105.0, lat - 35.0);
    dlng <- transformlng(lng - 105.0, lat - 35.0);
    radlat <- lat / 180.0 * PI;
    magic <- sin(radlat);
    magic <- 1 - ee * magic * magic;
    sqrtmagic <- sqrt(magic);
    dlat <- (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * PI);
    dlng <- (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * PI);
    mglat <- lat + dlat;
    mglng <- lng + dlng;
    return(lng * 2 - mglng)
  }

gcj02_wgs84_lat<-function(lng, lat) {
  dlat <- transformlat(lng - 105.0, lat - 35.0);
  dlng <- transformlng(lng - 105.0, lat - 35.0);
  radlat <- lat / 180.0 * PI;
  magic <- sin(radlat);
  magic <- 1 - ee * magic * magic;
  sqrtmagic <- sqrt(magic);
  dlat <- (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * PI);
  dlng <- (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * PI);
  mglat <- lat + dlat;
  mglng <- lng + dlng;
  return(lat * 2 - mglat)
}

wgs84_lng<-gcj02_wgs84_lng(lng,lat)
wgs84_lat<-gcj02_wgs84_lat(lng,lat)
y<-apply(data.frame(wgs84_lng,wgs84_lat),1,list)
return(y)
}
