library(sp)
library(spatstat)
library(maptools)
data<-read.csv("G:/seat.csv",sep = ',',header =TRUE)
attach(data)
data.uid=data[,'uid']
data.gpa=data[,'gpa.all']
data.seat=data[,'X4月11日']
newdata=data.frame(data.uid,data.gpa,data.seat)

gpa2=c()
seat2=c()
a=1
for (i in 1:nrow(newdata)) {
  if(is.na(newdata[i,3])==FALSE&&newdata[i,2]>=3){
    gpa2[a]=newdata[i,2]
    seat2[a]=newdata[i,3]
    a=a+1
  }
  else{
    next()
  }
}
newdata2=data.frame(gpa2,seat2)


attr <- expand.grid(xc = 1:16, yc = 1:2)
attr2 <- expand.grid(xc = 2:15, yc = 3:5)
c1<- rbind(attr,attr2)
d<- data.frame(c1,seat1=74:1)
#total=merge(d,newdata2,by.x="seat1",by.y="seat2")#鍐呰繛鎺?
total=merge(d,newdata2,by.x="seat1",by.y="seat2",all.x=TRUE)
quad<-total#鏍锋柟鏁版嵁

seat3=c()
qx=c()
qy=c()
gpa3=c()
b=1
for (i in 1:nrow(quad)) {
  if(is.na(quad[i,4])==FALSE){
    seat3[b]=quad[i,1]
    qx[b]=quad[i,2]
    qy[b]=quad[i,3]
    gpa3[b]=quad[i,4]
    b=b+1
  }
  else{
    next()
  }
}
newdata3=data.frame(seat3,qx,qy,gpa3)#11涓ソ瀛︾敓

mm.ppp<-ppp(newdata3$qx,newdata3$qy,window=owin(xrange=c(0,16),yrange=c(0,5)))


ds<-density(mm.ppp,bw="nrdo",adjust = 1)
plot(ds,main='Seat density')
detach(data)



