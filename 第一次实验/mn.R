library(lctools)
library(maptools)
library(spdep)
library(sp)
library(spatstat)

#data的4月11日CSV数据读取
data<-read.csv("G:/seat.csv",sep = ',',header =TRUE)
attach(data)
data.uid=data[,'uid']
data.gpa=data[,'gpa.all']
data.seat=data[,'X4月11日']
newdata=data.frame(data.uid,data.gpa,data.seat)

    #筛选出绩点>3的并且座位不为空的
    gpa2=c()
    seat2=c()
    a=1
    for (i in 1:nrow(newdata)) {
      if(is.na(newdata[i,3])==FALSE){
        gpa2[a]=newdata[i,2]
        seat2[a]=newdata[i,3]
        a=a+1
      }
      else{
        next()#跳出本次循环进入下一次循环
      }
    }
    newdata2=data.frame(gpa2,seat2)
    
    #生成位置坐标数据和位置号码相互对应起来
    attr <- expand.grid(xc = 1:16, yc = 1:2)
    attr2 <- expand.grid(xc = 2:15, yc = 3:5)
    c1<- rbind(attr,attr2)
    d<- data.frame(c1,seat1=74:1)
    #total=merge(d,newdata2,by.x="seat1",by.y="seat2")
    total=merge(d,newdata2,by.x="seat1",by.y="seat2",all.x=TRUE)
    quad<-total

    #把空位置删除掉，保存位置、X、Y坐标、成绩
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
    newdata3=data.frame(seat3,qx,qy,gpa3)
    
#提取位置坐标并转化为空间点对象
spoint<-data.frame(newdata3$qx,newdata3$qy)
sptest <- SpatialPoints(spoint)
#找附近的邻居
nbk1 <- knn2nb(knearneigh(sptest, k = 5, longlat = TRUE))
#nbk1 <- poly2nb(knearneigh(sptest, k = 5, longlat = TRUE))
snbk1 <- make.sym.nb(nbk1)
#画出核密度分布图
plot(nb2listw(snbk1),cbind(spoint$newdata3.qx,spoint$newdata3.qy))
moran.test(newdata3$gpa3,nb2listw(snbk1))




