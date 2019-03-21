第一次作业
===
## 样方分析
### 数据说明：选取seat.csv中的4月11日的并且gpa>3的座位数据
## 实验基本步骤：
### 1.数据处理
### 2.样方选取与计数
### 3.计算VRM
### 4.结果分析

![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4%E6%9C%8811%E6%97%A5%E4%BD%8D%E7%BD%AE.JPG)
##### 座位分布图
### 1.数据处理
#### << 1.1在Seat.csv中提取位置和成绩数据

    library(sp)
    data<-read.csv("G:/seat.csv",sep = ',',header =TRUE)
    attach(data)
    data.uid=data[,'uid']
    data.gpa=data[,'gpa.all']
    data.seat=data[,'X4月11日']
    newdata=data.frame(data.uid,data.gpa,data.seat)#提取4月11日数据
    #提取成绩>3并且座位上有人的
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
#### << 1.2在Seat.csv中提数据与生成的位置坐标数据连接起来
    #产生74个个子的位置坐标和位置号
    attr <- expand.grid(xc = 1:16, yc = 1:2)
    attr2 <- expand.grid(xc = 2:15, yc = 3:5)
    c1<- rbind(attr,attr2)
    d<- data.frame(c1,seat1=74:1)#为74个座位编号  
    #total=merge(d,newdata2,by.x="seat1",by.y="seat2")#内连接
    #将生成位置(x,y)数据和座位号与csv表中的座位号和成绩相互对应(连接)起来
    total=merge(d,newdata2,by.x="seat1",by.y="seat2",all.x=TRUE)
    quad<-total
#### << 1.3画出座位分布图
    #将成绩大于3并且当天上课的学生的位置画出来
    coordinates(total) <- c("xc", "yc")
    gridded(total) <-TRUE  
    col1 <- colorRampPalette(c("red","green"))
    spplot(total, "gpa2", key.space = "left",colorkey =TRUE, col.regions = col1(20),main='讲台',xlab='               4月11日成绩>3位置分布图')
#### << 1.4对连接数据进行提纯，对应（seat,qx,qy,gpa3）
    #获得成绩大于3的并且位置上有人的数据
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

### 2.分取样方并且计数：6个样方

    count=c(0,0,0,0,0,0)#存取6个样方的位置点的个数
    for (i in 1:nrow(newdata3) ){
      if(newdata3[i,2]>0&&newdata3[i,2]<7&&newdata3[i,3]>0&&newdata3[i,3]<3){
        count[1]=count[1]+1
      }     
      if(newdata3[i,2]>6&&newdata3[i,2]<11&&newdata3[i,3]>0&&newdata3[i,3]<3){
        count[2]=count[2]+1
      }      
      if(newdata3[i,2]>10&&newdata3[i,2]<17&&newdata3[i,3]>0&&newdata3[i,3]<3){
        count[3]=count[3]+1
      }      
      if(newdata3[i,2]>0&&newdata3[i,2]<7&&newdata3[i,3]>2&&newdata3[i,3]<6){
        count[4]=count[4]+1
      }      
      if(newdata3[i,2]>6&&newdata3[i,2]<11&&newdata3[i,3]>2&&newdata3[i,3]<6){
        count[5]=count[5]+1
      }      
      if(newdata3[i,2]>10&&newdata3[i,2]<17&&newdata3[i,3]>2&&newdata3[i,3]<6){
        count[6]=count[6]+1
      }
    }
    group=c('第一组','第二组','第三组','第四组','第五组','第六组')
    cnb<-data.frame(group,count)#得出6个样方和对应的样方内的坐标点数

### 3.计算均值、方差、VRM
    meanvalue<-1/6*sum(count)#计算均值
    sum1=0
    for(i in 1:length(count)){
      sum1=sum1+(count[i]-meanvalue)^2
    }
    variance=1/5*sum1#计算方差
    result=variance/meanvalue
    
    if(result<0.5){#判断属于哪种分布
     
      print(paste0('VMR:  ',result))
      print("均匀分布")
    }
    if(result>0.5&&result<1.5){
      print(paste0('VMR:  ',result))
      print("随机分布")
    }
    if(result>1.5){
      print(paste0('VMR:  ',result))
      print("聚集分布")
    }

### 4.结果分析

![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4.11.JPG)
-
### 根据实验结果VMR趋近于1，因此4月11日，成绩大于3的学生选座为倾向于随机分布
#### 选取了四月12,13,16,18连续四天的数据进行样方分析计算结果如下
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4.12.JPG)
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4.13.JPG)
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4.16.JPG)
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/4.18.JPG)
## 从以上的5天的数据结果中可以看出：5天中有四天的结果都是 随机分布，因此成绩>3的学生的坐的位置特征趋于随机分布
## 结果产生原因：可能班上有进行1对1的帮扶，好学生帮助成绩不是很好的学生，这样造成了好学生选座不那么聚集，反映了班上的学习氛围比较好，没有小团体主义

# 2.核密度分析
### 代码：newdata3的数据处理方法和样方分析方法相同，所以这里就略写，关键代码如下：
    #获取newdata3的坐标使其转化为点模式对象
    mm.ppp<-ppp(newdata3$qx,newdata3$qy,window=owin(xrange=c(0,16),yrange=c(0,6)))
    ds<-density(mm.ppp,bw="nrdo",adjust = 1.5)
    plot(ds,main='核密度分析图')
    detach(data)
### 下面是5天的核密度分析图
### >>4月11日
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/411.png) 
### >>4月12日
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/412.png)
### >>4月13日
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/413.png)
### >>4月16日
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/416.png)
### >>4月18日
![](https://github.com/cuit201608/3-GROUP/blob/master/%E5%9B%BE/418.png)

### 通过核密度结果分析出：
### 成绩好的同学喜欢坐在教室的中前部分，可能是因为在前方能更清楚地听到老师的讲授，看到老师的课件，且由于左右两方视线较偏，所以成绩好的同学会更主动地集中在中前部分。
