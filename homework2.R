R=matrix(c(1.00,0.80,0.26,0.67,0.34,0.80,1.00,0.33,0.59,0.34,0.26,0.33,
           1.00,0.37,0.21,0.67,0.59,0.37,1.00,0.35,0.34,0.34,0.21,0.35,1.00),nrow=5,ncol=5)

#逆和特征根、特征向量
R_solve<-solve(R)
R_root<-eigen(R)


#问题2.2
library(readxl)
library(dplyr)
path<-R'(D:\书籍\多元统计分析\Py-R-mvstats5-master\mvstats5\exer\mvexer5.xlsx)'
worktime<-read_xlsx(path,sheet = 'E2.2')
worktime$X<-as.numeric(worktime$X)
#排序
worktime$X<-sort(worktime$X)
#breaks利用向量指定断点，并且得到分割后的各种数值
H<-hist(worktime$X,breaks = seq(900,3000,300))
datas<-data.frame('mid'=H$mids,'count'=H$counts,'fre'=H$density*300,
                  'sumfre'=cumsum(H$density*300))

#正态概率图
qqnorm(worktime$X);qqline(worktime$X)




#问题2.3

library(readxl)
library(readxl)
path<-R'(D:\书籍\多元统计分析\Py-R-mvstats5-master\mvstats5\exer\mvexer5.xlsx)'
d2.3<-worktime<-read_xlsx(path,sheet = 'E2.3')
d2.3$num<-1:10
colnames(d2.3)[1:2]<-c('smoke','time')
d2.3<-subset(d2.3,select = c(3,1,2))
#将是否吸烟转化为因子向量
#table类型可以看作有名字的向量
d2.3$smoke<-as.factor(d2.3$smoke)
barplot(table(d2.3$smoke,d2.3$time),beside = T,
        ylab = '人数',col = 1:2)
#图例要另外加，barplot函数的图例会与图像重叠
legend("topleft",legend=as.character(levels(d2.3$smoke)),fill=1:2)



#问题2.4    输入参数为数据框， 所在列，断点位置
h<-function(data,vec,bp){
  xname<-colnames(data)[vec]
  hist(data[[vec]],breaks = bp,xlab = xname,
       main = paste("Histogram of" , xname))
}

h(worktime,1,seq(900,3000,300))



