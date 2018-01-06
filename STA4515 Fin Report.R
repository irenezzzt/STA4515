source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
library(qvalue)
require(plotrix)

#read data
class1<-c(NA, rep("NULL", 7), NA)
class2<-c(rep("NULL", 8), NA)

diy<-read.csv("~/Desktop/GRADUATE/2017 FALL/Report/1011.assoc.tsv", 
              sep="\t",header = T, nrow=10^6 , colClasses=class1)

height<-read.csv("~/Desktop/GRADUATE/2017 FALL/Report/1697.assoc.tsv", 
                 sep="\t",header = T, nrow=10^6 , colClasses =class2 )

lung<-read.csv("~/Desktop/GRADUATE/2017 FALL/Report/20001_1001.assoc.tsv",
               sep="\t",header = T, nrow=10^6, colClasses = class2)


data<-data.frame(cbind(diy, height$pval, lung$pval))
colnames(data)<-c("id", "diy","height","lung")
head(data)
#----------------------------------------------------------------------------------

#data exploration
m<-length(data$height)
x<-seq(1,m,by=1)

par(mfrow=c(1,3))
hist(data$height,breaks=50)
hist(data$lung,breaks=50)
hist(data$diy,breaks=50)

#--------------------------------------------------------

#basic FWER, FDR, etc
alpha=0.0001
sum(data$height<alpha)
sum(p.adjust(data$height, method="bonferroni")<alpha)
sum(p.adjust(data$height, method="holm")<alpha)

sum(p.adjust(data$height, method="BH")<alpha)
qobj<-qvalue(p=data$height)
summary(qobj)
qobj$qvalues
head(data)
print_data<-as.data.frame(cbind(as.character(data[,1]),
                  p.adjust(data$height, method="bonferroni"),
                  p.adjust(data$height, method="holm"),
                  p.adjust(data$height, method="BH"),
                  qobj$qvalues))
colnames(print_data)<-c("id", "Bon","Holm", "BH","Q")      
as.data.frame(cbind(as.character(print_data[sort(print_data$Bon)[1:10],1]),
                    as.character(print_data[sort(print_data$Holm)[1:10],1]),
                    as.character(print_data[sort(print_data$BH)[1:10],1]),
                    as.character(print_data[sort(print_data$Q)[1:10],1])))
#------------------------------------------------



sum(data$lung<alpha)
sum(p.adjust(data$lung, method="bonferroni")<alpha)
sum(p.adjust(data$lung, method="holm")<alpha)
sum(p.adjust(data$lung, method="BH")<alpha)
qobj2<-qvalue(p=data$lung)
summary(qobj2)

sum(data$diy<alpha)
sum(p.adjust(data$diy, method="bonferroni")<alpha)
sum(p.adjust(data$diy, method="holm")<alpha)
sum(p.adjust(data$diy, method="BH")<alpha)
qobj3<-qvalue(p=data$diy)
summary(qobj3)


#-------------------------------------------------

plot(qobj)
plot(qobj2)
plot(qobj3)
par(mfrow=c(1,3))
hist(qobj)
hist(qobj2)
hist(qobj3)


#------------------------------------------------


smoke<-read.csv("~/Desktop/GRADUATE/2017 FALL/Report/1269.assoc.tsv", 
              sep="\t",header = T, nrow=10^6 , colClasses=class2)

data$smoke<-smoke$pval
data$BH_lung<-p.adjust(data$lung,method="BH")
data$BH_smoke<-p.adjust(data$smoke, method="BH")
attach(data)
data[BH_smoke<0.001,]
plot_data<-data[(BH_lung<0.01|BH_smoke<0.001),c(1,6,7)]
head(plot_data)

plot(x=plot_data$id, y=plot_data$BH_lung, col="red")
plot(plot_data$id, plot_data$BH_smoke, col="blue", add=T)
