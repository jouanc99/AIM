getwd()
setwd("C:/Users/Jouan/Desktop/行資管/期中報告/file")
dir()
dt<-read.csv("106年.csv")
head(dt)

careers<-c("average","a","b","c","d","e","f","g","h","i","others")
#barplot
barplot(dt[,4],names.arg=careers,main="Total Income",col="corn flower blue",xlab="Careers",ylab="Dollars",cex.names=0.7, ylim=c(0, 3000000))
barplot(dt[,12],names.arg=careers,main="Leisure Expenditure",col="cadetblue2",xlab="Careers",ylab="Dollars",cex.names=0.7,ylim=c(0, 200000))
barplot(dt[,14],names.arg=careers,main="Education Expenditure",col="darkslategray1",xlab="Careers",ylab="Dollars",cex.names=0.7,ylim=c(0, 80000))
barplot(dt[,15],names.arg=careers,main="Total Expenditure",col="aliceblue",xlab="Careers",ylab="Dollars",cex.names=0.7,ylim=c(0, 1600000))


a<-c(2741928)
i<-c(944126)
names=c("民意代表、主管及經理人員","基層技術工及勞工")
dollars<-matrix(c(a,i),2,1)
colors<-c("lavender","orchid4")
barplot(dollars,names.arg=names,main="Comparison-Total Income",col=colors,xlab="Careers",ylab="Dollars",cex.names=1,ylim=c(0,3000000),beside=TRUE)

a1<-c(76163)
i1<-c(14875)
names=c("民意代表、主管及經理人員","基層技術工及勞工")
dollars1<-matrix(c(a1,i1),2,1)
colors<-c("lavender","orchid4")
barplot(dollars1,names.arg=names,main="Comparison-Education",col=colors,xlab="Careers",ylab="Dollars",cex.names=1,ylim=c(0,90000),beside=TRUE)

a2<-c(168814)
i2<-c(31400)
names=c("民意代表、主管及經理人員","基層技術工及勞工")
dollars2<-matrix(c(a2,i2),2,1)
colors<-c("lavender","orchid4")
barplot(dollars2,names.arg=names,main="Comparison-Leisure",col=colors,xlab="Careers",ylab="Dollars",cex.names=1,ylim=c(0,200000),beside=TRUE)

#ggplot
data1<-read.csv("106t.csv")
library(ggplot2)
head(data1)
#y軸轉指數
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}


under<-ggplot(data=data1,
              aes(x=data1$X,y=data1$total.income))+
  scale_y_continuous(labels=fancy_scientific)
under+
  geom_bar(stat="identity",fill="#5599FF")+
  ggtitle("Total income")+
  xlab("Careers")+ylab("Dollars")+
  theme(panel.background=element_rect(fill="#F0F8FF",size=2,linetype="solid"),plot.title=element_text(hjust = 0.5))

under1<-ggplot(data=data1,
              aes(x=data1$X,y=data1$education))+
  scale_y_continuous(labels=fancy_scientific)
under1+
  geom_bar(stat="identity",fill="#D1BBFF")+
  ggtitle("Education Expenditure")+
  xlab("Careers")+ylab("Dollars")+
  theme(panel.background=element_rect(fill="#E6E6FA",size=2,linetype="solid"),plot.title=element_text(hjust = 0.5))

under2<-ggplot(data=data1,
               aes(x=data1$X,y=data1$leisure))+
  scale_y_continuous(labels=fancy_scientific)
under2+
  geom_bar(stat="identity",fill="#FFB6C1")+
  ggtitle("Leisure Expenditure")+
  xlab("Careers")+ylab("Dollars")+
  theme(panel.background=element_rect(fill="#FFF0F5",size=2,linetype="solid"),plot.title=element_text(hjust = 0.5))

