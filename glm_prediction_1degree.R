rm(list = ls())
library(MASS)
par(mfrow=c(1,2))
library(readxl)
par(mar = c(4,6,4,2))
#par(mfcol = c(3,1))
data<- read_excel("glm-R0.xlsx",sheet=2)
model=data[1:900,]#
R0=model$R0
R01=model$R01
popu=model$popu
inci=model$incidence
cro=model$cropland
gra=model$grassland
med=model$mediratio
gdp=model$GDP
y=exp(-7.012+1.480*log(popu)-0.907*log(gdp)-0.174*cro+1.492*gra-0.001*med+0.165*R0)#estimate
y1=exp(-7.012+1.480*log(popu)-0.907*log(gdp)-0.174*cro+1.492*gra-0.001*med+0.165*R01)#
ycase=y*popu/1000000#
y1case=y1*popu/1000000#
y2=y1case-ycase#
A=rbind(ycase,y1case,y2)

z=exp(-25.073+0.002*log(popu)-1.056*log(gdp)-0.260*cro+1.106*gra-0.003*med+0.134*R0)#estimate
z1=exp(-25.073+0.002*log(popu)-1.056*log(gdp)-0.260*cro+1.106*gra-0.003*med+0.134*R01)#
zcase=z*popu/1000000#
z1case=z1*popu/1000000#
z2=z1case-zcase#
B=rbind(zcase,z1case,z2)


o=exp(11.049+2.958*log(popu)-0.758*log(gdp)-0.096*cro+1.878*gra-0.001*med+0.196*R0)#estimate
o1=exp(11.049+2.958*log(popu)-0.758*log(gdp)-0.096*cro+1.878*gra-0.001*med+0.196*R01)#
ocase=o*popu/1000000#
o1case=o1*popu/1000000#
o2=o1case-ocase#
C=rbind(ocase,o1case,o2)
plot(inci,xaxt="n",yaxt="n",pch=15,ylim=c(0,1500),type="l",col="black",lwd=1,ann=F,cex=1.5)
par(new=TRUE)
plot(y,xaxt="n",yaxt="n",pch=15,ylim=c(0,1500),type="l",col="red",lwd=1.5,ann=F,cex=1.5)
par(new=TRUE)
plot(y1,xaxt="n",yaxt="n",pch=15,ylim=c(0,1500),type="l",col="blue",lwd=1.5,ann=F,cex=1.5)

axis(2, at=c(0,500,1000,1500),padj=0,tcl=-0.3,cex.axis="1.5",las=2)
#mtext(side=2,text="Incidence (1/1000000)",line=3.5,cex=1.5)
axis(1,padj=0,tcl=-0.3,cex.axis="1.5",las=1)
#mtext(side=1,text="Month",line=2.5,cex=1.5)
legend("topleft",legend=c("observe","estimate","warming"),col=c("black","red","blue"),lwd=2,bty="n",cex=1.3,pch=15)

plain<-ts(t(B)[1:300,],frequency=12,start=c(1986,1))
highland<-ts(t(B)[301:600,],frequency=12,start=c(1986,1))
hill<-ts(t(B)[601:900,],frequency=12,start=c(1986,1))

real=plain[,1]
sim=plain[,2]

##wet
a5=(real[cycle(real)==5])
a6=(real[cycle(real)==6])
a7=(real[cycle(real)==7])
a8=(real[cycle(real)==8])
a9=(real[cycle(real)==9])
a10=(real[cycle(real)==10])
wet.real=as.vector(cbind(a5,a6,a7,a8,a9,a10))


b5=(sim[cycle(sim)==5])
b6=(sim[cycle(sim)==6])
b7=(sim[cycle(sim)==7])
b8=(sim[cycle(sim)==8])
b9=(sim[cycle(sim)==9])
b10=(sim[cycle(sim)==10])
wet.sim=as.vector(cbind(b5,b6,b7,b8,b9,b10))


(mean(wet.sim)-mean(wet.real))/mean(wet.real)*100
#sim=(mean(wet.sim)-2*sd(wet.sim))
#real=(mean(wet.real)-2*sd(wet.real))
#(sim-real)/real*100

#wilcox.test(wet.sim,wet.real,conf.int = T, conf.level = 0.95)
#-1.172146  /median(wet.real)
##dry
a1=(real[cycle(real)==1])
a2=(real[cycle(real)==2])
a3=(real[cycle(real)==3])
a4=(real[cycle(real)==4])
a11=(real[cycle(real)==11])
a12=(real[cycle(real)==12])
dry.real=as.vector(cbind(a1,a2,a3,a4,a11,a12))


b1=(sim[cycle(sim)==1])
b2=(sim[cycle(sim)==2])
b3=(sim[cycle(sim)==3])
b4=(sim[cycle(sim)==4])
b11=(sim[cycle(sim)==11])
b12=(sim[cycle(sim)==12])
dry.sim=as.vector(cbind(b1,b2,b3,b4,b11,b12))

(sum(dry.sim)-sum(dry.real))/sum(dry.real)*100



#wilcox.test(dry.sim,dry.real,conf.int = T, conf.level = 0.95)
#0.7386764/median(dry.real)
