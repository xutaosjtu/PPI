# Analysis of date and party hub in human protein network
# 
# Author: tao.xu
###############################################################################

#################### APCC 
#all tissues
subset = colnames(data)
#brain tissues
subset = scan(what = character())
Cerebellum
CerebellumPeduncles
CingulateCortex
Fetalbrain
GlobusPallidus
Hypothalamus
MedullaOblongata
OccipitalLobe
OlfactoryBulb
ParietalLobe
Pituitary
Pons
PrefrontalCortex
SubthalamicNucleus
Thalamus
Wholebrain
pineal_day
pineal_night


tissue.data = data[, subset ]
APCC=NULL;
for(j in 1:dim(tissue.data)[1]){
	rtmp=NULL;
	I=which(n1[j,]==1)
	if(length(I)!=0){
		for(i in 1:length(I)){
			rtmp[i]=cor(tissue.data[I[i],],tissue.data[j,])
		}
		APCC[j]=sum(rtmp,na.rm=T)/(length(I))
	}
	else{
		APCC[j]=0
	}
}
names(APCC) = rownames(tissue.data)
APCC = APCC[as.character(HumanData[,1])]

EVV = NULL;
EVV = apply(tissue.data, 1, var, na.rm = T)
EVV[which(is.na(EVV))] = 0
EVV = rank(EVV)/max(rank(EVV))
names(EVV) = rownames(tissue.data)
EVV = EVV[as.character(HumanData[,1])]
HumanData$evv = EVV
####################
setwd("F:/Xiao")
files = dir("new result/Modules", pattern = "*.csv", full.names = T)

fullgenes = NULL
for(f in files){
	genes = read.csv(f)
	fullgenes = rbind(fullgenes , genes)
}

HumanData = HumanData[order(HumanData$apcc, decreasing = T),]

pdf("Human apcc 2.pdf")
plot(HumanData$participation.cofficient,HumanData$within.module.degree, type="n", xlab="Participation coefficient", ylab="Within-community degree", ylim=c(-4,19),xlim=c(0,1),xaxt="n", yaxt="n",bty = "n", main = "Human")
rect(0,-4,0.05,2.5,col = "grey80",border=NA)
rect(0.05,-4,0.62,2.5,col = rgb(255, 225, 225,maxColorValue = 255),border=NA)
rect(0.62,-4,0.8,2.5,col = rgb(225, 255, 225,maxColorValue = 255),border=NA)
rect(0.8,-4,1,2.5,col = rgb(210, 225, 255,maxColorValue = 255),border=NA)
rect(0,2.5,0.3,19,col = rgb(250, 250, 210,maxColorValue = 255),border=NA)
rect(0.3,2.5,0.75,19,col = rgb(235, 220, 255,maxColorValue = 255),border=NA)
rect(0.75,2.5,1,19,col = rgb(255, 245, 238,maxColorValue = 255),border=NA)

lines(c(0,0),c(-4,19))
lines(c(1,1),c(-4,19))
lines(c(0,1),c(-4,-4))
lines(c(0,1),c(19,19))

lines(c(0.1,0.1),c(-4,-4+0.4))
lines(c(0.2,0.2),c(-4,-4+0.6))
lines(c(0.3,0.3),c(-4,-4+0.4))
lines(c(0.4,0.4),c(-4,-4+0.6))
lines(c(0.5,0.5),c(-4,-4+0.4))
lines(c(0.6,0.6),c(-4,-4+0.6))
lines(c(0.7,0.7),c(-4,-4+0.4))
lines(c(0.8,0.8),c(-4,-4+0.6))
lines(c(0.9,0.9),c(-4,-4+0.4))

lines(c(1,1-0.02),c(-2,-2))
lines(c(1,1-0.03),c(0,0))
lines(c(1,1-0.02),c(2,2))
lines(c(1,1-0.03),c(4,4))
lines(c(1,1-0.02),c(6,6))
lines(c(1,1-0.03),c(8,8))
lines(c(1,1-0.02),c(10,10))
lines(c(1,1-0.03),c(12,12))
lines(c(1,1-0.02),c(14,14))
lines(c(1,1-0.03),c(16,16))
lines(c(1,1-0.02),c(18,18))

lines(c(0,0.02),c(-2,-2))
lines(c(0,0.03),c(0,0))
lines(c(0,0.02),c(2,2))
lines(c(0,0.03),c(4,4))
lines(c(0,0.02),c(6,6))
lines(c(0,0.03),c(8,8))
lines(c(0,0.02),c(10,10))
lines(c(0,0.03),c(12,12))
lines(c(0,0.02),c(14,14))
lines(c(0,0.03),c(16,16))
lines(c(0,0.02),c(18,18))

require("fields")
for (i in 1:dim(HumanData)[1]){
	points(HumanData$participation.cofficient[i],HumanData$within.module.degree[i],col = tim.colors(200)[HumanData$apcc[i]*100+100],pch = 19)
}

image.plot( zlim=c(-1,1), col = tim.colors(200)[1: 200], legend.only=TRUE, smallplot = c(.93,.97,0.22,0.82))
dev.off()


################