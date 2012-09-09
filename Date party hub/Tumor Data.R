# TODO: Add comment
# 
# Author: xutao
###############################################################################


data.tumor = read.csv("Tumor data/tumors_U95_20070810.raw.csv")

### read in the tumor sample subset
#	use the code in tumor samples.R

subset = subset [which(subset %in% colnames(data.tumor))]
data.tumor = data.tumor[,c("X",subset)]

rname = unique(data.tumor$symbol)
maxexp = NULL
for( i in rname){
	tmp = data.tumor[which(data.tumor$symbol == i),]
	maxexp = rbind(maxexp, apply(tmp[,2:151], 2, max, na.rm = T ) )
	
}
rownames(maxexp) = rname

tissue.data = maxexp
n1 = n1[rname, rname]

APCC=NULL;
for(j in 1:dim(tissue.data)[1]){
	rtmp=NULL;
	I=which(n1[j,]!=0)
	if(length(I)!=0){
		for(i in 1:length(I)){
			rtmp[i]=cor(tissue.data[I[i],],tissue.data[j,], use = "pairwise.complete.obs")
		}
		APCC[j]=sum(rtmp,na.rm=T)/(length(I))
	}
	else{
		APCC[j]=0
	}
}
names(APCC) = rownames(tissue.data)
