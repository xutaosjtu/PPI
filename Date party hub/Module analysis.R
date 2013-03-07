# TODO: Module overlap
# 
# Author: xutao
###############################################################################


data = read.csv("Supplementary figure 1.csv", header = F)
FHCmodule=scan()
93
154
178
227
58
201
92
139
374
41
172
49
111
79
25
84
28
10
10
9
65
21
7

UHCmodule = scan()
410
431
488
343
119
358
149
307
382
255
146
203
183
16

rst = data
for(i in 1:length(FHCmodule)){
	for(j in 1:length(UHCmodule)){
		rst[i,j] = data[i,j]/min(FHCmodule[i],UHCmodule[j])
	}
}

rect(0,-4,0.05,2.5,col = "grey80",border=NA)
rect(0.05,-4,0.62,2.5,col = rgb(255, 225, 225,maxColorValue = 255),border=NA)
rect(0.62,-4,0.8,2.5,col = rgb(225, 255, 225,maxColorValue = 255),border=NA)
rect(0.8,-4,1,2.5,col = rgb(210, 225, 255,maxColorValue = 255),border=NA)
rect(0,2.5,0.3,19,col = rgb(250, 250, 210,maxColorValue = 255),border=NA)
rect(0.3,2.5,0.75,19,col = rgb(235, 220, 255,maxColorValue = 255),border=NA)
rect(0.75,2.5,1,19,col = rgb(255, 245, 238,maxColorValue = 255),border=NA)

roles<-function(x){
	if(x[2]<2.5){ 
		if(x[1]<0.05) return(1)
		else if(x[1]<0.62) return(2)
		else if (x[1]<0.8) return(3)
		else return(4)
	}
	else {
		if(x[1]<0.3) return(5)
		else if(x[1]<0.75) return(6)
		else return(7)
	}
}

tmp=apply(HCup.att[,2:3], 1, roles)
HCup.att$role = tmp
tapply(HCup.att$degree, INDEX = HCup.att$role, function(x) length(x)/3801)


tmp=apply(HJD.att[,2:3], 1, roles)
HJD.att$role = tmp
tapply(HJD.att[,1], INDEX = HJD.att$role, function(x) length(x)/2233)

annotation = read.csv("GO annotation.sgd.csv", header=F,stringsAsFactors=FALSE)
annotation = read.csv("dbxref.tab", header =F, sep = "\t")

tmp = sapply(annotation[,11], function(x) unlist(strsplit(x, fixed=T, split="|"))[1])

annotation[,11] = tmp

HJD.att$locus = rownames(HJD.att)
tmp = merge(HJD.att, annotation, by.x = "locus", by.y="V11", all.x = T, all.y = F, sort = F)

HCup.att$locus = rownames(HCup.att)
tmp = merge(HCup.att, annotation, by.x = "locus", by.y="V11", all.x = T, all.y = F, sort = F)