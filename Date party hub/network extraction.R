# TODO: Add comment
# 
# Author: xutao
###############################################################################


cor2link = function(Zeta, threshold){
	ggm=NULL;
	for(i in 1:(dim(Zeta)[1]-1)){
		for(j in (i+1):dim(Zeta)[1]){
			if(abs(Zeta[i,j])> threshold){
				tmp=c(colnames(Zeta)[i], colnames(Zeta)[j], 
						Zeta[i,j])
				ggm=rbind(ggm,tmp)
			}
		}
	}
	return(ggm)
}

extractlink = function( data, attr, module = 1){
	index = NULL;
	for(i in 1:length(module)){
		index = c(index,attr$names.APCC.[which(attr$module == module[i])]) 
	}
	index = as.character(index)
	colnames(data) = rownames(data)
	index = index[which(index %in% rownames(data))]
	module = data[index, index]
	module_link = cor2link(module, 0)
	return(module_link)
}

module15 = extractlink(n1 , attr = HumanData , module = c(15))
write.csv(module9, file = "module_9_all.csv", row.names = F)

module4_6 = extractlink(HJD_all , module = c(4, 6))
write.csv(module4_6, file = "module_4_6_all.csv", row.names = F)



###	calculate correlation after extraction of subnetwork
module15_link[,3] = apply(module15_link, 1, function(x) cor(tissue.data[x[1],], tissue.data[x[2],], use = "pairwise.complete.obs"))
















