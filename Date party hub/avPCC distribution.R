# TODO: Add comment
# 
# Author: tao.xu
###############################################################################

#########################################################
HCup.role.t = rep(0, length(HCup.role))
HCup.role.t[which(HCup.role == 1)] =2
HCup.role.t[which(HCup.role == 2)] =1
HCup.role.t[which(HCup.role == 3)] =6
HCup.role.t[which(HCup.role == 4)] =3
HCup.role.t[which(HCup.role == 5)] =7
HCup.role.t[which(HCup.role == 6)] =5
HCup.role.t[which(HCup.role == 7)] =4
avg = tapply(HCup.apcc, INDEX = as.factor(HCup.role.t), mean)
plot(avg, col = rainbow(7), cex = 4, ylim = range(HCup.apcc),
		main = "UHC", ylab = "Avergae PCC with interaction partners", xlab = "Node role")
points(HCup.apcc~HCup.role.t, col = rainbow(7)[HCup.role.t], pch = c(2:6,8,22)[HCup.role.t])

#######################################################
HJD.role.t = rep(0, length(HJD.role))
HJD.role.t[which(HJD.role == 1)] =1
HJD.role.t[which(HJD.role == 2)] =2
HJD.role.t[which(HJD.role == 3)] =6
HJD.role.t[which(HJD.role == 4)] =3
HJD.role.t[which(HJD.role == 5)] =5
#HJD.role.t[which(HJD.role == 6)] =5
#HJD.role.t[which(HJD.role == 7)] =4
avg = tapply(HJD.apcc, INDEX = as.factor(HJD.role.t), mean)
avg =  c(avg[1:3], NA, avg[4:5],NA)
plot(avg, col = rainbow(7)[as.numeric(names(avg))], cex = 4, ylim = range(HJD.apcc),
		main = "FHC", ylab = "Avergae PCC with interaction partners", xlab = "Node role")
points(HJD.apcc~HJD.role.t, col = rainbow(7)[HJD.role.t], pch = c(2:6,8,22)[HJD.role.t])

