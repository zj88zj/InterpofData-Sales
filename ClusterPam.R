################## R Code for robust clustering with PAM #######################

h = read.csv("hospital.csv")
x = log(1+0.001*h[sample(4703,1000),-c(1:4,10:11)])
pairs(x,pch=".")
z = princomp(x)$scores[,c(1:4)]
pairs(z)
library(cluster)
# If you want to use the same factors that you got from SAS
# then save the SAS dataset as CSV using the export
hout = read.csv("hout.csv")
pairs(hout)
pam(hout[,-c(1,5)],k=10)
plot(silhouette(pam(hout[,-c(1,5)],k=10)))
plot(silhouette(pam(hout[,-c(1,5)],k=12)))
plot(silhouette(pam(hout[,-c(1,5)],k=3)))
plot(hclust(dist(hout[,-c(1,5)])))
cutree(hclust(dist(hout[,-c(1,5)])),17) -> cl
table(cl)
