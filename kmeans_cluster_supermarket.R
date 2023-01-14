
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Supermarket K-Means Clustering
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



minimarket = read.csv("C:/Users/thami/OneDrive/Desktop/Pastas Git/Market_Basket_Analysis/minimarket.csv",sep =";")


library(corrplot)



mini=minimarket
boxplot(mini$lucro )
boxplot(mini$faturamento )
boxplot(mini$metas )
boxplot(mini$publ )
round(cor(mini[,4:7]),2)
library(gmodels)

CrossTable(mini$zona, mini$idade, prop.r = F, prop.c = F,
           prop.chisq = F,prop.t=T)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
mini.dr=mini[,4:7]  
head(mini.dr)
mini.p=scale(mini.dr) 
head(mini.p)

mini.dist=dist(mini.p) 

hc=hclust(mini.dist, method = 'ward.D2')
plot(hc, hang=-1)
abline(h=12, col=2)
abline(h=5, col=3)
mini$hcward=cutree(hc,2) 


mini$empresa[mini$hcward==1]
mini$empresa[mini$hcward==2]

plot(mini$lucro, mini$faturamento,pch=19, 
     col=c(1,2)[mini$hcward]); grid()

library(ggplot2)
ggplot(mini) +
  aes(x = lucro, y = faturamento, colour = hcward) +
  geom_point(size = 3, pch=c(15,17)[mini$hcward]) +
  scale_color_gradient() +
  theme_gray()

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
par(mfrow=c(2,2))
boxplot(mini$lucro~mini$hcward,main='lucro', col=topo.colors(2))
boxplot(mini$faturamento~mini$hcward, main='faturamento', col=topo.colors(2))
boxplot(mini$metas~mini$hcward, main='metas', col=topo.colors(2))
boxplot(mini$publ~mini$hcward, main="publ", col=topo.colors(2))


library(gmodels)
CrossTable(mini$zona, mini$hcward, prop.chisq = F,prop.t=F, prop.c = F)
CrossTable(mini$idade, mini$hcward, prop.chisq = F,prop.t=F , prop.c = F)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library(cluster) 
mini=minimarket
mini$zona=as.factor(mini$zona)
mini$idade=as.ordered(mini$idade) 


mix.dist=daisy(mini[,-1]) 
hcmix=hclust(mix.dist, method = 'ward.D2')
plot(hcmix, hang=-1)
mini$hcmx=cutree(hcmix,2)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

par(mfrow=c(2,2))
boxplot(mini$lucro~mini$hcmx,main='lucro', col=topo.colors(2))
boxplot(mini$faturamento~mini$hcmx, main='faturamento', col=topo.colors(2))
boxplot(mini$metas~mini$hcmx, main='metas', col=topo.colors(2))
boxplot(mini$publ~mini$hcmx, main="publ", col=topo.colors(2))


library(gmodels)
CrossTable(mini$zona, mini$hcmx, prop.chisq = F,prop.t=F )
CrossTable(mini$idade, mini$hcmx, prop.chisq = F,prop.t=F )

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
names(mini)
mini$zona=as.factor(mini$zona)
mini$idade=as.ordered(mini$idade) 
pesos=c(.5,.4,1,1,1,1) 

mix.dist2=daisy(mini[,-c(1,8,9)], weights = pesos)
hc2=hclust(mix.dist2, method = 'ward.D2')
plot(hc2, hang=-1)
mini$hcmx2=cutree(hc2,2)
#
# drivers
par(mfrow=c(2,2))
boxplot(mini$lucro~mini$hcmx2,main='lucro', col=topo.colors(2))
boxplot(mini$faturamento~mini$hcmx2, main='faturamento', col=topo.colors(2))
boxplot(mini$metas~mini$hcmx2, main='metas', col=topo.colors(2))
boxplot(mini$publ~mini$hcmx2, main="publ", col=topo.colors(2))

library(gmodels)
CrossTable(mini$zona, mini$hcmx2, prop.chisq = F,prop.t=F , prop.c = F)
CrossTable(mini$idade, mini$hcmx2, prop.chisq = F,prop.t=F , prop.c = F)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#kmeans
#set.seed(11) 
#kmn=kmeans(k=4, nstart=25)



library(fpc)
set.seed(18) 
kmd=pamk(mix.dist, 
         diss = T, 
         k=2:8, 
         criterion = "ch"  , 
         critout = T) 
kmd$nc


mini$kmedoid=kmd$pamobject$clustering
table(mini$kmedoid)
boxplot(mini$meta~mini$kmedoid)


#medoides
kmd$pamobject$medoids

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#cluster
library(cluster)
set.seed(11)
kpam=pam(mix.dist, diss = T, 3)
?clusplot # só roda com objeto pam, clara 
clusplot(kpam, shade = F,labels=2,col.clus="blue",col.p="red",
         span=T,main="Cluster Mapping",cex=1.2)
##############################################################
set.seed(18) 
kmd2=pamk(mix.dist, 
          diss = T, 
          k=2, 
          criterion = "ch"  ) 

mini$kmedoid2=kmd2$pamobject$clustering




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# kmedoid x hcmx 
library(gmodels)
CrossTable(mini$kmedoid2, mini$hcmx, prop.chisq = F, prop.t=F)
CrossTable(mini$kmedoid, mini$hcmx, prop.chisq = F, prop.t=F)
CrossTable(mini$kmedoid, mini$kmedoid2, prop.chisq = F, prop.t=F)


cluster.stats(mix.dist, mini$hcmx, mini$kmedoid)$corrected.rand
cluster.stats(mix.dist, mini$hcmx, mini$kmedoid2)$corrected.rand
cluster.stats(mix.dist, mini$kmedoid, mini$kmedoid2)$corrected.rand


cluster.stats(mix.dist, mini$hcmx) $within.cluster.ss # hcmx
cluster.stats(mix.dist, mini$kmedoid2) $within.cluster.ss  # kmedoid2
cluster.stats(mix.dist, mini$kmedoid) $within.cluster.ss  # kmedoid









