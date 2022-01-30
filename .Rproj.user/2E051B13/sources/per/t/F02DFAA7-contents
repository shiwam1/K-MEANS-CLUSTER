list.files()
df=read.csv("Mall_Customers.csv")
head(df)
summary(df)
#CHANGING GENDER COLUMN AS BINARY VARIABLE
df$Gender=as.factor(df$Gender)
levels(df$Gender)=0:(length(df$Gender)-1)
#here MALE=0, FEMALE=1
df$Gender=as.numeric(as.character(df$Gender))
#VISUALIZING AGE AND GENDER
range(df$Age);range(df$Gender)
plot(df$Age,df$Gender,main = "age and gender plot",xlab = "GENDER",ylab = "AGE",col="red")
hist(df$Gender,main = "HISTOGRAM OF GENDER",xlab = "GENDER",col = "blue")
hist(df$Age,main = "HISTOGRAM OF AGE",xlab = "AGE",col = "orange")
boxplot(df$Age~df$Gender,ylab = "Age",xlab = "Gender",col="red",main="BOXPLOT OF AGE AND GENDER")
##########CLUSTER ANALYSIS########
########K_MEANS_CLUSTERING(NON HIERARCHICAL APPROACH)######

#AT FIRST FINDIING THE DISTANCE MATRIX OF THE DATASET
#applying
str(df)
mod=kmeans(df[,-c(1,2,3)],6)
cc=mod$centers#cluster centroid
cc
rownames(cc)=c("centroid1","centroid2","centroid3","centroid4","centroid5","centroid6")
cc
#DISTANCE OF OBSERVATIONS FROM CLUSTER CENTROIDS 
#NOW MAKING A MATRIX OF SIZE= 200(OBSER.) x (6 clusters + 1 more column)
#here 1 more column for clusterID(ON THIS COLUN WE SEE WHICH OBSERVATION IS ASSIGNED TO WHICH CLUSTER)


#INITIALIZE (200 X 7) MATRIX
DM= matrix(NA,200,7);head(DM)
rownames(DM)=c(1:200);head(DM)
colnames(DM)=c("clusterID","Dist_clust1","Dist_clust2","Dist_clust3",
               "Dist_clust4","Dist_clust5","Dist_clust6")
head(DM)
#CLUSTER ASSIGNMENT
DM[,1]=mod$cluster;head(DM)
#COMPUTE DISTANCE (6 DISTANCE FOR 6 CENTROID)
for (i in 1:200) {
  DM[i,2]=dist(rbind(df[i,],cc[1,]),method = "euclidean")
  DM[i,3]=dist(rbind(df[i,],cc[2,]),method = "euclidean")
  DM[i,4]=dist(rbind(df[i,],cc[3,]),method = "euclidean")
  DM[i,5]=dist(rbind(df[i,],cc[4,]),method = "euclidean")
  DM[i,6]=dist(rbind(df[i,],cc[5,]),method = "euclidean")
  DM[i,7]=dist(rbind(df[i,],cc[6,]),method = "euclidean")

}
head(DM)
#ORDER BY CLUSTER ID
head(DM[order(DM[,1]),])
#DISTANCE BETWEEN CLUSTER CENTROID
dist(cc,method = "euclidean",diag = T,upper = T)
#SUMMARY OF CLUSTER DISTANCES
DM2=matrix(NA,7,2);DM2
rownames(DM2)=c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6","overall")
colnames(DM2)=c("observation","Avg.dist_in_cluster");DM2#GIVES DISPERSION VALUE
DM2[1:6,1]=mod$size;DM2
DM2[7,1]=sum(DM2[1:6,1])
for (i in 1:6) {
  DM2[i,2]=mean(DM[which(DM[,1]==i),]);DM2
  DM2[7,2]=mean(DM2[1:6,2]);DM2
}

head(DM2)
#LINE CHART OR PROFIT PLOT
#PARALLEL COORDINATES PLOT(HELPS IN CHARACTERIZING THE CLUSTER)
maxv=apply(cc,2, max)
minv=apply(cc,2, min)
cc.so1=as.data.frame(scale(cc,center =minv,scale = maxv-minv))
cc.so1#here change the cluster as scaling
library(MASS)
par(mar=c(0,0,0,0))
par(mar=c(7.1,8.1,3,8.1),xpd=T,las=2,cex.axis=0.7)
parcoord(cc.so1,col = gray(0:5/6),lty = c(1:6),lwd=2,las=3)
axis(2,at=c(0,0.5,1),labels = c(0,0.5,0.1))
title(ylab = "scaled cluster centroids")
legend("bottomright",inset = c(-0.3,0)
       ,c("centroid1","centroid2","centroid3","centroid4","centroid5","centroid6")
       ,lty = c(1:6),cex=0.6,x.intersp = 0.4,y.intersp = 0.5,col = gray(0:5/6),bty = "n")
#plot cluster#2D REPRESENTATION OF CLUSTER
library(cluster)
clusplot(df[,-c(1,2,3)],mod$cluster,main = "2D REPRESENTATION OF CLUSTER",color = T
         ,labels = 2,lines = 0,shade = T)






