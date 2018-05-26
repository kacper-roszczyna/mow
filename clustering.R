install.packages("ggplot2")
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,
         by.x=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"),
         by.y=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

#general data overwiev before clustering
for_clustering=d3[c("Dalc.x", "Walc.x")]
#histograms
hist(for_clustering$Dalc.x)
hist(for_clustering$Walc.x)
hist(for_clustering$Dalc.x+for_clustering$Walc.x)

#boxplots
boxplot(for_clustering$Dalc.x)
boxplot(for_clustering$Walc.x)
boxplot(for_clustering$Dalc.x+for_clustering$Walc.x)

#simple plot
plot(for_clustering$Dalc.x, for_clustering$Walc.x)
#density plot
ggplot2::ggplot(mapping=ggplot2::aes(for_clustering$Dalc.x, for_clustering$Walc.x)) + 
  ggplot2::geom_count()

#k-means
k_means_results = kmeans(x = for_clustering, 
                         centers = 2, 
                         iter.max = 500, 
                         nstart = 1, 
                         algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
                         trace=FALSE)
k_means_centers = data.frame(matrix(unlist(k_means_results[2]), nrow=2, ncol = 2, byrow=F, dimnames = list(c(1, 2), c("Dalc", "Walc"))))
ggplot2::ggplot(mapping=ggplot2::aes(for_clustering$Dalc.x, for_clustering$Walc.x)) + 
  ggplot2::geom_count() + 
  ggplot2::geom_point(mapping=ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc), color="red")

#mean shift clustering
install.packages("MeanShift")
means_data = t(for_clustering)
# This shows that simple means shift doesn't do so well. So let's try ramping it up a bit
means_clusters = MeanShift::msClustering(means_data)
plot( means_data[1,], means_data[2,], col=means_clusters$labels+2, cex=0.8,
      pch=16, xlab="Dalc", ylab="Walc" )
points( means_clusters$components[1,], means_clusters$components[2,],
        col=2+( 1:ncol( means_clusters$components ) ), cex=1.8, pch=16 )
#This really doesn't look that much OK
h_factor=quantile( dist( t( means_data ) ), 0.53727 )
means_clusters = MeanShift::msClustering(means_data, h = h_factor)
plot( means_data[1,], means_data[2,], col=means_clusters$labels+2, cex=0.8,
      pch=16, xlab="Dalc", ylab="Walc" )
points( means_clusters$components[1,], means_clusters$components[2,],
        col=2+( 1:ncol( means_clusters$components ) ), cex=1.8, pch=16 )
#Neither does this though :/
h_factor=quantile( dist( t( means_data ) ), 0.53726 )
means_clusters = MeanShift::msClustering(means_data, h = h_factor)
plot( means_data[1,], means_data[2,], col=means_clusters$labels+2, cex=0.8,
      pch=16, xlab="Dalc", ylab="Walc" )
points( means_clusters$components[1,], means_clusters$components[2,],
        col=2+( 1:ncol( means_clusters$components ) ), cex=1.8, pch=16 )

#This won't work for sure - dbscan
install.packages("dbscan")
db_clusters1 = dbscan::dbscan(for_clustering, 1)
plot(for_clustering[,1], for_clustering[,2], pch=16, col=db_clusters1$cluster, xlab="Dalc", ylab="Walc")
db_clusters1 = dbscan::dbscan(for_clustering, 0.1)
plot(for_clustering[,1], for_clustering[,2], pch=16, col=db_clusters1$cluster, xlab="Dalc", ylab="Walc")

#Hierarchial clustering
colnames(for_clustering) = c("D", "W")
install.packages("dplyr")
install.packages("magrittr")
library(magrittr)
dclu = for_clustering %>% dplyr::add_count(for_clustering$D, for_clustering$W)
dclu2 = dclou
differ = duplicated(dclu)
dclu = dclu[!duplicated(dclu),]
dclu = dclu[, c("D", "W", "n")]
dclu$label <- paste(as.character(dclu$D), as.character(dclu$W), sep = "_")
hclusters = hclust(dist(dclu2), "complete")
plot(hclusters, labels=dclu$label, hang = -1, cex=0.6 )

