#used for more advanced visualization
install.packages("ggplot2")

setwd(".")
dir.create(file.path(getwd(), "img/kmeans"), recursive = TRUE)

#import data set
d1 = read.table("student-mat.csv", sep = ",", header = TRUE)
d2 = read.table("student-por.csv", sep = ",", header = TRUE)
#merge data excluding duplicates of students
d3 = merge(
  d1,
  d2,
  by.x = c(
    "school",
    "sex",
    "age",
    "address",
    "famsize",
    "Pstatus",
    "Medu",
    "Fedu",
    "Mjob",
    "Fjob",
    "reason",
    "nursery",
    "internet"
  ),
  by.y = c(
    "school",
    "sex",
    "age",
    "address",
    "famsize",
    "Pstatus",
    "Medu",
    "Fedu",
    "Mjob",
    "Fjob",
    "reason",
    "nursery",
    "internet"
  )
)
print(nrow(d3)) # 382 students

#general data overwiev before clustering
#separate out data for clustering
for_clustering = d3[c("Dalc.x", "Walc.x")]
#bar plots of student drinking habits
ggplot2::ggplot(
  mapping = ggplot2::aes(for_clustering$Dalc.x),
  colnames = c("1", "2", "3", "4", "5")
) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Amount drunk", title = "Daily alcohol consumption")

ggplot2::ggplot(
  mapping = ggplot2::aes(for_clustering$Walc.x),
  colnames = c("1", "2", "3", "4", "5")
) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Amount drunk", title = "Weekend alcohol consumption")
ggplot2::ggplot(
  mapping = ggplot2::aes(for_clustering$Walc.x + for_clustering$Dalc.x),
  colnames = c("2", "3", "4", "5", "6", "7", "8", "9", "10")
) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Amount drunk", title = "Total alcohol consumption")

ggplot2::ggplot(mapping = ggplot2::aes(for_clustering$Walc.x + (for_clustering$Dalc.x ^ 2))) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Amount drunk", title = "Total alcohol consumption weighted")

#boxplots
ggplot2::ggplot(mapping = ggplot2::aes(x = "daily", y = for_clustering$Dalc.x)) +
  ggplot2::stat_boxplot() +
  ggplot2::labs(title = "Daily alcohol consumption", y = "amount")
ggplot2::ggplot(mapping = ggplot2::aes(x = "weekend", y = for_clustering$Walc.x)) +
  ggplot2::stat_boxplot() +
  ggplot2::labs(title = "Weekendly alcohol consumption", y = "amount")
ggplot2::ggplot(mapping = ggplot2::aes(x = "total", y = for_clustering$Dalc.x +
                                         for_clustering$Walc.x)) +
  ggplot2::stat_boxplot() +
  ggplot2::labs(title = "Total alcohol consumption", y = "amount")
ggplot2::ggplot(mapping = ggplot2::aes(
  x = "total",
  y = (for_clustering$Dalc.x ^ 2) + for_clustering$Walc.x
)) +
  ggplot2::stat_boxplot() +
  ggplot2::labs(title = "Total alcohol consumption weighted", y = "amount")

#density plot
ggplot2::ggplot(mapping = ggplot2::aes(for_clustering$Dalc.x, for_clustering$Walc.x)) +
  ggplot2::geom_count() +
  ggplot2::scale_size_area() +
  ggplot2::labs(title = "Density plot of students alcohol consumption", x =
                  "Daily alcohol consumption", y = "Weekend alcohol consumption") +
  ggplot2::coord_fixed()

ggplot2::ggplot(mapping = ggplot2::aes(for_clustering$Dalc.x ^ 2, for_clustering$Walc.x)) +
  ggplot2::geom_count() +
  ggplot2::scale_size_area() +
  ggplot2::labs(title = "Density plot of students alcohol consumption", x =
                  "Daily alcohol consumption squared", y = "Weekend alcohol consumption")

###############################################################################k-means
## preparing parameters for the algorithm
iterations = c(
  1,
  5,
  10,
  25,
  50,
  100,
  150,
  200,
  250,
  400,
  500,
  750,
  1000,
  1250,
  1500,
  2000,
  2500,
  3000,
  5000,
  10000
)
algorithms = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
n_starts = c(1, 3, 5, 10, 15, 25, 50)
parameters = c()
parameters$iterations = sort(as.numeric(rep(
  iterations, length(algorithms) * length(n_starts)
)))
parameters$algorithms = rep(algorithms, length(iterations) * length(n_starts))
parameters$n_starts = rep(n_starts, length(iterations) * length(algorithms))
parameters = data.frame(matrix(
  unlist(parameters),
  ncol = 3,
  dimnames = list(c(), c("iterations", "algorithm", "n_starts"))
))
parameters$iterations = as.integer(as.character(parameters$iterations))
parameters$n_starts = as.integer(as.character(parameters$n_starts))
parameters = parameters[order(parameters[, 1], parameters[, 2], parameters[, 3]), ]
parameters$eval = 0.0


#Run the algorithm for each parameter
res <- by(parameters, 1:nrow(parameters), function(row) {
  print(row)
  k_means_results = kmeans(
    x = for_clustering,
    centers = 2,
    iter.max = row$iterations,
    nstart = row$n_starts,
    algorithm = c(as.character(row$algorithm)),
    trace = FALSE
  )
  k_means_centers = data.frame(matrix(
    unlist(k_means_results[2]),
    nrow = 2,
    ncol = 2,
    byrow = F,
    dimnames = list(c(1, 2), c("Dalc", "Walc"))
  ))
  print(k_means_centers)
  print(dist(k_means_centers, "euclidean"))
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      for_clustering$Dalc.x,
      for_clustering$Walc.x,
      color = k_means_results$cluster
    )
  ) +
    ggplot2::geom_count() +
    ggplot2::geom_point(
      mapping = ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc, color =
                               c(1, 2)),
      shape = 18,
      size = 7
    ) +
    ggplot2::labs(
      title = "Alcohol consumption clustering",
      subtitle = paste(
        "max iterations:",
        as.character(row$iterations),
        "\t",
        "algorithm used:",
        as.character(row$algorithm),
        "\t",
        "starting clusters:",
        as.character(row$n_starts),
        sep = " "
      ),
      y = "Weekend consumption",
      x = "Daily consumption"
    ) +
    ggplot2::coord_fixed()
  ggplot2::ggsave(paste("img/kmeans/kmeans", as.character(row$iterations), as.character(row$algorithm), as.character(row$n_starts), ".png", sep="_"))
  row$eval = as.double(dist(k_means_centers, "euclidean"))
  return(row)
})

#ugly workaround to incorporate the cluster distance into parameters
for (i in 1:length(res)) {
  res_row = res[i]
  eval = res_row[[1]]$eval
  parameters$eval[i] <- eval
}

parameters = parameters[order(-parameters$eval),]
print("10 best K-means")
print(parameters[1:10,])
best = parameters[1:10,]

#########################################attempt at playing with centroid starting points
originsm = matrix(c(5, 1, 5, 1), nrow = 2, ncol = 2)
origins = data.frame(originsm)
k_means_results = kmeans(
  x = for_clustering,
  centers = originsm,
  iter.max = 10,
  nstart = 3,
  algorithm = c("MacQueen"),
  trace = FALSE
)
k_means_centers = data.frame(matrix(
  unlist(k_means_results[2]),
  nrow = 2,
  ncol = 2,
  byrow = F,
  dimnames = list(c(1, 2), c("Dalc", "Walc"))
))
print(k_means_centers)
ggplot2::ggplot(
  mapping = ggplot2::aes(for_clustering$Dalc.x, for_clustering$Walc.x, color =
                           k_means_results$cluster)
) +
  ggplot2::geom_count() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc, color =
                             c(1, 2)),
    shape = 18,
    size = 7
  ) +
  ggplot2::geom_point(mapping = ggplot2::aes(origins$X1, origins$X2, color =
                                               -1)) +
  ggplot2::labs(
    title = "Alcohol consumption clustering",
    subtitle = paste(
      "max iterations:",
      as.character(10),
      "\t",
      "algorithm used:",
      as.character("MacQueen"),
      "\t",
      "starting clusters:",
      as.character(3),
      sep = " "
    ),
    y = "Weekend consumption",
    x = "Daily consumption"
  ) +
  ggplot2::coord_fixed()
ggplot2::ggsave(paste("img/kmeans/kmeans", "custom_centroids", ".png", sep = "_"))

#########################Well none look too great. Let's try something a little different. Let's assume we have three clusters and two of them
#############################claim that the user is not a drunk

k_means_results = kmeans(
  x = for_clustering,
  centers = matrix(c(1, 1, 5, 5, 1, 5), ncol = 2, nrow =
                     3),
  iter.max = 1000,
  nstart = 100,
  algorithm = c("Hartigan-Wong"),
  trace = FALSE
)
k_means_centers = data.frame(matrix(
  unlist(k_means_results[2]),
  nrow = 3,
  ncol = 2,
  byrow = F,
  dimnames = list(c(1, 2, 3), c("Dalc", "Walc"))
))
k_means_centers$idx = c(1, 2, 3)
print(k_means_centers)
ggplot2::ggplot(
  mapping = ggplot2::aes(for_clustering$Dalc.x, for_clustering$Walc.x, color =
                           k_means_results$cluster)
) +
  ggplot2::geom_count() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc, color =
                             c(1, 2, 3)),
    shape = 18,
    size = 7
  ) +
  ggplot2::labs(
    title = "Alcohol consumption clustering with 3 clusters",
    subtitle = paste(
      "max iterations:",
      as.character(1000),
      "\t",
      "algorithm used:",
      as.character("Hartigan"),
      "\t",
      "starting clusters:",
      as.character(100),
      sep = " "
    ),
    y = "Weekend consumption",
    x = "Daily consumption"
  ) +
  ggplot2::coord_fixed()
ggplot2::ggsave(paste("img/kmeans/kmeans", "3clusters", ".png", sep = "_"))


###########################Perhaps let's try a little bit of scaling do Dalc like an x^2 scale

for_clustering_scaled = for_clustering
for_clustering_scaled$Dalc.x = for_clustering_scaled$Dalc.x ^ 2
k_means_results = kmeans(
  x = for_clustering_scaled,
  centers = 2,
  iter.max = 1000,
  nstart = 100,
  algorithm = c("Hartigan-Wong"),
  trace = FALSE
)
k_means_centers = data.frame(matrix(
  unlist(k_means_results[2]),
  nrow = 2,
  ncol = 2,
  byrow = F,
  dimnames = list(c(1, 2), c("Dalc", "Walc"))
))
print(k_means_centers)
ggplot2::ggplot(
  mapping = ggplot2::aes(
    for_clustering_scaled$Dalc.x,
    for_clustering_scaled$Walc.x,
    color = k_means_results$cluster
  )
) +
  ggplot2::geom_count() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc, color =
                             c(1, 2)),
    shape = 18,
    size = 7
  ) +
  ggplot2::labs(
    title = "Alcohol consumption clustering",
    subtitle = paste(
      "max iterations:",
      as.character(1000),
      "\t",
      "algorithm used:",
      as.character("Hartigan"),
      "\t",
      "starting clusters:",
      as.character(100),
      sep = " "
    ),
    y = "Weekend consumption",
    x = "Daily consumption"
  )
ggplot2::ggsave(paste("img/kmeans/kmeans", "sclaed_clustering", ".png", sep =
                        "_"))

##############################################################################mean shift clustering
install.packages("MeanShift")
means_data = t(for_clustering)
# This shows that simple means shift doesn't do so well. So let's try ramping it up a bit
means_clusters = MeanShift::msClustering(means_data)
plot(
  means_data[1, ],
  means_data[2, ],
  col = means_clusters$labels + 2,
  cex = 0.8,
  pch = 16,
  xlab = "Dalc",
  ylab = "Walc"
)
points(
  means_clusters$components[1, ],
  means_clusters$components[2, ],
  col = 2 + (1:ncol(means_clusters$components)),
  cex = 1.8,
  pch = 16
)
#This really doesn't look that much OK
h_factor = quantile(dist(t(means_data)), 0.53727)
means_clusters = MeanShift::msClustering(means_data, h = h_factor)
plot(
  means_data[1, ],
  means_data[2, ],
  col = means_clusters$labels + 2,
  cex = 0.8,
  pch = 16,
  xlab = "Dalc",
  ylab = "Walc"
)
points(
  means_clusters$components[1, ],
  means_clusters$components[2, ],
  col = 2 + (1:ncol(means_clusters$components)),
  cex = 1.8,
  pch = 16
)
#Neither does this though :/
h_factor = quantile(dist(t(means_data)), 0.53726)
means_clusters = MeanShift::msClustering(means_data, h = h_factor)
plot(
  means_data[1, ],
  means_data[2, ],
  col = means_clusters$labels + 2,
  cex = 0.8,
  pch = 16,
  xlab = "Dalc",
  ylab = "Walc"
)
points(
  means_clusters$components[1, ],
  means_clusters$components[2, ],
  col = 2 + (1:ncol(means_clusters$components)),
  cex = 1.8,
  pch = 16
)

##########################################################This won't work for sure - dbscan
install.packages("dbscan")
db_clusters1 = dbscan::dbscan(for_clustering, 1)
plot(
  for_clustering[, 1],
  for_clustering[, 2],
  pch = 16,
  col = db_clusters1$cluster,
  xlab = "Dalc",
  ylab = "Walc"
)
db_clusters1 = dbscan::dbscan(for_clustering, 0.1)
plot(
  for_clustering[, 1],
  for_clustering[, 2],
  pch = 16,
  col = db_clusters1$cluster,
  xlab = "Dalc",
  ylab = "Walc"
)

#############################################################Hierarchial clustering
install.packages("dplyr")
install.packages("magrittr")
library(magrittr)
library(dplyr)
colnames(for_clustering) = c("D", "W")
dclu = for_clustering %>% dplyr::add_count(for_clustering$D, for_clustering$W)
differ = duplicated(dclu)
dclu = dclu[!duplicated(dclu), ]
dclu = dclu[, c("D", "W", "n")]
dclu$label <-
  paste(as.character(dclu$D), as.character(dclu$W), sep = "_")
hclusters = hclust(dist(dclu), "complete")
plot(hclusters,
     labels = dclu$label,
     hang = -1,
     cex = 0.6)

##################################################EM clustering
install.packages("EMCluster")
library(EMCluster)
emclust_data <- for_clustering
ret <- EMCluster::init.EM(emclust_data, nclass = 2)
ret.new <-
  EMCluster::assign.class(emclust_data, ret, return.all = FALSE)
str(ret.new)
emclust_data$cluster = ret.new$class
plot(
  emclust_data[, 1],
  emclust_data[, 2],
  pch = 16,
  col = emclust_data$cluster,
  xlab = "Dalc",
  ylab = "Walc"
)


