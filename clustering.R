install.packages("ggplot2")
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
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

plot(for_clustering$Dalc.x, for_clustering$Walc.x)
