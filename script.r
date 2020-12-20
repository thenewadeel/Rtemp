set.seed(786)
DF<-read.csv('input.csv',sep=';')
print('Found columns:')
print(colnames(DF))
#DF$Age
#DF$Age<-as.integer(as.factor(DF$Age))
#DF$Age<-scale(DF$Age)

#for (col in colnames(DF)){
#print('Doing')
#print(col)
#DF[col]<-as.integer(as.factor(DF[col]))
#DF[col]<-scale(DF[col])
#}

for (col in 1:ncol(DF)){
#print('Doing')
#print(col)
DF[,col]<-as.integer(as.factor(DF[,col]))
DF[,col]<-scale(DF[,col])
}
#str(DF)
dist_matrix=dist(DF,method='euclidean')
hclust_avg=hclust(dist_matrix,method='average')
plot(hclust_avg)

cut_avg=cutree(hclust_avg,k=3)
rect.hclust(hclust_avg,k=3,border=2:6)
abline(h=3,col='red')

avg_dend_obj=as.dendrogram(hclust_avg)
suppressPackageStartupMessages(library(dendextend))
avg_col_dend=color_branches(avg_dend_obj,h=3)
plot(avg_col_dend)








