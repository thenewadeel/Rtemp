set.seed(786)
DF<-read.csv('input.csv',sep=';')
DF$list..<-NULL
head(DF)
print(colnames(DF))
head(summary(DF$favourite),n=10)
### Visualizations to describe variables to answer the following :
DF$favourite<-tolower(trimws(DF$favourite))# trims whitespace, lowers case
head(summary(DF$favourite),n=4)
plot(head(summary(DF$favourite),n=4))
plot(sort(table(trimws(DF$favourite)),decreasing=TRUE)[2:25])#plots after doing all other stuff
superheroes=c("Joker","Batman","Avenger","Captain")
superherofans=NULL
for (hero in superheroes){
superherofans<-append(superherofans,subset(DF, grepl(hero,favourite)))
}
superherofans


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








