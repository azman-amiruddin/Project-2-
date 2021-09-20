library(readxl)
library(factoextra)
library(corrplot)
Data_Inci <- read_excel("D:/FILE KULIAH/SEMESTER 8/Inci/Data Inci.xlsx",
                           col_types = c("skip", "numeric", "numeric",
                                         "numeric", "numeric","numeric", "numeric",
                                         "numeric", "numeric","numeric", "numeric")) 

Data_Inci
View(Data_Inci)
summary(Data_Inci)
#Jumlah Cluster Optimal
silhouette_score <- function(k){
  km <- hclust(Data_Inci,method = "centroid" )
  ss <- silhouette(km$cluster, dist(Data_Inci))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE)
fviz_nbclust(Data_Inci, FUN = hcut, method = "silhouette")

#Uji korelasi
corrmatrix <- cor(Data_Inci)
corrplot(corrmatrix, method = 'number')

#Melakukan PCA mengatasi Multikolinearitas 
#PCA
PCA<-prcomp(Data_Inci)
PCA
Variansi<-PCA$sdev^2
Variansi
summary(PCA)
PCA_scores<-PCA$x[,1:2]
PCA_scores

#membuat matriks jarak (D)
Dist <-dist(PCA_scores,method = "euclidean")
Dist
#Analisis Cluster 
library(cluster)
kluster_hirarki<-hclust(Dist,method = "centroid")
str(as.dendrogram(kluster_hirarki)) #proses average
dendrogram2 <-plot(kluster_hirarki) #Menampilkan dendrogram
#menentukan banyaknya kelompok
group2 <-cutree(kluster_hirarki,2)
kelompok2<-cbind(group2)
kelompok2
plot_2 <- plot(kluster_hirarki,hang=-1,col='black',
              main='Cluster Dendrogram Centroid Linkage',
              sub='',xlab='Indeks Provinsi',ylab='Indeks Jarak')
rect.hclust(kluster_hirarki, k=2, border= 2:5)
