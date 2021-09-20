library(readxl)
library(factoextra)
Data_Inci <- read_excel("D:/FILE KULIAH/SEMESTER 8/Inci/Data Inci.xlsx",
                        col_types = c("skip", "numeric", "numeric",
                                      "numeric", "numeric","numeric", "numeric",
                                      "numeric", "numeric","numeric", "numeric"))
summary(Data_Inci)
#df<- scale(Data_Inci)
silhouette_score <- function(k){
  km <- pam(Data_Inci, k)
  ss <- silhouette(km$cluster, dist(Data_Inci))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE)
fviz_nbclust(Data_Inci, pam, method = "silhouette")
#Uji Multiko
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

pam.res <- pam(PCA_scores, 2)
print(pam.res)
pam.res$medoids
head(pam.res$clustering)
fviz_cluster (pam.res, 
              palette = c("#00AFBB", "#FC4E07", "#2E9FDF", "#006600"), # color palette
              ellipse.type = "t",  #Concentration ellipse 
              repel = TRUE, # Avoid label overplotting (slow) 
              ggtheme = theme_classic() 
)
