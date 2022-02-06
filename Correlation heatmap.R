# load package
library(pheatmap)
library(ggplot2)
library(reshape2)
mydata <- read.delim("distance_matrix.txt", sep = "\t", header = TRUE)
mydata <- as.matrix(mydata)
mode(mydata)<-"numeric"
print(mydata)
mydata[is.na(mydata)] = 0
hclust.com <- function(mydata) hclust(mydata, method="complete")
h.ori <- heatmap(mydata,hclustfun=hclust.com,dendrogram = "row",main = "Fig1")

cormat <- round(cor(mydata),2)
View(cormat)
cormat[is.na(cormat)] = 0
melted_cormat <- melt(cormat, na.rm = TRUE)
ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  theme_classic()

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  coord_fixed()