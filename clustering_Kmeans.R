setwd("F:/blogs/otw upload/clustering/")

# Load libraries
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)

# Read the stats
wines <- read.csv("Wine.csv")

# View first 6 rows
View(head(wines))

# Remove the Type column
wines <- wines[,-14]

##Analysis
# Structure of Wine data set
str(wines)
# Summary of Wine data set
summary(wines)

# Histogram for each Attribute
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue2", colour="black") +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency") 

# Correlation matrix 
corrplot(cor(wines), type="upper", method="ellipse", tl.cex=0.9)

# Relationship between Phenols and Flavanoids
ggplot(wines, aes(x=Total_Phenols, y=Flavanoids)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

##Preparation
# Normalization
winesNorm <- as.data.frame(scale(wines))

# Original data
p1 <- ggplot(wines, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Original data")

# Normalized data 
p2 <- ggplot(winesNorm, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Normalized data")

# Subplot
grid.arrange(p1, p2, ncol=2)

##kmeans execution
# Execution of k-means with k=2
set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=2)

# Cluster to which each point is allocated
wines_k2$cluster

# Cluster centers
wines_k2$centers

# Cluster size
wines_k2$size

# Between-cluster sum of squares
wines_k2$betweenss

# Within-cluster sum of squares
wines_k2$withinss

# Total within-cluster sum of squares 
wines_k2$tot.withinss

# Total sum of squares
wines_k2$totss

##HOw Many Cluster
bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(winesNorm, centers=i)$betweenss
  wss[i] <- kmeans(winesNorm, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Subplot
grid.arrange(p3, p4, ncol=2)

#Result
# Execution of k-means with k=3
set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=3)

# Mean values of each cluster
aggregate(wines, by=list(wines_k2$cluster), mean)

# Clustering 
ggpairs(cbind(wines, Cluster=as.factor(wines_k2$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")
