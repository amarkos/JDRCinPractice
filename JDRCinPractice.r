########## Joint Dimension Reduction and Clustering in Practice
##### MDAG Workshop - SASA 2021
##############  Nov 29 2021 
########## Author: Angelos Markos, amarkos@eled.duth.gr                      

# Before the workshop, please install necessary packages (lines 7 to 14)
list.of.packages <- c('devtools','leaflet', 'ggfortify', 'rio', 'dplyr', 'geojson', 'geojsonio')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
install.packages("clustrd")
if (!("UBbipl" %in% installed.packages()[,"Package"]))
{
  install.packages("https://www.wiley.com/legacy/wileychi/gower/supp/UBbipl_3.0.6.tar.gz",repos = NULL, type = "source") 
}
#################################

# Load clustrd package and mybond data set
library("clustrd")
# Load mybond data set 
data(mybond)
# Show first lines of the data frame
head(mybond)
# Show structure of the data frame
str(mybond)
# Show data description
help(mybond)

# Calculate the correlation matrix
round(cor(mybond[,1:7]), 2)

############### Tandem Analysis (PCA followed by K-means) 

# Apply Principal Component Analysis on the correlation matrix
outPCA <- prcomp(mybond[,1:7], center = TRUE, scale = TRUE)
# Create the PCA biplot with labeled observations 
library("ggfortify")
autoplot(outPCA, data = mybond[,1:7], 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label = TRUE)

# Alternatively
library(UBbipl)
outPCAbipl <- PCAbipl(mybond[,c(1:7)],scaled.mat = TRUE,colours = "red",pch.samples = 15,pos = "Hor",label=TRUE, rotate.degrees = 35)

# Apply K-means on the PCA scores (2 dimensions) with K = 3 and 100 random starts
outK <- kmeans(outPCA$x[, 1:2], centers = 3, nstart = 100)
outK

# Show the clusters on the PCA bibplot (with different colour)
autoplot(outPCA, data = mybond[,1:7], colour = outK$cluster,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4, label = TRUE)

############### Reduced K-means ############################

# Apply Reduced K-means with 3 clusters and 2 dimensions
outRKM <- cluspca(data = mybond[, 1:7], nclus = 3, ndim = 2, nstart = 100, seed = 1234)
outRKM

# Plot Reduced K-means solution with object labels
# Show parallel coordinate plot to facilitate cluster description
plot(outRKM, cludesc = TRUE, objlabs = TRUE)

# Plot objects and cluster centroids
plot(outRKM, what = c(TRUE, FALSE), objlabs = TRUE)
help(plot.cluspca)

# Cluster quality assessment based on Average Silhouette Width 
bestRKM <- tuneclus(data = mybond[, 1:7], nclusrange = 2:6, ndimrange = 1:5, 
                    method = "RKM", criterion = "asw", seed = 1234)
bestRKM
# Show the cluster membership vector of the best solution
bestRKM$clusobjbest$cluster

############### Cluster Correspondence Analysis #############
# Show first lines of the categorical variables
head(mybond[, 8:10])

# Apply Cluster Correspondence Analysis with 3 clusters and 2 dimensions
outClusCA <- clusmca(mybond[, 8:10], 3, 2, method = "clusCA", seed = 1234)
outClusCA

# Plot ClusCA solution (allow for overlapping labels)
# Show barplots with standardized residuals to facilitate cluster description
plot(outClusCA, cludesc = TRUE, max.overlaps = 20)

############### Mixed Reduced K-means #############
outMixedRKM <- cluspcamix(mybond, 3, 2, seed = 1234)
outMixedRKM 

# Plot Mixed Reduced K-means solution (allow for overlapping labels)
plot(outMixedRKM, cludesc = TRUE, max.overlaps = 20)

###################################################

# Clustering Airbnb listings in Cape Town
# Import the data from URL
library(rio)
airbnb <- import("http://amarkos.gr/datasets/airbnb.Rdata")

# Show data frame structure
str(airbnb)

# Apply Mixed Reduced 3-means
# Exclude longitude and lattitude (1st and 2nd columns)
outMixedRKM3 <- cluspcamix(airbnb[,-c(1:2)], 3, 2, nstart = 3, seed = 1234)
table(outMixedRKM3$cluster)

# Plot the solution with cluster description
plot(outMixedRKM3, cludesc = TRUE, max.overlaps = 80)

## A useful function to describe the clusters is `catdes()` 
## from the `FactoMineR` package.
# install.packages
# library(FactoMineR)
# outCAT <- catdes(cbind(airbnb, as.factor(outMixedRKM3$cluster)),35)
## continuous vars that characterise the first cluster
# outCAT$quanti$`1`
## categorical vars that characterise the first cluster
# outCAT$category$`1`
## etc..

# Neighbourhoods GEOJSON file contains full list of Cape town neighbourhoods with geospatial data that we will use to visualise information on the map.
library(dplyr)
library(geojsonio)
library(leaflet)
nb_geo <- geojson_read('http://data.insideairbnb.com/south-africa/wc/cape-town/2021-10-26/visualisations/neighbourhoods.geojson', what = 'sp')

# We use the leaflet package to display listings from the three groups 
# using lat/long information. This will give us an idea of geographical distribution of the clusters.
c1 <- airbnb %>% 
  filter(outMixedRKM3$cluster == 1)

c2 <- airbnb %>% 
  filter(outMixedRKM3$cluster == 2) 

c3 <- airbnb %>% 
  filter(outMixedRKM3$cluster == 3)

library(leaflet)
leaflet() %>% setView(lng = 18.423300, lat = -33.918861, zoom = 10) %>%
  addTiles() %>%
  addPolygons(data = nb_geo, color = "#444444", weight = 2, opacity = 1) %>%
  addCircleMarkers(  lng = c1$longitude,
                     lat = c1$latitude,
                     radius = 2,
                     stroke = FALSE,
                     color = "blue",
                     fillOpacity = 0.5,
                     group = "c1"
  ) %>%
  addCircleMarkers(  lng = c2$longitude,
                     lat = c2$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "green",
                     fillOpacity = 0.5,
                     group = "c2"
  )%>%
  addCircleMarkers(  lng = c3$longitude,
                     lat = c3$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "red",
                     fillOpacity = 0.5,
                     group = "c3"
  )

# Apply Mixed Reduced 4-means
outMixedRKM4 <- cluspcamix(airbnb[,-c(1:2)], 4, 3, nstart = 3, seed = 1234)
table(outMixedRKM4$cluster)

plot(outMixedRKM4, cludesc = TRUE, max.overlaps = 80)

c1 <- airbnb %>% 
  filter(outMixedRKM4$cluster == 1)

c2 <- airbnb %>% 
  filter(outMixedRKM4$cluster == 2) 

c3 <- airbnb %>% 
  filter(outMixedRKM4$cluster == 3)

c4 <- airbnb %>% 
  filter(outMixedRKM4$cluster == 4)

leaflet() %>% setView(lng = 18.423300, lat = -33.918861, zoom = 10) %>%
  addTiles() %>%
  addPolygons(data = nb_geo, color = "#444444", weight = 2, opacity = 1) %>%
  addCircleMarkers(  lng = c1$longitude,
                     lat = c1$latitude,
                     radius = 2,
                     stroke = FALSE,
                     color = "blue",
                     fillOpacity = 0.5,
                     group = "c1"
  ) %>%
  addCircleMarkers(  lng = c2$longitude,
                     lat = c2$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "purple",
                     fillOpacity = 0.5,
                     group = "c2"
  )%>%
  addCircleMarkers(  lng = c3$longitude,
                     lat = c3$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "green",
                     fillOpacity = 0.5,
                     group = "c3"
  )%>%
  addCircleMarkers(  lng = c4$longitude,
                     lat = c4$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "red",
                     fillOpacity = 0.5,
                     group = "c4"
  )

############### Assessing Cluster Stability 
# (topic not covered in the workshop)

# Assess global cluster stability 
# with 50 bootstrapping samples (this will take some time...)
boot_RKM <- global_bootclus(mybond[, 1:7], nclusrange = 2:6,
                            method = "RKM", nboot = 50, nstart = 100, seed = 1234)

summary(boot_RKM$rand)
# Create boxplots showing ARI distribution per clustering solution
boxplot(boot_RKM$rand, xlab = "Number of clusters", ylab = "adjusted Rand Index")

# Assess local cluster stability of the most stable solution
bootres <- local_bootclus(mybond[, 1:7], nclus = 3, ndim = 2,
                          method = "RKM", nboot = 50, nstart = 100, seed = 1234)
# Create boxplots showing ARI distribution per cluster
boxplot(bootres$Jaccard, xlab = "cluster number", ylab = "Jaccard similarity")