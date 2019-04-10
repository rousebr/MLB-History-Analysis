MLBData <- read.csv(file.choose())
#Changes all -- values in the GB column to 0
MLBData$GB <- gsub("--",0,MLBData$GB)

head(MLBData)
#Check for any missing data/NA
MLBData[!complete.cases(MLBData),]

library("ggfortify")
library("ggplot2")
library(devtools)
library("factoextra")
library("ggbiplot")
library("psych")
library('tidyverse')

###Some general exploration and visualization of data
#Check column names to see which data is useful
colnames(MLBData)

#This command gives a summary table of how many time each team appears in the overall data set
table(MLBData[,"Tm"])

#Basic Scatterplot using specified X and Y variables
qplot(data=MLBData, x=W, y=R)

#Obtaining the mean number of wins for each team within the data set
mwins <- tapply(MLBData$W, MLBData$Tm, mean, na.rm=TRUE)
mwins

#Boxplot displaying overall Runs per game performance of teams in respective leagues over history of MLB
MLBplot <- ggplot(data=MLBData, aes(x=Lg, y=R/G)) + geom_jitter(aes(color=W)) + geom_boxplot(alpha=I(.7), outlier.color=NA)
MLBplot

#Scatterplot for all teams in MLB history with general prediction lines for teams in each league over history of league
tt <- ggplot(data=MLBData, aes(x=R/G, y=W, color=Lg))
tt + geom_point() + geom_smooth(fill=NA)

#The historical offensive output with a prediction line for each league, separately. 
tt + geom_point() + facet_grid(Lg~.) + geom_smooth(fill=NA, scales="free")

#Filtering for just the years 2011-2015
MLBfilt <- (MLBData$Year==2011) | (MLBData$Year==2012) | (MLBData$Year==2013) | (MLBData$Year==2014) | (MLBData$Year==2015)
MLBSet <- MLBData[MLBfilt,]

#Setting the data to be used with the new filter from the previous step
MLBFacet <- ggplot(data=MLBSet, aes(x=R/G, y=W, color=Lg))

#Separating the data by year only
MLBFacet + geom_point(size=I(3)) + facet_grid(Year~., scales="free")

#Separating the data by League only
MLBFacet + geom_point(size=I(3)) + facet_grid(.~Lg, scales="free")

#Now by both Year and League
MLBFacet + geom_point(size=I(3)) + facet_grid(Year~Lg, scales="free")

##Actual PC Analysis setup and execution
#Subsetting out the useful columns
MLBData.cut <- MLBData[c(7:11,15,16,18:21,3,5)]
head(MLBData.cut)

#Isolating further for strictly numbers (for PCA section)
dr <- MLBData.cut[c(1:11)]
dr

#PCA function selecting the variables I find useful for analyze for PC Analysis
MLB.pca <- prcomp(dr, center=TRUE, scale.=TRUE)

#Loadings for each variable in Principal Components and proportion of variance explained
MLB.pca
summary(MLB.pca)

#A chart to look for ideal number of Principal Components Where biggest dropoff ends is ideal
fviz_eig(MLB.pca)
#Chart to see how each variable pulls a data point in PC Analysis.
fviz_pca_var(MLB.pca, 
             col.va="contrib",
             gradient.cols=c(1:11),
             repel=TRUE)

#Plot PCA Function for entirety of MLB History (a mite messy)
autoplot(MLB.pca, data=MLBData.cut, colour="Lg", loadings=TRUE, loadings.label=TRUE, loadings.colour='blue', 
         loadings.label.size=5, loadings.label.colour='red') + ggtitle("PC Analysis of MLB History")

#Perform process again for a single year to make this clearer
MLB2001 <- MLBData[MLBData.cut$Year==2001,]
MLB2001 <- MLB2001[c(7:11,15,16,18:21,3,4,5)]
MLB2001
rownames(MLB2001) <- MLB2001$Tm
dr2 <- MLB2001[c(1:11)]
dr2

MLB2001.pca <- prcomp(dr2, center=TRUE, scale.=TRUE)
MLB2001.pca
summary(MLB2001.pca)
fviz_eig(MLB2001.pca)

#Loadings chart for variables
fviz_pca_var(MLB2001.pca,
             col.va="contrib",
             gradient.cols=c(1:11),
             repel=TRUE)

#PC predictions for each team in 2001
MLB2001.predict.pca <- predict(MLB2001.pca)
MLB2001.predict.pca[,1:2]

#PCA Plot for single year, nice and clean with loadings
groups <- as.factor(MLB2001$Lg)
fviz_pca_biplot(MLB2001.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969") # Individual color

#Flashier version to show grouping ellipses
fviz_pca_ind(MLB2001.pca,
             col.ind = groups, # color by groups
             palette = c(1:6),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

#Factor Analysis
MLBData.num <- MLBData[,c(7:11,15,16,18:21)]
fit.MLB1 <- principal(MLBData.num, nfactors=4)
fit.MLB1

#Rotate to get better values for factors
fit.MLB <- factanal(MLBData.num, 4, rotation="varimax")
print(fit.MLB, digits=2, cutoff=.3, sort=TRUE)
load.ings <- fit.MLB$loadings[,1:2]