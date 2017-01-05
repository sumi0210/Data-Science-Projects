#########################
#         HW 2          # 
#########################


# SDGB 7844: Statistical Methods and Computation I
# Sumi Choudhury

graphics.off()

# read the data file
rawData <- readLines('ACS_13_5YR_DP03_with_ann.csv')
rawData <- rawData[-2]

# clean the data file, convert missing values to NA's, select the variable and ID columns
rawData <- read.csv(textConnection(rawData), header=TRUE, na.strings=c('**', '-', '(X)', ''))
econData <- rawData[, c('GEO.id', 'GEO.id2', 'GEO.display.label', 'HC03_VC07')]

# read the data file
rawData <- readLines('ACS_13_5YR_DP04_with_ann.csv')
rawData <- rawData[-2]

# clean the data file, convert missing values to NA's, select the variables and ID column
rawData <- read.csv(textConnection(rawData), header=TRUE, na.strings=c('**', '-', '(X)', ''))
houseData <- rawData[, c('GEO.id', 'HC03_VC65', 'HC03_VC84', 'HC03_VC112')]

# merge the tables
geoData <- merge(econData, houseData, by.x='GEO.id', by.y='GEO.id', all=TRUE)
geoData$GEO.id2 = as.factor(geoData$GEO.id2)

# assign column names to the variables; transform the variable in column 7
colnames(geoData)[4] <- 'Unemployment'
colnames(geoData)[5] <- 'Rent'
colnames(geoData)[6] <- 'No Vehicle'
geoData[,7] <- 100 - geoData[, 7]
colnames(geoData)[7] <- 'High Occupancy'

#compute the summary statistics for each variable
summary_stat <- sapply(geoData[,4:7], mean, na.rm=TRUE)
summary_stat

summary_stat <- rbind(summary_stat, sapply(geoData[,4:7], median, na.rm=TRUE))
summary_stat

summary_stat <- rbind(summary_stat, sapply(geoData[,4:7], sd, na.rm=TRUE))
summary_stat

summary_stat <- rbind(summary_stat, sapply(geoData[,4:7], max, na.rm=TRUE))
summary_stat

summary_stat <- rbind(summary_stat, sapply(geoData[,4:7], min, na.rm=TRUE))
summary_stat

# create a readable summary stat table
rownames(summary_stat) = c('Mean', 'Median', 'Standard Deviation', 'Max.', 'Min.')
summary_stat <- t(summary_stat)
cat("Summary Statistics for the ACS Data\n")
print(summary_stat)

# plot histograms for each variable and create labels
dev.new()
subp <- par(mfrow=c(2,2),ps=20)
for ( i in colnames(geoData[,4:7]) ){ 
  plot(hist(geoData[,i], plot=F), main=i, xlab='%', ylab='# Obs')
}

# find the missing values for each variable and sum by column, compute rows
# that are incomplete and compute percentage of incomplete census tracts
cat("\n\nNumber of missing values in Data\n")
print(apply(is.na(geoData[,4:7]), 2, sum))
cat(paste('Census Tracts with incomplete data:', sum(!complete.cases(geoData)), '\n'))
P <- nrow(geoData[!complete.cases(geoData),])/nrow(geoData)
P

# construct a scatterplot matrix of the original variables
dev.new()
pairs(geoData[,4:7], main = "Scatterplot matrix of original variables")

# transform the variables to reduce skewness and add to data frame
geoData['t_Unemployment'] <- log1p(geoData['Unemployment'])
geoData['t_Rent'] <- log1p(geoData['Rent'])
geoData['t_No Vehicle'] <- sqrt(geoData['No Vehicle'])
geoData['t_High Occupancy'] <- log1p(geoData['High Occupancy'])

# construct a scatterplot matrix of the transformed variables
dev.new()
pairs(geoData[,8:11], main = "Scatterplot matrix of transformed variables")

# construct a correlation matrix of the transformed variables
cor_mat_t_var <- cor(geoData[,8:11], use = 'pairwise.complete.obs')
cat('\n\nCorrelation matrix of transformed variables\n')
print(cor_mat_t_var)

# compute mean and standard deviations of transformed variables
# exclude NA's
t_means <- sapply(geoData[,8:11], mean, na.rm=TRUE)
t_sd <- sapply(geoData[,8:11], sd, na.rm=TRUE)

# set the function to compute the Townsend index for each region
# by summing the z-scores of all variables for each region
getIndex <- function (x) {
  return(sum((x - t_means)/t_sd))
}

# compute the Townsend Index by summing the z-scores of all variables for
# each region, adding the index to the data frame, and sorting by index
# in decreasing order; add rank to data frame, excluding NA's
geoData[,'Townsend Index'] <- apply(geoData[,8:11], 1, getIndex)
geoData[,'Townsend Index']
geoData <- geoData[order(geoData[,'Townsend Index'], decreasing=TRUE),]
geoData[!is.na(geoData[,'Townsend Index']),'Rank'] <- c(1:sum(!is.na(geoData[,'Townsend Index'])))

# find the most deprived and least deprived census tracts, determined by ranking
# 1 = the most deprived
cat('\n\nMost deprived census tract:\n')
print(geoData[which.min(geoData[,13]),c(2,3,12,13)])
cat('\nLeast deprived census tract:\n')
print(geoData[which.max(geoData[,13]),c(2,3,12,13)])
cat('\n\n')

# load required packages
library(RColorBrewer)
library(rgdal) 

# read in shape files - readOGR() is from the rgdal package
NYmap <- readOGR(dsn='tl_2013_36_tract.shp', layer='tl_2013_36_tract')

# extract New York County census tracts; match by ID
NYmap <- NYmap[is.element(NYmap@data[,'GEOID'], geoData[,'GEO.id2']),]
geoData <- geoData[order(geoData[,'GEO.id2']),]
NYmap <- NYmap[order(NYmap@data[,'GEOID']),]

# convert the numeric vector into factor vector, break up into 8 intervals
print(range(geoData[,'Rank'], na.rm=TRUE))
breaks.factor <- cut(geoData[,'Rank'], breaks=as.integer(seq(from=1, to=279, length=9)))

# color palette
color.palette <- brewer.pal(n=length(levels(breaks.factor)),"Spectral")
color.coding <- color.palette[as.numeric(breaks.factor)]

# plot map; include legend
dev.new()
plot(NYmap, col=color.coding, main='Color Coding of NY county map based on Townsend Rank')
legend("topleft", legend=levels(breaks.factor),
       fill=color.palette, ncol=2)

# extract the census tract for Lowenstein and plot coordinates
print(geoData[geoData[,'GEO.id2']=='36061014500',])
xy <- coordinates(NYmap[NYmap@data[,'GEOID']=='36061014500',])

# mark on the map and add label
arrows(x0=xy[1], y0=xy[2], x1=xy[1]-0.05, y1=xy[2], length=0.1, lwd=2)
text(xy[1]-0.08, xy[2], labels="Lowenstein\n(Rank = 260)")


