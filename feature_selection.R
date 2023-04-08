set.seed(12345)
library(e1071)
source('msvmRFE.R')
BR3_data <- read.csv('BR3_data.csv')
BR3_meta <- read.csv('BR3_meta.csv')
BR3 <- BR3_data[-c(1)] 
label <- BR3_meta[ , ncol(BR3_meta), drop = FALSE]
ChemoResponse <- ifelse(label$ChemoResponse == "Sensitive",1,0)
BR3 <- cbind(ChemoResponse, BR3)

# Take a look at the expected input structure
dim(BR3)
BR3[1:5,1:5]

# Basic usage
svmRFE(BR3, k=10, halve.above=500)

# Set up cross validation
nfold = 10
nrows = nrow(BR3)
folds = rep(1:nfold, len=nrows)[sample(nrows)]
folds
folds = lapply(1:nfold, function(x) which(folds == x))
folds

# Perform feature ranking on all training sets
results = lapply(folds, svmRFE.wrap, BR3, k=10, halve.above=100)
length(results)
results

# Obtain top features across ALL folds
top.features = WriteFeatures(results, BR3, save=F)
head(top.features)

# Estimate generalization error using a varying number of top features
featsweep = lapply(1:5, FeatSweep.wrap, results, BR3)
featsweep

# Make plot
no.info = min(prop.table(table(BR3[,1])))
errors = sapply(featsweep, function(x) ifelse(is.null(x), NA, x$error))

dev.new(width=4, height=4, bg='white')
PlotErrors(errors, no.info=no.info)
dev.off()