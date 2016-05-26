
####################
#### TITLE:     Two functions to calculate ROC and AUC.
#### Contents:
####
#### Source Files: https://github.com/NeuroStat/R-scripts
#### First Modified: 26/05/2016
#### Author: Han Bossier
#### Notes:
#################



##
###############
### ROC
###############
##

# You will need a ground truth (GTmap) and an observed p-map (PValmap)
# Returns data frame

# Caluclate ROC values based on P-map
ROC_PMAP <- function(GTmap, PValmap,number.thresholds=100){
	# The thresholds, the GT in one array and the p-map in one array
	thresholds <- seq(0,1,length.out=number.thresholds)
	GT <- array(GTmap, dim = prod(dim(GTmap)))
	pmap <- array(PValmap, dim = prod(dim(PValmap)))

	# For loop over the thresholds to calculate false and true positives
	TFP <- TTP <- c()
	for(t in 1:length(thresholds)){
		# First (re)-threshold the maps
		Tpmap <- array(0, dim = prod(dim(PVal)))
		idTpmap <- pmap <= thresholds[t]
		Tpmap[idTpmap] <- 1

		# False positive rate: if GT = 0, and p-map = 1, then difference equals -1
		FP <- round(sum((GT - Tpmap) == -1, na.rm = TRUE) / sum(GT == 0, na.rm=TRUE), 3)
		# True positive rate: if both GT and p-map have 1, then sum = 2
		TP <- round(sum((GT + Tpmap) == 2, na.rm = TRUE) / sum(GT, na.rm = TRUE), 3)

		# Save in vector (thresholdFP, thresholdTP)
		TFP <- c(TFP, FP)
		TTP <- c(TTP, TP)
	}
	# Save in data.frame
	ROC <- data.frame('value' = c(TFP,TTP), 'type' = rep(c('FP', 'TP'), each = number.thresholds), 'threshold' = round(rep(thresholds, 2),2))
	# Return it
	return(ROC)
}


##
###############
### AUC
###############
##


# AUC = Area under the curve
# Calculated using geometry

# Input are two vectors: false positives (x-axis of the ROC curve) and true positives (y-axis of the ROC cure).


# Function to caluclate area under curve
comp_auc <- function(FP,TP){
	# Calculate area under curve.
		# x-axis = FPR
		# y-axis = TPR
	auc <- 0
	for (i in 2:length(FP)) {
		auc <- auc + 0.5 * (FP[i] - FP[i-1]) * (TP[i] + TP[i-1])
	}
	return(data.frame(AUC = auc))
}
