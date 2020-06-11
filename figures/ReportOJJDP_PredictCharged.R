#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: 2016-06-21
# Filename: ReportOJJDP_PredictCharged.R
# Purpose: For the mixed methods paper, try to see what factors predict being charged.
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Instructions for setup:
# Run first 150 lines (or so) of ReportOJJDP.R
# Run first 40 lines (or so) of ReportOJJDP_LegalInvolvement.R
# Analyses data in arthurData.
# Outcome variable is YSI03 (Charged=1, No=0)
# Predictors
#	age of the youth at SBP (YSI01)
#	age of youth at Referral (computed, youthAgeAtReferral)
#	age of the victim
#	age difference between youth and victim
#	gender of youth (FRY01: 1 is female, 0 is male)
#	gender of victim
#	interaction between genders (i.e. different if male-male vs male female?)
#	race (computed, Race)
#	total initial YSBP score
#	particular items from initial YSBP measure
#	differences by site (SITE_ID)
#	Select item from the JSOAP measure


#------------------------------------------------------------------------------
# 
# Compute youth age at referral
arthurData$youthAgeAtReferral <- c((as.Date(as.POSIXlt(arthurData$FR_date, origin="1582-10-15")) - as.Date(as.POSIXlt(arthurData$FRY02, origin="1582-10-15")))/365.25)
arthurData$youthAgeAtReferral[arthurData$youthAgeAtReferral < 1] <- NA
arthurData$youthAgeAtReferral[arthurData$youthAgeAtReferral > 100] <- NA

# Clean up missing value in age at SBP
arthurData$YSI01[arthurData$YSI01 %in% 99] <- NA

cor(arthurData$YSI01, arthurData$youthAgeAtReferral, use='pair')
# [1] 0.8180646
# Don't use both in a model at the same time!

# Race processing
# Race
youthRaceNames <- c("FRY03a_White", "FRY03b_Black", "FRY03c_Asian", "FRY03d_Indian", "FRY03e_Hispanic", "FRY03f_Other")
sel <- arthurData[,youthRaceNames] == "DK"
sel[is.na(sel)] <- FALSE
arthurData[,youthRaceNames][sel] <- NA
arthurData[,youthRaceNames] <- apply(sapply(arthurData[,youthRaceNames], as.character), 2, as.numeric)
arthurData[,youthRaceNames][is.na(arthurData[,youthRaceNames])] <- FALSE

head(arthurData[,youthRaceNames])

youthRaceData <- as.data.frame(lapply(as.data.frame(arthurData[,youthRaceNames]==1 & rowSums(arthurData[,youthRaceNames], na.rm=TRUE) < 2), factor, levels=c(FALSE, TRUE), labels=c("No", "Yes")))

head(youthRaceData)
youthRaceData$FRY03f_Other[(youthRaceData$FRY03c_Asian %in% "Yes") | (youthRaceData$FRY03d_Indian %in% "Yes")] <- "Yes"
youthRaceData$FRY03f_Other[(youthRaceData$FRY03a_White %in% "No") & (youthRaceData$FRY03b_Black %in% "No") & (youthRaceData$FRY03c_Asian %in% "No") & (youthRaceData$FRY03d_Indian %in% "No") & (youthRaceData$FRY03e_Hispanic %in% "No")] <- "Yes"

table(youthRaceData[,c('FRY03a_White', 'FRY03b_Black', 'FRY03e_Hispanic', 'FRY03f_Other')])

# Define New Race variable
arthurData$Race <- factor(apply(youthRaceData[, c('FRY03a_White', 'FRY03b_Black', 'FRY03e_Hispanic', 'FRY03f_Other')], 1, function(x){c("White", "Black", "Hispanic", "Other")[which(x %in% "Yes")]}), levels=c("White", "Black", "Hispanic", "Other"))
summary(arthurData$Race)


#------------------------------------------------------------------------------
# Read in and merge YSBP item and sum score data

ds_ysbp <- read.csv('dataYSBPItems.csv')

sel <- 1:34
head(ds_ysbp[,sel])
ds_ysbp <- ds_ysbp[,sel]

ysbpItems <- names(ds_ysbp)[c(-1, -2, -3, -34)]


# Process scores and partially missing data differently
for(i in 1:nrow(ds_ysbp)){
	if(!is.na(ds_ysbp$IntakeScore[i])){
		tem <- ds_ysbp[i, ysbpItems]
		tem[is.na(tem)] <- 0
		ds_ysbp[i, ysbpItems] <- tem
	}
}

ysbpItems27 <- ysbpItems[-27]


dim(arthurData)
arthurData <- merge(arthurData, ds_ysbp, by=c('SITE_ID', 'YouthID'), all.x=TRUE, all.y=FALSE)
dim(arthurData)

#------------------------------------------------------------------------------

summary(glm(YSI03 ~ SITE_ID, data=arthurData, family=binomial))	# Site differences
summary(glm(YSI03 ~ Race, data=arthurData, family=binomial))	# Race differences
summary(glm(YSI03 ~ Race, data=arthurData[arthurData$SITE_ID %in% "NE", ], family=binomial))
summary(glm(YSI03 ~ YSI01, data=arthurData, family=binomial))	# Age of SBP differences
summary(glm(YSI03 ~ youthAgeAtReferral, data=arthurData, family=binomial)) # Age of referral differences
summary(glm(YSI03 ~ FRY01, data=arthurData, family=binomial))	# Gender of youth differences
summary(glm(YSI03 ~ IntakeScore, data=arthurData, family=binomial))	# total initial YSBP score differences
summary(glm(formula(paste0('YSI03 ~ ', paste(ysbpItems27, collapse=' + '))), data=arthurData, family=binomial))	#
# Perhaps multicolinearity is messing things up
yres <- list()
for(i in ysbpItems){
	yres[[i]] <- glm(formula(paste0('YSI03 ~ ', i)), data=arthurData, family=binomial)
}
lapply(yres, summary)
summary(glm(YSI03 ~ SITE_ID + Race + YSI01 + FRY01 + IntakeScore, data=arthurData, family=binomial)) # Additive effects
summary(glm(YSI03 ~ SITE_ID * YSI01 * FRY01, data=arthurData, family=binomial)) # multiplicative effects

formula(paste0('YSI03 ~ ', paste(ysbpItems, collapse=' + ')))

#------------------------------------------------------------------------------




