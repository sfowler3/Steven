#--------------------------------------
# Table 1: Legal Involvement
data <- merge(theCombinedTables$Referral, theCombinedTables$Intake, all=TRUE, by=c("SITE_ID", "YouthID"))

if(forOjjdpPaper){
	data <- data[data$IGtx11 %in% 1,]
}

data$YSI06l_sexreg <- factor(data$YSI06l_sexreg, levels=c(0, 1), labels=c("no", "yes"))

data[data$YSI03 %in% "0;3", c('SITE_ID', 'YouthID', 'YSI03')]
data[data$YSI03 %in% "0;3", c('YSI03')] <- 3


data$YSI03[data$YSI03 == ""] <- NA
data$YSI03[data$YSI03 == "DK"] <- NA
data$YSI03[data$YSI03 == "N"] <- 0
data$YSI06g_sbp[data$YSI06g_sbp == "DK"] <- NA
data$YSI06h_parent_tx[data$YSI06h_parent_tx == "DK"] <- NA
data$YSI06h_parent_tx[data$YSI06h_parent_tx == "00"] <- "0"
data$IGtx11[data$IGtx11 == "DK"] <- NA
data$IGtx11[data$IGtx11 == ""] <- NA
data$IGtx11 <- factor(data$IGtx11)
data$YSI06g_sbp <- factor(data$YSI06g_sbp)
data$YSI06h_parent_tx <- factor(data$YSI06h_parent_tx)
data$YSI03 <- factor(data$YSI03)
summary(data[,c("SITE_ID", "YSI03", "YSI06g_sbp", "YSI06h_parent_tx", "IGtx11")])



#arthurData <- data # no intake required
arthurData <- data[data$IntakeDone,] # requires intake is done!!!
#arthurData <- data[data$IntakeDone | (data$NoIntake==0 & !is.na(data$NoIntake)),]

table1 <- rbind(
	table(arthurData$YSI03, arthurData$SITE_ID)["1",],   #Charged?
	table(arthurData$YSI04, arthurData$SITE_ID)["1",],  #Adjudicated?
	if(any(arthurData$YSI04 %in% 2)) {table(arthurData$YSI04, arthurData$SITE_ID)["2",]} else {numeric(length(theSites))} + if(any(arthurData$YSI04 %in% "3")) {table(arthurData$YSI04, arthurData$SITE_ID)["3",]} else {numeric(length(theSites))}, # Deferred/Pending
	table(arthurData$YSI06l_sexreg, arthurData$SITE_ID)["yes",],  #sex offender registration?
	if(any(arthurData$YSI06m_snotify %in% "1")) {table(arthurData$YSI06m_snotify, arthurData$SITE_ID)["1",]} else {numeric(length(theSites))},  #Notify Schools
	table(arthurData$YSI06g_sbp, arthurData$SITE_ID)["1",],    #Court required tx
	table(arthurData$YSI06h_parent_tx, arthurData$SITE_ID)["1",],    #Court required parent tx
	if(any(arthurData$YSI06k_minor %in% "1")) {table(arthurData$YSI06k_minor, arthurData$SITE_ID)["1",]} else {numeric(length(theSites))}    #No contact with minors?
)


table1a <- table(arthurData$YSI10, arthurData$SITE_ID)    # youth in child welfare
# 1 = Yes
# 2 = In Past
# 3 = No
# 0 = Missing?

table1 <- rbind(table1, table1a["1",] + table1a["2",])
rownames(table1) <- c("Charged?", "Adjudicated?", "Deferred/Pending?", "Sex Offender Registration?", "Notify schools?", "Court required Tx", "Court required parent tx", "No contact with minors", "Child Welfare Involved")

table1
mkpercent(table1, intakeN)

(table1full <- mkptable(table1, c(intakeN, sum(intakeN))))
write.csv(file='tableLegalInvolvement.csv', table1full)


rowSums(table1)
mkpercent(rowSums(table1), sum(intakeN))


#percent with ANY legal involvement as defined by the union of
# youth charged, youth adjudicated, and youth required treatment

tabLegal <- table(	(arthurData$YSI03 %in% "1") | # charged?
					(arthurData$YSI04 %in% "1") | # adjudicated?
					(arthurData$YSI06g_sbp %in% "1"), # court required treatment 
				arthurData$SITE_ID)

mkptable(tabLegal, c(intakeN, sum(intakeN)))


# Question: does youth charged (YSI03), partent required tx (YSI06h_parent_tx), or court order (YSI06g_sbp) make enrollment (IGtx11; being accpepted to group)  more likely?
# Does it differ by site?

arthurData[is.na(arthurData$IGtx11), "IGtx11"] <- 0
arthurData[arthurData$IGtx11 %in% 'RF', "IGtx11"] <- NA
arthurData[is.na(arthurData$YSI06g_sbp), "YSI06g_sbp"] <- 0
arthurData[is.na(arthurData$YSI06h_parent_tx), "YSI06h_parent_tx"] <- 0
arthurData[is.na(arthurData$YSI03), "YSI03"] <- 0

# Exclude new sites OR keep all of them?
arthurData2 <- arthurData[arthurData$SITE_ID %in% theSites[1:3], ]
arthurData2 <- arthurData

en1.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID, data=arthurData, family=binomial)
en2y.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI06g_sbp, data=arthurData, family=binomial)
en2p.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI06h_parent_tx, data=arthurData, family=binomial)
en2c.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI03, data=arthurData, family=binomial)
en3.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI06g_sbp + YSI06h_parent_tx + YSI03, data=arthurData, family=binomial)
en3b.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI03 + YSI06h_parent_tx + YSI06g_sbp, data=arthurData, family=binomial)
en3c.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID + YSI06g_sbp + YSI03 + YSI06h_parent_tx, data=arthurData, family=binomial)
en4.glm <- glm(as.numeric(as.character(IGtx11)) ~ SITE_ID * ( YSI06g_sbp + YSI06h_parent_tx + YSI03 ), data=arthurData, family=binomial)

en4s.glm <- step(en4.glm)

#TODO - Locate file prettyResults.R 
# Odds ratios and reporting
gcat(en4s.glm, 'YSI06g_sbp1')# court ordered
gcat(en2c.glm, 'YSI031') # youth charged
gcat(en2p.glm, 'YSI06h_parent_tx1') # parent required




table(enroll=arthurData2$IGtx11, site=arthurData2$SITE_ID, courtReq=arthurData2$YSI06g_sbp)

#------------------------------------------------------------------------------
# Predict enrollment from lots of variables

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


arthurData$IGtx11 <- factor(arthurData$IGtx11)


# Age at referral
# Compute youth age at referral
arthurData$youthAgeAtReferral <- c((as.Date(as.POSIXlt(arthurData$FR_date, origin="1582-10-15")) - as.Date(as.POSIXlt(arthurData$FRY02, origin="1582-10-15")))/365.25)
arthurData$youthAgeAtReferral[arthurData$youthAgeAtReferral < 1] <- NA
arthurData$youthAgeAtReferral[arthurData$youthAgeAtReferral > 100] <- NA

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

summary(glm(IGtx11 ~ SITE_ID, data=arthurData, family=binomial))	# Site differences
summary(glm(IGtx11 ~ Race, data=arthurData, family=binomial))	# Race differences
summary(glm(IGtx11 ~ YSI01, data=arthurData, family=binomial))	# Age of SBP differences
summary(glm(IGtx11 ~ youthAgeAtReferral, data=arthurData, family=binomial)) # Age of referral differences
summary(glm(IGtx11 ~ FRY01, data=arthurData, family=binomial))	# Gender of youth differences
summary(glm(IGtx11 ~ IntakeScore, data=arthurData, family=binomial))	# total initial YSBP score differences
summary(glm(formula(paste0('IGtx11 ~ ', paste(ysbpItems, collapse=' + '))), data=arthurData, family=binomial))	#
# Perhaps multicolinearity is messing things up
yres <- list()
for(i in ysbpItems){
	yres[[i]] <- glm(formula(paste0('IGtx11 ~ ', i)), data=arthurData, family=binomial)
}
lapply(yres, summary)
summary(glm(IGtx11 ~ SITE_ID + Race + YSI01 + FRY01 + IntakeScore, data=arthurData, family=binomial)) # Additive effects
summary(glm(IGtx11 ~ SITE_ID * YSI01 * FRY01, data=arthurData, family=binomial)) # multiplicative effects
summary(glm(IGtx11 ~ (SITE_ID + Race + YSI01 + FRY01)^2, data=arthurData, family=binomial)) # qaudratic effects

formula(paste0('YSI03 ~ ', paste(ysbpItems, collapse=' + ')))

st <- step(glm(IGtx11 ~ (SITE_ID + Race + YSI01 + FRY01)^2, data=arthurData, family=binomial))


# Specific gender*site interaction
summary(glm(IGtx11 ~ SITE_ID*FRY01, data=arthurData, family=binomial))	# Site differences

