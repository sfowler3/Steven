#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: 2014-11-10
# Filename: ReportOJJDP.R
# Purpose: Make tables and analyze data for OJJDP report
#------------------------------------------------------------------------------

# TODO break into smaller chunks, each with their own file.

#------------------------------------------------------------------------------
setwd('S:/CCAN/CCANEduc/OJJDP db/OJJDP Databases/SBP Analysis Files/R/Report')


#------------------------------------------------------------------------------

require(qdap) # used for text processing
require(effsize) # used for confidence intervals on effect sizes

source('ReadAccessOJJDP.R')


#------------------------------------------------------------------------------
# Are these the results needed for the OJJDP Journal Paper?
forOjjdpPaper <- FALSE #FALSE#TRUE

# Set above to FALSE for all the semi-annual reports.


#------------------------------------------------------------------------------

mkptable <- function(x, totals){
	x <- cbind(x, Total=rowSums(x))
	xp <- mkpercent(x, totals)
	ret <- matrix(paste(x, " (", xp*100, "%)", sep=""), nrow=nrow(x), ncol=ncol(x))
	rownames(ret) <- rownames(x)
	colnames(ret) <- colnames(x)
	return(ret)
}

.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

.replace_number <- function(x){
	.simpleCap(gsub(" ", "-", qdap::replace_number(x)))
}


#------------------------------------------------------------------------------
# some data cleaning


# Done previously now
#theCombinedTables$Intake$`SITE ID` <- factor(theCombinedTables$Intake$`SITE ID`, levels=c("CA", "Omaha", "NJ"), labels=c("CA", "NE", "NJ"))
#theCombinedTables$IntakeV$`SITE ID` <- factor(theCombinedTables$IntakeV$`SITE ID`, levels=c("CA", "Omaha", "NJ"), labels=c("CA", "NE", "NJ"))

# Check
levels(theCombinedTables$Intake$`SITE ID`)
levels(theCombinedTables$Referral$`SITE ID`)

# NYSouthernTier has referrals but no intakes.

nrow(theCombinedTables$Referral[theCombinedTables$Referral$`SITE ID` %in% "PA", 1:5])
nrow(theCombinedTables$Intake[theCombinedTables$Intake$`SITE ID` %in% "PA", 1:5])



#------------------------------------------------------------------------------




#------------------------------------------------------------------------------

#--------------------------------------
# Referral numbers
theCombinedTables$Referral$ReferralDone <- !is.na(theCombinedTables$Referral$FR_date)
referred <- table(theCombinedTables$Referral$ReferralDone, theCombinedTables$Referral$`SITE ID`, useNA="ifany")
referred

#--------------------------------------
# Intake by site and total
summary(theCombinedTables$Intake$NoIntake) # check what values exist
theCombinedTables$Intake$NoIntake[theCombinedTables$Intake$NoIntake %in% c('RF', 'DK')] <- NA
theCombinedTables$Intake$NoIntake <- as.numeric(as.character(theCombinedTables$Intake$NoIntake))
theCombinedTables$Intake$NoIntake <- 1 - theCombinedTables$Intake$NoIntake
theCombinedTables$Intake$IntakeDone <- !theCombinedTables$Intake$NoIntake
intaked <- table(theCombinedTables$Intake$IntakeDone, theCombinedTables$Intake$`SITE ID`, useNA="ifany")
intaked


#------------------
hist(apply(theCombinedTables$Intake, 1, prop.na))
# not great separation of complete vs incomplete data

table(theCombinedTables$Intake$NoIntake, theCombinedTables$Intake$`SITE ID`, useNA="ifany")

theCombinedTables$Intake[theCombinedTables$Intake$NoIntake==1 && theCombinedTables$Intake$IntakeDone != FALSE, ]
IntakeDateCheckBoxDisagree <- theCombinedTables$Intake[!xor(theCombinedTables$Intake$NoIntake, theCombinedTables$Intake$IntakeDone),c('SITE ID', 'YouthID', 'NoIntake', 'Intake_date')]
write.csv(file='dataIntakeDateCheckBoxDisagree.csv', IntakeDateCheckBoxDisagree, row.names=FALSE)

# Cross reference YouthID/SiteID combos that exist in intake but not in referral.
intakeWOReferral <- list()
multipleIntake <- list()
multipleReferral <- list()
for(asite in theSites){
	intakeWOReferral[[asite]] <- setdiff( theTables$Intake[[asite]]$YouthID, theTables$Referral[[asite]]$YouthID)
	multipleIntake[[asite]] <- table(theTables$Intake[[asite]]$YouthID)
	multipleIntake[[asite]] <- multipleIntake[[asite]][multipleIntake[[asite]] > 1]
	multipleReferral[[asite]] <- table(theTables$Referral[[asite]]$YouthID)
	multipleReferral[[asite]] <- multipleReferral[[asite]][multipleReferral[[asite]] > 1]
}

#check for intakes without referrals, duplicate referrals, duplicate intakes
intakeWOReferral
multipleIntake
multipleReferral
#------------------


refTotal <- pmax(colSums(intaked), colSums(referred)) #N.B. takes whichever is higher: the intake or the referral site total
intaked[1,] <- refTotal-intaked[2,]
round(t(t(intaked)/colSums(intaked)), 2)
round(colSums(intaked)/sum(colSums(intaked)), 2)
rowSums(intaked)
round(rowSums(intaked)/sum(colSums(intaked)), 2)


(enrolled <- table(theCombinedTables$Intake$IGtx11, theCombinedTables$Intake$`SITE ID`, useNA="ifany"))






siteN <- colSums(referred) #total referred by site
totalN <- sum(siteN) #total referrals (all sites)
intakeN <- intaked[2,]
enrollN <- enrolled["1", ]
if("CA" %in% theSites){
	enrollN["CA"] <- 111
}

#hard-coded graduation data
gradN <- c(CA=65, NE=52, NJ=72, MO=29, NV=21, NYSouthernTier=1, NYSTART=5, SC=22, PA=21, TX=45,
	AL=15, CH=16, DCAC=26, DCJD=4, CT=3, NM=0)
gradN <- gradN[theSites]
insHardCode <- c(CA=0, NE=0, NJ=2, MO=0, NV=5, NYSouthernTier=2, NYSTART=3, SC=1, PA=3, TX=58,
	AL=12, CH=8, DCAC=19, DCJD=3, CT=3, NM=6)
insHardCode <- insHardCode[theSites]

cbind(siteN, intakeN, enrollN, insHardCode, gradN)


#------------------------------------------------------------------------------

#--------------------------------------
source('ReportOJJDP_Referrals.R')


#--------------------------------------
source('ReportOJJDP_LegalInvolvement.R')


#--------------------------------------
source('ReportOJJDP_Demographics.R')




#------------------------------------------------------------------------------
# Reason given for no intake
findVariable('R_NoIntake', theCombinedTables)

data <- theCombinedTables$Intake


tableNoIntake <- table(data$R_NoIntake, data$`SITE ID`)
rownames(tableNoIntake) <- c('Caregiver declined', 'Referral source withdrew referral', 'Cannot locate', 'Youth lives or moved out of area', 'Youth placed in higher level of care', 'Could not accomodate due to language')

tableNoIntake <- rbind(tableNoIntake, Missing=(siteN-intakeN)-colSums(tableNoIntake))

(tableNoIntakeFull <- mkptable(tableNoIntake, c(siteN, totalN)))

write.csv(file='tableReasonNoIntake.csv', tableNoIntakeFull)

write.csv(file='tableReasonNoEnroll.csv', table(data$IGtx12))


# reason group was not recommended
(tableGroupRec <- table(data$IGtx01[data$IntakeDone == TRUE], data$`SITE ID`[data$IntakeDone == TRUE]))
tableGroupRecFull <- mkptable(tableGroupRec, c(intakeN, sum(intakeN)))

sel <- data$IGtx01 %in% "0"
IN <- function(x, table) match(x, table, nomatch = 0) > 0
IGtx02 <- t(sapply(strsplit(as.character(data$IGtx02[sel]), ";"), IN, x=c(1:3)))
colnames(IGtx02) <- c('Does not meet inclusion criteria', 'Trauma symptoms warrant TF-CBT instead', 'Other')
IGtx02 <- as.data.frame(IGtx02)
(tableGroupRecNoWhy <- t(sapply(lapply(IGtx02, table, data$`SITE ID`[sel]), function(x){x[2,]})))
(tableGroupRecNoWhyFull <- mkptable(tableGroupRecNoWhy, c(tableGroupRec["0",], sum(tableGroupRec["0",]))))


write.csv(file='tableGroupRecommended.csv', tableGroupRecFull)
write.csv(file='tableGroupRecommendedNoWhy.csv', tableGroupRecNoWhyFull)


#------------------------------------------------------------------------------
# Program flow from referral to intake to enrollment to graduation

flowTable <- rbind(Referred=siteN, Intaked=intakeN, Enrolled=enrollN, Completed=gradN)

(flowtablefull <- mkptable(flowTable, cbind(rep(totalN, length(theSites)+1), c(siteN, totalN), c(intakeN, sum(intakeN)), c(enrollN, sum(enrollN)))))

write.csv(file='tableFlow.csv', flowtablefull)

#----------
# Hand made table and figure for the flow of referral to intake to enrollment to completion

##FOR PUBLICATION##
#flowTable <- flowTable[,1:3]
##FOR PUBLICATION##

flowTable <- cbind(flowTable, Total=rowSums(flowTable))


# Flow table with no total
flowTableNT <- flowTable[, 1:length(theSites)]
flowTable <- flowTableNT


png(file='plotFlowSite.png', height=720, width=480)
dotchart(flowTable[nrow(flowTable):1,], xlab='Count', xlim=c(0, max(flowTable)))
dev.off()

png(file='plotFlowStage.png', height=720, width=480)
dotchart(t(flowTable)[ncol(flowTable):1,], xlab='Count', xlim=c(0, max(flowTable)))
dev.off()


# Anonymize site names
flowTableA <- flowTable
colnames(flowTableA) <- c(LETTERS[1:length(theSites)])#, "Total")

siteLimit <- length(theSites) #4

png(file='plotFlowSiteABC.png', height=720, width=480)
dotchart(flowTableA[nrow(flowTableA):1,1:siteLimit], xlab='Count', xlim=c(0, max(flowTable)))
dev.off()

png(file='plotFlowStageABC.png', height=720, width=480)
dotchart(t(flowTableA)[siteLimit:1,], xlab='Count', xlim=c(0, max(flowTable)))
dev.off()


#--------------------------------------
# Completion by site and total, graduation
data <- merge(theCombinedTables$Exit, theCombinedTables$Intake, all.x=TRUE, all.y=FALSE, by=c("SITE ID", "SITE_ID", "YouthID"))

data$PE01 <- factor(data$PE01, levels=1:11)

tableGraduated <- table(data$PE01, data$`SITE ID`)
tableGraduated <- table(data$PE01[data$IGtx11 %in% "1"], data$`SITE ID`[data$IGtx11 %in% "1"], useNA='ifany')
#tableGraduated <- cbind(tableGraduated, Total=rowSums(tableGraduated))
gna <- c('completed', 'declined', 'death', 'moved', 'cannot locate', 'excessive miss', 'youth moved more restric', 'youth juv det', 'reason unknown', 'other', 'change in placement')
rownames(tableGraduated) <- gna[as.numeric(rownames(tableGraduated))]
#insHardCode <- intakeN - colSums(tableGraduated)[-ncol(tableGraduated)]
#insHardCode <- c(insHardCode, sum(insHardCode))
tableGraduated <- rbind(InServ=insHardCode, tableGraduated)
tableGraduatedFull <- mkptable(tableGraduated, c(enrollN, sum(enrollN)))


write.csv(tableGraduatedFull, file='tableProgramExitReason.csv')


numGraduated <- tableGraduated[1,]


del <- na.omit(data[data$IGtx11=="1" & data$SITE_ID=="NJ" & data$PE01=="1" & !is.na(data$PE01), c('SITE ID', 'YouthID', 'PE01', 'IGtx11')])
del <- del[order(del$YouthID),]
del

data <- merge(theCombinedTables$Exit, theCombinedTables$Intake, all.x=TRUE, all.y=FALSE, by=c("SITE ID", "YouthID"))
data <- theCombinedTables$Exit
data$SITE_ID <- data$`SITE ID`

#---------------------------
# Trouble shoot NJ grads probelm

nj <- theCombinedTables$Intake[theCombinedTables$Intake$`SITE ID` == "NJ",]
sel <- nj$IGtx11==1
sel1 <- !is.na(sel)
nj2 <- nj[sel1 & sel,]


# Get YouthID in NJ who are still in service (have intake but no exit
pr <- data[(data$YouthID %in% nj2$YouthID) & data$SITE_ID=="NJ" & is.na(data$PE01), "YouthID"]
pr <- matrix(pr)
colnames(pr) <- "YouthID"
pr <- data.frame(YouthID=pr[order(pr),])
print(pr, row.names=FALSE)


njy <- nj2[sel, "YouthID"]


#--------------------------------------
#Number of sessions completed

data <- merge(theCombinedTables$Exit, theCombinedTables$Intake, all.x=TRUE, all.y=FALSE, by=c("SITE ID", "YouthID"))
#data <- theCombinedTables$Exit
data$SITE_ID <- data$`SITE ID`


#-----------
# Debugging a data merge issue related to new change of adding DK as option for missing number of sessions
theCombinedTables$Exit[theCombinedTables$Exit$`SITE ID` %in% 'NE', 'PEY04']
del <- theCombinedTables$Exit[theCombinedTables$Exit$`SITE ID` %in% 'NE', c('YouthID', 'PEY04')]
del[order(del$YouthID), ]
table(theCombinedTables$Exit$PEY04)
del2 <- merge(theTables$Exit$NE, theTables$Exit$LA, all=TRUE)
table(del2$PEY04)
#-----------



gradData <- data[data$PE01=="1" & ( data$IGtx11=="1" | is.na(data$IGtx11) ),]

sessionVars <- c('PEY03', 'PEY04', 'PEC05', 'PEC06')

#------------
# Mean (SD) of treatment duration (in days)
txdateVars <- c('Intake_date', 'Dateadmin', 'LastServiceDate')
summary(gradData[,txdateVars])
txDuration <- gradData$LastServiceDate - gradData$Intake_date
txDuration[txDuration < 0] <- NA
summary(as.numeric(txDuration))
hist(as.numeric(txDuration))
meansd(txDuration)

require(knitr)
kable(aggregate(txDuration ~ SITE_ID, data=gradData, meansd))
kable(aggregate(txDuration ~ I(SITE_ID %in% "NE"), data=gradData, meansd))


#------------
gradData[,sessionVars] <- apply(gradData[,sessionVars], 2, function(x){as.numeric(as.character(x))})

sapply(gradData[,sessionVars], table)


numSess <- aggregate(cbind(PEY03, PEY04, PEC05, PEC06)~`SITE ID`, meansd, data=gradData)
numSess <- rbind(numSess, c('Total', sapply(gradData[,sessionVars], meansd)))
tableNumSess <- t(numSess[,-1])
colnames(tableNumSess) <- numSess[,1]
rownames(tableNumSess) <- c('Group Youth Sessions', 'Indiv Youth Sessions', 'Group Caregiver Sessions', 'Family Sessions')
tableNumSess

neNumSess <- gradData[gradData$`SITE ID`=="NE" & !is.na(gradData$PEY04), c("YouthID", "PEY04", "Dateadmin")]
neNumSess[order(neNumSess$Dateadmin),]


subset(gradData, `SITE ID`=="NE" & YouthID=="113-01-01", c('SITE ID', 'YouthID', 'PEY03', 'PEY04', 'PEC05', 'PEC06'))


write.csv(tableNumSess, file="tableNumSess.csv")

png("plotNumSess.png")
plot(NA, xlim=range(gradData$PEY04, na.rm=TRUE), ylim=c(4.5, 7.5), xlab="Number of Individual Sessions", ylab="Site", yaxt='n')
axis(side=2, at=7:5, labels=c("CA", "NE", "NJ"), las=1)#, "MO", "NV", "NYSTART", "SC"), las=1)
points(y=jitter(rep(7, length(gradData$PEY04[gradData$SITE_ID=="CA"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="CA"])
points(y=jitter(rep(6, length(gradData$PEY04[gradData$SITE_ID=="NE"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="NE"])
points(y=jitter(rep(5, length(gradData$PEY04[gradData$SITE_ID=="NJ"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="NJ"])
#points(y=jitter(rep(4, length(gradData$PEY04[gradData$SITE_ID=="MO"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="MO"])
#points(y=jitter(rep(3, length(gradData$PEY04[gradData$SITE_ID=="NV"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="NV"])
#points(y=jitter(rep(2, length(gradData$PEY04[gradData$SITE_ID=="NYSTART"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="NYSTART"])
#points(y=jitter(rep(1, length(gradData$PEY04[gradData$SITE_ID=="SC"])), amount=.2), x=gradData$PEY04[gradData$SITE_ID=="SC"])
dev.off()



#--------------------------------------
# Services Provided to Youth Graduates
summary(gradData[,c("PEF08", "PE09_Treat", "PE10_CBT", "PE11_TFCBT", "PE12")])

gradData <- gradData[!is.na(gradData$IGtx11),]

del <- gradData[gradData$`SITE ID`=='NJ', c('SITE ID', 'YouthID', 'PE01', 'IGtx11', 'PEF08', 'PE10_CBT')]
del[!is.na(del$`SITE ID`),]


table(gradData$PEF08, gradData$`SITE ID`) #Family safety plan provided?
#table(gradData$PE09_Treat, gradData$`SITE ID`) #Treatment programs provided to the Youth with SBP
table(gradData$PE10_CBT, gradData$`SITE ID`) #Did the treatment include OU CBT SBP Group treatment program?
table(gradData$PE11_TFCBT, gradData$`SITE ID`) #Trauma Focused treatment for Youth with SBP
table(gradData$PE12, gradData$`SITE ID`) #Case management services provided

tableGServ <- rbind(
	safetyPlan=table(gradData$PEF08, gradData$`SITE ID`)["1", ],
	txCBT=table(gradData$PE10_CBT, gradData$`SITE ID`)["1", ],
	tfNone=table(gradData$PE11_TFCBT, gradData$`SITE ID`)["0",],
	tfSome=table(gradData$PE11_TFCBT, gradData$`SITE ID`)["1",],
	tfCompleted=table(gradData$PE11_TFCBT, gradData$`SITE ID`)["2",]
)
tableGServ

sel <- gradN > 0
tableGServFull <- mkptable(tableGServ[,sel], c(gradN[sel], sum(gradN[sel])))

write.csv(tableGServFull, file='tableGraduatedServices.csv')


#----------
# Process Case mgt
cmna <- c("Transportation", "Child Care", "Housing", "Job Support", "Basic Needs", "School", "MH Services", "Substance Abuse")
lsPE12 <- lapply(strsplit(gradData$PE12, split=";", fixed=TRUE), as.numeric)
dfPE12 <- as.data.frame(matrix(NA, nrow=nrow(gradData), ncol=length(cmna)))
names(dfPE12) <- cmna
for(i in 1:length(cmna)){
	dfPE12[[cmna[i]]] <- sapply(lsPE12, "[", i)
}
pe12fun <- function(x){ y <- rep(FALSE, 8); y[which(1:8 %in% x)] <- TRUE; return(y) }
dfPE12 <- data.frame(t(apply(dfPE12, 1, pe12fun)))
names(dfPE12) <- cmna

table(dfPE12[,"Child Care"], gradData$`SITE ID`)
table(dfPE12[,"Basic Needs"], gradData$`SITE ID`)
table(dfPE12[,"School"], gradData$`SITE ID`)
table(dfPE12[,"MH Services"], gradData$`SITE ID`)
tableCaseMgtList <- lapply(dfPE12, table, y=gradData$`SITE ID`)

tableCaseMgt <- lapply(tableCaseMgtList, "[", "TRUE", , drop=FALSE)
tableCaseMgt <- matrix(unlist(tableCaseMgt), ncol=length(theSites), byrow=TRUE, dimnames=list(names(tableCaseMgtList), theSites))
tableCaseMgt <- tableCaseMgt[c("Child Care", "Basic Needs", "School", "MH Services", "Transportation", "Housing", "Job Support", "Substance Abuse"), ]
tableCaseMgt

tableCaseMgtFull <- mkptable(tableCaseMgt[,sel], c(gradN[sel], sum(gradN[sel])))

write.csv(file='tableCaseMgt.csv', tableCaseMgtFull)


lapply(tableCaseMgtList, mkpercent, totals=c(gradN, 5))
#----------
# old version


tableCaseMgt2 <- rbind(table(dfPE12[,"Child Care"], gradData$SITE_ID)[2,1:3],
	table(dfPE12[,"Basic Needs"], gradData$SITE_ID)[2,1:3],
	table(dfPE12[,"School"], gradData$SITE_ID)[2,1:3],
	table(dfPE12[,"MH Services"], gradData$SITE_ID)[2,1:3])
round(t(t(tableCaseMgt2)/numGraduated), 2)

tableCaseMgt <- rbind(
	table(gradData$PEF08, gradData$SITE_ID)[2,], #Family safety plan provided?
	table(gradData$PE10_CBT, gradData$SITE_ID)[2,],
	table(gradData$PE11_TFCBT, gradData$SITE_ID),
	table(gradData$PE12, gradData$SITE_ID)[!apply(table(gradData$PE12, gradData$SITE_ID), 1, rowZero),]
)
rownames(tableCaseMgt)[1:2] <- as.character(Variables[match(c("PEF08", "PE10_CBT"), Variables$Names), "Labels"])
tableCaseMgt
round(t(t(tableCaseMgt)/numGraduated), 2)


#--------------------------------------
# Number of court reports

tableGradCourtRep <- table(gradData$CourtReports, gradData$`SITE ID`)
selSites <- gradN > 0
write.csv(cbind(tableGradCourtRep[,selSites], rowSums(tableGradCourtRep[,selSites])), file='tableGCourtRep.csv')
gradN[selSites] - colSums(tableGradCourtRep[,selSites])

write.csv(table(gradData$CourtReports, gradData$SITE_ID)[,1:3], file="tableCourtReports.csv")
gradData$CourtReports[gradData$CourtReports==99] <- NA

useNames <- names(gradN[selSites])
#useNames <- c("CA", 'NE', 'NJ')
aggregate(CourtReports~SITE_ID, meansd, data=subset(gradData, `SITE ID` %in% useNames))
fcat(lmCR <- lm(CourtReports~SITE_ID, data=subset(gradData, `SITE ID` %in% useNames)))

require(multcomp)

sulmCR <- summary(mclmNumCR <- glht(lmCR, linfct=mcp(SITE_ID="Tukey")))
df <- sulmCR$df
tstat <- sulmCR$test$tstat[1]
pval <- sulmCR$test$pvalues[1]
2*tstat / sqrt(df) # Cohen's d








#--------------------------------------
# scratch 
# Inservice = has intake enrolled but exit is missing (PE01)
deldata <- merge(theCombinedTables$Exit, theCombinedTables$Intake, all=TRUE, by=c("SITE ID", "YouthID"))
deldata$SITE_ID <- deldata$`SITE ID`
delIn <- deldata[deldata$IGtx11=="1" & !is.na(deldata$IGtx11) & deldata$SITE_ID=="NJ" & is.na(deldata$PE01), "YouthID"]
delIn <- deldata[deldata$IGtx11=="1" & !is.na(deldata$IGtx11) & is.na(deldata$PE01), c('SITE ID', "YouthID")]

# Table Service status reasons for discharge
# program completion: Still in Service
table(delIn$`SITE ID`)


#------------------------------------------------------------------------------
# Of the youth who had an intake and enrolled in treatment, what percentage was a safety plan discussed with the caregiver – I believe the variable is YSafetyPlan

data <- theCombinedTables$Intake

data$YSafetyPlan <- factor(data$YSafetyPlan, levels=c(0, 1, 8, 9), labels=c('No', 'Yes', 'NA', 'DK'))
data$IGtx11 <- factor(data$IGtx11, levels=c(0, 1, 'DK', 'RF'), labels=c('No', 'Yes', 'DK', 'RF'))

tabES <- table(Enrolled=data$IGtx11, SafeyPlan=data$YSafetyPlan)

mkptable(tabES['Yes', , drop=FALSE], sum(tabES['Yes', ]))
#    No        Yes         NA       DK       Total       
#Yes "15 (5%)" "298 (93%)" "4 (1%)" "4 (1%)" "321 (100%)"



write.csv(file='tabEnrollSafety.csv', tabES)



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Victim Enrollment, demographics, and cohabitation
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

source('ReportOJJDP_Victims.R')

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# YSBP Measure
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

source('ReportOJJDP_YSBP.R')




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# FSSS / PSSS Measure
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

source('ReportOJJDP_FSSS.R')


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CBCL Measure
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# People that have exit data but not intake data on CBCL

##### Caregiver on Youth #####
dataIn <- theCombinedTables$CBCC
dataIn$SITE_ID <- dataIn$`SITE ID`
dataOut <- theCombinedTables$CBCCExit
dataOut$SITE_ID <- dataOut$`SITE ID`

dataM <- merge(dataIn, dataOut, by=c('YouthID', 'SITE ID', 'SITE_ID'), suffixes=c('.In', '.Out'), all=TRUE)


# Has exit internalizing but missing intake internalizing
write.csv(file='dataCbccExitNoIntake.csv', dataM[is.na(dataM$InternalizingTS.In) & !is.na(dataM$InternalizingTS.Out), c('YouthID', 'SITE ID')])

##### Youth Self-report #####
dataIn <- theCombinedTables$CBCY
dataIn$SITE_ID <- dataIn$`SITE ID`
dataOut <- theCombinedTables$CBCYExit
dataOut$SITE_ID <- dataOut$`SITE ID`

dataM <- merge(dataIn, dataOut, by=c('YouthID', 'SITE ID', 'SITE_ID'), suffixes=c('.In', '.Out'), all=TRUE)

# Has exit internalizing but missing intake internalizing
write.csv(file='dataCbcyExitNoIntake.csv', dataM[is.na(dataM$InternalizingTS.In) & !is.na(dataM$InternalizingTS.Out), c('YouthID', 'SITE ID')])


#--------------------------------------
# TS = Total Score = Raw Score
# NS = Normed Score =  T Score


#--------------------------------------
# Intake CBCL

##### Caregiver on Youth #####
data <- theCombinedTables$CBCC
data$SITE_ID <- data$`SITE ID`

useScore <- 'NS' #NS, TS #NS = Normed Score (i.e. T-score), # TS = Total Score (i.e. Raw score)
varIn <- paste0('Internalizing', useScore)
varEx <- paste0('Externalizing', useScore)
formIn <- as.formula(paste0(varIn, ' ~ SITE_ID'))
formEx <- as.formula(paste0(varEx, ' ~ SITE_ID'))

# Internalizing Caregiver on Youth at Intake
ci <- aggregate(formIn, meansdn, data=data)

# Externalizing Caregiver on Youth at Intake
ce <- aggregate(formEx, meansdn, data=data)

cit <- meansdn(data[,varIn])
cet <- meansdn(data[,varEx])

# Site differences
summary(lmEc <- lm(formEx, data=data))
anova(lmEc)
fcat(lmEc)

summary(lmIc <- lm(formIn, data=data))
anova(lmIc)
fcat(lmIc)

datacs <- data[,c('SITE ID', 'YouthID', 'CaregiverID', varIn, varEx)]


##### Youth Self-report#####
data <- theCombinedTables$CBCY
data$SITE_ID <- data$`SITE ID`

# Internalizing Youth Self-report at Intake
yi <- aggregate(formIn, meansdn, data=data)

# Externalizing Youth Self-report at Intake
ye <- aggregate(formEx, meansdn, data=data)


yit <- meansdn(data[,varIn])
yet <- meansdn(data[,varEx])


# Site differences
summary(lmEy <- lm(formEx, data=data))
anova(lmEy)
fcat(lmEy)

summary(lmIy <- lm(formIn, data=data))
anova(lmIy)
fcat(lmIy)


dataInCBCL <- merge(merge(ci, ce, all=TRUE), merge(yi, ye, all=TRUE), all=TRUE, by='SITE_ID', suffixes=c('.Care', '.Youth'))
dataInCBCL <- dataInCBCL[match(theSites, dataInCBCL[,'SITE_ID']),]
dataInCBCL <- rbind(sapply(dataInCBCL, as.character), c('Total', cit, cet, yit, yet))
tableInCBCL <- t(dataInCBCL)


write.csv(tableInCBCL, file=paste0('tableInCBCL_', useScore, '.csv'))

fcat(lmEc)
fcat(lmIc)
fcat(lmEy)
fcat(lmIy)


datays <- data[,c('SITE ID', 'YouthID', varIn, varEx)]
datams <- merge(datays, datacs, by=c('SITE ID', 'YouthID'), suffixes=c('.youth', '.care'))
round(cor(datams[,!(names(datams) %in% c('SITE ID', 'YouthID', 'CaregiverID'))], use='pair'), 3)


#--------------------------------------
# Exit CBCL

##### Caregiver on Youth #####
data <- theCombinedTables$CBCCExit
data$SITE_ID <- data$`SITE ID`

# Internalizing Caregiver on Youth at Exit
aggregate(formIn, meansd, data=data)

# Externalizing Caregiver on Youth at Exit
aggregate(formEx, meansd, data=data)

meansd(data[,varIn])
meansd(data[,varEx])


##### Youth Self-report#####
data <- theCombinedTables$CBCYExit
data$SITE_ID <- data$`SITE ID`

# Internalizing Youth Self-report at Exit
aggregate(formIn, meansd, data=data)

# Externalizing Youth Self-report at Exit
aggregate(formEx, meansd, data=data)

meansd(data[,varIn])
meansd(data[,varEx])

