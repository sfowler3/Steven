#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: 2014-12-03
# Filename: ReportOJJDP_Victims.R
# Purpose: Victim Enrollment, demographics, and cohabitation
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------



#------------------------------------------------------------------------------



#------------------------------------------------------------------------------



#------------------------------------------------------------------------------

yvKey <- read.table('KeyYouthIDVictimID1.txt', header=TRUE)
vyKey <- read.table('KeyVictimID1YouthID.txt', header=TRUE)


arthurData <- merge(yvKey, theCombinedTables$Victims, all.x=TRUE, all.y=FALSE, by.x=c("SITE_ID", "VictimID1", "YouthID"), by.y=c("SITE_ID", "VictimID1", "YouthID"))
#arthurData <- merge(arthurData, theCombinedTables$IntakeV, all.x=TRUE, all.y=FALSE, by.x=c("SITE_ID", "VictimID1", "YouthID"), by.y=c("SITE_ID", "VictimID1", "YouthID"))

findVariable("IGtx131", theCombinedTables)

#look
arthurData[,c("SITE_ID", "YouthID", "VictimID1", "IGtx131")]

#--------------------------------------
# Number of victims per youth by site



NumVicPerYouth <- data.frame(SITE_ID=character(), YouthID=character(), NumVictims=numeric(), stringsAsFactors=FALSE)
for(i in theSites){
	tem.site <- yvKey[yvKey$SITE_ID == i, ]
	for(j in unique(tem.site$YouthID)){
		tem.dat <- tem.site[tem.site$YouthID==j,]
		if(all(!is.na(tem.dat$VictimID1))){
			tem.res <- data.frame(SITE_ID=i, YouthID=j, NumVictims=nrow(tem.dat), stringsAsFactors=FALSE)
		} else if(all(is.na(tem.dat$VictimID1))){
			tem.res <- data.frame(SITE_ID=i, YouthID=j, NumVictims=0, stringsAsFactors=FALSE)
		} else {
			print(paste('YouthID ', j, ' in SITE_ID ', i, 'has some missing and some non-missing victims.', sep=''))
		}
		NumVicPerYouth <- rbind(NumVicPerYouth, tem.res)
	}
}
NumVicPerYouth$SITE_ID <- factor(NumVicPerYouth$SITE_ID)
NumVicPerYouth$YouthID <- factor(NumVicPerYouth$YouthID)

vpyTable <- table(NumVicPerYouth$NumVictims, NumVicPerYouth$SITE_ID)

(vpyTable <- vpyTable[, theSites])
rowSums(vpyTable)

(vpyTableFull <- mkptable(vpyTable, c(siteN, totalN)))

write.csv(file='tableVictimsPerYouth.csv', vpyTableFull)

#number of youth without victim information
tabMissVic <- siteN - colSums(vpyTable[-1,,drop=FALSE])


write.csv(file='tableMissingVictim.csv', mkptable(matrix(tabMissVic, nrow=1), c(siteN, totalN)))

#--------------------------------------
# Victim N to use

for(i in unique(vyKey$SITE_ID)){
	print(paste("SITE_ID ", i, " has ", nrow(vyKey[vyKey$SITE_ID==i,]), " victims."))
}

(vicN <- t(t(vpyTable) %*% matrix(0:(nrow(vpyTable)-1))))

#vicN[8] <- 48


#--------------------------------------
# victim referral

# Number of victims at each site
vidSpots <- grep("VictimID1", names(arthurData))
length(unique(unlist(arthurData[arthurData$SITE_ID=="LA", vidSpots])))
length(unique(unlist(arthurData[arthurData$SITE_ID=="NE", vidSpots])))
length(unique(unlist(arthurData[arthurData$SITE_ID=="NJ", vidSpots])))

#--------------------------------------
# Num Victims
arthurData <- theCombinedTables$Referral
table(arthurData$NumVictims, arthurData$`SITE ID`)


arthurData <- theCombinedTables$Victims
table(arthurData$NumVictims, arthurData$`SITE ID`)
numVictimReferral <- aggregate(VictimID1 ~ SITE_ID, data=arthurData, FUN=length)
colnames(numVictimReferral) <- c('SITE_ID', 'ReferralCount')

arthurData <- theCombinedTables$IntakeV
numVictimIntake <- table(arthurData$IGtx131, arthurData$`SITE ID`)["1",]
#numVictimIntake <- aggregate(VictimID1 ~ SITE_ID, data=arthurData, FUN=length)
#colnames(numVictimIntake) <- c('SITE_ID', 'IntakeCount')
numVictimEnroll <- table(arthurData$VicEnroll1, arthurData$`SITE ID`)["Yes",]

arthurData <- theCombinedTables$Exit
table(arthurData$NVictimsServed, arthurData$`SITE ID`)
numVictimServed <- aggregate(VictimID1 ~ SITE_ID, data=arthurData, FUN=length)
#numVictimServed <- matrix(c(theSites, numeric(length(theSites))), nrow=length(theSites))
colnames(numVictimServed) <- c('SITE_ID', 'ServedCount')

#Compare to number of victim exit forms
numVictimExit <- aggregate(VictimID1 ~ SITE_ID, data=theCombinedTables$ExitV, FUN=length)
colnames(numVictimExit) <- c('SITE_ID', 'ExitCount')


numVictimReInEx <- merge(merge(numVictimReferral, numVictimServed, by='SITE_ID', all=TRUE), numVictimExit, by='SITE_ID', all=TRUE)
numVictimReInEx[is.na(numVictimReInEx)] <- 0
rownames(numVictimReInEx) <- theSites
victimFlow <- t(cbind(
	VictimTotal=c(vicN),
	ReferralCount=numVictimReInEx[,c('ReferralCount')],
	IntakeCount=numVictimIntake,
	EnrollCount=numVictimEnroll,
	ServedCount=numVictimReInEx[,c('ServedCount')],
	ExitCount=numVictimReInEx[,c('ExitCount')]))
(victimFlow <- cbind(victimFlow, Total=rowSums(victimFlow)))

# TODO percentages once we figure out what is appropriate

#victimFlow <- mkptable(t(numVictimReInEx[,-1]), c(numVictimReInEx[,2], sum(numVictimReInEx[,2])))

write.csv(file='tableVictimFlow.csv', victimFlow)

# Reason for exit
tableVGraduated <- table(theCombinedTables$ExitV$PE01v1, theCombinedTables$ExitV$`SITE ID`)

gvna <- gna
rownames(tableVGraduated) <- gvna[as.numeric(rownames(tableVGraduated))]
tableVGraduatedFull <- mkptable(tableVGraduated, c(victimFlow['ExitCount',]))
write.csv(file='tableVictimGraduated.csv', tableVGraduatedFull)

#--------------------------------------
#Victim Gender and Age

data <- theCombinedTables$Victims
data <- data[!is.na(data$VictimID1), ]
data[, c('SITE ID', 'VictimID1', 'V1I01')]

del <- data[data$SITE_ID=="NYSTART", c('SITE ID', 'VictimID1', 'V1I01')]
#del <- data[, c('SITE ID', 'VictimID1', 'V1I01')]
del <- del[order(as.character(del$VictimID1)),]

#delTab <- table(del$`SITE ID`, del$VictimID1)
delTab <- table(del$VictimID1)
delTab[delTab > 1]


# Victim Cleaning
sel <- data$`SITE ID`=='PA' & data$VictimID1=='001-03-01' & is.na(data$MeasureStatus)
data <- data[!sel, ] # That is, drop this record

sel <- data$`SITE ID`=='NYSouthernTier' & data$VictimID1=='047-03-01' & is.na(data$YouthID)
data <- data[!sel, ] # That is, drop the missing youth ID instance of this ID


# Write changes to Victims Table
theCombinedTables$Victims <- data
######



# Gender
data$V1I01[data$V1I01 %in% 'm'] <- "0" #recode m and male

tableVicGen <- table(data$V1I01, data$SITE_ID)
tableVicGenKU <- rbind(Known=colSums(tableVicGen[1:2,]), Unknown=(vicN-colSums(tableVicGen[1:2,])))

(tableVicGenFull <- mkptable(tableVicGen, c(tableVicGenKU[1,], sum(tableVicGenKU[1,]))))
(tableVicGenKUFull <- mkptable(tableVicGenKU, c(vicN, sum(vicN))))
(tableVicGenFull <- rbind(
	Known=tableVicGenKUFull[1,],
	Male=tableVicGenFull["0",],
	Female=tableVicGenFull["1",],
	Unknown=tableVicGenKUFull[2,]
	))

write.csv(file='tableVictimGender.csv', tableVicGenFull)


#------------------------------------------------------------------------------
# Victim outcomes

# Number of CBCL forms available
# at intake
aggregate(VictimID ~ SITE_ID, data=theCombinedTables$CBCV, FUN=length)
# at exit
aggregate(VictimID ~ SITE_ID, data=theCombinedTables$CBCVExit, FUN=length)


# Number of non-missing scores available at intake
# for Externalizing
aggregate(ExternalizingTS ~ SITE_ID, data=theCombinedTables$CBCV, FUN=function(x){length(!is.na(x))})
# for Internalizing
#aggregate(InternalizingTS ~ SITE_ID, data=theCombinedTables$CBCV, FUN=function(x){length(!is.na(x))})
# found same number

# Number of non-missing scores available at exit
# for Externalizing
aggregate(ExternalizingTS ~ SITE_ID, data=theCombinedTables$CBCVExit, FUN=function(x){length(!is.na(x))})
# for Internalizing
#aggregate(InternalizingTS ~ SITE_ID, data=theCombinedTables$CBCVExit, FUN=function(x){length(!is.na(x))})
# found same number

# DSM IV
aggregate(UCLA_IV_01 ~ SITE_ID, data=theCombinedTables$UCLACareV, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_IV_01 ~ SITE_ID, data=theCombinedTables$UCLACareVExit, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_IV_01 ~ SITE_ID, data=theCombinedTables$UCLAClinV, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_IV_01 ~ SITE_ID, data=theCombinedTables$UCLAClinVExit, FUN=function(x){length(!is.na(x))})

# DSM V
aggregate(UCLA_V_01 ~ SITE_ID, data=theCombinedTables$UCLACareV, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_V_01 ~ SITE_ID, data=theCombinedTables$UCLACareVExit, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_V_01 ~ SITE_ID, data=theCombinedTables$UCLAClinV, FUN=function(x){length(!is.na(x))})
aggregate(UCLA_V_01 ~ SITE_ID, data=theCombinedTables$UCLAClinVExit, FUN=function(x){length(!is.na(x))})



#------------------------------------------------------------------------------


# Age
# at offense
data$V1I03[data$V1I03==99 | data$V1I03 > 50] <- NA
(vaOff <- cbind(t(aggregate(V1I03 ~SITE_ID, meansd, data=data)), c('total', meansd(data$V1I03))))

write.csv(file='tableVictimAgeOffense.csv', vaOff)



# at referral
data$V1I02[data$V1I02==99] <- NA
(vaRef <- cbind(t(aggregate(V1I02 ~SITE_ID, meansd, data=data)), c('total', meansd(data$V1I02))))

write.csv(file='tableVictimAgeReferral.csv', vaRef)


#Ages differ across site?
lmOff <- lm(V1I03 ~ SITE_ID, data=data) #offense
lmRef <- lm(V1I02 ~ SITE_ID, data=data) # referral


require(multcomp)

lmX <- lmRef
fcat(lmX)

(so <- summary(mcX <- glht(lmX, linfct=mcp(SITE_ID="Tukey"))))
(df <- so$df)
tstat <- so$test$tstat
(pval <- so$test$pvalues[abs(tstat) > 3])
(dval <- 2*tstat[abs(tstat) > 3] / sqrt(df)) # Cohen's d
tstat <- tstat[abs(tstat) > 3]

tcatpost(tstat, df, pval)


#--------------------------------------
#Youth Victim Relationship
data <- theCombinedTables$Victims
data <- data[!( (data$`SITE ID` %in% 'PA') & !(data$MeasureStatus %in% 'Completed') ),]
theCombinedTables$Victims <- data

data$V1I06 <- factor(data$V1I06, levels=c(1:12, "DK"), labels=c("Full bio sib", "half sib", "step sib", "foster sib", "cousin", "other relative", "school mate", "neighbor", "friend", "sibling unsure 1/2 or full", "Other", "Victim not in youth social network", "DK"))
table6 <- table(data$V1I06, data$`SITE ID`)
(table6 <- rbind(table6, Missing=vicN-colSums(table6)))

(table6full <- mkptable(table6, c(vicN, sum(vicN))))

write.csv(file='tableVictimRel.csv', table6full)

# Percentage of those known because Jane asked for this at least once
mkptable(table6, c(colSums(table6[-nrow(table6),]), sum(table6[-nrow(table6),])))


#--------------------------------------
# Youth-Victim Cohabitation
# lived together
# Victim lives with youth
data <- theCombinedTables$Victims

tableCohab <- table(data$V1I07, data$SITE_ID)
tableCohab <- tableCohab[c("1", "0"), ] # tableCohab[c("1", "0", "RF"), ]
tableCohab[3,] <- vicN-colSums(tableCohab[1:2,])
#tableCohab <- rbind(tableCohab, vicN-colSums(tableCohab[1:2,]))

(tableCohabFull <- mkptable(tableCohab, c(vicN, sum(vicN))))

write.csv(file='tableVictimCohab.csv', tableCohabFull)


(testCohab <- chisq.test(tableCohab[1:2,colSums(tableCohab)!=0]))
(phiCohab <- sqrt(testCohab$statistic/(sum(testCohab$observed)*(min(dim(testCohab$observed))-1))))
testCohab$residuals

(testCohab <- chisq.test(tableCohab[1:2,1:3]))
(phiCohab <- sqrt(testCohab$statistic/(sum(testCohab$observed)*(min(dim(testCohab$observed))-1))))



# Still live together?
sel <- data$V1I07=="1" & !is.na(data$V1I07)
tableStillCohab <- table(data$V1I08[sel], data$SITE_ID[sel], useNA='ifany')
# Check
all(colSums(tableStillCohab)==tableCohab[1,])

tableStillCohab[3,] <- colSums(tableStillCohab[-c(1, 2), , drop=FALSE])
(tableStillCohab <- tableStillCohab[c(2, 1, 3), ])

(tableStillCohabFull <- mkptable(tableStillCohab, c(tableCohab[1,], sum(tableCohab[1,]))))

write.csv(file='tableStillCohab.csv', tableStillCohabFull)



tableStillCohab2 <- tableStillCohab[1:2, colSums(tableStillCohab)!= 0]
(testStillCohab <- chisq.test(tableStillCohab2))
sqrt(testStillCohab$statistic/(sum(testStillCohab$observed)*(min(dim(testStillCohab$observed))-1)))


stillLive <- data[data$V1I07=="1" & !is.na(data$V1I07),]
stillLiveNowNo <- stillLive[stillLive$V1I08=="0", c('SITE_ID', 'VictimID1', 'YouthID')]

write.csv(stillLiveNowNo, file='ReunificationOfNoLongerCohab.csv', row.names=FALSE)


#-----------------------
# Reunion at exit?

# Compare version 1 and version 1.5 to see if there are reunifications we don't know about yet because the Youth have not yet exited.

# Version 1
dataReunion <- merge(stillLiveNowNo, theCombinedTables$Exit, all.x=TRUE, all.y=FALSE, by.x=c("SITE_ID", "YouthID"), by.y=c("SITE_ID", "YouthID"))

# Version 1.5
#dataReunion <- merge(stillLiveNowNo, theCombinedTables$Exit, all.x=FALSE, all.y=FALSE, by.x=c("SITE_ID", "YouthID"), by.y=c("SITE_ID", "YouthID"))

# Version 2: Experimental
#dataReunion <- merge(stillLiveNowNo, theCombinedTables$Exit, all.x=FALSE, all.y=TRUE, by.x=c("SITE_ID", "YouthID"), by.y=c("SITE_ID", "YouthID"))
#dataReunion <- dataReunion[dataReunion$PE01%in%"1", ]

dataReunion[, c("SITE_ID", "YouthID", paste("PTx43", c("a", "b", "c"), sep=""), paste("PTx44", c("a", "b", "c"), sep=""))]

tableReunionExit <- table(dataReunion$PTx44a, dataReunion$SITE_ID, useNA="ifany")
#Check
all(colSums(tableReunionExit)[1:length(theSites)]==tableStillCohab["0",])

tableReunionExit[3,] <- colSums(tableReunionExit[-c(1, 2),, drop=FALSE])
(tableReunionExit <- tableReunionExit[c(2, 1, 3),])
tableReunionExit <- tableReunionExit[,1:length(theSites)]

(tableReunionExitFull <- mkptable(tableReunionExit, c(tableStillCohab["0",], sum(tableStillCohab["0",]))))

write.csv(file='tableReunionExit.csv', tableReunionExitFull)


# scratch
del <- dataReunion[is.na(dataReunion$PTx44a), c("SITE_ID", "YouthID", "PTx44a", 'PE01')]


#-----------------------
# Reunion at exit or post?
dataReunion2 <- merge(dataReunion, theCombinedTables$SBPPost, all.x=TRUE, all.y=FALSE, suffixes=c(".Exit", ".Post"), by.x=c("SITE_ID", "YouthID"), by.y=c("SITE ID", "YouthID"))

table(dataReunion2$PTx44a.Exit, dataReunion2$SITE_ID, useNA="ifany")

table(dataReunion2$PTx44a.Post, dataReunion2$SITE_ID, useNA="ifany")

tableReunionEP <- table(dataReunion2$PTx44a.Exit=="1" | dataReunion2$PTx44a.Post=="1", dataReunion2$SITE_ID, useNA="ifany")
tableReunionEP <- tableReunionEP[c(2, 1, 3), 1:length(theSites)]
tableReunionEP[4,] <- tableStillCohab["0",] - colSums(tableReunionEP[1:3,])
tableReunionEP

(tableReunionEPF <- mkptable(tableReunionEP, c(tableStillCohab["0",], sum(tableStillCohab["0",]))))

write.csv(file='tableReunionExitPost.csv', tableReunionEPF)

# Chi-square
x <- tableReunionEP[,1:3]
x.test <- chisq.test(x)
x.test
sqrt(x.test$statistic/(sum(x.test$observed)*(min(dim(x.test$observed))-1)))



#--------------------------------------
# victim enrollment


arthurData <- merge(yvKey, theCombinedTables$Victims, all.x=TRUE, all.y=FALSE, by.x=c("SITE_ID", "VictimID1", "YouthID"), by.y=c("SITE_ID", "VictimID1", "YouthID"))
arthurData <- merge(arthurData, theCombinedTables$IntakeV, all.x=TRUE, all.y=FALSE, by.x=c("SITE_ID", "VictimID1", "YouthID"), by.y=c("SITE_ID", "VictimID1", "YouthID"))
arthurData <- merge(arthurData, theCombinedTables$Intake, all.x=TRUE, all.y=TRUE, by.x=c("SITE_ID", "VictimID1", "YouthID"), by.y=c("SITE_ID", "VictimID1", "YouthID"))

arthurData[, c("SITE_ID", "VictimID1", "YouthID")]

# IGtx131 is victim intake from Victim Intake Table
theCombinedTables$IntakeV$IGtx131[theCombinedTables$IntakeV$IGtx131 %in% " 1"] <- "1"
theCombinedTables$IntakeV$IGtx131[theCombinedTables$IntakeV$IGtx131 %in% ""] <- "DK"
theCombinedTables$IntakeV$IGtx131 <- factor(theCombinedTables$IntakeV$IGtx131)

# Victim enrollment
table(theCombinedTables$IntakeV$IGtx131, theCombinedTables$IntakeV$`SITE ID`)

#Youth enrollment
table(arthurData$IGtx11, arthurData$SITE_ID)

arthurData$IGtx131[arthurData$IGtx131==""] <- "DK"
arthurData$IGtx131[arthurData$IGtx131==" 1"] <- "1"
arthurData$IGtx131 <- factor(arthurData$IGtx131)

(tabV <- table(arthurData$IGtx131, arthurData$SITE_ID)[,theSites])

sel <- arthurData$IGtx11=="1" #When youth enroll
(tabVwY <- table(arthurData$IGtx131[sel], arthurData$SITE_ID[sel], useNA='ifany')[,theSites])

sel <- arthurData$IGtx11!="1" #When youth NOT enroll
(tabVwNY <- table(arthurData$IGtx131[sel], arthurData$SITE_ID[sel], useNA='ifany')[,theSites])


(table4 <- rbind(YouthEnrolled=enrollN, VictimEnrolled=tabV["1",], whenYouth=tabVwY["1",], whenNotYouth=tabVwNY["1",]))

table4[3,] <- table4[2,]-table4[4,]
table4

(table4full <- mkptable(table4, t(rbind(c(siteN, sum(siteN)), c(vicN, sum(vicN)), c(table4[2,], sum(table4[2,])), c(table4[2,], sum(table4[2,]))))))

write.csv(file='tableVictimEnrollYouth.csv', table4full)


# Start of trying to do this count the hard (but accurate) way
yvEnroll <- matrix(0, nrow=3, ncol=length(theSites))
rownames(yvEnroll) <- c('whenYouth', 'whenNotYouth')
colnames(yvEnroll) <- theSites
for(asite in theSites){
	for(ayouth in unique(yvKey[yvKey$SITE_ID==asite,"YouthID"])){
		tem.yi <- theCombinedTables$Intake
		tem.yi <- tem.yi[tem.yi$SITE_ID==asite & tem.yi$YouthID==ayouth, ]
		tem.yi <- tem.yi[!is.na(tem.yi$YouthID),]
		if(nrow(tem.yi) > 0){
			if(is.na(tem.yi$IGtx11)){
				rsel <- 3
			} else if(tem.yi$IGtx11=="1" | tem.yi$IGtx11==" 1"){
				rsel <- 1
			} else {rsel <- 2}
			tem.v <- theCombinedTables$IntakeV
			tem.v <- tem.v[tem.v$SITE_ID==asite & tem.v$YouthID==ayouth, ]
			tem.v <- tem.v[!is.na(tem.yi$YouthID),]
			
			yvEnroll[rsel, asite] <- yvEnroll[rsel, asite] + 1
		}
	}
}
# end 




table4a <- table4[3:4,]
colnames(table4a) <- theSites
rownames(table4a) <- c("Enroll", "Not")
(table4a <- table4a[,colSums(table4a) > 0])

table4aExp <- matrix(rep(colSums(table4a), each=2), nrow=2)/2
table4a.chi <- sum((table4a - table4aExp)**2/table4aExp)
table4a.chi
1-pchisq(table4a.chi, df=prod(dim(table4a)-1))
sqrt(table4a.chi/(sum(table4a)*(min(dim(table4a))-1)))


yd <- data.frame(YouthID=unique(arthurData$YouthID.IntV), NumVictims=NA)
yd <- yd[!is.na(yd$YouthID),]
for(v in yd$YouthID){
	theVics <- unique(arthurData[arthurData$YouthID.IntV == v, "VictimID1V"])
	theVics <- as.numeric(na.omit(theVics))
	yd[yd$YouthID==v, "NumVictims"] <- length(theVics)
	print(theVics)
}
yd <- merge(yd, theCombinedTables$Intake, by="YouthID")
table(yd$NumVictims, yd$`SITE ID`)
table5 <- table(yd$NumVictims[yd$IntakeDone], yd$`SITE ID`[yd$IntakeDone], useNA="ifany")
table5 <- cbind(table5, Total=rowSums(table5))
table5 <- rbind(table5, DK=(colSums(table5)-c(intakeN, sum(intakeN))))
table5
round(t(t(table5)/c(intakeN, sum(intakeN))), 2)



yd <- data.frame(YouthID=arthurData$YouthID.RefV, SITE_ID=arthurData$SITE_ID, NumVictims=NA)
yd$SITE_ID[yd$SITE_ID=="Omaha"] <- "NE"
yd <- yd[!duplicated(yd),]
yd <- yd[!is.na(yd$YouthID),]
for(s in unique(yd$SITE_ID)){
	theYouth <- yd$YouthID[yd$SITE_ID==s]
	for(v in theYouth){
		theVics <- unique(arthurData[arthurData$YouthID.RefV == v & arthurData$SITE_ID==s, "VictimID1V"])
		theVics <- as.character(na.omit(theVics))
		yd[yd$YouthID==v, "NumVictims"] <- length(theVics)
		print(theVics)
	}
}
yd <- merge(yd, theCombinedTables$Intake, by="YouthID")
table(yd$NumVictims, yd$`SITE ID`)
table5 <- table(yd$NumVictims[yd$IntakeDone], yd$`SITE ID`[yd$IntakeDone], useNA="ifany")
table5 <- cbind(table5, Total=rowSums(table5))
table5 <- rbind(table5, DK=(colSums(table5)-c(intakeN, sum(intakeN))))
table5
round(t(t(table5)/c(intakeN, sum(intakeN))), 2)


#--------------------------------------
# Victim already in therapy

table(theCombinedTables$Victims$V1I11, theCombinedTables$Victims$SITE_ID)


#--------------------------------------
# Reasons victim did not enroll
data <- theCombinedTables$IntakeV
#data <- 

vnin <- c(
	'victim out of service area', #1
	'victim referred/receiving treatment elsewhere', #2
	'victim did not want services', #3
	'no victim', #4
	'victim is an adult', #5
	'victim is unknown', #6
	'service barriers (e.g. transportation, child care)', #7
	'intake pending', #8
	'system barriers (e.g. victim not referred)', #9
	'youth with PSB not served', #10
	'unable to serve due to special needs', #11
	'Other', #12
	'DK'
)

vnen <- c(
	'victim referred/receiving treatment elsewhere', # 1
	'victim did not want services', # 2
	'victim out of service area', # 3
	'service barriers (e.g. transportation, child care)', # 4
	'victim did not follow through with services', # 5
	'youth with SBP not served by agency', # 6
	'trying to enroll victim for services', # 7
	'no trauma symptoms, treatment not recommended', # 8
	'family seen but victim not seen individually', # 9
	'unable to serve due to special needs', #10
	'victim not part of youth social network', #11
	'victim unknown', #12
	'victim could not participate due to age', #13
	'victim and/or family not referred', #14
	'other', #15
	'DK'
)

# bad intake names to recode
names(table(factor(data$VicNoIntakeR1[!(data$VicNoIntakeR1 %in% 1:12)])))

data$VicNoIntakeR1[data$VicNoIntakeR1 %in% "Child not treated - acted out"] <- '12'
data$VicNoIntakeR1[data$VicNoIntakeR1 %in% "parent declined to return calls"] <- '12'
data$VicNoIntakeR1 <- factor(data$VicNoIntakeR1, levels=c(1:12, 'DK'), labels=vnin)

tableVicNI <- table(data$VicNoIntakeR1, data$SITE_ID)
tableVicNI["DK",] <- (vicN-tabV["1",])-colSums(tableVicNI[-length(vnin),])


tableVicNIFull <- mkptable(tableVicNI, c(colSums(tableVicNI), sum(tableVicNI)))

write.csv(file='tableVictimNoIntake.csv', tableVicNIFull)


# bad enroll names to recode
names(table(factor(data$VicNoEnroll[!(data$VicNoEnroll %in% 1:15)])))

data$VicNoEnroll <- factor(data$VicNoEnroll, levels=c(unique(c(levels(data$VicNoEnroll), 1:15))))
data$VicNoEnroll[data$VicNoEnroll %in% ""] <- 'DK'
data$VicNoEnroll[data$VicNoEnroll %in% "NA- Info not available to parent"] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "acted out with neighbor"] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "collaborated with therapist"] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "Family dropped out."] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "not referred; acted out on cousin"] <- '14'
data$VicNoEnroll[data$VicNoEnroll %in% "referred out due to capacity issues"] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "victim never referred"] <- '14'
data$VicNoEnroll[data$VicNoEnroll %in% "parent declined services for victim"] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "Family did not engage in services."] <- '15'
data$VicNoEnroll[data$VicNoEnroll %in% "Working on coordinating services with the family."] <- '15'
data$VicNoEnroll <- factor(data$VicNoEnroll, levels=c(1:15, 'DK'), labels=vnen)

# Require intake
sel <- data$IGtx131 %in% "1"
tableVicNE <- table(data$VicNoEnroll[sel], data$SITE_ID[sel])
tableVicNE["DK",] <- (tabV["1",])-colSums(tableVicNE[-length(vnen),])



tableVicNEFull <- mkptable(tableVicNE, c(colSums(tableVicNE), sum(tableVicNE)))

write.csv(file='tableVictimNoEnroll.csv', tableVicNEFull)

#sel <- data$Dateofentry > as.POSIXct(as.Date("2014-07-01"))
#vne <- data.frame(SITE_ID=data$`SITE ID`[sel], IGtx14=data$IGtx14[sel], Dateofentry=data$Dateofentry[sel])
#vne[!is.na(vne$IGtx14), ]

#vneAll <- data.frame(SITE_ID=data$`SITE ID`, VictimID1=data$VictimID1, IGtx14=data$IGtx14)
#write.csv(na.omit(vneAll), file="ReasonVictimDidNotEnroll.csv", row.names=FALSE)







#--------------------------------------
# Look at distribution of victim treatment start dates
data <- theCombinedTables$IntakeV
data$IGtx131[data$IGtx131==" 1"] <- "1"
vicEnroll <- factor(data$IGtx131, levels=c("0", "1", "DK"), labels=c("No", "Yes", NA))
vicStart <- as.Date(as.POSIXlt(data$IGtx13b1, origin="1582-10-15"))
vicStart[vicEnroll=="Yes" & !is.na(vicEnroll)]
vicStartGood <- vicStart[vicStart > as.Date("2000-01-01") & vicStart < as.Date("2017-07-31") & !is.na(vicStart)]
breakDates <- as.Date(paste(paste(rep(2011:2018, each=4), rep(c('01', '04', '07', '10'), times=7), sep='-'), '01', sep='-'))
hist(vicStartGood, breaks=6, freq=TRUE)


png('plotQuarterlyVictimEnrollments.png', width=600, height=480)
hist(vicStartGood, breaks=breakDates, freq=TRUE, xaxt='n', xlab='Year', main='Quaterly Number of Victim Enrollments')
axis(side=1, at=breakDates[seq(1, length(breakDates), by=4)], labels=2011:2018)
axis(side=2, at=seq(0, 12, by=2))
dev.off()


vicStartTable <- table(vicStartGood)
vicStartDates <- as.Date(names(vicStartTable))
dateRange <- as.numeric(range(vicStartDates))

sim.rows <- 10000
sim.mat <- matrix(NA, nrow=sim.rows, ncol=length(vicStartTable))
for(i in 1:sim.rows){
	rancum <- runif(length(vicStartTable))
	sim.mat[i,] <- cumsum(rancum)/sum(rancum)*sum(vicStartTable)
}

sim.res <- apply(sim.mat, 2, quantile, probs=c(0.025, 0.975))

png("plotCumulativeVictimsUnif.png")
plot(vicStartDates, cumsum(vicStartTable), xlab="Date", ylab="Number of Enrolled Victims", main="Cumulative Enrolled Victims over Time with Uniform Model", pch=16)
lines(seq(dateRange[1], dateRange[2], length.out=length(vicStartDates)), sim.res[1,], lty=2)
lines(seq(dateRange[1], dateRange[2], length.out=length(vicStartDates)), sim.res[2,], lty=2)
lines(seq(dateRange[1], dateRange[2], length.out=length(vicStartDates)), colMeans(sim.res), lty=1)
dev.off()
# Looks like enrollment dates are NOT approximately uniformly distributed
# 

exp.cdf <- function(x, data, area){
	area*(1-exp(-x*data))
}

buildfun <- function(param, dataX, dataY){
	pred <- exp.cdf(param, dataX, dataY[length(dataY)])
	return(sum((dataY - pred)^2))
}

x <- as.numeric(vicStartDates)-as.numeric(vicStartDates[1])
y <- cumsum(vicStartTable)

exp.res <- optimize(f=buildfun, dataX=x, dataY=y, interval=c(0, 2))

pred.y <- exp.cdf(exp.res$minimum, data=x, area=y[length(y)])

plot(vicStartDates, y)
lines(vicStartDates, pred.y)

exp.nlm <- nlm(f=buildfun, dataX=x, dataY=y, p=0.05, fscale=300, hessian=TRUE)

sim.rows <- 10000
sim.mat <- matrix(NA, nrow=sim.rows, ncol=length(vicStartTable))
for(i in 1:sim.rows){
	rancum <- rexp(length(vicStartTable), rate=exp.res$minimum)
	sim.mat[i,] <- sort(rancum) - min(rancum) + dateRange[1]# /max(rancum)*dateRange[2] + dateRange[1]#sum(vicStartTable)
}

sim.res <- apply(sim.mat, 2, quantile, probs=c(0.025, 0.975))

sim.len <- ncol(sim.res)

png("plotCumulativeVictimsExp.png")
plot(vicStartDates, cumsum(vicStartTable), xlab="Date", ylab="Number of Enrolled Victims", main="Cumulative Enrolled Victims over Time with Exponential Model", pch=16)
lines(sim.res[1,], (1:sim.len)/sim.len*sum(vicStartTable), lty=2)
lines(sim.res[2,], (1:sim.len)/sim.len*sum(vicStartTable), lty=2)
lines(vicStartDates, pred.y)
dev.off()

#--------------------------------------
# Apology session dates



findVariable("ApologyS", theCombinedTables)


data <- theCombinedTables$Exit

table(data$ApologyS, data$`SITE ID`)

data$ApologyS[data$ApologyS %in% 99] <- NA

apologyDate <- as.Date(as.POSIXlt(data$ApologyDate, origin="1582-10-15"))
apologyDateGood <- apologyDate[apologyDate > as.Date("2000-01-01") & !is.na(apologyDate)]
apologyDateGood[order(apologyDateGood)]
hist(apologyDateGood, breaks=4)
diff(apologyDateGood[order(apologyDateGood)])

bmp("plotDiffApology.bmp", width=600, height=480)
plot(
	as.Date(round(embed(as.numeric(apologyDateGood[order(apologyDateGood)][-1]), 7) %*% matrix(1/7, nrow=7)), origin="1970-01-01"),
	embed(as.numeric(diff(apologyDateGood[order(apologyDateGood)])), 7) %*% matrix(1/7, nrow=7),
	xlab="Date in 6-Month Intervals",
	ylab="Time Difference in Days",
	xaxt="n",
	main="Running Mean Time Difference Between Apology Dates",
	type='o'
)
charDates <- c('2011-12-01', paste(rep(2012:2015, each=2), rep(c('06', '12'), times=4), '01', sep='-'))
axDates <- as.Date(charDates)
axis(side=1, at=axDates, labels=charDates)
dev.off()



png('plotQuarterlyApologySessions.png', width=600, height=480)
hist(apologyDateGood, breaks=breakDates, freq=TRUE, xaxt='n', xlab='Year', main='Quaterly Number of Apology Sessions')
axis(side=1, at=breakDates[seq(1, length(breakDates), by=4)], labels=2011:2018)
axis(side=2, at=seq(0, 12, by=1))
dev.off()

