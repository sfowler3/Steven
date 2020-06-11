#--------------------------------------
# Table 3: Demographics
data <- merge(theCombinedTables$Referral, theCombinedTables$Intake, all=TRUE, by=c("SITE ID", "SITE_ID", "YouthID"))

data$YSI06l_sexreg <- factor(data$YSI06l_sexreg, levels=c(0, 1), labels=c("no", "yes"))
data$FRY01[data$FRY01 %in% c("M", "m")] <- 0

if(forOjjdpPaper){
	data <- data[data$IGtx11 %in% 1,]
}

arthurData <- data


(genTab <- table(arthurData$FRY01, arthurData$SITE_ID)) #Youth gender
###---###
genRow <- mkptable(genTab["0", , drop=FALSE], c(colSums(genTab), sum(genTab)))
###---###
write.csv(file='tableGender.csv', mkptable(genTab, c(colSums(genTab), sum(genTab))))


youthAgeAtReferral <- c((as.Date(as.POSIXlt(arthurData$FR_date, origin="1582-10-15")) - as.Date(as.POSIXlt(arthurData$FRY02, origin="1582-10-15")))/365.25)
youthAgeAtReferral <- as.numeric(youthAgeAtReferral)
youthAgeAtReferral[youthAgeAtReferral < 1] <- NA
youthAgeAtReferral[youthAgeAtReferral > 100] <- NA
summary(youthAgeAtReferral)
meansd(youthAgeAtReferral)
(ageTab <- aggregate(youthAgeAtReferral ~ arthurData$SITE_ID, FUN=meansd))

###---###
ageRow <- data.frame(t(c(ageTab[,2], meansd(youthAgeAtReferral))))
names(ageRow) <- c(theSites, 'Total')
###---###
write.csv(file='tableAge.csv', ageRow)


# do sites differ in age at referral
lmAge <- lm(youthAgeAtReferral ~ SITE_ID, data=arthurData)
summary(lmAge)
fcat(lmAge)
require(multcomp)
sulmAge <- summary(mclmAge <- glht(lmAge, linfct=mcp(SITE_ID="Tukey")))
(ests <- sulmAge$test$coefficients[abs(sulmAge$test$tstat) > 3])
df <- sulmAge$df
(tstat <- sulmAge$test$tstat[abs(sulmAge$test$tstat) > 3])
(pval <- sulmAge$test$pvalues[abs(sulmAge$test$tstat) > 3])
t2d(tstat, df) # Cohen's d

tcatpost(tstat, df, pval)

b <- confint(mclmAge)
png('plotAgeDiff2.png', width=480/7*10, height=480/7*10)
b$confint <- b$confint[order(b$confint[,3]),]
#b$confint <- b$confint[order(b$confint[,1]),]
par(mar=c(5, 12, 4, 2) + 0.1)
plot(b, xlab='Age Difference (Years)', main='95% family-wise confidence level')
dev.off()


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

raceArray <- array(unlist(lapply(youthRaceData, table, y=arthurData$SITE_ID, useNA="ifany")), dim=c(2, length(theSites), length(youthRaceNames)))
raceTable <- t(raceArray[2,1:length(theSites),])
colnames(raceTable) <- sort(unique(arthurData$SITE_ID))
rownames(raceTable) <- matrix(unlist(strsplit(youthRaceNames, split="_")), 2)[2,]
raceTable[nrow(raceTable),] <- colSums(raceArray[,,1])[1:length(theSites)] - colSums(raceTable[1:(length(youthRaceNames)-1), ])[1:length(theSites)]
###---###
raceRow <- raceTable
###---###

(raceTableFull <- mkptable(raceTable, c(siteN, totalN)))
write.csv(raceTableFull, file='tableRace.csv')


#------------------
# Race dot chart

racePT <- raceTable
racePT["Other",] <- colSums(racePT[c(3, 4, 6),]) # combine Asian, Indian, and Other
racePT <- racePT[c("White", "Black", "Hispanic", "Other"),]
racePTP <- mkpercent(racePT, colSums(racePT))

(raceChi <- chisq.test(racePT))
sqrt(raceChi$statistic/(sum(raceChi$observed)*min(dim(raceChi$observed))))


#colnames(racePTP) <- paste(colnames(racePTP), "-", c("CII", "RSAFE", "WISE"))

png(file='plotRaceEthnicityOfYouth.png', height=600+120, width=480)
dotchart(racePTP[4:1,]*100, xlim=c(0,100), xlab="Percent", main='Race/Ethnicity of Referred Youth')
dev.off()



#------------------
# Race stacked barplot
png('plotRaceEthnicityOfYouthBarplot.png', height=480+120*2, width=600+120*4)
counts <- t(t(racePT)/colSums(racePT))*100#table(mtcars$vs, mtcars$gear)
#colnames(counts) <- theSite
#colnames(counts) <- paste('Site', 1:length(theSites))
par(mar=c(7, 4, 2, .7) + 0.1, xpd=TRUE)
barplot(counts, main="", ylab='Percent', las=1,
  xlab="Site", col=colorspace::rainbow_hcl(4))
legend("bottomright", legend = rownames(counts), fill=colorspace::rainbow_hcl(4), inset=c(0, -.13), horiz=TRUE, cex=1.5)
#colorspace::rainbow_hcl
#colorspace::sequential_hcl
#colorspace::heat_hcl
#colorspace::terrain_hcl
#colorspace::diverge_hcl
dev.off()




#---------------


ycRel <- c("Birth parent", "Adoptive parent", "Step parent", "Foster parent", "Grandparent", "Other relative", "Other nonrelative")
datay <- theCombinedTables$Referral
datac <- theCombinedTables$ReferralC
# Recode odd values of relationship
datay$FRC01[!(datay$FRC01 %in% 1:7)] <- NA
datay$FRC01 <- factor(datay$FRC01)
datac$FRC01[!(datac$FRC01 %in% 1:7)] <- NA
datac$FRC01 <- factor(datac$FRC01)
(rely <- table(datay$FRC01, datay$`SITE ID`))
(relc <- table(datac$FRC01, datac$`SITE ID`))
rownames(rely) <- ycRel[as.numeric(rownames(rely))]
rownames(relc) <- ycRel[as.numeric(rownames(relc))]


# TODO COMMENT OUT LINE BELOW FOR PA/TX SITES
arthurData <- merge(arthurData, theCombinedTables$ReferralC, all=TRUE, by=c("SITE ID", "SITE_ID", "YouthID"))
#arthurData <- merge(arthurData, theCombinedTables$ReferralC, all.x=TRUE, all.y=FALSE, by=c("SITE ID", "SITE_ID", "YouthID"))
arthurData <- arthurData[!is.na(arthurData$YouthID), ] # Drop badly merged rows
#Other demographics
if(forOjjdpPaper){
	arthurData <- arthurData[arthurData$IGtx11 %in% 1,]
}


del <- arthurData[arthurData$FRC01.x != arthurData$FRC01.y, c("SITE ID", "YouthID", "CaregiverID.x", "CaregiverID.y", "FRC01.x", "FRC01.y")]
del <- del[!is.na(del$`SITE ID`),]
write.csv(file='ReportNEConflictedYouthCaregiverRelationship.csv', del)

write.csv(genRow, file='tableGender.csv')
write.csv(ageRow, file='tableAge.csv')
raceRow

# Cleaning in prep for demTable
table(arthurData$FRY04)
arthurData$FRY04[arthurData$FRY04=="E"] <- 1
arthurData$FRY04[arthurData$FRY04=="English"] <- 1
arthurData$FRY04[arthurData$FRY04==0] <- "DK"

arthurData$FRY04Other[arthurData$FRY04Other==0] <- NA
arthurData$FRY04Other[arthurData$FRY04Other==99] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=='none'] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="N/A"] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="English"] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="DK"] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="Spanish"] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="Spanish, secondary"] <- NA
arthurData$FRY04Other[arthurData$FRY04Other=="Speaks/comprehends some Spanish"] <- NA
arthurData$FRY04Other <- factor(arthurData$FRY04Other)

summary(arthurData$FRY04Other)


# People used the Other option to spec English as a second language
del <- arthurData[arthurData$FRY04Other=="English", c('SITE_ID', 'YouthID', 'FRY04', 'FRY04Other')]
del[!is.na(del$SITE_ID),]

del <- arthurData[arthurData$FRY04Other=="DK", c('SITE_ID', 'YouthID', 'FRY04', 'FRY04Other')]
del[!is.na(del$SITE_ID),]

del <- arthurData[arthurData$FRY04Other=="Spanish", c('SITE_ID', 'YouthID', 'FRY04', 'FRY04Other')]
del[!is.na(del$SITE_ID),]

del <- arthurData[arthurData$SITE_ID=="SC", c('SITE_ID', 'YouthID', 'CaregiverID', 'FRC03')]
del[!is.na(del$SITE_ID),]

#tabCYRel <- table(factor(arthurData$FRC01, levels=sort(unique(c(1:7, arthurData$FRC01)))), arthurData$SITE_ID)

# Filter out badly merged / double counted YouhtIDs
filteredArthurData <- arthurData[, c('SITE_ID', 'YouthID', 'FRC01.y')]
dim(filteredArthurData)
filteredArthurData <- filteredArthurData[!duplicated(filteredArthurData[, c('SITE_ID', 'YouthID')]),]
dim(filteredArthurData)

tabCYRel <- table(factor(filteredArthurData$FRC01.y, levels=sort(unique(c(1:7, filteredArthurData$FRC01.y)))), filteredArthurData$SITE_ID)
class(tabCYRel) <- "matrix"
tabCYRel <- as.data.frame(tabCYRel)
tabCYRel['DK',] <- siteN - colSums(tabCYRel[1:7,])
tabCYRel <- as.matrix(tabCYRel)

arthurData[arthurData$SITE_ID %in% 'CT', c('SITE_ID', 'YouthID', 'FRC01.y')]

demTable <- rbind(
	Spanish=table(arthurData$FRY04, arthurData$SITE_ID)["2",], #language spanish
	OtherLang=colSums(table(arthurData$FRY04Other, arthurData$SITE_ID)), #language other
	FemCare=table(arthurData$FRC03, arthurData$SITE_ID)[2,], #caregiver is female
	tabCYRel, #caregiver relationship to youth
	MotherTerm=table(arthurData$FRC07Mother, arthurData$SITE_ID)["5",], #mother rights terminated
	FatherTerm= if(any(arthurData$FRC07Father %in% 5)) {table(arthurData$FRC07Father, arthurData$SITE_ID)["5",]} else {numeric(length(theSites))}, #father rights terminated, also father unknown
	FatherUnk=if(any(arthurData$FRC07Father %in% 8)) {table(arthurData$FRC07Father, arthurData$SITE_ID)["8",]} else {numeric(length(theSites))}, #Father unknown
	ChangePlc=table(arthurData$FRC02.y, arthurData$SITE_ID)[2,] #children change placement due to SBP
)
(demTableFull <- mkptable(demTable, c(siteN, totalN)))

demTableFull2 <- rbind(
	YouthIsMale=genRow, #gender
	Age=ageRow, #age at referral
	RaceEthnicity=rep("", length(theSites)+1),
	mkptable(raceRow, c(siteN, totalN)),
	BLANK=rep("", length(theSites)+1),
	demTableFull
)

write.csv(demTableFull2, file='tableDem.csv')

# Primary Caregiver gender fiddle - percentage of female caregivers known, not total number of caregivers
round(table(arthurData$FRC03, arthurData$SITE_ID)[2,]/colSums(table(arthurData$FRC03, arthurData$SITE_ID)[1:2,]), 2)*100
round(sum(table(arthurData$FRC03, arthurData$SITE_ID)[2,])/sum(table(arthurData$FRC03, arthurData$SITE_ID)[1:2,]), 2)*100


# Changes in placement
summary(as.table(table(arthurData$FRC02.y, arthurData$SITE_ID)[1:2, ])) #two factor are not independent

sum(as.table(table(arthurData$FRC02.y, arthurData$SITE_ID)[1:2, ])) # calculate N used


expFreq <- sum(table(arthurData$FRC02.y, arthurData$SITE_ID)[2, ])/totalN * colSums(table(arthurData$FRC02.y, arthurData$SITE_ID)[,])
obsFreq <- table(arthurData$FRC02.y, arthurData$SITE_ID)[2, ]

chisq.value <- sum(((obsFreq-expFreq)**2)/expFreq)
1-pchisq(chisq.value, df=2)

indSitePlace <- chisq.test(table(arthurData$FRC02.y, arthurData$SITE_ID)[1:2,])
indSitePlace$observed
indSitePlace$expected

t(round(indSitePlace$residual, 1))

write.csv(file='tableChangePlacement.csv', t(round(indSitePlace$residual, 1)))

# Cramer's V/Phi effect size
sqrt(indSitePlace$statistic/(sum(indSitePlace$observed)*min(dim(indSitePlace$observed))))


#------------------------------------------------
# For color bar chart on Race

write.csv(file='tableRacePercent.csv', mkpercent(raceRow, siteN))



