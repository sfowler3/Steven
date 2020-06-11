
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

#-----------
# Caregivers
#-----------


cbnames <- paste("SC0", 1:8, "B", sep="")
canames <- paste("SC0", 1:8, "A", sep="")

cdata <- theCombinedTables$SatisC

round(colMeans(cdata[,cbnames], na.rm=TRUE)[c(5, 6, 7, 2)], 2)
round(colMeans(cdata[,canames], na.rm=TRUE)[c(5, 6, 7, 2)], 2)

range(colMeans(cdata[,canames], na.rm=TRUE))

caregiverBeforeAfterMeans <- cbind(
	colMeans(cdata[,cbnames], na.rm=TRUE)[c(5, 6, 7, 2)],
	colMeans(cdata[,canames], na.rm=TRUE)[c(5, 6, 7, 2)]
)
write.csv(file='tableSatisC.csv', caregiverBeforeAfterMeans)


# sample size
colSums(cdata[,canames] > 0, na.rm=TRUE)
mean(colSums(cdata[,canames] > 0, na.rm=TRUE))


# Stats
for(i in 1:length(canames)){
	tcat(t.test(cdata[,canames[i]]-cdata[,cbnames[i]]))
}

#------
# Youth
#------

ybnames <- paste("SC0", 1:4, "B", sep="")
yanames <- paste("SC0", 1:4, "A", sep="")

ydata <- theCombinedTables$SatisY

round(colMeans(ydata[,ybnames], na.rm=TRUE), 2)
round(colMeans(ydata[,yanames], na.rm=TRUE), 2)

range(colMeans(ydata[,yanames], na.rm=TRUE))


youthBeforeAfterMeans <- cbind(
	colMeans(ydata[,ybnames], na.rm=TRUE),
	colMeans(ydata[,yanames], na.rm=TRUE)
)
write.csv(file='tableSatisY.csv', youthBeforeAfterMeans)

# sample size
colSums(ydata[,yanames] > 0, na.rm=TRUE)
mean(colSums(ydata[,yanames] > 0, na.rm=TRUE))

# Stats
for(i in 1:length(yanames)){
	tcat(t.test(ydata[,yanames[i]]-ydata[,ybnames[i]]))
}



#------------------------------------------------------------------------------
# Summary statistics on you who completed the youth statisfaction survey
# These were requested by Jane for a paper that is reporting them
# 2017-08-18


ydata <- theCombinedTables$SatisY
ydatas <- ydata[apply(ydata[,yanames], 1, function(x) !all(is.na(x))),]
ydatas <- ydatas[!(ydatas$`SITE ID` %in% c('PA', 'TX')), ]
ydatas$YID <- paste0(ydatas$`SITE ID`, ydatas$YouthID)

cdata <- theCombinedTables$SatisC
cdatas <- cdata[apply(cdata[,canames], 1, function(x) !all(is.na(x))),]
cdatas <- cdatas[!(cdatas$`SITE ID` %in% c('PA', 'TX')), ]
cdatas$CID <- paste0(cdatas$`SITE ID`, cdatas$CaregiverID)

edata <- theCombinedTables$Exit
edatas <- edata[!(edata$`SITE ID` %in% c('PA', 'TX')), ]
edatas$YID <- paste0(edatas$`SITE ID`, edatas$YouthID)

rdata <- theCombinedTables$Referral
rdatas <- rdata[!(rdata$`SITE ID` %in% c('PA', 'TX')), ]
rdatas$YID <- paste0(rdatas$`SITE ID`, rdatas$YouthID)

cedata <- theCombinedTables$ExitC
cedatas <- cedata[!(cedata$`SITE ID` %in% c('PA', 'TX')), ]
cedatas$CID <- paste0(cedatas$`SITE ID`, cedatas$CaregiverID)

crdata <- theCombinedTables$ReferralC
crdatas <- rdata[!(crdata$`SITE ID` %in% c('PA', 'TX')), ]
crdatas$CID <- paste0(crdatas$`SITE ID`, crdatas$CaregiverID)

edatar <- edatas[edatas$YID %in% ydatas$YID,]
rdatar <- rdatas[rdatas$YID %in% ydatas$YID,]
cedatar <- cedatas[cedatas$CID %in% cdatas$CID,]
crdatar <- crdatas[crdatas$CID %in% cdatas$CID,]

#--------------------------------------
numSess <- aggregate(cbind(PEY03, PEY04, PEC05, PEC06)~`SITE ID`, meansd, data=edatar)
numSess <- rbind(numSess, c(NA, sapply(edatar[,sessionVars], meansd)))
tableNumSess <- t(numSess[,-1])
colnames(tableNumSess) <- numSess[,1]
colnames(tableNumSess)[ncol(tableNumSess)] <- 'Total'
rownames(tableNumSess) <- c('Group Youth Sessions', 'Indiv Youth Sessions', 'Group Caregiver Sessions', 'Family Sessions')
tableNumSess

exitYN <- table(edatar$`SITE ID`)
exitYN <- exitYN[!(names(exitYN) %in% c('PA', 'TX'))]
exitYN <- exitYN[exitYN != 0]
exitYN <- c(exitYN, Total=sum(exitYN))

tableNumSess <- rbind(N=exitYN, tableNumSess)
write.csv(tableNumSess, file="tableNumSessSatis.csv")


#--------------------------------------
# Primary Reason service ended (PE01) – drop down text

data <- merge(edatar, theCombinedTables$Intake, all.x=TRUE, all.y=FALSE, by=c("SITE ID", "SITE_ID", "YouthID"))

data$PE01 <- factor(data$PE01, levels=1:11)


tableGraduated <- table(data$PE01[data$IGtx11 %in% "1"], data$`SITE ID`[data$IGtx11 %in% "1"], useNA='ifany')

rownames(tableGraduated) <- gna[as.numeric(rownames(tableGraduated))]
tableGraduated <- rbind(InServ=insHardCode, tableGraduated)
tableGraduated <- tableGraduated[, !(colnames(tableGraduated) %in% c('PA', 'TX'))]
tableGraduatedFull <- mkptable(tableGraduated, c(colSums(tableGraduated), sum(colSums(tableGraduated))))
tableGraduatedFull <- rbind(N=c(colSums(tableGraduated), sum(colSums(tableGraduated))), tableGraduatedFull)

write.csv(tableGraduatedFull, file='tableProgramExitReasonSatis.csv')


#--------------------------------------
# Youth charged - %  for categories
# Youth SBP Treatment ordered % for categories

data <- merge(rdatar, theCombinedTables$Intake, all.x=TRUE, all.y=FALSE, by=c("SITE_ID", "SITE ID", "YouthID"))


data$YSI06l_sexreg <- factor(data$YSI06l_sexreg, levels=c(0, 1), labels=c("no", "yes"))

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
	table(arthurData$YSI04, arthurData$SITE_ID)["2",] + table(arthurData$YSI04, arthurData$SITE_ID)["3",], # Deferred/Pending
	table(arthurData$YSI06l_sexreg, arthurData$SITE_ID)["yes",],  #sex offender registration?
	table(arthurData$YSI06m_snotify, arthurData$SITE_ID)["1",],  #Notify Schools
	table(arthurData$YSI06g_sbp, arthurData$SITE_ID)["1",],    #Court required tx
	table(arthurData$YSI06h_parent_tx, arthurData$SITE_ID)["1",],    #Court required parent tx
	table(arthurData$YSI06k_minor, arthurData$SITE_ID)["1",]    #No contact with minors?
)


table1a <- table(arthurData$YSI10, arthurData$SITE_ID)    # youth in child welfare
# 1 = Yes
# 2 = In Past
# 3 = No
# 0 = Missing?

table1 <- rbind(table1, table1a["1",] + table1a["2",])
rownames(table1) <- c("Charged?", "Adjudicated?", "Deferred/Pending?", "Sex Offender Registration?", "Notify schools?", "Court required Tx", "Court required parent tx", "No contact with minors", "Child Welfare Involved")


table1


tab1N <- table(arthurData$`SITE ID`)

(table1full <- mkptable(table1, c(tab1N, sum(tab1N))))
table1full <- rbind(N=c(tab1N, sum(tab1N)), table1full)
table1full <- table1full[,!(colnames(table1full) %in% c('PA', 'TX'))]
write.csv(file='tableLegalInvolvementSatis.csv', table1full)


#--------------------------------------
# Number of sessions for Caregivers

numSess <- aggregate(cbind(PEY04, PEC06, PEY03)~`SITE ID`, meansd, data=cedatar)
# Indiv Sess, Fam Sess, Group Sess

numSess <- rbind(numSess, c(NA, sapply(cedatar[,c('PEY04', 'PEC06', 'PEY03')], meansd)))
tableNumSess <- t(numSess[,-1])
colnames(tableNumSess) <- numSess[,1]
colnames(tableNumSess)[ncol(tableNumSess)] <- 'Total'
rownames(tableNumSess) <- c('Indiv Sess', 'Fam Sess', 'Group Sess')
tableNumSess

exitCN <- table(cedatar$`SITE ID`)
exitCN <- exitCN[!(names(exitCN) %in% c('PA', 'TX'))]
exitCN <- exitCN[exitCN != 0]
exitCN <- c(exitCN, Total=sum(exitCN))

tableNumSess <- rbind(N=exitCN, tableNumSess)


write.csv(tableNumSess, file="tableNumSessCaregiverSatis.csv")


#--------------------------------------
# Caregiver-youth relationships according to caregivers form

tabRelC <- table(crdatar$FRC01, crdatar$`SITE ID`, useNA='ifany')
rownames(tabRelC)[rownames(tabRelC) %in% as.character(1:7)] <- c("Birth parent", "Adoptive parent", "Step parent", "Foster parent", "Grandparent", "Other relative", "Other nonrelative")

tabRelC <- tabRelC[, !(colnames(tabRelC) %in% c('PA', 'TX'))]
tabRelCFull <- mkptable(tabRelC, c(colSums(tabRelC), sum(colSums(tabRelC))))
tabRelCFull <- rbind(N=c(colSums(tabRelC), sum(colSums(tabRelC))), tabRelCFull)

write.csv(tabRelCFull, file='tableCaregiverRelationshipSatis.csv')


