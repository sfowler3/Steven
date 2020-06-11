

#%in% 1   vs != 0
enrolledYouth <- theCombinedTables$Intake[theCombinedTables$Intake$IGtx11 != 0, c('YouthID', 'SITE ID')]
me <- base::merge(enrolledYouth, ysbpData, all=FALSE, by.x=c('YouthID', 'SITE ID'), by.y=c('YouthID', 'SITE_ID'))
me <- me[!duplicated(me[,1:2]),]

summary(me$InScore)
dim(me[!is.na(me$InScore) & !is.na(me$PostScore_1), ])
dim(me[ !is.na(me$PostScore_1), ])

# Those that have a post but no intake
write.csv(file='dataYsbpPostNoIntake.csv', me[!is.na(me$PostScore_1) & is.na(me$InScore), c('YouthID', 'SITE ID')])

# Those with post but not exit
write.csv(file='dataYsbpPostNoExit.csv', me[!is.na(me$PostScore_1) & is.na(me$OutScore), c('YouthID', 'SITE ID')])

# Those with exit but not intake
write.csv(file='dataYsbpExitNoIntake.csv', me[!is.na(me$OutScore) & is.na(me$InScore), c('YouthID', 'SITE ID')])


me[me$YouthID %in% '001-01-01' & me$`SITE ID` %in% 'SC',]



#--------------------------------------
# Pre-post ysbp scores

require(psych)



data <- theCombinedTables$SBPIntake

isbpnames <- paste("SBP_IP_", sprintf("%02i", 1:30), sep="")
(allLev <- unique(unlist(lapply(data[,isbpnames], function(x){sort(unique(x))}))))

# Re-factorize
data[,isbpnames] <- sapply(data[,isbpnames], factor, levels=allLev)

data[,isbpnames][data[,isbpnames] == "DK"] <- NA
data[,isbpnames][data[,isbpnames] == "99"] <- NA
data[,isbpnames][data[,isbpnames] == "P" & !is.na(data[,isbpnames])] <- "1"
data[,isbpnames][data[,isbpnames] == "p" & !is.na(data[,isbpnames])] <- "1"
data[,isbpnames][data[,isbpnames] == "00" & !is.na(data[,isbpnames])] <- "0"
data[,isbpnames][data[,isbpnames] == "o" & !is.na(data[,isbpnames])] <- "0"
data[,isbpnames][data[,isbpnames] == "0." & !is.na(data[,isbpnames])] <- "0"
data[,isbpnames][data[,isbpnames] == "P0" & !is.na(data[,isbpnames])] <- NA
data[,isbpnames][data[,isbpnames] == "RF" & !is.na(data[,isbpnames])] <- NA
data[,isbpnames][data[,isbpnames] == "[" & !is.na(data[,isbpnames])] <- NA

data[,isbpnames] <- apply(sapply(data[,isbpnames], as.character), 2, as.numeric)

ysbpInScores <- apply(data[,isbpnames], 1, sum.na)
ysbpInScores2 <- rowMeans(data[,isbpnames], na.rm=TRUE)*30


#item means
apply(data[,isbpnames], 2, sum.na)
colMeans(data[,isbpnames], na.rm=TRUE)


#--------------------------------------
# 2016-02-24
# Frequency table for YSBP Item Responses at Intake

dataF <- as.data.frame(lapply(data[,isbpnames], factor, levels=0:3))
dataI <- data
dataI[,isbpnames] <- dataF

ysbpTab <- t(sapply(dataI[,isbpnames], table))
row.names(ysbpTab) <- ysbpItemText

ysbpTab <- t(mkptable(t(ysbpTab), c(rowSums(ysbpTab), sum(ysbpTab))))

write.csv(file='tabYSBPItemFreq.csv', ysbpTab)


# End 2016-02-24
#--------------------------------------


plot(ysbpInScores2, ysbpInScores)

ysbpIn <- data.frame(YouthID=data$YouthID, SITE_ID=data$`SITE ID`, InScore=ysbpInScores, InDate=data$YSBP01)


subset(ysbpIn, ysbpIn$InScore ==106)
subset(data, data$YouthID == '031-01-01')


ysbp.pair.cov <- cov(data[,isbpnames], use="pair")
factanal(covmat=ysbp.pair.cov, factors=1) 
factanal(covmat=cov2cor(ysbp.pair.cov), factors=1)

y.e <- eigen(ysbp.pair.cov)
y.ecor <- eigen(cov2cor(ysbp.pair.cov))

plot(eigen(cov2cor(ysbp.pair.cov))$values, type='o')

y.pca <- princomp(covmat=ysbp.pair.cov, n.obs=nrow(data))
# First 5 components gives 60-80% of variance explained

(ysbp.fmodel <- omega(ysbp.pair.cov, nfactors=3))

# Tried various number of factors and generally found
#  strong positive loadings of every variable on the general factor
#  exception of
#   10. Shares sexual pictures or videos of self with others.
#   11. Pushes others to share sexual pictures or videos of themselves.
#   27. Has been arrested, charged, or interviewed by the authorities (such as police or Child Welfare) for a possible illegal sexual behavior.
#   28. Has molested a child or been accused.
#   30. Any other concerning sexual behavior – Describe:

omega(ysbp.pair.cov, nfactors=3)$alpha
# alpha reliability of 0.9044607


#--------------------------------------
dataIntakeItems <- data[,c(2:5,  match(isbpnames, names(data)))]

#--------------------------------------

data <- theCombinedTables$SBPExit

esbpnames <- paste("SBP_EP_", sprintf("%02i", 1:30), sep="")
unique(unlist(lapply(data[,esbpnames], function(x){sort(unique(x))})))

data[,esbpnames][data[,esbpnames] == "DK"] <- NA
data[,esbpnames][data[,esbpnames] == "7"] <- NA
data[,esbpnames][data[,esbpnames] == "p"] <- 1
data[,esbpnames][data[,esbpnames] == "P"] <- 1



data[,esbpnames] <- apply(sapply(data[,esbpnames], as.character), 2, as.numeric)
dataExit <- data

ysbpOutScores <- apply(data[,esbpnames], 1, sum.na)
ysbpOutScores2 <- rowMeans(data[,esbpnames], na.rm=TRUE)*30


#item means
apply(data[,esbpnames], 2, sum.na)
colMeans(data[,esbpnames], na.rm=TRUE)

par(mfrow=c(6, 5), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
lapply(data[,esbpnames], hist, xlim=c(0, 3))


subset(data, data[,esbpnames][30]==1)

ysbpEx <- data.frame(YouthID=data$YouthID, SITE_ID=data$`SITE ID`, OutScore=ysbpOutScores, OutDate=data$YSBP01e)

plot(ysbpOutScores2, ysbpOutScores)

hist(ysbpOutScores)

ysbpData <- merge(ysbpIn, ysbpEx, all=TRUE, by=c("YouthID", "SITE_ID"))

(prePostT <- t.test(ysbpData$InScore, ysbpData$OutScore, paired=TRUE))
tcat(prePostT)
# t(137) = 12.48, p < 0.001, d =  2.13


aggregate(InScore ~ SITE_ID, data=ysbpData, meansd)
aggregate(OutScore ~ SITE_ID, data=ysbpData, meansd)

meansd(ysbpData$InScore)
meansd(ysbpData$OutScore)


png("plotYSBPInOut.png")
#pdf("plotYSBPInOut.pdf")
plot(jitter(rep(1, nrow(ysbpData)), factor=10), jitter(ysbpData$InScore), xlim=c(0.5, 2.5), xaxt='n', xlab='', ylab='YSBP Scores', main="YSBP Intake and Exit Scores")
points(jitter(rep(2, nrow(ysbpData)), factor=5), jitter(ysbpData$OutScore))
axis(side=1, at=c(1, 2), labels=c("Intake", "Exit"))
dev.off()


bmp("plotYSBPInOutNE.bmp")
ysbpDataN <- ysbpData[ysbpData$SITE_ID=="NE", ]
plot(jitter(rep(1, nrow(ysbpDataN)), factor=10), jitter(ysbpDataN$InScore), xlim=c(0.5, 2.5), xaxt='n', xlab='', ylab='YSBP Scores', main="YSBP Intake and Exit Scores for NE")
points(jitter(rep(2, nrow(ysbpDataN)), factor=5), jitter(ysbpDataN$OutScore))
axis(side=1, at=c(1, 2), labels=c("Intake", "Exit"))
dev.off()



# Look for site differnces at intake
# probably should write these to a table
fcat(lm(InScore ~ SITE_ID, data=ysbpData))
(yint <- aggregate(InScore ~ SITE_ID, data=ysbpData, meansdn))
(yintt <- meansdn(ysbpData$InScore))

write.csv(file='tableYSBPIntake.csv', t(data.frame(SITE_ID=c(as.character(yint$SITE_ID), "Total"), InScore=c(yint$InScore, yintt))))

# Look for site differnces at exit
# probably should write these to a table
fcat(lm(OutScore ~ SITE_ID, data=ysbpData))
(yext <- aggregate(OutScore ~ SITE_ID, data=ysbpData, meansdn))
(yextt <- meansdn(ysbpData$OutScore))

write.csv(file='tableYSBPExit.csv', t(data.frame(SITE_ID=c(as.character(yext$SITE_ID), "Total"), OutScore=c(yext$OutScore, yextt))))



# major contraction of the state space


plot(ysbpData$InScore, ysbpData$OutScore)


#--------------------------------------
# Alex Dopp needed

aggregate(InScore ~ SITE_ID, ysbpData, meansd)
aggregate(OutScore ~ SITE_ID, ysbpData, meansd)

aggregate(InScore ~ 1, ysbpData, meansd)
aggregate(OutScore ~ 1, ysbpData, meansd)

tcat(t.test(ysbpData$OutScore - ysbpData$InScore))
print(with(ysbpData, cohen.d(OutScore, InScore, paired=TRUE, na.rm=TRUE)$conf.int*2))
print(paste0("Pre M(SD): ", with(ysbpData, meansd(InScore[!is.na(OutScore)]))))
print(paste0("Post M(SD): ", with(ysbpData, meansd(OutScore[!is.na(InScore)]))))


for(asite in theSites){
	cat(asite, '\n')
	try(with(ysbpData[ysbpData$SITE_ID %in% asite,], tcat(t.test(OutScore - InScore))))
	try(print(with(ysbpData[ysbpData$SITE_ID %in% asite,], cohen.d(OutScore, InScore, paired=TRUE, na.rm=TRUE)$conf.int*2)))
	try(print(paste0("Pre M(SD): ", with(ysbpData[ysbpData$SITE_ID %in% asite,], meansd(InScore[!is.na(OutScore)])))))
	try(print(paste0("Post M(SD): ", with(ysbpData[ysbpData$SITE_ID %in% asite,], meansd(OutScore[!is.na(InScore)])))))
	cat('\n')
}

# End Alex Dopp needed
#--------------------------------------


#--------------------------------------
dataExitItems <- data[,c(2:5,  match(esbpnames, names(data)))]

#--------------------------------------


#--------------------------------------
# Inservice data processing

data <- theCombinedTables$SBPInserv


ssbpnames <- paste("SBP_SP_", sprintf("%02i", 1:30), sep="")
unique(unlist(lapply(data[,ssbpnames], function(x){sort(unique(x))})))


data[,ssbpnames][data[,ssbpnames] == "DK"] <- NA
data[,ssbpnames][data[,ssbpnames] == "Dk"] <- NA
data[,ssbpnames][data[,ssbpnames] == "RF"] <- NA
data[,ssbpnames][data[,ssbpnames] == "P" & !is.na(data[,ssbpnames])] <- "1"
data[,ssbpnames][data[,ssbpnames] == "p" & !is.na(data[,ssbpnames])] <- "1"
data[,ssbpnames][data[,ssbpnames] == "00" & !is.na(data[,ssbpnames])] <- "0"


data[,ssbpnames] <- apply(sapply(data[,ssbpnames], as.character), 2, as.numeric)
data$YSBP01s <- as.Date(data$YSBP01s)

res.s <- list()
res.si <- list()
for(si in unique(data$`SITE ID`)){
	theYouths <- unique(data[data$`SITE ID`==si, "YouthID"])
	for(yo in theYouths){
		sel <- data$`SITE ID`==si & !is.na(data$`SITE ID`==si) & data$YouthID==yo & !is.na(data$YouthID==yo)
		tem <- data[sel, c("SITE_ID", "YouthID", "CaregiverID", "YSBP01s", ssbpnames)][order(data$YSBP01s),]
		tem$Score <- apply(tem[,ssbpnames], 1, sum.na)
		tem <- tem[!is.na(tem$Score),]
		tem <- tem[order(tem$YSBP01s),]
		if(nrow(tem)>0){
			tem$Order <- 1:nrow(tem)
			res.s[[paste(si, yo, sep="")]] <- tem[, c("SITE_ID", "YouthID", "YSBP01s", "Score", "Order")]
			res.si[[paste(si, yo, sep="")]] <- tem[, c("SITE_ID", "YouthID", "CaregiverID", "YSBP01s", ssbpnames, "Score", "Order")]
		}
	}
}

numInTx <- max(sapply(res.s, nrow))

t2w <- function(x){
	dates <- as.list(x$YSBP01s)
	names(dates) <- paste("YSBP01s_", x$Order, sep="")
	scores <- as.list(x$Score)
	names(scores) <- paste("Score_", x$Order, sep="")
	ret <- data.frame(SITE_ID=x$SITE_ID[1], YouthID=x$YouthID[1], dates, scores)
}

del <- lapply(res.s, t2w)
ysbpInservData <- Reduce(function(...) merge(..., all=TRUE), del)

ysbpData <- merge(ysbpData, ysbpInservData, all=TRUE)


#--------------------------------------

t2w <- function(x){
	dates <- as.list(x$YSBP01s)
	names(dates) <- paste("YSBP01s_", x$Order, sep="")
	scores <- as.list(x$Score)
	names(scores) <- paste("Score_", x$Order, sep="")
	items <- lapply(x[,ssbpnames], as.list)
	for(i in 1:length(items)){names(items[[i]]) <- paste(names(items)[i], x$Order, sep="_")}
	names(items) <- NULL
	ret <- data.frame(SITE_ID=x$SITE_ID[1], YouthID=x$YouthID[1], CaregiverID=x$CaregiverID[1], dates, items, scores)
	return(ret)
}

del <- lapply(res.si, t2w)
dataInserviceItems <- Reduce(function(...) merge(..., all=TRUE), del)


#--------------------------------------




#--------------------------------------
# Post-service data processing

data <- theCombinedTables$SBPPost


psbpnames <- paste("PTx_SP_", sprintf("%02i", 1:30), sep="")
unique(unlist(lapply(data[,psbpnames], function(x){sort(unique(x))})))


unique(unlist(sapply(data[,psbpnames], unique)))
data[,psbpnames][data[,psbpnames] == "DK"] <- NA
data[,psbpnames][data[,psbpnames] == "RF"] <- NA


data[,psbpnames] <- apply(sapply(data[,psbpnames], as.character), 2, as.numeric)
data$`PTx Adm` <- as.Date(data$`PTx Adm`)
dataPost <- data

res.p <- list()
res.pi <- list()
for(si in unique(data$`SITE ID`)){
	theYouths <- unique(data[data$`SITE ID`==si, "YouthID"])
	for(yo in theYouths){
		sel <- data$`SITE ID`==si & !is.na(data$`SITE ID`==si) & data$YouthID==yo & !is.na(data$YouthID==yo)
		tem <- data[sel, c("SITE_ID", "YouthID", "CaregiverID", "PTx Adm", psbpnames)][order(data$`PTx Adm`),]
		tem$PostScore <- apply(tem[,psbpnames], 1, sum.na)
		tem <- tem[!is.na(tem$PostScore),]
		tem <- tem[order(tem$`PTx Adm`),]
		if(nrow(tem)>0){
			tem$Order <- 1:nrow(tem)
			res.p[[paste(si, yo, sep="")]] <- tem[, c("SITE_ID", "YouthID", "PTx Adm", "PostScore", "Order")]
			res.pi[[paste(si, yo, sep="")]] <- tem[, c("SITE_ID", "YouthID", "CaregiverID", "PTx Adm", psbpnames, "PostScore", "Order")]
		}
	}
}

del <- data[data$`SITE ID` %in% 'NJ' & data$YouthID %in% '010-01-01', ]
del[order(del$`PTx Adm`),1:10]
duplicated(del[,c(2:40)])

numPostTx <- max(sapply(res.p, nrow))

t2w <- function(x){
	dates <- as.list(x$`PTx Adm`)
	names(dates) <- paste("PTx_Adm_", x$Order, sep="")
	scores <- as.list(x$PostScore)
	names(scores) <- paste("PostScore_", x$Order, sep="")
	ret <- data.frame(SITE_ID=x$SITE_ID[1], YouthID=x$YouthID[1], dates, scores)
}

del <- lapply(res.p, t2w)
ysbpPostData <- Reduce(function(...) merge(..., all=TRUE), del)

ysbpData <- merge(ysbpData, ysbpPostData, all=TRUE)
ysbpData <- ysbpData[order(ysbpData$SITE_ID),]


#--------------------------------------

t2w <- function(x){
	dates <- as.list(x$`PTx Adm`)
	names(dates) <- paste("PTx_Adm_", x$Order, sep="")
	scores <- as.list(x$PostScore)
	names(scores) <- paste("PostScore_", x$Order, sep="")
	ret <- data.frame(SITE_ID=x$SITE_ID[1], YouthID=x$YouthID[1], dates, scores)
	items <- lapply(x[,psbpnames], as.list)
	for(i in 1:length(items)){names(items[[i]]) <- paste(names(items)[i], x$Order, sep="_")}
	names(items) <- NULL
	ret <- data.frame(SITE_ID=x$SITE_ID[1], YouthID=x$YouthID[1], CaregiverID=x$CaregiverID[1], dates, items, scores)
	return(ret)
}

del <- lapply(res.pi, t2w)
dataPostItems <- Reduce(function(...) merge(..., all=TRUE), del)


#--------------------------------------


#--------------------------------------

dataIntakeItems$IntakeScore <- apply(dataIntakeItems[,isbpnames], 1, sum.na)
dataExitItems$ExitScore <- apply(dataExitItems[,esbpnames], 1, sum.na)

names(dataIntakeItems)[names(dataIntakeItems) %in% "SITE ID"] <- "SITE_ID"
names(dataExitItems)[names(dataExitItems) %in% "SITE ID"] <- "SITE_ID"

# Drop cargiver ID
dataIntakeItems$CaregiverID <- NULL
dataInserviceItems$CaregiverID <- NULL
dataExitItems$CaregiverID <- NULL
dataPostItems$CaregiverID <- NULL

allItemNames <- list(names(dataIntakeItems), names(dataInserviceItems), names(dataExitItems), names(dataPostItems))
uniItemNames <- unique(unlist(allItemNames))
length(unlist(allItemNames)) - length(uniItemNames)

sapply(allItemNames, length)
sapply(allItemNames, function(x){length(unique(x))})


# merge item data
mi1 <- merge(dataIntakeItems, dataInserviceItems, all=TRUE)

# people who have intake but no inservices
setdiff(paste(dataIntakeItems$SITE_ID, dataIntakeItems$YouthID), paste(dataInserviceItems$SITE_ID, dataInserviceItems$YouthID))

# people who have inservices but no intake
setdiff(paste(dataInserviceItems$SITE_ID, dataInserviceItems$YouthID), paste(dataIntakeItems$SITE_ID, dataIntakeItems$YouthID))

mi2 <- merge(mi1, dataExitItems, all=TRUE)

# people who have Exit but no intake/inservice
setdiff(paste(dataExitItems$SITE_ID, dataExitItems$YouthID), paste(mi1$SITE_ID, mi1$YouthID))


mi3 <- merge(mi2, dataPostItems, all=TRUE)


# people who have Post but no intake/inservice/exit
setdiff(paste(dataPostItems$SITE_ID, dataPostItems$YouthID), paste(mi2$SITE_ID, mi2$YouthID))



mi3 <- mi3[order(mi3$SITE_ID, as.character(mi3$YouthID)),]

write.csv(file='dataYSBPItems.csv', mi3, row.names=FALSE)


#--------------------------------------

exitToPostDuration <- as.Date(ysbpData$PTx_Adm_1) - as.Date(ysbpData$OutDate)
exitToPostDuration[exitToPostDuration < 0] <- NA

meansd(exitToPostDuration)
#[1] "127.3 (101.5)"
#> 127.3/7
#[1] 18.18571
#> 127.3/365.25*12
#[1] 4.182341 months

# Old/Final: 122.9 (102.4)



#------
require(foreign)
ysbpDataS <- ysbpData
dtna <- c(paste("YSBP01s_", 1:numInTx, sep=""), paste("PTx_Adm_", 1:numPostTx, sep=""))
scna <- c(paste("Score_", 1:numInTx, sep=""), paste("PostScore_", 1:numPostTx, sep=""))
dtna2 <- c(paste("ISDt", 1:numInTx, sep=""), paste("PSDt", 1:numPostTx, sep=""))
scna2 <- c(paste("Score", 1:numInTx, sep=""), paste("Post", 1:numPostTx, sep=""))

ysbpDataS <- ysbpDataS[,c(names(ysbpData)[1:4], dtna, scna)]
names(ysbpDataS) <- c(names(ysbpData)[1:4], dtna2, scna2)
ysbpDataS[,dtna2] <- apply(sapply(ysbpDataS[,dtna2], as.character), 2, gsub, pattern="-", replacement="/")
write.foreign(df=ysbpDataS, datafile="JaneYSBPData.csv", codefile="JaneYSBPData.sps", package="SPSS")

# Duplicated IDs at Intake for NE
#  "121-01-01" "102-01-01" "043-01-01" "138-01-01"
# other sites do not have duplicates.
#------

(midPostT <- t.test(ysbpData$Score_3, ysbpData$OutScore, paired=TRUE))
tcat(midPostT)

ysbp.lm <- lm(OutScore - InScore ~ SITE_ID, data=ysbpData)
anova(ysbp.lm)
fcat(ysbp.lm)

ysbpm.lm <- lm(OutScore - Score_3 ~ SITE_ID, data=ysbpData)
anova(ysbpm.lm)
fcat(ysbpm.lm)

ysbpm.lm <- lm(Score_3 - InScore ~ SITE_ID, data=ysbpData)
anova(ysbpm.lm)
fcat(ysbpm.lm)

(inMidT <- t.test(ysbpData$InScore, ysbpData$Score_3, paired=TRUE))
tcat(inMidT)

tcat(t.test(ysbpData$PostScore_1 - ysbpData$OutScore))
#[1] "t(84) = 1.1, p = 0.27, d =  0.24"


pscoreNames <- grep('PostScore', names(ysbpData), value=TRUE)
atLeastOnePostScore <- apply(ysbpData[,pscoreNames], 1, function(x){any(!is.na(x))})
table(atLeastOnePostScore)


#png("plotYSBPInMidOut.png")
#pdf("plotYSBPInOut.pdf")
tiff("plotYSBPInMidOut2.tif", res=600, width=7, height=7, units="in", compression="lzw")
plot(jitter(rep(1, nrow(ysbpData)), factor=10), jitter(ysbpData$InScore), xlim=c(0.5, 3.5), xaxt='n', xlab='', ylab='YSBP Scores', main="")
points(jitter(rep(2, nrow(ysbpData)), factor=10/2), jitter(ysbpData$Score_3))
points(jitter(rep(3, nrow(ysbpData)), factor=10/3), jitter(ysbpData$OutScore))
axis(side=1, at=1:3, labels=c("Intake", "Midpoint", "Exit"))
dev.off()

png('plotYSBPInMidOutHist.png', width=120*6, height=120*3)
par(mfrow=c(1, 3))
h1 <- hist(ysbpData$InScore, xlab='YSBP Score', main='Intake')
abline(v=mean(ysbpData$InScore, na.rm=TRUE))
hist(ysbpData$Score_3, breaks=h1$breaks, ylim=range(h1$counts), xlab='YSBP Score', main='Midpoint')
abline(v=mean(ysbpData$Score_3, na.rm=TRUE))
hist(ysbpData$OutScore, breaks=h1$breaks, ylim=range(h1$counts), xlab='YSBP Score', main='Exit')
abline(v=mean(ysbpData$OutScore, na.rm=TRUE))
dev.off()


#--------------------------------------
# Item frequencies for program summary

exitFL <- list()#list("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
postFL <- list()
#names(exitFL) <- theSites
#names(postFL) <- theSites

exitFL2 <- exitFL
postFL2 <- postFL

for( asite in theSites ){

	exitF <- sapply(lapply(dataExit[dataExit$`SITE ID`==asite,esbpnames], factor, levels=c(0, 1, 2, 3)), table)
	exitFL[[asite]] <- exitF[, exitF[2,]!=0 | exitF[3,]!=0 | exitF[4,]!=0, drop=FALSE]
	exitFL2[[asite]] <- exitF
	
	postF <- sapply(lapply(dataPost[dataPost$`SITE ID`==asite, psbpnames], factor, levels=c(0, 1, 2, 3)), table)
	postFL[[asite]] <- postF[,postF[2,]!=0 | postF[3,]!=0 | postF[4,]!=0, drop=FALSE]
	postFL2[[asite]] <- postF
}




rcountexit <- sapply(lapply(lapply(dataExit[,esbpnames], function(x){as.numeric(!(x %in% 0) & !is.na(x))}), factor, levels=c(0, 1)), table)

er <- as.data.frame(lapply(lapply(dataExit[,esbpnames], function(x){as.numeric(!(x %in% 0) & !is.na(x))}), factor, levels=c(0, 1)))
er$YouthID <- dataExit$YouthID
er$SITE_ID <- dataExit$`SITE ID`

# Number of youth showing any PSB at exit
recidExit <- sum(apply(er[,esbpnames], 1, function(x){any(x %in% 1)}))
recidExit
nrow(er)
round(recidExit/nrow(er), 2) #proportion


ocountpost <- sapply(lapply(lapply(dataPost[, psbpnames], function(x){as.numeric(!(x %in% 0) & !is.na(x))}), factor, levels=c(0, 1)), table)
pr <- as.data.frame(lapply(lapply(dataPost[, psbpnames], function(x){as.numeric(!(x %in% 0) & !is.na(x))}), factor, levels=c(0, 1)))
pr$YouthID <- dataPost$YouthID
pr$SITE_ID <- dataPost$`SITE ID`

# Number of youth showing any PSB at post
recidPostAll <- apply(pr[,psbpnames], 1, function(x){any(x %in% 1)})
recidPostTab <- table(recidPostAll, paste(pr$YouthID, pr$SITE_ID, sep='_'))
recidPostTabShort <- recidPostTab[,recidPostTab['TRUE',] != 0]
recidPost <- ncol(recidPostTabShort)
recidPost
ncol(recidPostTab)
round(recidPost/ncol(recidPostTab), 2) #proportion



rcountpost <- list()
for(i in psbpnames){
	form <- formula(paste0(i, " ~ SITE_ID + YouthID"))
	rcountpost[[i]] <- sum(aggregate(form, data=pr, FUN=table)[,3][,2] != 0)
}
# Compare two versions of count
rbind(uniqueCount=as.data.frame(rcountpost), totalCount=ocountpost[2,])

#WRITE THIS
write.csv(file='tableRecidivism.csv', t(rbind(rCountExit=rcountexit[2,], rCountPost=as.data.frame(rcountpost))))






dataExit[dataExit$`SITE ID` %in% 'NJ' & dataExit[,esbpnames[30]] %in% 1,]


# Look at non-zero post kid in NE
ps <- apply(dataPost[, psbpnames], 1, sum, na.rm=TRUE)
sel <- dataPost$`SITE ID`=='NE' & ps!=0
cbind(dataPost[sel, c('SITE ID', 'YouthID', psbpnames)], ps[sel])

# Look at the "Other" responses
dataPost[!is.na(dataPost$PTx_SP_38Other) & !(dataPost$PTx_SP_38Other %in% 0), c('SITE ID', 'YouthID', 'PTx_SP_30', 'PTx_SP_38Other')]
dataExit[!(dataExit[,esbpnames[30]] %in% 0), c('SITE ID', 'YouthID', esbpnames[30], "SBP_EP_30Other")]

# Recidivism percent/count
(exitRecid <- table(ysbpData$OutScore!=0, ysbpData$SITE_ID, useNA='ifany'))
exitRecid <- exitRecid[1:2,]
mkptable(exitRecid, c(colSums(exitRecid), sum(exitRecid)))

(postRecid <- table(apply(ysbpData[,paste('PostScore_', 1:21, sep='')], 1, sum, na.rm=TRUE)!=0, ysbpData$SITE_ID, useNA='ifany'))
postRecid <- postRecid[1:2,]
mkptable(postRecid, c(colSums(postRecid), sum(postRecid)))

ysbpItemText <- c(
	"Tries to touch others in a sexual way.",
	"Forces or pressures others to do sexual acts.",
	"Asks others to engage in sexual acts with him or her.",
	"Rubs body against people.",
	"Touches others’ genitals or breasts in a violent, aggressive, or hurtful way.",
	"Shows sex (private) parts to others.",
	"Tries to peek at people when they are nude or undressing.",
	"Looks at pornography on the Internet.",
	"Engages in sexual chat on phone, Internet, or texting.",
	"Shares sexual pictures or videos of self with others.",
	"Pushes others to share sexual pictures or videos of themselves.",
	"Talks about sexual things more than normal for his/her age",
	"Seems focused on sexual media (prefers sexual things on TV, in videos, animation, video games, magazines, pictures, etc.).",
	"Makes unwanted or inappropriate sexual comments to others.",

	"Steals undergarments from others.",
	"Masturbates or touches self openly.",
	"Masturbates more than is normal for his/her age.",
	"Uses objects or things in an unusual sexual way, such as for masturbation.",
	"Dresses or acts in a sexually provocative way, more than most kids his/her age.",
	"Seems preoccupied with sex more than is normal for his/her age.",
	"Makes sexual threats.",
	"Teases others in a sexual way.",
	"Has been in trouble at school for acting in a sexual way.",
	"Makes others feel uncomfortable in a sexual way.",
	"Has been in trouble at home for acting in a sexual way.",
	"Has been in trouble in the community or neighborhood for acting in a sexual way.",
	"Has been arrested, charged, or interviewed by the authorities (such as police or Child Welfare) for a possible illegal sexual behavior.",
	"Has molested a child or been accused.",
	"Has raped someone or been accused.",
	"Any other concerning sexual behavior – Describe:"
)

write.csv(data.frame(ItemNumber=1:30, ItemText=ysbpItemText), file='ysbpRecidTable.csv', row.names=FALSE)



#--------------------------------------
# Plotting of ysbp trajectories

#numPostTx <- 9
#numInTx <- 10

###### EEEEEEEEEK #########
###### Oh No, Batman! #########
###### This should be cleaned in the data #########
###### instead of fixed here #########
###### See email with Jane Silovsky and Carrie Strunsky #########
ysbpData[ysbpData$YouthID %in% "089-01-01" & ysbpData$SITE_ID %in% "SC", 'OutScore'] <- 0
###### EEEEEEEEEK #########


inTxSeq <- seq(1, numInTx, by=2)
inTxSeq <- matrix(c(inTxSeq, rep(NA, length(inTxSeq))), nrow=2, byrow=TRUE)
inTxSeq <- inTxSeq[1:numInTx]
inTxSeq[1] <- paste("Tx", inTxSeq[1])
inTxSeq


tem.times <- c(1, seq(4, 11, length.out=numInTx), 12, seq(15, 19, length.out=numPostTx))
tem.labels <- c("Intake", inTxSeq, "Exit", "Post Tx 1", rep(NA, 3), "5", rep(NA, 4), "10")
tem.vars <- c("InScore", paste("Score_", 1:numInTx, sep=""), "OutScore", paste("PostScore_", 1:numPostTx, sep=""))

# SHOULD NOT GIVE ERROR!
cbind(tem.times, tem.labels, tem.vars)





bmp("plotYSBP.bmp")
matplot(tem.times, t(ysbpData[,tem.vars]), type='o', pch=16, lty=1, xaxt='n', ylim=c(0, 90), xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
dev.off()

jpeg("plotYSBP1.jpg", quality=99, height=9/2, width=12/2, units="in", res=144)
matplot(tem.times, t(ysbpData[,tem.vars]), type='o', pch=16, lty=1, xaxt='n', xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
dev.off()

require(plotrix)
jpeg("plotYSBP2.jpg", quality=99, height=9/2, width=12/2, units="in", res=144)
matplot(tem.times, t(ysbpData[,tem.vars]), type='o', pch=16, lty=1, xaxt='n', yaxt='n', ylim=c(0, 50), xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
axis.break(axis=2, breakpos=45, style='slash')
axis(side=2, at=c(seq(0, 40, by=10), 50), labels=c(seq(0, 40, by=10), 90),las=1)
dev.off()

#bmp('plotYSBPBox.bmp')
pdf('plotYSBPBox.pdf')
boxplot(x=list(
	Intake=c(ysbpData[,tem.vars[1]]),
	Tx1=c(ysbpData[,tem.vars[2]]),
	Tx2=c(ysbpData[,tem.vars[3]]),
	Tx3=c(ysbpData[,tem.vars[4]]),
	Tx4=c(ysbpData[,tem.vars[5]]),
	"Tx5-10"=unlist(ysbpData[,tem.vars[6:11]]),
	Exit=c(ysbpData[,tem.vars[12]]),
	Post=unlist(ysbpData[,tem.vars[13:21]])
	), ylab="YSBP Score", main="Box Plot of YSBP Over Treatment", ylim=c(0, 50), yaxt='n')
axis.break(axis=2, breakpos=45, style='slash')
axis(side=2, at=c(seq(0, 40, by=10), 50), labels=c(seq(0, 40, by=10), 90),las=1)
dev.off()

require(plotrix)
jpeg("plotYSBP2J.jpg", quality=99, height=9/2, width=12/2, units="in", res=144)
#matplot(matrix(tem.times+runif(length(tem.vars)*nrow(ysbpData), -.3, .3), nrow=length(tem.vars)), t(ysbpData[,tem.vars]), type='o', pch=16, cex=.75, lty=1, xaxt='n', yaxt='n', ylim=c(0, 50), xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
#axis(side=1, at=tem.times, labels=tem.labels)
#axis.break(axis=2, breakpos=45, style='slash')
#axis(side=2, at=c(seq(0, 40, by=10), 50), labels=c(seq(0, 40, by=10), 90),las=1)
matplot(matrix(tem.times+runif(length(tem.vars)*nrow(ysbpData), -.3, .3), nrow=length(tem.vars)), t(ysbpData[,tem.vars]), type='o', pch=16, lty=1, xaxt='n', yaxt='n', ylim=c(0, 75), xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
axis.break(axis=2, breakpos=70, style='slash')
axis(side=2, at=c(seq(0, 65, by=10), 75), labels=c(seq(0, 65, by=10), 90),las=1)
dev.off()




dt.diff <- t(apply(sapply((ysbpData[ , dtna]), as.numeric), 1, diff))
dt.range <- range(dt.diff, na.rm=TRUE)


ysbpData$InDate <- as.Date(ysbpData$InDate)
ysbpData$OutDate <- as.Date(ysbpData$OutDate)
dtna2 <- c('InDate', grep('YSBP01s_', dtna, fixed=TRUE, value=TRUE), 'OutDate', grep('PTx_Adm_', dtna, fixed=TRUE, value=TRUE))
ysbpData[, paste0('DaysSince', 1:length(dtna2))] <- ysbpData[,dtna2] - ysbpData[, rep(dtna2[1], length(dtna2))]
for(i in paste0('DaysSince', 1:length(dtna2))) ysbpData[,i] <- as.numeric(ysbpData[,i], units='days')


ysbpData$ID <- paste0(ysbpData$SITE_ID, ysbpData$YouthID)


rysbpData <- reshape(ysbpData, direction='long', varying=list(DaysSince1=paste0('DaysSince', 1:length(dtna2)), Date=dtna2, Score=tem.vars), v.names=c('DaysSince1', 'Date', 'Score'))

rysbpData <- rysbpData[order(rysbpData$ID, rysbpData$Date),]
rysbpData <- rysbpData[apply(rysbpData[,c('DaysSince1', 'Date', 'Score')], 1, function(x) !all(is.na(x))), ]
rysbpData[!is.na(rysbpData$DaysSince1) & rysbpData$DaysSince1 < 0, 'Score'] <- NA
rysbpData$YearsSince1 <- rysbpData$DaysSince1/365.25




plot(rysbpData$DaysSince1/365.25, rysbpData$Score, xlim=c(0, max(rysbpData$DaysSince1, na.rm=TRUE))/365.25, xlab='Years since Intake', ylab='Score', pch=c(1, 16)[(rysbpData$time==12) + 1], col=c('black', 'red')[(rysbpData$time==12) + 1])




require(lme4)

r1 <- glmer.nb(Score ~ 1 + YearsSince1 + (1 + YearsSince1 | ID), data=rysbpData)
r2 <- glmer(Score ~ 1 + YearsSince1 + (1 + YearsSince1 | ID), data=rysbpData, family=poisson)






 ### NOT DONE ###
require(plotrix)
jpeg("plotYSBPDate.jpg", quality=99, height=9/2, width=12/2, units="in", res=144)
plot()
matplot(tem.times, t(ysbpData[,tem.vars]), type='o', pch=16, lty=1, xaxt='n', yaxt='n', ylim=c(0, 75), xlab="Occasion", ylab="YSBP Sum Score", main="Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
axis.break(axis=2, breakpos=70, style='slash')
axis(side=2, at=c(seq(0, 65, by=10), 75), labels=c(seq(0, 65, by=10), 90),las=1)
dev.off()



jpeg("plotYSBPLog.jpg", quality=99)
matplot(tem.times, log(t(ysbpData[,tem.vars])+1/6), , type='o', pch=16, lty=1, xaxt='n', xlab="Occasion", ylab="Started Log of YSBP Sum Score", main="Log of Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
dev.off()


# Not sure if I believe this sample size plot
jpeg("plotYSBPSample.jpg", quality=99)
plot(tem.times, nrow(ysbpData)-colSums(apply(ysbpData[,tem.vars], 2, is.na)), type='o', xaxt='n', xlab="Occasion", ylab="Total Sample Size", , main="N of Youth with Sexual Behavior Problems Scores over Time")
axis(side=1, at=tem.times, labels=tem.labels)
dev.off()




# YSBP plots by site
require(plotrix)
for(Site in theSites){
	jpeg(paste("plotYSBP", Site, ".jpg", sep=""), quality=99, height=9/2, width=12/2, units="in", res=144)
	matplot(tem.times, t(ysbpData[ysbpData$SITE_ID==Site,tem.vars]), type='o', pch=16, lty=1, xaxt='n', yaxt='n', ylim=c(0, 50), xlab="Occasion", ylab="YSBP Sum Score", main=paste(Site, "Youth with Sexual Behavior Problems Scores over Time"))
	axis(side=1, at=tem.times, labels=tem.labels)
	axis.break(axis=2, breakpos=45, style='slash')
	axis(side=2, at=c(seq(0, 40, by=10), 50), labels=c(seq(0, 40, by=10), 90),las=1)
	dev.off()
}


#--------------------------------------
# Negative Binomial Regression


require(glmmADMB) #glmmadmb
require(MASS) #glm.nb

grep('Score_', fixed=TRUE, value=TRUE, names(ysbpData))

# investigate over-dispersion
cbind( sapply(ysbpData[,grep('Score', fixed=TRUE, value=TRUE, names(ysbpData))], mean, na.rm=TRUE),
	sapply(ysbpData[,grep('Score', fixed=TRUE, value=TRUE, names(ysbpData))], var, na.rm=TRUE) )
# Is the variance much larger than the mean?
# Ohhh yes! For in and out scores, the var is 8-9x the mean.


summary(m1 <- glm.nb(OutScore ~ InScore, data=ysbpData))
summary(m2 <- glm.nb(OutScore ~ InScore + SITE_ID, data=ysbpData))



# The initial ysbp factor model
#  with factor 3 as the electronic media and masturbation factor
ysbp.fmodel


itemsOrFactor <- 'items'
allNum <- 1:30
if(itemsOrFactor == 'items'){
	useNum <- 8:11
	title <- 'Do the electronic media items change differently?'
	legen <- c('Full Scale', 'Electronic Media Items', 'Other Items')
} else {
	useNum <- c(8:13, 15, 17:18)
	title <- 'Does the technology/masturbation factor change differently?'
	legen <- c('Full Scale', 'Electronic Media Factor', 'Other Factors')
}
unuseNum <- setdiff(1:30, useNum)
electFactorItems <- sprintf('%02i', useNum)
unelectFactorItems <- sprintf('%02i', unuseNum)
allFactorItems <- sprintf('%02i', allNum)

# mi3 has the item-level YSBP data
electFactorItemsMat <- sapply(paste0('P_', electFactorItems), grep, x=names(mi3), fixed=TRUE, value=TRUE)
unelectFactorItemsMat <- sapply(paste0('P_', unelectFactorItems), grep, x=names(mi3), fixed=TRUE, value=TRUE)
allFactorItemsMat <- sapply(paste0('P_', allFactorItems), grep, x=names(mi3), fixed=TRUE, value=TRUE)

#scoreFUN <- sum.na
scoreFUN <- function(x){mean.na(x)*30}
electFactorSumScores <- apply(electFactorItemsMat, 1, function(x){apply(mi3[,x], 1, scoreFUN)})
unelectFactorSumScores <- apply(unelectFactorItemsMat, 1, function(x){apply(mi3[,x], 1, scoreFUN)})
allFactorSumScores <- apply(allFactorItemsMat, 1, function(x){apply(mi3[,x], 1, scoreFUN)})

nwave <- ncol(electFactorSumScores)

colMeans(electFactorSumScores, na.rm=TRUE)
matplot(t(electFactorSumScores), type='l')


pdf('plotYsbpElectronic.pdf', height=9/2, width=12/2)
plot(tem.times, colMeans(electFactorSumScores, na.rm=TRUE), type='l', col='red', main=title, xlab='Occasion', ylab='YSBPI Sum Score', ylim=c(0,10), xaxt='n')
lines(tem.times, colMeans(unelectFactorSumScores, na.rm=TRUE), col='blue')
lines(tem.times, colMeans(allFactorSumScores, na.rm=TRUE), col='black')
legend('topright', legend=legen, col=c('black', 'red', 'blue'), lty=1, lwd=3)
axis(side=1, at=tem.times, labels=tem.labels)

dev.off()


matplot(jitter(t(electFactorSumScores)), type='l')
lines(1:nwave, colMeans(electFactorSumScores, na.rm=TRUE), lwd=5)





