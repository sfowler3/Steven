arthurData <- merge(theCombinedTables$PSSS, theCombinedTables$PSSSExit, by=c("YouthID", "SITE ID"), all=TRUE)


psNames <- sprintf("PSSS%02d", 1:35) #Note: also has stress scale: items 36-48
inNames <- psNames[c(1, 5, 8, 11, 22, 24)] #Involvement
ppNames <- psNames[c(2, 14, 21, 27)] #Positive Parenting
ipNames <- psNames[c(3, 6, 13, 19)] #Ineffective Parenting
epNames <- psNames[c(18, 20)] #Effective Parenting
msNames <- psNames[c(4, 7, 9, 10, 12, 15, 16, 17, 23, 25, 26)] #Monitoring/Supervision
ssNames <- psNames[c(28:35)] #Social Support

revNames <- psNames[c(3, 4, 7, 12, 13, 16, 48)]

sort(unique( unlist(sapply(arthurData[,psNames], function(x) sort(unique(x))))))

psData <- arthurData[,psNames]
psData[psData=='dk'] <- NA
psData[psData=='32'] <- 2.5
psData[psData=='55'] <- 5
psData[psData=='58'] <- NA
psData[psData=='85'] <- NA
psData <- apply(psData, 2, function(x){as.numeric(as.character(x))})
summary(psData)





ps.cov <- cov(psData, use="pair")

psData.rev <- psData
psData.rev[,revNames[-length(revNames)]] <- 5 - psData.rev[,revNames[-length(revNames)]] + 1 #max - score + min

# Also reverse the Ineffective parenting items
psData.rev[,ipNames] <- 5 - psData.rev[,ipNames] + 1

psRev.cov <- cov(psData.rev, use="pair")
psRev.e <- eigen(psRev.cov)

plot(psRev.e$values) # six factors suggested
cumsum(psRev.e$values)/sum(psRev.e$values) # 6 factors captures 70% of variability



omega(psData.rev, nfactors=7)
omega(psData.rev, nfactors=7)$alpha
# alpha reliability is 0.8789162


# Exit

epsNames <- sprintf("PSSSe%02d", 1:35) #Note: also has stress scale: items 36-48
einNames <- epsNames[c(1, 5, 8, 11, 22, 24)] #Involvement
eppNames <- epsNames[c(2, 14, 21, 27)] #Positive Parenting
eipNames <- epsNames[c(3, 6, 13, 19)] #Ineffective Parenting
eepNames <- epsNames[c(18, 20)] #Effective Parenting
emsNames <- epsNames[c(4, 7, 9, 10, 12, 15, 16, 17, 23, 25, 26)] #Monitoring/Supervision
essNames <- epsNames[c(28:35)] #Social Support

erevNames <- epsNames[c(3, 4, 7, 12, 13, 16, 48)]

# Data cleaning
epsData <- arthurData[,epsNames]
sort(unique(unlist(sapply(epsData, unique))))
epsData[epsData==25] <- 2.5
epsData[epsData==47] <- NA
epsData[epsData=='N/A'] <- NA
head(epsData[,c(15, 23)])
sapply(epsData[,c(15, 23)], table)
epsData[epsData=='N/A'] <- NA
epsData[,c(15, 23)] <- apply(epsData[,c(15, 23)], 2, as.numeric)
summary(epsData)
sort(unique(unlist(sapply(epsData, unique))))


epsData.rev <- epsData
epsData.rev[,erevNames[-length(erevNames)]] <- 5 - epsData.rev[,erevNames[-length(revNames)]] + 1 #max - score + min

# Also reverse the Ineffective parenting items
epsData.rev[,eipNames] <- 5 - epsData.rev[,eipNames] + 1


#--------------------------------------
# Pre-post differences in SKILLS

preSkillsItems <- psData.rev[, !(colnames(psData.rev) %in% ssNames) ]
posSkillsItems <- epsData.rev[, !(colnames(epsData.rev) %in% essNames)]

omega(preSkillsItems)$alpha

preSkills <- apply(preSkillsItems, 1, mean, na.rm=TRUE)
posSkills <- apply(posSkillsItems, 1, mean, na.rm=TRUE)
summary(posSkills-preSkills)

hist(na.omit(posSkills-preSkills))

psssttest <- t.test(posSkills-preSkills, type="paired", var.equal=TRUE)
tcat(psssttest)
# t(108) = 2.22, p < 0.05, d =  0.43


jpeg("plotFSSSSkillsPrePost.jpg", quality=99)
#pdf("plotFSSSSkillsPrePost.pdf")
prden <- density(na.omit(preSkills))
plot(prden, xlim=c(1, 5), main="Change in FSSS Skills Scale", xlab="Score", ylab="Density", lwd=2)
poden <- density(na.omit(posSkills))
poden$y <- poden$y/max(poden$y)*max(prden$y)
lines(poden$x, poden$y, col='blue', lwd=2, lty=2)
legend("topleft", legend=c("Intake FSSS", "Exit FSSS"), col=c("black", "blue"), lwd=2.5, lty=c(1, 2))
text(x=2, y=.4, tcat(psssttest))
dev.off()


#--------------------------------------
# Pre-post differences in STRESS

stressNames <- sprintf("PSSS%02d", 36:48)
estressNames <- sprintf("PSSSe%02d", 36:48)

stressRev <- length(stressNames)

stressDat <- arthurData[,stressNames]
stressDat[stressDat=="N/A"] <- NA
stressDat[stressDat=="2`"] <- 2
stressDat <- sapply(stressDat, as.numeric)
# Reverse: r <- max - score + min
stressDat[,stressRev] <- 4 - stressDat[,stressRev] + 1

estressDat <- arthurData[,estressNames]
estressDat[estressDat=="N/A"] <- NA
estressDat[estressDat=="nm"] <- NA
estressDat <- sapply(estressDat, as.numeric)
# Reverse: r <- max - score + min
estressDat[,stressRev] <- 4 - estressDat[,stressRev] + 1

preStressItems <- stressDat
posStressItems <- estressDat

omega(preStressItems)$alpha

preStress <- apply(stressDat, 1, mean, na.rm=TRUE)
posStress <- apply(estressDat, 1, mean, na.rm=TRUE)
summary(posStress-preStress)

hist(na.omit(posStress-preStress))

stressttest <- t.test(posStress-preStress)
tcat(stressttest)
#t(106) = -7.62, p < 0.001, d =  -1.48


jpeg("plotFSSSStressPrePost.jpg", quality=99)
#pdf("plotFSSSStressPrePost.pdf")
prden <- density(na.omit(preStress))
plot(prden, xlim=c(0, 5), main="Change in FSSS Stesss Scale", xlab="Score", ylab="Density", lwd=2)
poden <- density(na.omit(posStress))
poden$y <- poden$y/max(poden$y)*max(prden$y)
lines(poden$x, poden$y, col='blue', lwd=2, lty=2)
legend("topleft", legend=c("Intake Stress", "Exit Stress"), col=c("black", "blue"), lwd=2.5, lty=c(1, 2))
text(x=2.3, y=.05, tcat(stressttest))
dev.off()


#--------------------------------------
# Pre-post differences in SUPPORT

preSupportItems <- psData.rev[, (colnames(psData.rev) %in% ssNames) ]
posSupportItems <- epsData.rev[, (colnames(epsData.rev) %in% essNames)]

omega(preSupportItems)$alpha

preSupport <- apply(preSupportItems, 1, mean, na.rm=TRUE)
posSupport <- apply(posSupportItems, 1, mean, na.rm=TRUE)
summary(posSupport-preSupport)

hist(na.omit(posSupport-preSupport))

psssttest <- t.test(posSupport-preSupport, type="paired", var.equal=TRUE)
tcat(psssttest)

jpeg("plotFSSSSupportPrePost.jpg", quality=99)
#pdf("plotFSSSSupportPrePost.pdf")
prden <- density(na.omit(preSupport))
plot(prden, xlim=c(1, 5), main="Change in FSSS Support Scale", xlab="Score", ylab="Density", lwd=2)
poden <- density(na.omit(posSupport))
poden$y <- poden$y/max(poden$y)*max(prden$y)
lines(poden$x, poden$y, col='blue', lwd=2, lty=2)
legend("topleft", legend=c("Intake FSSS", "Exit FSSS"), col=c("black", "blue"), lwd=2.5, lty=c(1, 2))
text(x=2, y=.3, tcat(psssttest))
dev.off()



