#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: 2014-11-10
# Filename: ForeignKeysOJJDP.R
# Purpose: Read in OJJDP data and make foreign keys system for merging
#   1) Youth tables into Victims tables
#   2) Victim tables in Youth tables
#   3) Youth Caregiver
#   4) Caregiver Youth
#   5) Victim Caregiver
#   6) Caregiver Victim
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# read in OJJDP data

source('ReadAccessOJJDP.R')

#------------------------------------------------------------------------------
# Make Youth-Victim key table

theKeysToMake <- matrix(
	c('YouthID', 'VictimID1',
	'VictimID1', 'YouthID',
	'CaregiverID', 'YouthID',
	'YouthID', 'CaregiverID',
	'CaregiverID', 'VictimID1',
	'VictimID1', 'CaregiverID'), nrow=6, ncol=2, byrow=TRUE)

AllKeys <- list()

for(i in 1:nrow(theKeysToMake)){
	print(theKeysToMake[i,])
	uniqueVarID <- theKeysToMake[i, 1]
	matchVarID <- theKeysToMake[i, 2]
	yvKey <- NULL
	for(site in theSites){
		# Get YouthID column for all tables in a particular site
		youthL <- lapply(lapply(theTables, "[[", site), "[[", uniqueVarID)
		youthL <- lapply(youthL, as.character)
		theYouth <- unique(unlist(youthL))
		theYouth <- sort(theYouth)
		hasYouth <- lapply(lapply(lapply(theTables, "[[", site), names), match, x=uniqueVarID)
		hasVictim <- lapply(lapply(lapply(theTables, "[[", site), names), match, x=matchVarID)
		hasBoth <- unlist(hasYouth) & unlist(hasVictim)
		hasBoth[is.na(hasBoth)] <- FALSE
		yvTables <- lapply(theTables, "[[", site)[hasBoth]
		for(youth in theYouth){
			yvList <- list()
			for(j in 1:length(yvTables)){
				candidateTable <- yvTables[[j]]
				del <- candidateTable[candidateTable[,uniqueVarID]==youth, c(uniqueVarID, matchVarID)]
				if(nrow(del) > 0) {
					del$TableName <- names(yvTables)[j]
				} else {
					noMatchTable <- names(yvTables)[j]
				}
				yvList[[j]] <- del[!is.na(del[,uniqueVarID]),]
			}
			#yvList <- lapply(yvTables, subset, subset= uniqueVarID==youth, select=c(uniqueVarID, matchVarID))
			yvPairs <- Reduce(function(...) merge(..., all=T), yvList)
			if(nrow(yvPairs) > 0){
				yvPairs <- yvPairs[!duplicated(yvPairs[,-3]),]
				yvPairs <- data.frame(SITE_ID=site, yvPairs)
				yvPairs <- yvPairs[!is.na(yvPairs[,matchVarID]),]
			}
			if(nrow(yvPairs) == 0){
				yvPairs <- data.frame(site, youth, NA, noMatchTable)
				names(yvPairs) <- c('SITE_ID', uniqueVarID, matchVarID, 'TableName')
			}
			yvKey <- rbind(yvKey, yvPairs)
		}
	}
	AllKeys[[i]] <- yvKey
}

names(AllKeys) <- apply(theKeysToMake, 1, paste, sep='', collapse='')

#------------------------------------------------------------------------------
for(aname in names(AllKeys)){
  assign(paste0("ds_",aname), AllKeys[[aname]])
}
#------------------------------------------------------------------------------
