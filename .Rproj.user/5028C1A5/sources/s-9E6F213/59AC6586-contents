#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: January 2013 Originally
#   2014-12-03 Cleaned up and reorganized
# Filename: ReadAccessOJJDP.R
# Purpose: Read in all the OJJDP data.  Do some minimal combining.
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Some useful cleaning and printing functions

mkpercent <- function(x, totals){round(t(t(x)/totals), 2)}

meansd <- function(x, digits=1){
  fmt <- paste0('%.', digits, 'f')
  mstr <- sprintf(fmt, mean(x, na.rm=TRUE))
  sstr <- sprintf(fmt, sd(x, na.rm=TRUE))
  paste0(mstr, " (", sstr, ")")
}

meansdn <- function(x, digits=1){
  fmt <- paste0('%.', digits, 'f')
  mstr <- sprintf(fmt, mean(x, na.rm=TRUE))
  sstr <- sprintf(fmt, sd(x, na.rm=TRUE))
  nstr <- paste0('N=', sum(!is.na(x)))
  paste0(mstr, " (", sstr, "; ", nstr, ")")
}

findVariable <- function(variable, dataList){
	sapply(lapply(dataList, names), match, x=variable)
}

rowZero <- function(x){all(x == rep(0, length(x)))}

rowNA <- function(x){all(is.na(x))}


sum.na <- function(x){
	if(all(is.na(x))){
		return(NA)
	} else {return(sum(x, na.rm=TRUE))}
}

mean.na <- function(x){
	if(all(is.na(x))){
		return(NA)
	} else {
		x[is.na(x)] <- 0
		return(mean(x, na.rm=TRUE))
	}
}

prop.na <- function(x){
	sum(is.na(x))/length(x)
}

#------------------------------------------------------------------------------
require(RODBC)
#------------------------------------------------------------------------------
db_info <- list("DCAC" = "OJJDP Database DCAC - cleaned for report.accdb",
                "CHI"  = "Chicago OJJDP Database - cleaned for reporting.accdb",
                "CT"   = "Hartford OJJDP Database - cleaned for reporting.accdb")

theSites <- c("DCAC", "CH", "CT")

myTableNames <- c("Intake", "IntakeC", "IntakeV", "Referral", "ReferralC", "ReferralV", "Exit", "ExitC", "ExitV")
acTableNames <- c("Intake", "Intake - Caregivers", "Intake - Victims", "OJJDP Referral",
                  "OJJDP Referral Caregiver", "Victims", "Program Exit", "Program Exit caregivers", "Program Exit - Victims")

theTables <- list()
for(site in 1:length(theSites)){
  channel <- odbcConnectAccess2007(db_info[[site]], pwd = "SBP")
  for(k in 1:length(acTableNames)){
    theTables[[myTableNames[k]]][[site]] <- sqlFetch(channel, acTableNames[k], na.strings=c("NA", "DK"), stringsAsFactors=FALSE)
  }
  odbcClose(channel)
}

#------------------------------------------------------------------------------

vector.equal <- function(x, tol=.Machine$double.eps ^ 0.5){all( abs(x - mean(x)) < tol)}
row.equal <- function(x){all(x[1]==x)}

#------------------------------------------------------------------------------
theCombinedTables <- list()
idnames <- c('YouthID', 'CaregiverID', 'VictimID', 'VictimID1')
for(atable in names(theTables)){

  names(theTables[[atable]]) <- theSites
		
		if(nrow(theTables[[atable]][[theSites[1]]]) > 0){
			theTables[[atable]][[theSites[1]]][, "SITE ID"] <- theSites[1]
		}
		theCombinedTables[[atable]] <- theTables[[atable]][[theSites[1]]]
		for(asite in theSites[-1]){
			if(nrow(theTables[[atable]][[asite]]) > 0){
				theTables[[atable]][[asite]][, "SITE ID"] <- asite
			}
			theCombinedTables[[atable]] <- merge(theCombinedTables[[atable]], theTables[[atable]][[asite]], all=TRUE)
		}
		if(nrow(theCombinedTables[[atable]]) > 0){
			theCombinedTables[[atable]]$`SITE ID` <- factor(theCombinedTables[[atable]]$`SITE ID`, levels=theSites)
			theCombinedTables[[atable]]$SITE_ID <- theCombinedTables[[atable]]$`SITE ID`
			tab.names <- names(theCombinedTables[[atable]])

			tem.idnames <- na.omit(tab.names[match(idnames, tab.names)])
			notAllIDsMissing <- !apply(theCombinedTables[[atable]][,tem.idnames, drop=FALSE], 1, rowNA)
			theCombinedTables[[atable]] <- theCombinedTables[[atable]][notAllIDsMissing,]
		}

}

sapply(theCombinedTables, dim)

sapply(theTables[['Intake']], dim)
