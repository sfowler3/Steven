#--------------------------------------
# Table 2: Referral Source
data <- theCombinedTables$Referral
rna <- c(
	"Juvenile Services", #1
	"Child Welfare", #2
	"Court",		 #3
	"Mental Health", #4
	"School",		 #5
	"Medical",		 #6
	"Other",		 #7
	"Child Advocacy",#8
	"Parent",		 #9
	"District Attorney",#10
	"Pub Def",		 #11
	"Law Enforcement",#12
	"DK")			 #13
data$FR01 <- factor(data$FR01, levels=1:length(rna), labels=rna)

# recode TX Other="MDT" as "Child Advocacy"
data$FR01[data$SITE_ID %in% 'TX' & data$FR01_other %in% 'MDT'] <- "Child Advocacy"
data$FR01[data$SITE_ID %in% 'TX' & data$FR01_other %in% c('Child Advocacy Center', 'Child Advocacy  Center', 'Child Advocacy Cetner')] <- "Child Advocacy"
data$FR01[data$SITE_ID %in% 'TX' & data$FR01_other %in% c("Children's Hospital", "Children's hospital")] <- "Medical"
#data$FR01_other[data$SITE_ID %in% 'TX' & data$FR01_other %in% 'MDT'] <- 
data$FR01[data$SITE_ID %in% 'NM' & data$FR01_other %in% 'Defense Attorney'] <- "Pub Def"

table2 <- table(data$FR01, data$`SITE ID`, useNA="ifany") #Referral source
table2 <- table2[c(1, 2, 3, 8, 9, 4, 5, 6, 12, 10, 7, 11, 13, 14),]
table2["Other",] <- table2["Other",] + table2["DK",] + table2[14,]# Add Other and Missing
table2["District Attorney",] <- table2["District Attorney",] + table2["Pub Def",] # Add DA and Pub def
table2 <- table2[1:11,]
table2

#-------------
# Dealing with "Other"

del <- data[is.na(data$FR01),c("SITE ID", "YouthID", "FR01", "FR01_other")]
del <- del[do.call(order, del), ]
del

del <- data[!is.na(data$FR01_other),c("SITE ID", "YouthID", "FR01", "FR01_other")]
del <- del[do.call(order, del), ]
del

del <- data[!is.na(data$FR01_other) & data$FR01 %in% "Other", c("SITE ID", "YouthID", "FR01", "FR01_other")]
del <- del[do.call(order, del), ]
del
write.csv(del, file='dataRefSourceOtherRecode.csv', row.names=FALSE)






#-------------
# Write table

#Check TRUE
all(colSums(table2) == siteN)


(table2full <- mkptable(table2, c(siteN, totalN)))


write.csv(file='tableReferralSource.csv', table2full)



#---------
# Figure illustrating the various referral sources

table2p <- rowSums(table2)/sum(table2)
table2p <- sort(table2p)
png(file='plotReferralSources.png')
dotchart(table2p*100, xlim=c(0,100), xlab="Percent", main='Referral Sources')
dev.off()

#---------

#------------------------------------------------------------------------------
# Youth referral percent of sexual abuse types - no longer used

# data <- theCombinedTables$Referral
# 
# table(data$YSI10abusetypes)
# #table(data$YSI10abusetypes[data$SITE_ID %in% c('TX', 'PA')])
# 
# 
# ab.ty <- strsplit(as.character(data$YSI10abusetypes), ";", fixed=TRUE)
# # one list element per Youth, with (possibly multiple) abuse types in each list element
# 
# 
# # Quick version, just unlists the elements are makes the table
# 
# ab.tyTable <- table(unlist(ab.ty))
# abuseTypes <- c("Sexual abuse", "Physical abuse", "Neglect", "In need of treatment", "In need of supervision", "DK")
# names(ab.tyTable) <- abuseTypes
# ab.tyTable
# ab.tyTableP <- mkpercent(ab.tyTable, sum(ab.tyTable))*100
# ab.tyTableP
# 
# paste("Of the ", sum(referred), " referred Youth, ", ab.tyTable[1], " indicated sexual abuse (", ab.tyTableP[1], "%), ", ab.tyTable[2], " physical abuse (", ab.tyTableP[2], "%), ", ab.tyTable[3], " neglect (", ab.tyTableP[3], "%).  ", .replace_number(ab.tyTable[4]), " were in need of treatment (", ab.tyTableP[4], "%), ", ab.tyTable[5], " were in need of supervision (", ab.tyTableP[5], "%).  ", .replace_number(ab.tyTable[6]), " indicated they did not know the type of abuse/neglect (", ab.tyTableP[6], "%).  Note that these are not mutually exclusive categories.  A single youth could be subject to all of the above, none of the above, or any subset in between.", sep="")
# 


