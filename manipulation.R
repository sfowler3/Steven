#------------------------------------------------------------------------------
# Author: Steven Fowler
# Created: 10 February 2020
# Last Updated: 11 June 2020
# Purpose: Read in OJJDP Access databases, pull and organize necessary data
#          for analysis/tables
# Changes: - initial tables/figures complete
#------------------------------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
library(RODBC)
library(magrittr)
library(janitor)
library(dplyr)
library(knitr)
library(kableExtra)
library(readr)
library(rlist)

#get ID pairings for table 7
source('ForeignKeysOJJDP.R')
#remove all objects except ID pairing datasets
rm(list=setdiff(ls(),ls()[grepl("ds_",ls())]))

#list containing site IDs and database locations
db_info <- list("DCAC" = "OJJDP Database DCAC - cleaned for report.accdb",
                "CHI"  = "Chicago OJJDP Database - cleaned for reporting.accdb",
                "CT"   = "Hartford OJJDP Database - cleaned for reporting.accdb")

pwd <- readChar("pwd.txt",file.info("pwd.txt")$size)

#first col = variable name used in this script, second col = corresponding table name in Access
table_name <- list(
  "Intake"    = "Intake",
  "IntakeC"   = "Intake - Caregivers",
  "IntakeV"   = "Intake - Victims",
  "Referral"  = "OJJDP Referral",
  "ReferralC" = "OJJDP Referral Caregiver",
  "ReferralV" = "Victims",
  "CBCY"      = "CBC Youth",
  "CBCC"      = "CBC Caregiver on Youth Intake",
  "JSOAP"     = "Data Table",
  "ExitY"     = "Program Exit"
)

#create empty list with size equal to number of tables being used
df_list <- vector(mode = "list", length = length(table_name))

#populate the dataframes
for (i in 1:length(db_info)) {
  #loop through each database
  channel <- odbcConnectAccess2007(db_info[[i]], pwd = pwd)
  
  for (k in 1:length(table_name)) {
    #loop through each table
    tbl <- data.frame(lapply(sqlFetch(channel, table_name[[k]]), as.character), #pull table
                 stringsAsFactors = FALSE) #defactorize
    
    if (nrow(tbl) > 0) { #only add data if table has rows
    
      tbl$SITE <- names(db_info[i]) #add site ID instead of relying on database column
      
      if (is.null(df_list[[k]])) {
        #if element of list for the table is empty, fill it
        df_list[[k]] <- tbl
      } else {
        df_list[[k]] <- rbind(df_list[[k]], tbl) #if element is not empty, add onto it
      }
      
      #pull the combined dataframe out, assign it to a variable name
      df <- as.data.frame(df_list[[k]])
      colnames(df) <- names(df_list[[k]])
      assign(names(table_name[k]), df)
    }
  }
  odbcCloseAll()
}

# ---- tables-used ---------------------------------------------------------------

#blank formatted table, used to fill in blank columns for each site missing data
formatted_table_template <- data.frame(
  matrix(ncol = 2+length(db_info), nrow = 0)
) %>% 
  dplyr::mutate_all(as.character)
colnames(formatted_table_template) <- c("desc",names(db_info),"TOTAL")

#make totals table
make_total_table <- function(raw_table = raw_table, filter_by = list()) {
  if(length(filter_by)) {
    for (i in 1:length(filter_by)) {
      if(i == 1) {
        formatted_table <- raw_table %>% 
          dplyr::filter(.data[[names(filter_by)[i]]] == filter_by[[i]])
      } else {
        formatted_table <- formatted_table %>% 
          dplyr::filter(.data[[names(filter_by)[i]]] == filter_by[[i]])
      }
    }
  } else {
    formatted_table <- raw_table
  }
  formatted_table <- formatted_table %>%
    dplyr::group_by(SITE) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    tidyr::spread(SITE, count) %>%
    dplyr::mutate(TOTAL = rowSums(.[]),
                  desc = "N") %>%
    dplyr::select(desc, dplyr::everything())
  return(formatted_table)
}

total_referrals   <- make_total_table(Referral)
total_intakes     <- make_total_table(Intake, list(NoIntake = 1))
total_enrollments <- make_total_table(Intake, list(NoIntake = 1, IGtx11 = 1))
total_graduations <- make_total_table(ExitY, list(PE01 = 1))

ds_age <- Referral %>% 
  dplyr::select(SITE, FR_date, FRY02) %>% 
  dplyr::mutate(age = as.integer(as.double(difftime(FR_date, FRY02, units = "days") / 365.25))) 

age_by_site <- ds_age %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::summarise(avg = mean(age, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(SITE, avg) %>% 
  dplyr::mutate(desc = "rdesc02")

cbcl_cg <- CBCC %>% 
  dplyr::select(SITE, InternalizingNS, ExternalizingNS) %>% 
  dplyr::mutate_at(c("InternalizingNS","ExternalizingNS"), as.integer)

cbcl_cg_totals <- cbcl_cg %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::summarise(N_ext = sum(!is.na(ExternalizingNS)),
                   N_int = sum(!is.na(InternalizingNS)))

cbcl_y <- CBCY %>% 
  dplyr::select(SITE, InternalizingNS, ExternalizingNS) %>% 
  dplyr::mutate_at(c("InternalizingNS","ExternalizingNS"), as.integer)  

cbcl_y_totals <- cbcl_y %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::summarise(N_ext = sum(!is.na(ExternalizingNS)),
                   N_int = sum(!is.na(InternalizingNS)))
  
# ---- functions-used ---------------------------------------------------------------

format_table <- function(unformatted_table, template, summarise_condition = "none", add_N_row = "none", template_start = 1, append_row = NULL) {
  formatted_table <- unformatted_table %>% 
    dplyr::select(SITE,dplyr::starts_with("rdesc")) %>%
    dplyr::group_by(SITE)
  if (summarise_condition == "none") {
    formatted_table <- formatted_table %>% 
      dplyr::summarise_all(sum, na.rm = TRUE)
  } else if (summarise_condition == "logical") {
    formatted_table <- formatted_table %>%
      dplyr::summarise_if(is.logical, sum, na.rm = TRUE)
  }
  formatted_table <- formatted_table %>% 
    dplyr::ungroup() %>% 
    tidyr::gather("desc","count",dplyr::starts_with("rdesc")) %>% 
    tidyr::spread(SITE,count)
  if (!is.null(append_row)) {
    formatted_table <- formatted_table %>% 
      dplyr::full_join(append_row, by = c("desc",unique(Referral$SITE))) %>%
      dplyr::arrange(desc)
  }
  formatted_table <- formatted_table %>% 
    dplyr::mutate(desc = template[[1]][dplyr::row_number()+template_start-1]) %>% 
    dplyr::mutate(TOTAL = rowSums(dplyr::select(., -desc)))
  if (add_N_row=="table") {
    formatted_table <- formatted_table %>% 
      janitor::adorn_totals("row", na.rm = T, name = "N") %>% 
      dplyr::slice(match(c("N",formatted_table$desc[1:length(formatted_table$desc)]), desc)) #move N row to the top of df
  }
  if (add_N_row=="referral") {
    formatted_table <- formatted_table %>% 
      rbind(total_referrals) %>% 
      dplyr::slice(match(c("N",formatted_table$desc[1:length(formatted_table$desc)]), desc)) #move N row to the top of df
  }
  if (add_N_row=="intake") {
    formatted_table <- formatted_table %>% 
      rbind(total_intakes) %>% 
      dplyr::slice(match(c("N",formatted_table$desc[1:length(formatted_table$desc)]), desc)) #move N row to the top of df
  }
  return(formatted_table)
}

calc_pct <- function(x) {
    pct <- round(x / sum(x) * 100)
    x <- paste0(x, " (", pct, "%)")
    return(x)
}

style_table <- function(tbl, packed_rows = list(), header_row = TRUE) {
  styled_table <- tbl %>% 
    knitr::kable() %>% 
    kableExtra::kable_styling(bootstrap_options = c("condensed", "striped"), font_size = 14) %>% 
    kableExtra::column_spec(1, color = "#fff", background = "#338CC9", bold = T) %>% 
    kableExtra::row_spec(0,color = "#fff", background = "#338CC9", bold = T)
  if(header_row) {
    styled_table <- styled_table %>% 
      kableExtra::row_spec(1,color = "#fff", background = "#338CC9", bold = T)
  }
  if (length(packed_rows)) {
    packed_rows <- packed_rows[order(sapply(packed_rows, function(x) x[1], simplify=TRUE), decreasing=FALSE)]
    for(i in 1:length(packed_rows)) {
      row_start <- packed_rows[[i]][1]
      row_end <- packed_rows[[i]][length(packed_rows[[i]])]
      styled_table <- styled_table %>% 
        kableExtra::pack_rows(names(packed_rows[i]), row_start, row_end, label_row_css = "background-color: #338CC9; color: #fff")
    }
  }
  return(styled_table)
}



# ---- body ---------------------------------------------------------------

# Table 1: Referrals of Youth with PSB to the Program: Referral Sources

template_tbl1 <- data.frame(
  desc = c(
    "Juvenile Justice",
    "Child Welfare",
    "Child Advocacy Center",
    "Parent",
    "Mental Health",
    "School",
    "Medical",
    "Law Enforcement",
    "Attorney or Public Defender",
    "Multidisciplinary Team",
    "Other/Missing"
  ),
  stringsAsFactors = FALSE
)

#separate other/missing?

tbl1 <- Referral %>% 
  dplyr::mutate(rdesc1  =  FR01 == 1,                # Juvenile Services
                rdesc2  =  FR01 == 2,                # Child Welfare
                rdesc3  =  FR01 == 8,                # CAC
                rdesc4  =  FR01 == 9,                # parent
                rdesc5  =  FR01 == 4,                # mental health
                rdesc6  =  FR01 == 5,                # school
                rdesc7  =  FR01 == 6,                # medical
                rdesc8  =  FR01 == 12,               # law enforcement
                rdesc9  =  FR01 %in% c(10:11),       # Attorney or Public Defender
                rdesc10 =  FR01 == 13,               # Multidisciplinary Team
                rdesc11 = !FR01 %in% c(1:2,4:6,8:13) # Other/Missing
  ) %>%    
  dplyr::select(SITE, dplyr::starts_with("rdesc"))

formatted_tbl1 <- format_table(tbl1, template_tbl1, add_N_row = "table")
formatted_tbl1_pct <- formatted_tbl1 %>%  
  dplyr::slice(2:dplyr::n()) %>% 
  dplyr::mutate_at(dplyr::vars(-desc), calc_pct)

formatted_tbl1_nrow <- dplyr::slice(formatted_tbl1, 1)

formatted_tbl1 <- rbind(formatted_tbl1_nrow, formatted_tbl1_pct)

styled_tbl1 <- style_table(formatted_tbl1)
  
#Table 2: Juvenile and Family Court Involvement with Youth with Sexual Behavior Problems Determined Appropriate for Community-Based PSB Program
#must have intake complete

template_tbl2 <- data.frame(
  desc = c(
    "Youth Charged for Sexual Offense",
    "Youth Adjudicated",
    "Adjudication deferred or pending",
    "Sex Offender Registration Required",
    "School Notification Required by Court",
    "PSB Treatment Court Ordered",
    "Parent Participation in Treatment Ordered",
    "No Contact with Minors/Victims Order",
    "Past or Present Child Welfare Involvement"
  ),
  stringsAsFactors = FALSE
)

tbl2 <- Intake %>% 
  dplyr::filter(NoIntake=="1") %>% 
  dplyr::inner_join(Referral, by = c("YouthID","SITE")) %>% 
  dplyr::mutate(rdesc01 = YSI03            == 1,         # Youth Charged for Sexual Offense
                rdesc02 = YSI04            == 1,         # Youth Adjudicated
                rdesc03 = YSI04            %in% c(2,3),  # Adjudication deferred or pending
                rdesc04 = YSI06l_sexreg    == 1,         # Sex Offender Registration Required
                rdesc05 = YSI06m_snotify   == 1,         # School Notification Required by Court
                rdesc06 = YSI06g_sbp       == 1,         # PSB Treatment Court Ordered
                rdesc07 = YSI06h_parent_tx == 1,         # PSB Treatment Court Ordered
                rdesc08 = YSI06k_minor     == 1,         # No Contact with Minors/Victims Order
                rdesc09 = YSI10            %in% c(1,2))  # Past or Present Child Welfare Involvement

formatted_tbl2 <- format_table(tbl2, template_tbl2, add_N_row = "intake")

formatted_tbl2_pct <- formatted_tbl2 %>%  
  dplyr::slice(2:dplyr::n()) %>% 
  dplyr::mutate_at(dplyr::vars(-desc), calc_pct)

formatted_tbl2_nrow <- dplyr::slice(formatted_tbl2, 1)

formatted_tbl2 <- rbind(formatted_tbl2_nrow, formatted_tbl2_pct)

styled_tbl2 <- style_table(formatted_tbl2)

# Table 3: Demographic Information about the Youth with PSB Referred to Services and Their Caregivers

template_tbl3 <- data.frame(
  desc = c(
    "Youth is Male",
    "Average Age",
    "Caucasian",
    "African American",
    "Asian",
    "Native American",
    "Hispanic/Latino",
    "Other/Multiple",
    "Primary Language Spanish",
    "Primary Language Other",
    "Primary Caregiver Gender Female",
    "Birth Parent",
    "Adoptive Parent",
    "Step Parent",
    "Foster Parent",
    "Grandparent",
    "Other Relative",
    "Other Nonrelative",
    "Missing",
    "Mother's Rights Terminated",
    "Father's Rights Terminated",
    "Father Unknown",
    "Children Change Placement due to PSB"),stringsAsFactors = FALSE)

race <- Referral %>% 
  dplyr::select(YouthID, SITE, dplyr::starts_with("FRY03"), -FRY03f_Otherspecify) %>% 
  dplyr::mutate_at(dplyr::vars(FRY03a_White:FRY03f_Other),function(x) { ifelse(is.na(x) | x == "DK" | x == 0,0,1)}) %>% 
  dplyr::mutate(FRY03f_Other    = FRY03f_Other    == 1 | rowSums(.[3:8]) > 1) %>% 
  dplyr::mutate(FRY03a_White    = FRY03a_White    == 1 & !FRY03f_Other,
                FRY03b_Black    = FRY03b_Black    == 1 & !FRY03f_Other,
                FRY03c_Asian    = FRY03c_Asian    == 1 & !FRY03f_Other,
                FRY03d_Indian   = FRY03d_Indian   == 1 & !FRY03f_Other,
                FRY03e_Hispanic = FRY03e_Hispanic == 1 & !FRY03f_Other)

tbl3a <- Referral %>%
  dplyr::inner_join(race, by = c("YouthID","SITE")) %>% 
  dplyr::mutate(
    rdesc01 = FRY01 == 0,        # Youth is Male
    rdesc02 = 0,                 # avg age placeholder
    rdesc03 = FRY03a_White.y,    # Caucasian
    rdesc04 = FRY03b_Black.y,    # African American FRY03b_Blac
    rdesc05 = FRY03c_Asian.y,    # Asian FRY03c_Asian
    rdesc06 = FRY03d_Indian.y,   # Native American
    rdesc07 = FRY03e_Hispanic.y, # Hispanic/ Latino
    rdesc08 = FRY03f_Other.y,    # Other/Multiple FRY03f_Other
    rdesc09 = FRY04 == 2,        # Primary Language Spanish
    rdesc10 = FRY04 == 3)        # Primary Language Other

formatted_tbl3a <- format_table(tbl3a, template_tbl3, summarise_condition =  "logical", append_row = age_by_site, add_N_row = "referral") %>% 
  dplyr::mutate(TOTAL = ifelse(desc=="Average Age",mean(ds_age$age, na.rm = TRUE),TOTAL))

formatted_tbl3a_nrow <- dplyr::slice(formatted_tbl3a,1)

template_tbl3$ID <- seq.int(nrow(template_tbl3))

formatted_tbl3a_pct <- formatted_tbl3a %>% 
  dplyr::slice(2:dplyr::n()) %>% 
  tidyr::gather(SITE, value, -desc) %>% #convert to long data format
  dplyr::group_by(SITE) %>% 
  dplyr::mutate(pct =
      ifelse(desc %in% "Average Age" & SITE != "TOTAL",
         paste0(round(value, digits = 1), " (", round(sd(ds_age$age[which(ds_age$SITE %in% SITE)], na.rm = T), digits = 1), ")"), #avg age, sd of each site
         ifelse(desc=="Average Age" & SITE == "TOTAL",
                paste0(round(value, digits = 1), " (", round(sd(ds_age$age, na.rm = T), digits = 1), ")"), #avg age, sd of full dataset
                ifelse(desc != "Average Age" & SITE != "TOTAL",
                       paste0(value, " (", round(value / length(which(Referral$SITE %in% SITE)) * 100), "%)"), #avg of each site
                       paste0(value, " (", round(value / nrow(Referral) * 100), "%)"))))) %>%  #avg of full dataset
  dplyr::ungroup() %>% 
  dplyr::select(-value) %>% 
  tidyr::spread(SITE,pct) %>% #convert back to wide data
  dplyr::left_join(template_tbl3, by = "desc") %>% #reorder based on template
  dplyr::mutate(ID = ifelse(is.na(ID),0,ID)) %>% 
  dplyr::arrange(ID) %>% 
  dplyr::select(-ID)

formatted_tbl3a <- rbind(formatted_tbl3a_nrow, formatted_tbl3a_pct)

tbl3b <- ReferralC %>% 
  dplyr::mutate(
    rdesc11 = FRC03       == 1,                   # Primary Caregiver Gender Female   use dataset of cgs with known genders
    rdesc12 = FRC01       == 1,                   # Birth Parent
    rdesc13 = FRC01       == 2,                   # Adoptive Parent
    rdesc14 = FRC01       == 3,                   # Step Parent
    rdesc15 = FRC01       == 4,                   # Foster Parent
    rdesc16 = FRC01       == 5,                   # Grandparent
    rdesc17 = FRC01       == 6,                   # Other Relative
    rdesc18 = FRC01       == 7,                   # Other Nonrelative
    rdesc19 = FRC01       == "DK" | is.na(FRC01), # Missing relationship with mother
    rdesc20 = FRC07Mother == 5,                   # Mother’s Rights Terminated
    rdesc21 = FRC07Father == 5,                   # Father’s Rights Terminated 
    rdesc22 = FRC07Father == 8,                   # Father Unknown
    rdesc23 = FRC02       == 1)                   # Children Change Placement due to PSB
 
formatted_tbl3b <- format_table(tbl3b, template_tbl3, summarise_condition = "logical", template_start = 11) %>% 
  tidyr::gather(SITE, value, -desc) %>% #convert to long
  dplyr::group_by(SITE) %>% 
  dplyr::mutate(pct =
                  ifelse(!SITE %in% "TOTAL",
                         paste0(value, " (", round(value / length(which(Referral$SITE %in% SITE)) * 100), "%)"), #avg of each site
                         paste0(value, " (", round(value / nrow(Referral) * 100), "%)"))) %>% #avg of full dataset
  dplyr::ungroup() %>% 
  dplyr::select(-value) %>% 
  tidyr::spread(SITE,pct) %>% #convert to wide
  dplyr::left_join(template_tbl3, by = "desc") %>% #reorder based on template
  dplyr::arrange(ID) %>% 
  dplyr::select(-ID)

formatted_tbl3 <- rbind(formatted_tbl3a, formatted_tbl3b)
styled_tbl3 <- style_table(formatted_tbl3, list("Youth/Race Ethnicity*" = 4:12,
                                                "Caregiver Relationship to Youth" = 13:20)) %>% 
  kableExtra::footnote(general = "* For percentages here, we are using the percent of known caregivers.", general_title = "")

# Table 4. Of those who completed an Intake, Reasons Given for Why Group Treatment was not Recommended

template_tbl4 <- data.frame(
  desc = c(
    "Treatment Not Recommended",
    "Does not meet inclusion criteria",
    "Trauma symptoms warrant TF-CBT instead",
    "Other"),stringsAsFactors = FALSE)

template_tbl4$ID <- seq.int(nrow(template_tbl4))

tbl4 <- Intake %>% 
  dplyr::select(SITE, NoIntake, IGtx01, IGtx02) %>% 
  dplyr::filter(NoIntake == 1) %>% 
  dplyr::mutate(rdesc01 = IGtx01 == 0,      # treatment not recommended
                rdesc02 = IGtx02 == 1,      # does not meet inclusion criteria
                rdesc03 = IGtx02 == 2,      # trauma symptoms warrant TF-CBT instead
                rdesc04 = IGtx02 == 3) %>%  # other
  dplyr::select(SITE, dplyr::starts_with("rdesc"))

formatted_tbl4 <- format_table(tbl4, template_tbl4, add_N_row = "intake")

formatted_tbl4_nrow <- dplyr::slice(formatted_tbl4,1) %>% 
  dplyr::mutate(ID = 0)

formatted_tbl4 <- formatted_tbl4 %>% 
  dplyr::slice(2:dplyr::n()) %>% 
  tidyr::gather(SITE, value, -desc) %>% #convert to long
  dplyr::group_by(SITE) %>% 
  dplyr::mutate(pct =
                  ifelse(SITE != "TOTAL",
                         paste0(value, " (", round(value / as.integer(total_intakes[names(total_intakes)=="CHI"]) * 100), "%)"), #avg of each site
                         paste0(value, " (", round(value / total_intakes$TOTAL * 100), "%)"))) %>% #avg of full dataset
  dplyr::ungroup() %>% 
  dplyr::select(-value) %>% 
  tidyr::spread(SITE,pct) %>% #convert to wide
  dplyr::left_join(template_tbl4, by = "desc") %>% 
  rbind(formatted_tbl4_nrow) %>% 
  dplyr::arrange(ID) %>% 
  dplyr::select(-ID)

styled_tbl4 <- style_table(formatted_tbl4)

# Table 5. Caregiver and Youth Rated Internalizing and Externalizing T-scores on the Child Behavior Checklist
# and Youth Behavior Report at Intake for All Youth

make_cbcl <- function(ds_long, role) {
  
  ds_total <- ds_long %>% 
    dplyr::group_by(SITE) %>% 
    dplyr::summarise(N_ext = sum(!is.na(ExternalizingNS)),
                     N_int = sum(!is.na(InternalizingNS)))
  
  cbcl_tbl <- ds_long %>% 
    dplyr::group_by(SITE) %>% 
    dplyr::summarise(mean_int = round(mean(InternalizingNS, na.rm = T), digits = 2),
                     mean_ext = round(mean(ExternalizingNS, na.rm = T), digits = 2),
                     std_int = paste0(" (",round(sd(InternalizingNS, na.rm = T), digits = 2),"; "),
                     std_ext = paste0(" (",round(sd(ExternalizingNS, na.rm = T), digits = 2),"; ")) %>% 
    dplyr::full_join(ds_total, by = "SITE") %>% 
    dplyr::mutate_at(c("N_ext","N_int"), function(x) {paste0("N=",x,")")}) %>% 
    tidyr::unite(internalizing, c("mean_int","std_int","N_int"), sep = "", remove = T) %>% 
    tidyr::unite(externalizing, c("mean_ext","std_ext","N_ext"), sep = "", remove = T) %>% 
    tidyr::gather(desc, value, -SITE) %>% 
    tidyr::spread(SITE, value) %>% 
    dplyr::mutate(N_total = ifelse(desc == "externalizing", sum(ds_total$N_ext),
                                   sum(ds_total$N_int)),
                  mean_total = ifelse(desc == "externalizing", round(mean(ds_long$ExternalizingNS, na.rm = T), digits = 2),
                                      round(mean(ds_long$InternalizingNS, na.rm = T), digits = 2)),
                  std_total = ifelse(desc == "externalizing", round(sd(ds_long$ExternalizingNS, na.rm = T), digits = 2),
                                     round(sd(ds_long$InternalizingNS, na.rm = T), digits = 2)), 
                  TOTAL = paste0(mean_total," (",std_total,"; N=",N_total,")"),
                  desc = ifelse(desc == "externalizing", paste0(role," Rated Externalizing"),
                                paste0(role," Rated Internalizing"))) %>%
    dplyr::select(-dplyr::ends_with("_total"))
  
  return(cbcl_tbl)
    
}
  
formatted_tbl5 <- rbind(make_cbcl(cbcl_cg,"Caregiver"),make_cbcl(cbcl_y,"Youth")) %>% 
  dplyr::full_join(formatted_table_template, by=c("desc","TOTAL",unique(cbcl_y$SITE),unique(cbcl_cg$SITE))) %>% 
  dplyr::select(desc, sort(names(db_info)), TOTAL) %>% 
  dplyr::mutate_all(function(x) {ifelse(is.na(x),"N/A",x)})

styled_tbl5 <- style_table(formatted_tbl5, header_row = FALSE)



  
# Table 6 reports a summary of the youth JSOAP-II measure at intake. 

template_tbl6 <- data.frame(
  desc = c("N","Total","Sexual_Drive","Impulsive","Intervention","Stability","Static","Dynamic"),
  stringsAsFactors = FALSE
)
template_tbl6$ID <- seq.int(nrow(template_tbl6))
  
jsoap_ds_long <- JSOAP %>% 
  dplyr::filter(MeasureStatus == "Completed") %>% 
  dplyr::filter_at(dplyr::vars(jso1:jso28), dplyr::all_vars(!is.na(.))) %>% 
  dplyr::mutate_at(dplyr::vars(jso1:jso28), as.integer) %>% 
  dplyr::mutate(static1       = rowSums(dplyr::select(.,jso1:jso8)),
                static2       = rowSums(dplyr::select(.,jso9:jso16)),
                dynamic1      = rowSums(dplyr::select(.,jso17:jso23)),
                dynamic2      = rowSums(dplyr::select(.,jso24:jso28)),
                static_total  = rowSums(dplyr::select(.,jso1:jso16)),
                dynamic_total = rowSums(dplyr::select(.,jso17:jso28)),
                jsoap_total   = rowSums(dplyr::select(.,jso1:jso28))) %>% 
  dplyr::select(SITE, dplyr::starts_with("static"), dplyr::starts_with("dynamic"), jsoap_total)

jsoap_nrow <- jsoap_ds_long %>%
  dplyr::count(SITE) %>% 
  tidyr::gather(desc, value, -SITE) %>% 
  tidyr::spread(SITE, value) %>% 
  dplyr::mutate(TOTAL = rowSums(dplyr::select(.,unique(jsoap_ds_long$SITE))),
                desc = "N")

formatted_tbl6 <- jsoap_ds_long %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::summarise(
    mean_static1       = mean(static1),
    mean_static2       = mean(static2),
    mean_dynamic1      = mean(dynamic1),
    mean_dynamic2      = mean(dynamic2),
    mean_static_total  = mean(static_total),
    mean_dynamic_total = mean(dynamic_total),
    mean_jsoap_total   = mean(jsoap_total),
    sd_static1         = sd(static1),
    sd_static2         = sd(static2),
    sd_dynamic1        = sd(dynamic1),
    sd_dynamic2        = sd(dynamic2),
    sd_static_total    = sd(static_total),
    sd_dynamic_total   = sd(dynamic_total),
    sd_jsoap_total     = sd(jsoap_total)
      ) %>%
  dplyr::mutate_if(is.numeric, function(x) {round(x, digits = 1)}) %>% 
  dplyr::mutate(
    Total        = paste0(mean_jsoap_total," (",sd_jsoap_total,")"),
    Sexual_Drive = paste0(mean_static1," (",sd_static1,")"),
    Impulsive    = paste0(mean_static2," (",sd_static2,")"),
    Intervention = paste0(mean_dynamic1," (",sd_dynamic1,")"),
    Stability    = paste0(mean_dynamic2," (",sd_dynamic2,")"),
    Static       = paste0(mean_static_total," (",sd_static_total,")"),
    Dynamic      = paste0(mean_dynamic_total," (",sd_dynamic_total,")")
      ) %>% 
  dplyr::select(SITE,Total,Sexual_Drive,Impulsive,Intervention,Stability,Static,Dynamic) %>% 
  tidyr::gather(desc, value, -SITE) %>% 
  tidyr::spread(SITE, value) %>% 
  dplyr::mutate(
    TOTAL_mean = c(
      mean(jsoap_ds_long$dynamic_total),
      mean(jsoap_ds_long$static2),
      mean(jsoap_ds_long$dynamic1),
      mean(jsoap_ds_long$static1),
      mean(jsoap_ds_long$dynamic2),
      mean(jsoap_ds_long$static_total),
      mean(jsoap_ds_long$jsoap_total)),
    TOTAL_sd = c(
      sd(jsoap_ds_long$dynamic_total),
      sd(jsoap_ds_long$static2),
      sd(jsoap_ds_long$dynamic1),
      sd(jsoap_ds_long$static1),
      sd(jsoap_ds_long$dynamic2),
      sd(jsoap_ds_long$static_total),
      sd(jsoap_ds_long$jsoap_total))
  ) %>% 
  dplyr::mutate_if(is.numeric, function(x) {round(x, digits = 1)}) %>%
  dplyr::mutate(TOTAL = paste0(TOTAL_mean," (",TOTAL_sd,")")) %>% 
  dplyr::select(-c(TOTAL_mean,TOTAL_sd)) %>% 
  rbind(jsoap_nrow) %>% 
  dplyr::full_join(template_tbl6, by = "desc") %>% 
  dplyr::arrange(ID) %>% 
  dplyr::select(-ID)

styled_tbl6 <- style_table(formatted_tbl6)

# Table 7. Number of Victims Known at Time of Referral of the Youth with PSB

victim_count <-
  dplyr::bind_rows(lapply(ds_YouthIDVictimID1, as.character),lapply(ds_VictimID1YouthID, as.character),lapply(ds_CaregiverIDVictimID1, as.character),lapply(ds_VictimID1CaregiverID, as.character)) %>%
  dplyr::select(SITE_ID, VictimID1) %>% 
  dplyr::filter(!is.na(VictimID1)) %>% 
  dplyr::group_by(SITE_ID) %>%
  dplyr::distinct() %>%
  dplyr::summarise(Count = n()) %>% 
  tidyr::spread(SITE_ID, Count) %>% 
  dplyr::mutate(TOTAL = rowSums(.)) %>% 
  dplyr::mutate(Count = "TOTAL Victims") %>% 
  dplyr::select(Count, dplyr::everything()) %>% 
  dplyr::mutate_all(as.character)

ds_tbl7_long <- dplyr::bind_rows(lapply(ds_YouthIDVictimID1, as.character),lapply(ds_VictimID1YouthID, as.character)) %>% 
  dplyr::select(-TableName) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(SITE_ID,YouthID) %>% 
  dplyr::summarise(N = sum(!is.na(VictimID1))) %>% 
  dplyr::group_by(SITE_ID, N) %>% 
  dplyr::summarise(Total = n())

victims_with_multiple_youth <- dplyr::bind_rows(lapply(ds_YouthIDVictimID1, as.character),lapply(ds_VictimID1YouthID, as.character)) %>% 
  dplyr::select(-TableName) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(SITE_ID, VictimID1) %>% 
  dplyr::summarise(assoc_youth = dplyr::n()) %>% 
  dplyr::filter(!is.na(VictimID1) & assoc_youth > 1)

formatted_tbl7 <- ds_tbl7_long %>%   
  tidyr::spread(SITE_ID, Total) %>% 
  dplyr::mutate_all(function(x) {ifelse(is.na(x),0,x)}) %>% 
  tidyr::gather(SITE,"X",as.vector(unique(ds_YouthIDCaregiverID$SITE_ID))) %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::mutate(pct = paste0("(",round((X/sum(X)) * 100),"%)")) %>%
  tidyr::unite(Victims, c("X","pct"), sep = " ", remove = T) %>%
  tidyr::spread(SITE, Victims) %>%
  dplyr::rename("Victims" = N) %>%
  dplyr::group_by(Victims) %>%
  dplyr::mutate(TOTAL = paste0(sum(ds_tbl7_long$Total[ds_tbl7_long$N==Victims]),
                               " (",round((sum(ds_tbl7_long$Total[ds_tbl7_long$N==Victims])/total_referrals$TOTAL)*100),"%)")) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Victims =
                  ifelse(!Victims %in% c(0:1),
                         paste0(Victims, " Victims"),
                         ifelse(Victims == 1,
                                paste0(Victims, " Victim"),
                                "Victim was unknown or missing/NA"))) %>% 
  dplyr::full_join(victim_count, by=c("Victims" = "Count","TOTAL",unique(ds_tbl7_long$SITE_ID))) %>% 
  dplyr::rename("CHI" = CH,
                "description" = Victims) %>% 
  dplyr::full_join(dplyr::mutate_all(total_referrals, as.character), by=c("description" = "desc",names(db_info),"TOTAL")) %>% 
  dplyr::mutate(description = ifelse(description=="N","Youth N",description), 
                order = ifelse(grepl("Youth N", description), 1,
                               ifelse(grepl("[[:digit:]]", description), 2,
                                      ifelse(grepl("TOTAL", description), 3, 4)))) %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(-order)

styled_tbl7 <- style_table(formatted_tbl7)

# Table 8 provides the number of youth with PSB who were referred, enrolled, and graduated from the program. 

ds_tbl8 <- rbind(total_referrals, total_intakes, total_enrollments, total_graduations)
ds_tbl8[,1] <- c("Referrals","Intakes","Enrollments","Completed")

template_tbl8 <- data.frame(
  desc = c("N", ds_tbl8$desc),
  stringsAsFactors = FALSE
)
template_tbl8$ID <- seq.int(nrow(template_tbl8))

formatted_tbl8 <- ds_tbl8 %>% 
  tidyr::gather(SITE, total, -desc) %>% 
  dplyr::group_by(SITE) %>%
  dplyr::mutate(n_ref = as.integer(total_referrals[,which(names(total_referrals) %in% SITE)]),
                n_int = as.integer(total_intakes[,which(names(total_intakes) %in% SITE)]),
                n_enr = as.integer(total_enrollments[,which(names(total_enrollments) %in% SITE)]),
                pct   = ifelse(desc %in% "Referrals",  paste0(total," (",round(total/total_referrals$TOTAL * 100), "%)"),
                        ifelse(desc %in% "Intakes",    paste0(total," (",round(total/n_ref * 100), "%)"),
                        ifelse(desc %in% "Enrollments",paste0(total," (",round(total/n_int * 100), "%)"),
                        ifelse(desc %in% "Completed",  paste0(total," (",round(total/n_enr * 100), "%)"),NA))))) %>% 
  dplyr::select(desc, SITE, pct) %>% 
  tidyr::spread(SITE, pct) %>% 
  rbind(total_referrals) %>% 
  dplyr::full_join(template_tbl8, by = "desc") %>% 
  dplyr::arrange(ID) %>% 
  dplyr::select(-ID)

styled_tbl8 <- style_table(formatted_tbl8)

# ---- figures ---------------------------------------------------------------

# Frequency of Referrals by Source Across Sites
ds_fig_referral_source <- tbl1 %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("rdesc")), function(x) {as.integer(ifelse(is.na(x),0,x))}) %>% 
  janitor::adorn_totals("row") %>% 
  dplyr::filter(SITE %in% "Total") %>% 
  tidyr::gather(SITE, count) %>% 
  dplyr::mutate(pct = count/sum(count)) %>% 
  dplyr::mutate(SITE = template_tbl1$desc) %>% 
  dplyr::arrange(dplyr::desc(pct))

png(file='plotReferralSources.png')
dotchart(rev(ds_fig_referral_source$pct*100),
         labels=rev(ds_fig_referral_source$SITE),
         xlim=c(0,100),
         xlab="Percent", main='Referral Sources')
dev.off()

# Site Differences in Families Referred: Racial/Ethnic Differences
ds_fig_race <- format_table(tbl3a, template_tbl3, summarise_condition =  "logical", append_row = age_by_site, add_N_row = "none")
ds_fig_race <- ds_fig_race[3:8,0:length(db_info)+1] %>% 
  dplyr::mutate_if(is.double, function(x) {x/sum(x) * 100}) %>% 
  tibble::column_to_rownames("desc")

png('plotRaceEthnicityOfYouthBarplot.png', height=480+120*2, width=600+120*4)
par(mar=c(7, 4, 2, .7) + 0.1, xpd=TRUE)
barplot(as.matrix(ds_fig_race), main = "Site Differences in Families Referred: Racial/Ethnic Differences",
        xlab = "Site", ylab = "Percent", las = 1, col=c("darkblue","red","purple","green","orange","pink"))
legend("bottomright", legend = rownames(ds_fig_race),
       fill = c("darkblue","red","purple","green","orange","pink"),
       inset=c(0, -.15), horiz=TRUE, cex=1)
dev.off()

# Number of Youth with PSB Referred, Completed an Intake, Enrolled in Services, and Graduated from Services by Site
ds_fig_summary_by_site <- ds_tbl8 %>% 
  dplyr::select(-TOTAL) %>% 
  tidyr::gather(SITE, total, -desc) %>% 
  dplyr::mutate(SITE = as.factor(SITE)) %>% 
  dplyr::arrange(match(desc, c("Referrals","Intakes","Enrollments","Completed")))

png(file='plotSummaryBySite.png')
dotchart(rev(ds_fig_summary_by_site$total), labels = rev(ds_fig_summary_by_site$desc), groups = rev(ds_fig_summary_by_site$SITE), main="", xlab="Count")
dev.off()

# Number of Youth with PSB at Each Site by Category of Referred, Completed an Intake, Enrolled in Services
ds_fig_summary_by_timepoint <- ds_fig_summary_by_site %>% 
  dplyr::mutate(desc = factor(desc, levels = c("Referrals","Intakes","Enrollments","Completed"))) %>% 
  dplyr::arrange(match(SITE, names(db_info))) %>% 
  dplyr::arrange(SITE)

png(file='plotSummaryByTimepoint.png')
dotchart(rev(ds_fig_summary_by_timepoint$total), labels = rev(ds_fig_summary_by_timepoint$SITE), groups = rev(ds_fig_summary_by_timepoint$desc), main="", xlab="Count")
dev.off()



                               
                               
                              
 
                    



  
  


  







#use dataset of cgs with known genders
#primary cg distinction?
#table 7, do not recalculate
  #foreign keys ojjdp.r
