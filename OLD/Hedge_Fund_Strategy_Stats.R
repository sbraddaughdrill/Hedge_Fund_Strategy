# TODO: Add comment
# 
# Author: Brad
# File: Hedge_Fund_Strategy_Stats.R
# Version: 1.0
# Date: 02.27.2014
# Purpose: Compute descriptive statistics and regressions
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))
rm(list = ls(all.names = TRUE))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 3


if (Location == 1) {
  #setwd("C:/Research_temp2/")
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("F:/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 2) {
  #setwd("C:/Research_temp2/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/F/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 5) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","descr","fastmatch","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
                       "pander","pbapply","PerformanceAnalytics","plm","plyr","psych","quantreg","R.oo","R2wd",
                       "reporttools","reshape2","rms","RSQLite","sandwich","sqldf","stargazer","stringr",
                       "texreg","taRifx","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")
data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")

###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

start_year <- 1994
end_year <- 2011

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

#Fund Information
fund_table <- "EurekahedgeHF_Excel_aca_full14"
EurekahedgeHF_Excel_aca_full_import_vars_remove <- c("exposure_cash","exposure_commodities","exposure_currency","exposure_derivatives",
                                                     "exposure_equities","exposure_fixed_income","exposure_life_insurance",
                                                     "exposure_non_life_insurance","exposure_private_equity","exposure_real_estate",
                                                     "instrument_traded_cash","instrument_traded_commodities","instrument_traded_currency","instrument_traded_derivatives",
                                                     "instrument_traded_equities","instrument_traded_fixed_income","instrument_traded_life_insurance",
                                                     "instrument_traded_non_life_insurance","instrument_traded_private_equity","instrument_traded_real_estate",
                                                     "flagship","closed","dead","limited","invest_in_private_placements","managed_accounts_offered","ucits",
                                                     "management_fee_comments","management_fee_org",
                                                     "performance_fee_comments","performance_fee_org",
                                                     "other_fee_comments","other_fee_org",
                                                     "dividend_policy","dividend_policy_org","dividend_policy_comments",
                                                     "fund_closed","fund_closed_comments","fund_closed_org",
                                                     "high_water_mark","high_water_mark_comments","high_water_mark_org",
                                                     "hurdle_rate","hurdle_rate_comments","hurdle_rate_org",
                                                     "listed_on_exchange","listed_on_exchange_org","listed_on_exchange_comments",
                                                     "custodian","custodian1","custodian2","custodian3","custodian4","custodian5","custodian6",
                                                     "legal_advisor_offshore","legal_advisor_offshore1","legal_advisor_offshore2","legal_advisor_offshore3",
                                                     "legal_advisor_onshore","legal_advisor_onshore1","legal_advisor_onshore2","legal_advisor_onshore3","legal_advisor_onshore4",
                                                     "principal_prime_broker_broker","principal_prime_broker_broker1","principal_prime_broker_broker2","principal_prime_broker_broker3",
                                                     "principal_prime_broker_broker4","principal_prime_broker_broker5","principal_prime_broker_broker6","principal_prime_broker_broker7",
                                                     "principal_prime_broker_broker8",
                                                     "secondary_prime_broker_broker","secondary_prime_broker_broker1","secondary_prime_broker_broker2",
                                                     "secondary_prime_broker_broker3","secondary_prime_broker_broker4","secondary_prime_broker_broker5",
                                                     "secondary_prime_broker_broker6",
                                                     "synthetic_prime_broker","synthetic_prime_broker1","synthetic_prime_broker2","synthetic_prime_broker3","synthetic_prime_broker4",
                                                     "base_currency","minimum_investment_currency","reuters","strategy","secondary_investment_strategy",
                                                     "administrator","auditor","countries","equalisation_share_class","exchange_name","industry_focus","investment_geography",
                                                     "manager_profile",
                                                     "monthly_ret2","yearly_ret","limited_bin","synthetic_prime_broker_count")
EurekahedgeHF_Excel_aca_full_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==fund_table,c("field")]
EurekahedgeHF_Excel_aca_full_import_vars_keep1 <- EurekahedgeHF_Excel_aca_full_import_vars_keep0[!(EurekahedgeHF_Excel_aca_full_import_vars_keep0 %in% EurekahedgeHF_Excel_aca_full_import_vars_remove)]
EurekahedgeHF_Excel_aca_full_import_vars_keep2 <- paste(EurekahedgeHF_Excel_aca_full_import_vars_keep1,sep="",collapse=", ")

#Text Information
text_table <- "text_stats_ios"
text_stats_ios_import_vars_remove <- c("lines_ios")
text_stats_ios_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==text_table,c("field")]
text_stats_ios_import_vars_keep1 <- text_stats_ios_import_vars_keep0[!(text_stats_ios_import_vars_keep0 %in% text_stats_ios_import_vars_remove)]
text_stats_ios_import_vars_keep2 <- paste(text_stats_ios_import_vars_keep1,sep="",collapse=", ")

rm2(EurekahedgeHF_Excel_aca_full_import_vars_keep0,EurekahedgeHF_Excel_aca_full_import_vars_keep1,EurekahedgeHF_Excel_aca_full_import_vars_remove)
rm2(text_stats_ios_import_vars_keep0,text_stats_ios_import_vars_keep1,text_stats_ios_import_vars_remove)
rm2(descriptive_stats_tables,descriptive_stats_fields)


###############################################################################
cat("IMPORT AND FIX FUND DATA", "\n")
###############################################################################

query_EurekahedgeHF_Excel_aca_full <- ""
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "select       ",EurekahedgeHF_Excel_aca_full_import_vars_keep2, sep=" ")
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "from         ",fund_table, "                                ", sep=" ")
query_EurekahedgeHF_Excel_aca_full <- trim(gsub(" {2,}", " ", query_EurekahedgeHF_Excel_aca_full))

rm2(EurekahedgeHF_Excel_aca_full_import_vars_keep2)

#EurekahedgeHF_Excel_aca_full <- runsql("SELECT * FROM EurekahedgeHF_Excel_aca_full14",descriptive_stats_db)

EurekahedgeHF_Excel_aca_full <- data.frame(runsql(query_EurekahedgeHF_Excel_aca_full,descriptive_stats_db),
                                           total_fee=NA,
                                           fund_ret_mkt_neg=NA,
                                           yr_month=NA,
                                           stringsAsFactors=FALSE)

colnames(EurekahedgeHF_Excel_aca_full) <- tolower(colnames(EurekahedgeHF_Excel_aca_full))

EurekahedgeHF_Excel_aca_full2 <- unknown_to_NA(EurekahedgeHF_Excel_aca_full,unknowns_strings)

rm(EurekahedgeHF_Excel_aca_full)

EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,identifier]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"yr"]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"month"]),]

#Create total fees, negative return, and yr_month
EurekahedgeHF_Excel_aca_full2[,"total_fee"] <- rowMeans(EurekahedgeHF_Excel_aca_full2[,c("management_fee","performance_fee","other_fee")],na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]<0, EurekahedgeHF_Excel_aca_full2[,"mktadjret"], 0)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]), NA, EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"])
EurekahedgeHF_Excel_aca_full2[,"yr_month"] <- paste(EurekahedgeHF_Excel_aca_full2[,"yr"],sprintf("%02d", EurekahedgeHF_Excel_aca_full2[,"month"]),sep="_")

#Trim Years
monthly_data_all_yr_trim <- EurekahedgeHF_Excel_aca_full2[(EurekahedgeHF_Excel_aca_full2[,"yr"]>=start_year & EurekahedgeHF_Excel_aca_full2[,"yr"]<=end_year),]

rm2(EurekahedgeHF_Excel_aca_full2)

#Fix dates
monthly_data_all04 <- monthly_data_all_yr_trim

rm2(monthly_data_all_yr_trim)

monthly_data_all04_date_cols <- c("date_added","dead_date","inception_date","date","chgdt")
for (i in 1:length(monthly_data_all04_date_cols))
{
  #i <- 1
  #i <- 2
  monthly_data_all04[,monthly_data_all04_date_cols[i]] <- as.Date(monthly_data_all04[,monthly_data_all04_date_cols[i]], 
                                                                  format="%Y-%m-%d", 
                                                                  origin="1970-01-01")
}
rm2(monthly_data_all04_date_cols,i)

#Scale AUM and Minimum Invstment Size
monthly_data_all04[,"aum"] <- (as.numeric(monthly_data_all04[,"aum"])/1000000)
monthly_data_all04[,"aum_lag1"] <- (as.numeric(monthly_data_all04[,"aum_lag1"])/1000000)
monthly_data_all04[,"aum_lag2"] <- (as.numeric(monthly_data_all04[,"aum_lag2"])/1000000)
monthly_data_all04[,"aum_lag3"] <- (as.numeric(monthly_data_all04[,"aum_lag3"])/1000000)
monthly_data_all04[,"minimum_investment_size"] <- (as.numeric(monthly_data_all04[,"minimum_investment_size"])/1000000)

monthly_data_all04[,"aum_lag4"] <- (as.numeric(monthly_data_all04[,"aum_lag4"])/1000000)

#Strip out comments in parenetheses
monthly_data_all_strip_comments_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                          "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")

#Rename original columns
monthly_data_all05 <- rename.vars(monthly_data_all04, 
                                  monthly_data_all_strip_comments_cols, 
                                  paste(monthly_data_all_strip_comments_cols,"_org",sep=""))

rm2(monthly_data_all04)

strip_cols <- c(monthly_data_all_strip_comments_cols, 
                paste(monthly_data_all_strip_comments_cols,"_comments",sep=""))

monthly_data_all06 <-  data.frame(monthly_data_all05, 
                                  matrix(NA, ncol=length(strip_cols), nrow=nrow(monthly_data_all05), dimnames=list(c(), strip_cols)), 
                                  stringsAsFactors=FALSE)

monthly_data_all06 <- monthly_data_all06[,sort(colnames(monthly_data_all06), decreasing = FALSE)]

monthly_data_all06 <- strip_comments(monthly_data_all06,monthly_data_all_strip_comments_cols)
monthly_data_all06 <- as.data.frame(monthly_data_all06,stringsAsFactors=FALSE)

rm2(monthly_data_all05,monthly_data_all_strip_comments_cols,strip_cols)

#Get text before comments
monthly_data_all_yn_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                              "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all07 <- create_noncomments(monthly_data_all06,monthly_data_all_yn_cols)
monthly_data_all07 <- as.data.frame(monthly_data_all07,stringsAsFactors=FALSE)

rm2(monthly_data_all06,monthly_data_all_yn_cols)

#Check for uknowns
monthly_data_all_check_unknown_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                         "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all08 <- data.table(monthly_data_all07)[, (monthly_data_all_check_unknown_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), 
                                                     .SDcols = monthly_data_all_check_unknown_cols]
monthly_data_all08 <- as.data.frame(monthly_data_all08, stringsAsFactors=FALSE)

rm2(monthly_data_all07,monthly_data_all_check_unknown_cols)

#Change not specificied to NA
NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT APPILCABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","UPON REQUEST",
                "SUBJECT TO MANAGER'S DISCRETION")
monthly_data_all_not_specified_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                         "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all09 <- not_specified_to_na(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)
monthly_data_all09 <- as.data.frame(monthly_data_all09,stringsAsFactors=FALSE)

rm2(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)

#Change no phrases to NO
NO_Phrases <- c("NIL","NONE","NONE AFTER 12 MONTHS","NONE AFTER 1ST YEAR","NO DIVIDEND","NON DIVIDEND","LITTLE OR NO")

monthly_data_all_no_phrases_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                      "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all10 <- no_to_no(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)
monthly_data_all10 <- as.data.frame(monthly_data_all10,stringsAsFactors=FALSE)

rm2(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)

#Change yes phrases to YES
YES_Phrases <- c("RARELY","OCCASIONALLY")
monthly_data_all_yes_phrases_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                       "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all11 <- yes_to_yes(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)
monthly_data_all11 <- as.data.frame(monthly_data_all11,stringsAsFactors=FALSE)

rm2(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)

#Change Y/N to binary
monthly_data_all_yn_to_bin_cols <-  c("leverage", "lock_up")

bin_cols <- paste(monthly_data_all_yn_to_bin_cols,"_bin",sep="")

monthly_data_all12 <-  data.frame(monthly_data_all11, matrix(NA, ncol=length(bin_cols), nrow=nrow(monthly_data_all11), dimnames=list(c(), bin_cols)), stringsAsFactors=FALSE)

monthly_data_all12[,bin_cols] <-  monthly_data_all12[,monthly_data_all_yn_to_bin_cols]

monthly_data_all12 <- yn_to_binary(monthly_data_all12,bin_cols)
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

monthly_data_all12 <- data.table(monthly_data_all12)[, (bin_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = bin_cols]
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

rm2(monthly_data_all11,monthly_data_all_yn_to_bin_cols,bin_cols)

#Create domicile dummy
monthly_data_all13 <- data.frame(monthly_data_all12,
                                 domicile_onshore_bin=0,
                                 stringsAsFactors=FALSE)

rm2(monthly_data_all12)

monthly_data_all13[,"domicile"] <- gsub("[[:punct:]]", "", monthly_data_all13[,"domicile"])
monthly_data_all13[,"domicile"] <- trim(monthly_data_all13[,"domicile"])


monthly_data_all13[,"domicile_onshore_bin"] <- ifelse(is.na(monthly_data_all13[,"domicile"]), NA, 
                                                      ifelse(toupper(monthly_data_all13[,"domicile"])=="UNITED STATES", 1, 
                                                             ifelse(toupper(monthly_data_all13[,"domicile"])=="USA", 1, 
                                                                    ifelse(toupper(monthly_data_all13[,"domicile"])=="US", 1, monthly_data_all13[,"domicile_onshore_bin"]))))


#Convert size to numeric
monthly_data_all_num_cols <- c("fund_size_us_m")

for (i in 1:length(monthly_data_all_num_cols)) {
  #i <- 1
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- destring(monthly_data_all13[,monthly_data_all_num_cols[i]])
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- as.numeric(monthly_data_all13[,monthly_data_all_num_cols[i]])
}

#Remove unwanted columns
monthly_data_all14 <- monthly_data_all13[,!(colnames(monthly_data_all13) %in% c("lock_up","lock_up_comments","lock_up_org",
                                                                                "leverage","leverage_comments","leverage_org",
                                                                                "domicile","domicile_comments","domicile_org",
                                                                                "fund_size_us_m_comments","fund_size_us_m_org",
                                                                                "annualized_target_return_comments","annualized_target_return_org",
                                                                                "annualized_target_volatility_comments","annualized_target_volatility_org",
                                                                                "redemption_notification_period_comments","redemption_notification_period_org",
                                                                                "subscription_frequency_comments","subscription_frequency_org"))]

rm2(monthly_data_all13,monthly_data_all_num_cols)


#Make sure that fund has a strategy category

fund_type_remove <- monthly_data_all14[is.na(monthly_data_all14[,"main_investment_strategy"]),]
fund_type_remove1 <- unique(fund_type_remove[,identifier])
fund_type_remove2 <- !is.na(fund_type_remove1)

monthly_data_all15 <- monthly_data_all14[!(monthly_data_all14[,identifier] %in% fund_type_remove2),]

rm2(monthly_data_all14)


#Make sure funds have atleast 12 months of returns
firm <- count(monthly_data_all15, c(identifier))
firm_keep <- firm[firm[,"freq"]>=12,]
firm_keep <- firm[!is.na(firm[,c(identifier)]),]
row.names(firm_keep) <- seq(nrow(firm_keep))

monthly_data_all16 <- monthly_data_all15[(monthly_data_all15[,c(identifier)] %in% firm_keep[,c(identifier)]),]
row.names(monthly_data_all16) <- seq(nrow(monthly_data_all16))

rm(monthly_data_all15,firm,firm_keep)


#Trim AUM
monthly_data_all17 <- monthly_data_all16
monthly_data_all17 <- monthly_data_all16[!is.na(monthly_data_all16[,"aum"]),]

rm2(monthly_data_all16)

monthly_data_all18 <- monthly_data_all17
monthly_data_all18 <- monthly_data_all18[monthly_data_all18[,"aum"]>=0.1,]

rm2(monthly_data_all17)


#Finalize the data
monthly_data_all19 <- monthly_data_all18

rm2(monthly_data_all18)

monthly_data_all20 <- monthly_data_all19[rowSums(is.na(monthly_data_all19[,1:ncol(monthly_data_all19)]))<ncol(monthly_data_all19),]



monthly_data_all20 <- monthly_data_all20[order(monthly_data_all20[,identifier], 
                                               monthly_data_all20[,"yr"],
                                               monthly_data_all20[,"month"]),]

row.names(monthly_data_all20) <- seq(nrow(monthly_data_all20))

rm2(monthly_data_all19)

# aa <- unique(monthly_data_all11[,!(colnames(monthly_data_all11) %in% c("yr","month"))])
# 
# 
# bb <- unique(aa[,(colnames(aa) %in% c("subscription_frequency", "redemption_notification_period","redemption_frequency"))])
# 
# cols <- c("subscription_frequency", "redemption_notification_period","redemption_frequency")
# 
# bb1 <- data.frame(bb,
#                   matrix(0, ncol=length(cols)*3, nrow=nrow(bb), 
#                          dimnames=list(c(), c(paste(cols,"day",sep="_"),
#                                               paste(cols,"month",sep="_"),
#                                               paste(cols,"year",sep="_")))), 
#                   matrix(NA, ncol=length(cols)*3, nrow=nrow(bb), 
#                          dimnames=list(c(), c(paste(cols,"combined",sep="_"),
#                                               paste(cols,"converted",sep="_"),
#                                               paste(cols,"evaluated",sep="_")))), 
#                   stringsAsFactors=FALSE)
# 
# bb1 <- bb1[,sort(colnames(bb1))]
# 
# for (j in 1:length(cols)) {
#   
#   #j <- 1
#   #j <- 2
#   #j <- 3
#   
#   bb1[,paste(cols[j],"day",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                ifelse(grepl("day", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                       ifelse(grepl("daily", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                              bb1[,paste(cols[j],"day",sep="_")])))
#   
#   bb1[,paste(cols[j],"month",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                  ifelse(grepl("month", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                         ifelse(grepl("monthly", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                                bb1[,paste(cols[j],"month",sep="_")])))          
#   
#   bb1[,paste(cols[j],"year",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                 ifelse(grepl("year", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                        ifelse(grepl("annual", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                               bb1[,paste(cols[j],"year",sep="_")])))    
#   
#   bb1[,paste(cols[j],"combined",sep="_")] <- rowSums(bb1[,c(paste(cols[j],"day",sep="_"),
#                                                             paste(cols[j],"month",sep="_"),
#                                                             paste(cols[j],"year",sep="_"))],na.rm=TRUE)
#   bb1[,paste(cols[j],"combined",sep="_")] <- ifelse((is.na(bb1[,paste(cols[j],"day",sep="_")]) 
#                                                      & is.na(bb1[,paste(cols[j],"month",sep="_")]) 
#                                                      & is.na(bb1[,paste(cols[j],"month",sep="_")])), NA, bb1[,paste(cols[j],"combined",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("[[:alpha:]]", "", bb1[,cols[j]])
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub("([0-9]+).*$", "\\1", bb1[,cols[j]])
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub('.*-([0-9]+).*','\\1',bb1[,cols[j]])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("'","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("+","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub(";","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("$","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("%","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub(",","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("/","-",bb1[,paste(cols[j],"converted",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- trim(bb1[,paste(cols[j],"converted",sep="_")])
#   
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub(" ","1",bb1[,paste(cols[j],"converted",sep="_")]) 
#   
#   # bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#   #                                                ifelse(grepl("year", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#   #                                                       ifelse(grepl("annual", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#   #                                                              bb1[,paste(cols[j],"year",sep="_")])))  
#   
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(is.na(bb1[,paste(cols[j],"combined",sep="_")]), NA, 
#                                                      ifelse(bb1[,paste(cols[j],"combined",sep="_")]==0, NA, 
#                                                             ifelse(bb1[,paste(cols[j],"combined",sep="_")]>1, NA, 
#                                                                    bb1[,paste(cols[j],"converted",sep="_")])))    
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(bb1[,paste(cols[j],"converted",sep="_")]==" ", "1", bb1[,paste(cols[j],"converted",sep="_")])  
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(bb1[,paste(cols[j],"converted",sep="_")]=="-", NA, bb1[,paste(cols[j],"converted",sep="_")]) 
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("/[[:punct:]]?$/","",bb1[,paste(cols[j],"converted",sep="_")])
#   
# } 



###############################################################################
cat("IMPORT AND FIX TEXT DATA", "\n")
###############################################################################

query_text_stats_ios_full <- ""
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "select       ",text_stats_ios_import_vars_keep2, sep=" ")
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "from         ",text_table, "                  ", sep=" ")
query_text_stats_ios_full <- trim(gsub(" {2,}", " ", query_text_stats_ios_full))

#text_stats_ios_full <- runsql("SELECT * FROM text_stats_ios",descriptive_stats_db)

text_stats_ios_full <- data.frame(runsql(query_text_stats_ios_full,descriptive_stats_db),
                                  avg_grade_level_acf_ios=NA,
                                  avg_grade_level_ac_ios=NA,
                                  yr_month=NA,
                                  stringsAsFactors=FALSE)

#Create additonal average readbility measures
text_stats_ios_full[,"avg_grade_level_acf_ios"] <- rowMeans(text_stats_ios_full[,c("ari_ios","coleman_liau_ios","flesch_kincaid_ios")],na.rm=TRUE)
text_stats_ios_full[,"avg_grade_level_ac_ios"] <- rowMeans(text_stats_ios_full[,c("ari_ios","coleman_liau_ios")],na.rm=TRUE)
text_stats_ios_full[,"yr_month"] <- paste(text_stats_ios_full[,"yr"],sprintf("%02d", text_stats_ios_full[,"month"]),sep="_")

colnames(text_stats_ios_full) <- tolower(colnames(text_stats_ios_full))

text_stats_ios_full1 <- text_stats_ios_full[!(text_stats_ios_full[,identifier] %in% fund_type_remove2),]

rm2(text_stats_ios_full)

text_stats_ios_yr_trim <- text_stats_ios_full1[(text_stats_ios_full1[,"yr"]>=start_year & text_stats_ios_full1[,"yr"]<=end_year),]

rm2(text_stats_ios_full1)

text_stats_ios_sim_cols <- names(text_stats_ios_yr_trim)[grep("_similarity", names(text_stats_ios_yr_trim))] 

text_stats_ios <- text_stats_ios_yr_trim
for (i in 1:length(text_stats_ios_sim_cols))
{
  #i <- 1
  
  text_stats_ios <- text_stats_ios[!(is.na(text_stats_ios[,text_stats_ios_sim_cols[i]])),]
  
}

text_stats_ios_trim <- text_stats_ios[((!is.na(text_stats_ios[,"ari_ios"])) & 
                                         (!is.na(text_stats_ios[,"coleman_liau_ios"])) & 
                                         (!is.na(text_stats_ios[,"flesch_kincaid_ios"])) & 
                                         (!is.na(text_stats_ios[,"fog_ios"])) & 
                                         (!is.na(text_stats_ios[,"smog_ios"]))),]

rm2(text_stats_ios_import_vars_keep2,text_stats_ios_yr_trim,text_stats_ios_sim_cols,text_stats_ios)


###############################################################################
cat("MERGE FUND AND TEXT DATA", "\n")
###############################################################################

data0 <- merge(monthly_data_all20[,!(colnames(monthly_data_all20) %in% c("main_investment_strategy"))], text_stats_ios_trim, 
               by.x=c(identifier,"yr","month","yr_month"), by.y=c(identifier,"yr","month","yr_month"), 
               all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

data0 <- data0[order(data0[,identifier],
                     data0[,"yr"],
                     data0[,"month"],
                     data0[,"yr_month"]),]
row.names(data0) <- seq(nrow(data0))

rm2(fund_table,text_table,fund_type_remove,fund_type_remove2)
#rm2(monthly_data_all20,text_stats_ios_trim)


###############################################################################
cat("IMPORT AND FIX FACTOR DATA", "\n")
###############################################################################

#CARHART FACTORS
ffm_factors0 <- read.csv(file=paste(input_directory,"Factors\\Other_Factors\\","Factors_monthly.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
colnames(ffm_factors0) <- tolower(colnames(ffm_factors0))
colnames(ffm_factors0)[match("date",colnames(ffm_factors0))] <- c("time_period")
colnames(ffm_factors0)[match("year",colnames(ffm_factors0))] <- c("yr")

ffm_factors <- ffm_factors0[,!(colnames(ffm_factors0) %in% c("time_period","dateff"))]


#LIQUIDITY FACTORS
liquidity_factors0 <- read.csv(file=paste(input_directory,"Factors\\Other_Factors\\","Liquidity_factors.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
colnames(liquidity_factors0) <- tolower(colnames(liquidity_factors0))
colnames(liquidity_factors0)[match("month",colnames(liquidity_factors0))] <- c("time_period")

liquidity_factors0  <- data.frame(month=NA,
                                  yr=NA,
                                  liquidity_factors0,
                                  stringsAsFactors=FALSE)

liquidity_factors0[,"yr"] <- as.integer(substr(as.character(liquidity_factors0[,"time_period"]),1,4))
liquidity_factors0[,"month"] <- as.integer(substr(as.character(liquidity_factors0[,"time_period"]),5,6))

#liquidity_factors <- liquidity_factors0[,!(colnames(liquidity_factors0) %in% c("time_period","levels_of_aggregate_liquidity","innovations_in_aggregate_liquidity","notes"))]
liquidity_factors <- liquidity_factors0[,!(colnames(liquidity_factors0) %in% c("time_period","notes"))]
liquidity_factors <- liquidity_factors[!(liquidity_factors[,"traded_liquidity_factor"]==-99),]


#HEDGE FUND FACTORS
hedge_fund_risk_factors0 <- read.csv(file=paste(input_directory,"Factors\\","hedge_fund_risk_factors.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
colnames(hedge_fund_risk_factors0) <- tolower(colnames(hedge_fund_risk_factors0))

hedge_fund_risk_factors <- hedge_fund_risk_factors0[,!(colnames(hedge_fund_risk_factors0) %in% c("ptfsir","ptfsstk"))]


#MERGE FACTORS

factors_merge1 <- merge(ffm_factors,liquidity_factors,
                        by.x=c("yr","month"), by.y=c("yr","month"), 
                        all.x=TRUE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

factors_merge2 <- merge(factors_merge1,hedge_fund_risk_factors,
                        by.x=c("yr","month"), by.y=c("yr","month"), 
                        all.x=TRUE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

factors_merge <- factors_merge2[order(factors_merge2[,"yr"],
                                      factors_merge2[,"month"]),] 

row.names(factors_merge) <- seq(nrow(factors_merge))

rm2(hedge_fund_risk_factors0,ffm_factors0,liquidity_factors0)
rm2(hedge_fund_risk_factors,ffm_factors,liquidity_factors)
rm2(factors_merge1,factors_merge2)


###############################################################################
cat("MERGE IN FACTORS AND ALPHAS", "\n")
###############################################################################

fund_return_var <- c("monthly_ret","vwretx","vwretd","mktadjret")
#fund_return_var <- c("monthly_ret")

data1_nofactors <- data0[,!(colnames(data0) %in% fund_return_var)]

data1_factors0 <- merge(data0[,c(identifier,"yr","month",fund_return_var)], factors_merge, 
                        by.x=c("yr","month"), by.y=c("yr","month"), 
                        all.x=FALSE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data1_factors0  <- data.frame(data1_factors0, 
                              exret=data1_factors0[,"monthly_ret"] - data1_factors0[,"rf"],
                              abret=data1_factors0[,"monthly_ret"] - data1_factors0[,"rf"] - data1_factors0[,"mktrf"],
                              
                              stringsAsFactors=FALSE)

data1_factors0 <- data1_factors0[order(data1_factors0[,identifier],
                                       data1_factors0[,"yr"],
                                       data1_factors0[,"month"]),]
row.names(data1_factors0) <- seq(nrow(data1_factors0))

lag_vars <- c("exret")
lag_count <- 4

data1_factors <- data.frame(data1_factors0,
                            matrix(NA, ncol=length(lag_vars)*lag_count, nrow=nrow(data1_factors0)),
                            stringsAsFactors=FALSE)
colnames(data1_factors) <- c(colnames(data1_factors0),
                             paste(lag_vars[1],"_lag",seq(1,lag_count),sep=""))

for (i in 1:length(lag_vars))
{
  #i <- 1
  #i <- 2
  
  data1_factors[,paste(lag_vars[i],"_lag",seq(1,lag_count),sep="")] <-  create_lags2(data1_factors,lag_vars[i],identifier,lag_count)
  
} 

data1_factors <- data1_factors[,c(identifier,  colnames(data1_factors[,!(colnames(data1_factors) %in% c(identifier))]))]


rm2(data0,factors_merge,data1_factors0,lag_vars,lag_count,i)


###############################################################################
cat("COMPUTE ALPHAS", "\n")
###############################################################################

#Models
regression_equations_alpha <- data.frame(id=NA,
                                         include=NA,
                                         description=NA,
                                         model=NA,
                                         stringsAsFactors=FALSE)
regression_equations_alpha[1,] <- c(1,0,"mret","monthly_ret ~ ")
regression_equations_alpha[2,] <- c(2,0,"abret","abret ~ ")
regression_equations_alpha[3,] <- c(3,0,"mktrf","exret ~ mktrf")
regression_equations_alpha[4,] <- c(4,1,"ff","exret ~ mktrf + smb + hml")
regression_equations_alpha[5,] <- c(5,1,"ffm","exret ~ mktrf + smb + hml + umd")
regression_equations_alpha[6,] <- c(6,1,"ffml","exret ~ mktrf + smb + hml + innovations_in_aggregate_liquidity")
#regression_equations_alpha[6,] <- c(6,1,"ffml","exret ~ mktrf + smb + hml + umd+ traded_liquidity_factor")
regression_equations_alpha[7,] <- c(7,1,"hf7","exret ~ ptfsbd + ptfsfx + ptfscom + equity_market_factor + size_spread_factor + bond_market_factor + credit_spread_factor")
regression_equations_alpha[8,] <- c(8,1,"hf8","exret ~ ptfsbd + ptfsfx + ptfscom + equity_market_factor + size_spread_factor + bond_market_factor + credit_spread_factor + emerging_market_index")

regression_equations_alpha_include <- regression_equations_alpha[regression_equations_alpha[,c("include")]==1,]

#firm_keep_temp <- unique(data1_factors[,c(identifier)])
#data_alphas <- data1_factors[data1_factors[,c(identifier)] %in% firm_keep_temp[1:10],]

data_alphas <- ddply(data1_factors, identifier, rolling_reg_sub,equations=regression_equations_alpha_include,width=12,
                     .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
data_alphas <- ddply(data_alphas, identifier, rolling_reg_sub,equations=regression_equations_alpha_include,width=24,
                     .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
data_alphas <- ddply(data_alphas, identifier, rolling_reg_sub,equations=regression_equations_alpha_include,width=36,
                     .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
data_alphas <- ddply(data_alphas, identifier, rolling_reg_sub,equations=regression_equations_alpha_include,width=48,
                     .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
data_alphas <- ddply(data_alphas, identifier, rolling_reg_sub,equations=regression_equations_alpha_include,width=60,
                     .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

write.csv(data_alphas,file=paste(output_directory,"data_alphas.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(regression_equations_alpha,regression_equations_alpha_include)
#rm2(data1_factors)


###############################################################################
cat("MERGE IN FUNDS AND ALPHAS", "\n")
###############################################################################

#data_alphas <- read.csv(file=paste(output_directory,"data_alphas.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

#Get end of period alphas
data_alphas_period_trim0 <- data_alphas[,c(identifier,"yr",
                                           "month",
                                           colnames(data_alphas[,!(colnames(data_alphas) %in% c(identifier,"yr","month"))]))]

colnames(data_alphas_period_trim0) <- c(identifier,
                                        "yr",
                                        "month",
                                        paste(colnames(data_alphas_period_trim0[,!(colnames(data_alphas_period_trim0) %in% c(identifier,"yr","month"))]),"trim",sep="_"))

data_alphas_period_trim <- data_alphas_period_trim0[(data_alphas_period_trim0[,"yr"] %in% c(1999,2005,2011)
                                                     & data_alphas_period_trim0[,"month"] %in% c(12)),]

rm2(data_alphas_period_trim0)

#data_alphas <- data_alphas[data_alphas[,c("month")]==12,]
#row.names(data_alphas) <- seq(nrow(data_alphas))
data_alphas_full <- merge(data_alphas, data_alphas_period_trim[,c("yr","month",identifier,names(data_alphas_period_trim)[grep("int_", names(data_alphas_period_trim))])],
                          by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                          all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

rm2(data_alphas,data_alphas_period_trim)


data2_full <- merge(data1_nofactors, data_alphas_full, 
                    by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                    all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

#rm2(data1_nofactors,data_alphas_full)

data2_na_cols <- c("yr","pflow","nflow","aum", "mktadjret","mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_sq")
#"mnav_agg","mtna_agg","log_mtna_agg","age_y","sddret_agg","sddret_agg","turn_ratio_agg","exp_ratio_agg","mgmt_fee_agg"

data2_no_na <- data2_full

rm2(data2_full)

for (i in 1:length(data2_na_cols))
{
  #i <- 1
  data2_no_na <- data2_no_na[!(is.na(data2_no_na[,data2_na_cols[i]])),]
  
}
rm2(i,data2_na_cols)



###############################################################################
cat("COMPUTE ADDITIONAL VARIABLES", "\n")
###############################################################################

#EXRET_squared
#EXRET_neg
#ALPHAS_squared
#ALPHAs_neg


###############################################################################
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("nflow","nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                    "sdnet_flow","sdnet_flow_lag1",
                    "pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                    "sdpct_flow","sdpct_flow_lag1",
                    "exret","exret_lag1","exret_lag2","exret_lag3","exret_lag4",
                    "sharpe_ratio","sortino_ratio","minimum_investment_size")

data2 <- data2_no_na
for (i in 1:length(winsorize_vars))
{
  #i <- 1
  #i <- 2
  data2[,winsorize_vars[i]] <- 
    winsorize_both(data2[,winsorize_vars[i]],q=0.025)
  
} 
rm2(data2_no_na,winsorize_vars,i)


###############################################################################
cat("DESCRIPTIVE STATISTICS - VARIABLES", "\n")
###############################################################################

descrip_stats_data0 <- data2

descrip_stats_fund_vars_remove <- c("month",
                                    "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                    "nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                                    "pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                    "age_m","chgdt",
                                    "mktadjret_lag1_sq","mktadjret_lag2_sq","mktadjret_lag3_sq","mktadjret_lag4_sq",
                                    "sdnet_flow_lag1","sdpct_flow_lag1")

descrip_stats_data0 <- descrip_stats_data0[,!(colnames(descrip_stats_data0) %in% descrip_stats_fund_vars_remove)]

descrip_stats_ios_vars_remove <- c("month",
                                   "punct_ios","conjunctions_ios","prepositions_ios","normalized_space_ios", 
                                   "pronouns_ios","ttr_ios")

descrip_stats_data0 <- descrip_stats_data0[,!(colnames(descrip_stats_data0) %in% descrip_stats_ios_vars_remove)]


descrip_stats_data <- data.frame(descrip_stats_data0,
                                 year_group_id=NA,
                                 stringsAsFactors=FALSE)

descrip_stats_data[,"year_group_id"] <- ifelse((descrip_stats_data[,"yr"]>=1994 & descrip_stats_data[,"yr"]<=1999), 1, 
                                               ifelse((descrip_stats_data[,"yr"]>=2000 & descrip_stats_data[,"yr"]<=2005), 2, 
                                                      ifelse((descrip_stats_data[,"yr"]>=2006 & descrip_stats_data[,"yr"]<=2011), 3, 
                                                             descrip_stats_data[,"year_group_id"])))   

rm2(descrip_stats_data0)

descrip_stats_ios_sim_cols <- names(descrip_stats_data)[grep("pct_ios", names(descrip_stats_data))] 

descriptive_overall_vars_model1 <- list(note="PA",
                                        vars=c("pflow","sdpct_flow","exret","mktadjret","mktadjret_sq","fund_ret_mkt_neg",
                                               "aum","age_y",
                                               "total_fee","management_fee","performance_fee","other_fee",
                                               "sharpe_ratio","sortino_ratio","minimum_investment_size",
                                               "listed_on_exchange_bin","hurdle_rate_bin","high_water_mark_bin",
                                               "domicile_onshore_bin","leverage_bin","lock_up_bin",
                                               "flagship_bin","closed_bin","dead_bin"))
descriptive_overall_vars_model2 <- list(note="PB",
                                        vars=c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                                               "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                                               "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                                               "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                                               "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",descrip_stats_ios_sim_cols))
descriptive_overall_vars_model3 <- list(note="PC",
                                        vars=c("int_nonloading_ff_12","int_loading_ff_12","int_nonloading_ffm_12","int_loading_ffm_12","int_nonloading_ffml_12","int_loading_ffml_12",
                                               "int_nonloading_hf7_12","int_loading_hf7_12","int_nonloading_hf8_12","int_loading_hf8_12",
                                               "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
                                               "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24",
                                               "int_nonloading_ff_36","int_loading_ff_36","int_nonloading_ffm_36","int_loading_ffm_36","int_nonloading_ffml_36","int_loading_ffml_36",
                                               "int_nonloading_hf7_36","int_loading_hf7_36","int_nonloading_hf8_36","int_loading_hf8_36",
                                               "int_nonloading_ff_48","int_loading_ff_48","int_nonloading_ffm_48","int_loading_ffm_48","int_nonloading_ffml_48","int_loading_ffml_48",
                                               "int_nonloading_hf7_48","int_loading_hf7_48","int_nonloading_hf8_48","int_loading_hf8_48",
                                               "int_nonloading_ff_60","int_loading_ff_60","int_nonloading_ffm_60","int_loading_ffm_60","int_nonloading_ffml_60","int_loading_ffml_60",
                                               "int_nonloading_hf7_60","int_loading_hf7_60","int_nonloading_hf8_60","int_loading_hf8_60"))
descriptive_overall_vars_model4 <- list(note="PD",
                                        vars=c("int_nonloading_ff_12_trim","int_loading_ff_12_trim","int_nonloading_ffm_12_trim","int_loading_ffm_12_trim","int_nonloading_ffml_12_trim","int_loading_ffml_12_trim",
                                               "int_nonloading_hf7_12_trim","int_loading_hf7_12_trim","int_nonloading_hf8_12_trim","int_loading_hf8_12_trim",
                                               "int_nonloading_ff_24_trim","int_loading_ff_24_trim","int_nonloading_ffm_24_trim","int_loading_ffm_24_trim","int_nonloading_ffml_24_trim","int_loading_ffml_24_trim",
                                               "int_nonloading_hf7_24_trim","int_loading_hf7_24_trim","int_nonloading_hf8_24_trim","int_loading_hf8_24_trim",
                                               "int_nonloading_ff_36_trim","int_loading_ff_36_trim","int_nonloading_ffm_36_trim","int_loading_ffm_36_trim","int_nonloading_ffml_36_trim","int_loading_ffml_36_trim",
                                               "int_nonloading_hf7_36_trim","int_loading_hf7_36_trim","int_nonloading_hf8_36_trim","int_loading_hf8_36_trim",
                                               "int_nonloading_ff_48_trim","int_loading_ff_48_trim","int_nonloading_ffm_48_trim","int_loading_ffm_48_trim","int_nonloading_ffml_48_trim","int_loading_ffml_48_trim",
                                               "int_nonloading_hf7_48_trim","int_loading_hf7_48_trim","int_nonloading_hf8_48_trim","int_loading_hf8_48_trim",
                                               "int_nonloading_ff_60_trim","int_loading_ff_60_trim","int_nonloading_ffm_60_trim","int_loading_ffm_60_trim","int_nonloading_ffml_60_trim","int_loading_ffml_60_trim",
                                               "int_nonloading_hf7_60_trim","int_loading_hf7_60_trim","int_nonloading_hf8_60_trim","int_loading_hf8_60_trim"))

descriptive_overall_vars_model <- list(descriptive_overall_vars_model1,descriptive_overall_vars_model2,
                                       descriptive_overall_vars_model3,descriptive_overall_vars_model4)

descriptive_overall_vars_model_note <- sapply(descriptive_overall_vars_model, "[[", "note")
descriptive_overall_vars_model_vars <- sapply(descriptive_overall_vars_model, "[[", "vars")

descriptive_overall_vars_model_vars_all <- unlist(descriptive_overall_vars_model_vars)
descriptive_overall_vars_model_vars_all <- data.frame(id=NA,
                                                      descriptive_overall_vars_model_vars_all, 
                                                      stringsAsFactors=FALSE)
descriptive_overall_vars_model_vars_all[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_all))
colnames(descriptive_overall_vars_model_vars_all)[2] <- "var"

descriptive_overall_vars_model_vars_all <- descriptive_overall_vars_model_vars_all[order(descriptive_overall_vars_model_vars_all[,"id"],
                                                                                         descriptive_overall_vars_model_vars_all[,"var"]),]
row.names(descriptive_overall_vars_model_vars_all) <- seq(nrow(descriptive_overall_vars_model_vars_all))


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB)", "\n")
###############################################################################

descriptive_overall_groups <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                         stringsAsFactors=FALSE)

descriptive_overall_groups[1,] <- c(start_year,end_year)
descriptive_overall_groups[2,] <- c(1994,1999)
descriptive_overall_groups[3,] <- c(2000,2011)
descriptive_overall_groups[4,] <- c(2000,2005)
descriptive_overall_groups[5,] <- c(2006,2011)

# descriptive_overall_groups1 <- list(Start_yr=start_year,End_yr=end_year)
# descriptive_overall_groups2 <- list(Start_yr=1994,End_yr=1999)
# descriptive_overall_groups3 <- list(Start_yr=2000,End_yr=2011)
# descriptive_overall_groups4 <- list(Start_yr=2000,End_yr=2005)
# descriptive_overall_groups5 <- list(Start_yr=2006,End_yr=2011)
# 
# descriptive_overall_groups <- list(descriptive_overall_groups1,
#                                    descriptive_overall_groups2,
#                                    descriptive_overall_groups3,
#                                    descriptive_overall_groups4,
#                                    descriptive_overall_groups5)
# 
# descriptive_overall_groups_Start_yr <- sapply(descriptive_overall_groups, "[[", "Start_yr")
# descriptive_overall_groups_End_yr <- sapply(descriptive_overall_groups, "[[", "End_yr")

output_directory_descrip_stats_overall <- paste(output_directory,"descriptive_stats_overall","\\",sep="")
create_directory(output_directory_descrip_stats_overall,remove=1)

for (k in 1:nrow(descriptive_overall_groups))
{
  #k <- 1
  #k <- 2
  
  cat("START YEAR:", descriptive_overall_groups[k,1], "END YEAR:", descriptive_overall_groups[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups[k,2]),]
  
  fund_count <- as.numeric(length(unique(data_temp[,identifier],comparables=FALSE)))
  
  data_temp_no_id <- data_temp[,!(colnames(data_temp) %in% identifier)]
  
  descriptive_stats_temp_full_all_var <- describe2(data_temp_no_id[,descriptive_overall_vars_model_vars_all[,c("var")]])
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    
    
    
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name <- paste("descriptive_stats",descriptive_overall_groups[k,1],descriptive_overall_groups[k,2],descriptive_overall_vars_model_note_temp,"overall",sep="_")
    
    descriptive_stats_temp_full <- descriptive_stats_temp_full_all_var[descriptive_stats_temp_full_all_var[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)] <- format(round(descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)],  digits = 8))
    descriptive_stats_temp_full <- rbind(descriptive_stats_temp_full,c("number_of_funds",fund_count,matrix("", ncol=(ncol(descriptive_stats_temp_full)-2), nrow=1)))
    
    write.csv(descriptive_stats_temp_full,file=paste(output_directory_descrip_stats_overall,out_file_name,"_full.csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    descriptive_stats_temp <- descriptive_stats_temp_full[c("var","n","quartile1","median","mean","quartile3","sd")]
    #descriptive_stats_temp <- descriptive_stats_temp[match(descriptive_overall_vars_model_vars_temp, descriptive_stats_temp[,c("var")]),]
    write.csv(descriptive_stats_temp,file=paste(output_directory_descrip_stats_overall,out_file_name,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name)
    rm2(descriptive_stats_temp_full,descriptive_stats_temp)
    
  }
  rm2(data_temp,fund_count,data_temp_no_id,descriptive_stats_temp_full_all_var,l)
  
}
rm2(descriptive_overall_groups,output_directory_descrip_stats_overall,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR ", "\n")
###############################################################################

descriptive_overall_groups_by_year <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                 stringsAsFactors=FALSE)

descriptive_overall_groups_by_year[1,] <- c(start_year,end_year)

descriptive_stats_by_var_year <- "yr"

descriptive_stats_by_year <- c("mean","median")

output_directory_descrip_stats_by_year <- paste(output_directory,"descriptive_stats_yearly","\\",sep="")
create_directory(output_directory_descrip_stats_by_year,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_year))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_year[k,1], "END YEAR:", descriptive_overall_groups_by_year[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_year[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_year[k,2]),]
  
  fund_count_yr1 <- ddply(data_temp, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                            count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                            stringsAsFactors=FALSE)})
  fund_count_yr2 <- data.frame(temp_var="ZZZ",
                               var="number_of_funds", 
                               count=as.numeric(length(unique(data_temp[,identifier],comparables=FALSE))),
                               stringsAsFactors=FALSE)
  colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_year
  
  fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)
  
  data_temp_no_id <- data_temp[,!(colnames(data_temp) %in% identifier)]
  
  #descriptive_stats_temp_full_all_var_year <- describeBy2(descrip_stats_fund2,"yr")
  
  descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)
  
  assign("descriptive_stats_temp_full_all_var_year", descriptive_stats_temp_full_all_var_year, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_year[k,1],descriptive_overall_groups_by_year[k,2],descriptive_overall_vars_model_note_temp,"year",sep="_")
    
    descriptive_stats_yr_temp_full_trim <- descriptive_stats_temp_full_all_var_year[descriptive_stats_temp_full_all_var_year[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    
    for (m in 1:length(descriptive_stats_by_year))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_temp <- fund_count_yr
      colnames(fund_count_yr_temp)[match("count",names(fund_count_yr_temp))] <- descriptive_stats_by_year[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_year[m],sep="_")
      
      descriptive_stats_yr_temp_trim <- rbind(descriptive_stats_yr_temp_full_trim[c(descriptive_stats_by_var_year,"var",descriptive_stats_by_year[m])],
                                              fund_count_yr_temp)
      
      #descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, var~yr))
      descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, 
                                                          eval(parse(text=paste("var",descriptive_stats_by_var_year,sep="~")))))
      
      descriptive_stats_yr_temp2 <- merge(descriptive_stats_yr_temp, descriptive_overall_vars_model_vars_temp, 
                                          by.x=c("var"), by.y=c("var"), 
                                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[order(descriptive_stats_yr_temp2[,"id"]),]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      colnames(descriptive_stats_yr_temp2)[match("ZZZ",names(descriptive_stats_yr_temp2))] <- "Full"
      
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[,!(colnames(descriptive_stats_yr_temp2) %in% c("id"))]
      
      #Remove digits on number_of_funds
      descriptive_stats_yr_temp3 <- descriptive_stats_yr_temp2
      descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)] <- format(round(descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_yr_temp3[,1]=="number_of_funds"))
      {
        descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)] <- sprintf("%.0f",descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)]) 
      }
      
      #descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))] <- format(round(descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))],  digits = 6))
      
      descriptive_stats_yr_temp4 <- apply(descriptive_stats_yr_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_yr_temp4 <- as.data.frame(descriptive_stats_yr_temp4,stringsAsFactors=FALSE)
      
      
      write.csv(descriptive_stats_yr_temp4,file=paste(output_directory_descrip_stats_by_year,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_temp,out_file_name2,descriptive_stats_yr_temp_trim)
      rm2(descriptive_stats_yr_temp,descriptive_stats_yr_temp2,descriptive_stats_yr_temp3,descriptive_stats_yr_temp4)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_yr_temp_full_trim)
    
  }
  rm2(data_temp,fund_count_yr1,fund_count_yr2,fund_count_yr,data_temp_no_id,l)
  
}
rm2(descriptive_stats_by_var_year,descriptive_overall_groups_by_year,descriptive_stats_by_year,output_directory_descrip_stats_by_year,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR GROUP", "\n")
###############################################################################

descriptive_overall_groups_by_year_group <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                       stringsAsFactors=FALSE)

descriptive_overall_groups_by_year_group[1,] <- c(start_year,end_year)

descriptive_stats_by_var_year <- "year_group_id"

descriptive_stats_by_year_group <- c("mean","median")

output_directory_descrip_stats_by_year_group <- paste(output_directory,"descriptive_stats_yearly_group","\\",sep="")
create_directory(output_directory_descrip_stats_by_year_group,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_year_group))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_year_group[k,1], "END YEAR:", descriptive_overall_groups_by_year_group[k,2],"\n")
  
  data_temp_year_group <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_year_group[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_year_group[k,2]),]
  
  fund_count_yr_group1 <- ddply(data_temp_year_group, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                                             count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                                             stringsAsFactors=FALSE)})
  fund_count_yr_group2 <- data.frame(temp_var="ZZZ",
                                     var="number_of_funds", 
                                     count=as.numeric(length(unique(data_temp_year_group[,identifier],comparables=FALSE))),
                                     stringsAsFactors=FALSE)
  colnames(fund_count_yr_group2)[match("temp_var",names(fund_count_yr_group2))] <- descriptive_stats_by_var_year
  
  fund_count_yr_group <- rbind(fund_count_yr_group1,fund_count_yr_group2)
  
  data_temp_year_group_no_id <- data_temp_year_group[,!(colnames(data_temp_year_group) %in% identifier)]
  
  #descriptive_stats_temp_full_all_var_year_group <- describeBy2(descrip_stats_fund2,"yr")
  
  descriptive_stats_temp_full_all_var_year_group <- describeBy2(data_temp_year_group_no_id[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)
  
  assign("descriptive_stats_temp_full_all_var_year_group", descriptive_stats_temp_full_all_var_year_group, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_year_group[k,1],descriptive_overall_groups_by_year_group[k,2],descriptive_overall_vars_model_note_temp,"year",sep="_")
    
    descriptive_stats_yr_temp_full_trim <- descriptive_stats_temp_full_all_var_year_group[descriptive_stats_temp_full_all_var_year_group[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    
    for (m in 1:length(descriptive_stats_by_year_group))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_group_temp <- fund_count_yr_group
      colnames(fund_count_yr_group_temp)[match("count",names(fund_count_yr_group_temp))] <- descriptive_stats_by_year_group[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_year_group[m],sep="_")
      
      descriptive_stats_yr_temp_trim <- rbind(descriptive_stats_yr_temp_full_trim[c(descriptive_stats_by_var_year,"var",descriptive_stats_by_year_group[m])],
                                              fund_count_yr_group_temp)
      
      #descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, var~yr))
      descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, 
                                                          eval(parse(text=paste("var",descriptive_stats_by_var_year,sep="~")))))
      
      descriptive_stats_yr_temp2 <- merge(descriptive_stats_yr_temp, descriptive_overall_vars_model_vars_temp, 
                                          by.x=c("var"), by.y=c("var"), 
                                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[order(descriptive_stats_yr_temp2[,"id"]),]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      colnames(descriptive_stats_yr_temp2)[match("ZZZ",names(descriptive_stats_yr_temp2))] <- "Full"
      
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[,!(colnames(descriptive_stats_yr_temp2) %in% c("id"))]
      
      #Remove digits on number_of_funds
      descriptive_stats_yr_temp3 <- descriptive_stats_yr_temp2
      descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)] <- format(round(descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_yr_temp3[,1]=="number_of_funds"))
      {
        descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)] <- sprintf("%.0f",descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)]) 
      }
      
      #descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))] <- format(round(descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))],  digits = 6))
      
      descriptive_stats_yr_temp4 <- apply(descriptive_stats_yr_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_yr_temp4 <- as.data.frame(descriptive_stats_yr_temp4,stringsAsFactors=FALSE)
      
      
      write.csv(descriptive_stats_yr_temp4,file=paste(output_directory_descrip_stats_by_year_group,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_group_temp,out_file_name2,descriptive_stats_yr_temp_trim)
      rm2(descriptive_stats_yr_temp,descriptive_stats_yr_temp2,descriptive_stats_yr_temp3,descriptive_stats_yr_temp4)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_yr_temp_full_trim)
    
  }
  rm2(data_temp_year_group,fund_count_yr_group1,fund_count_yr_group2,fund_count_yr_group,data_temp_year_group_no_id,l)
  
}
rm2(descriptive_stats_by_var_year,descriptive_overall_groups_by_year_group,descriptive_stats_by_year_group,output_directory_descrip_stats_by_year_group,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY STRATEGY", "\n")
###############################################################################

descriptive_overall_groups_by_strategy <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                     stringsAsFactors=FALSE)

descriptive_overall_groups_by_strategy[1,] <- c(start_year,end_year)
descriptive_overall_groups_by_strategy[2,] <- c(1994,1999)
descriptive_overall_groups_by_strategy[3,] <- c(2000,2011)
descriptive_overall_groups_by_strategy[4,] <- c(2000,2005)
descriptive_overall_groups_by_strategy[5,] <- c(2006,2011)

descriptive_stats_by_var_strategy <- "main_investment_strategy"

descriptive_stats_by_strategy <- c("mean","median")

output_directory_descrip_stats_by_strategy <- paste(output_directory,"descriptive_stats_strategy","\\",sep="")
create_directory(output_directory_descrip_stats_by_strategy,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_strategy))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_strategy[k,1], "END YEAR:", descriptive_overall_groups_by_strategy[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_strategy[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_strategy[k,2]),]
  
  fund_count_yr1 <- ddply(data_temp, descriptive_stats_by_var_strategy, function(x) {data.frame(var="number_of_funds", 
                                                                                                count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                                stringsAsFactors=FALSE)})
  fund_count_yr2 <- data.frame(temp_var="ZZZ",
                               var="number_of_funds", 
                               count=as.numeric(length(unique(data_temp[,identifier],comparables=FALSE))),
                               stringsAsFactors=FALSE)
  colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_strategy
  
  fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)
  
  data_temp_no_id <- data_temp[,!(colnames(data_temp) %in% identifier)]
  
  #descriptive_stats_temp_full_all_var_strategy <- describeBy2(descrip_stats_fund2,"yr")
  descriptive_stats_temp_full_all_var_strategy <- describeBy2(data_temp_no_id[,c(descriptive_stats_by_var_strategy,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_strategy)
  
  assign("descriptive_stats_temp_full_all_var_strategy", descriptive_stats_temp_full_all_var_strategy, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_strategy[k,1],descriptive_overall_groups_by_strategy[k,2],descriptive_overall_vars_model_note_temp,"strategy",sep="_")
    
    descriptive_stats_strategy_temp_full_trim <- descriptive_stats_temp_full_all_var_strategy[descriptive_stats_temp_full_all_var_strategy[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    
    for (m in 1:length(descriptive_stats_by_strategy))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_temp <- fund_count_yr
      colnames(fund_count_yr_temp)[match("count",names(fund_count_yr_temp))] <- descriptive_stats_by_strategy[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_strategy[m],sep="_")
      
      descriptive_stats_strategy_temp_trim <- rbind(descriptive_stats_strategy_temp_full_trim[c(descriptive_stats_by_var_strategy,"var",descriptive_stats_by_strategy[m])],
                                                    fund_count_yr_temp)
      
      #descriptive_stats_strategy_temp <- suppressMessages(dcast(descriptive_stats_strategy_temp_trim, var~yr))
      descriptive_stats_strategy_temp <- suppressMessages(dcast(descriptive_stats_strategy_temp_trim, 
                                                                eval(parse(text=paste("var",descriptive_stats_by_var_strategy,sep="~")))))
      
      descriptive_stats_strategy_temp2 <- merge(descriptive_stats_strategy_temp, descriptive_overall_vars_model_vars_temp, 
                                                by.x=c("var"), by.y=c("var"), 
                                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_strategy_temp2 <- descriptive_stats_strategy_temp2[order(descriptive_stats_strategy_temp2[,"id"]),]
      row.names(descriptive_stats_strategy_temp2) <- seq(nrow(descriptive_stats_strategy_temp2))
      colnames(descriptive_stats_strategy_temp2)[match("ZZZ",names(descriptive_stats_strategy_temp2))] <- "Full"
      
      descriptive_stats_strategy_temp2 <- descriptive_stats_strategy_temp2[,!(colnames(descriptive_stats_strategy_temp2) %in% c("id"))]
      
      #Remove digits on number_of_funds
      descriptive_stats_strategy_temp3 <- descriptive_stats_strategy_temp2
      descriptive_stats_strategy_temp3[,2:ncol(descriptive_stats_strategy_temp3)] <- format(round(descriptive_stats_strategy_temp3[,2:ncol(descriptive_stats_strategy_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_strategy_temp3[,1]=="number_of_funds"))
      {
        #i <- 25
        
        descriptive_stats_strategy_temp3[i,2:ncol(descriptive_stats_strategy_temp3)] <- sprintf("%.0f",descriptive_stats_strategy_temp3[i,2:ncol(descriptive_stats_strategy_temp3)])
        
      }
      
      descriptive_stats_strategy_temp4 <- apply(descriptive_stats_strategy_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_strategy_temp4 <- as.data.frame(descriptive_stats_strategy_temp4,stringsAsFactors=FALSE)
      
      
      descriptive_stats_strategy_temp5 <- descriptive_stats_strategy_temp4[,c(colnames(descriptive_stats_strategy_temp4[,!(colnames(descriptive_stats_strategy_temp4) %in% c("Others","Full"))]),
                                                                              "Others",
                                                                              "Full")]
      
      write.csv(descriptive_stats_strategy_temp5,file=paste(output_directory_descrip_stats_by_strategy,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_temp,out_file_name2,descriptive_stats_strategy_temp_trim)
      rm2(descriptive_stats_strategy_temp,descriptive_stats_strategy_temp2,descriptive_stats_strategy_temp3)
      rm2(descriptive_stats_strategy_temp4,descriptive_stats_strategy_temp5)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_strategy_temp_full_trim)
    
  }
  rm2(data_temp,fund_count_yr1,fund_count_yr2,fund_count_yr,data_temp_no_id,l)
  
  
}
rm2(descriptive_stats_by_var_strategy,descriptive_overall_groups_by_strategy,descriptive_stats_by_strategy,output_directory_descrip_stats_by_strategy,k)

rm2(descrip_stats_data,descrip_stats_fund_vars_remove,descrip_stats_ios_vars_remove)
rm2(descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3,descriptive_overall_vars_model4)
rm2(descriptive_overall_vars_model)
rm2(descriptive_overall_vars_model_note,descriptive_overall_vars_model_vars,descriptive_overall_vars_model_vars_all)


###############################################################################
cat("CORRELATION MATRIX (PANEL A & B)", "\n")
###############################################################################

corr_decimals <- 3

corr_text_vars_ios_sim <- descrip_stats_ios_sim_cols[grep("_900pct_ios", descrip_stats_ios_sim_cols)] 

corr_text_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",corr_text_vars_ios_sim)

#correlation_stars_PA <- corstar(data2[,corr_text_vars_ios],round=corr_decimals)
correlation_stars_PA0 <- corstarsl(data2[,corr_text_vars_ios],round=corr_decimals)

correlation_stars_PA <- matrix("", ncol=nrow(correlation_stars_PA0), nrow=nrow(correlation_stars_PA0), 
                               dimnames=list(rownames(correlation_stars_PA0), rownames(correlation_stars_PA0)))

correlation_stars_PA0 <- data.frame(lapply(correlation_stars_PA0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PA0))
{
  
  temp_col_name <- colnames(correlation_stars_PA0)[i]
  correlation_stars_PA[,temp_col_name] <- correlation_stars_PA0[,temp_col_name]
  
}

diag(correlation_stars_PA) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PA <- data.frame(var=row.names(correlation_stars_PA),correlation_stars_PA, stringsAsFactors=FALSE)
row.names(correlation_stars_PA) <- seq(nrow(correlation_stars_PA))

output_directory_correlation <- paste(output_directory,"correlation","\\",sep="")
create_directory(output_directory_correlation,remove=1)

write.csv(correlation_stars_PA,file=paste(output_directory_correlation,"correlation_stars_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(descrip_stats_ios_sim_cols)
rm2(corr_text_vars_ios_sim,corr_text_vars_ios,output_directory_correlation)
rm2(correlation_stars_PA0,correlation_stars_PA,corr_decimals,temp_col_name,i)


###############################################################################
cat("COMPUTE DV FOR ABOVE AND BELOW SIMILARITY/READABILITY QUANTILE - IOS", "\n")
###############################################################################

quantile_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                       "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                       "all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios",
                       "all_similarity_100pct_ios","main_investment_strategy_similarity_100pct_ios",
                       "all_similarity_250pct_ios","main_investment_strategy_similarity_250pct_ios",
                       "all_similarity_500pct_ios","main_investment_strategy_similarity_500pct_ios",
                       "all_similarity_750pct_ios","main_investment_strategy_similarity_750pct_ios",
                       "all_similarity_900pct_ios","main_investment_strategy_similarity_900pct_ios")


quantile_vars_data_ios <- descriptive_stats_temp_full_all_var_year[tolower(descriptive_stats_temp_full_all_var_year[,"var"]) %in% quantile_vars_ios,
                                                                   c("yr","var","quartile1","quartile3")] 

quantile_vars_dv_temp_ios <- lapply(quantile_vars_ios,quantile_dvs,
                                    data=data2,
                                    group_var=c(identifier,"yr","month"),quantile_data=quantile_vars_data_ios,
                                    quantile_col_low="quartile1",quantile_col_high="quartile3")

quantile_vars_dv_temp2_ios <- do.call(cbind, quantile_vars_dv_temp_ios)
quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[order(quantile_vars_dv_temp2_ios[,identifier],
                                                               quantile_vars_dv_temp2_ios[,"yr"],
                                                               quantile_vars_dv_temp2_ios[,"month"]),]
row.names(quantile_vars_dv_temp2_ios) <- seq(nrow(quantile_vars_dv_temp2_ios))

quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[,unique(colnames(quantile_vars_dv_temp2_ios))]

rm2(quantile_vars_ios,quantile_vars_data_ios,quantile_vars_dv_temp_ios)
rm2(descriptive_stats_temp_full_all_var_strategy,descriptive_stats_temp_full_all_var_year)


###############################################################################
cat("MERGE QUANTILE DVs", "\n")
###############################################################################

quantile_vars_dv <- quantile_vars_dv_temp2_ios
quantile_vars_dv <- quantile_vars_dv[order(quantile_vars_dv[,identifier],
                                           quantile_vars_dv[,"yr"],
                                           quantile_vars_dv[,"month"]),]
row.names(quantile_vars_dv) <- seq(nrow(quantile_vars_dv))

data_all <- merge(data2, quantile_vars_dv, 
                  by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all <- data_all[order(data_all[,identifier],
                           data_all[,"yr"],
                           data_all[,"month"],
                           data_all[,"yr_month"]),]
row.names(data_all) <- seq(nrow(data_all))

ExportTable(data_fulll_db,"data_all",data_all)
write.csv(data_all,file=paste(output_directory,"data_all",".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
#data_all <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

rm2(quantile_vars_dv_temp2_ios,quantile_vars_dv,data2)


###############################################################################
cat("UNIVARIATE ANALYSIS - VARIABLES", "\n")
###############################################################################

#univariate_vars_dep <- c("pflow","mktadjret","exret")

univariate_vars_dep <- c("pflow","mktadjret","exret",
                         "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
                         "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")

univariate_vars_binary <- c("listed_on_exchange_bin","hurdle_rate_bin","high_water_mark_bin","domicile_onshore_bin",
                            "leverage_bin","lock_up_bin","flagship_bin","closed_bin","dead_bin")

univariate_vars_continuous <- c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                "sdpct_flow_lag1",
                                "exret_lag1","exret_lag2","exret_lag3","exret_lag4",
                                "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                "mktadjret_lag1_sq","mktadjret_lag2_sq","mktadjret_lag3_sq","mktadjret_lag4_sq",
                                "log_aum_lag1","log_aum_lag2","log_aum_lag3","log_aum_lag4",
                                "age_y","total_fee",
                                "sharpe_ratio","sortino_ratio",
                                "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                                "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                                "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                                "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                                "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")


# "performance_fee","management_fee","other_fee",

# "ari_ios_below_quartile1","ari_ios_above_quartile3",
# "coleman_liau_ios_below_quartile1","coleman_liau_ios_above_quartile3",
# "flesch_kincaid_ios_below_quartile1","flesch_kincaid_ios_above_quartile3",
# "fog_ios_below_quartile1","fog_ios_above_quartile3",
# "smog_ios_below_quartile1","smog_ios_above_quartile3",
# "avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3",
# "avg_grade_level_acf_ios_below_quartile1","avg_grade_level_acf_ios_above_quartile3",
# "avg_grade_level_ac_ios_below_quartile1","avg_grade_level_ac_ios_above_quartile3",
# 
# "all_similarity_050pct_ios_below_quartile1","all_similarity_100pct_ios_below_quartile1","all_similarity_250pct_ios_below_quartile1",
# "all_similarity_500pct_ios_below_quartile1","all_similarity_750pct_ios_below_quartile1","all_similarity_900pct_ios_below_quartile1",
# "all_similarity_050pct_ios_above_quartile3","all_similarity_100pct_ios_above_quartile3","all_similarity_250pct_ios_above_quartile3",
# "all_similarity_500pct_ios_above_quartile3","all_similarity_750pct_ios_above_quartile3","all_similarity_900pct_ios_above_quartile3",
# "main_investment_strategy_similarity_050pct_ios_below_quartile1","main_investment_strategy_similarity_100pct_ios_below_quartile1",
# "main_investment_strategy_similarity_250pct_ios_below_quartile1","main_investment_strategy_similarity_500pct_ios_below_quartile1",
# "main_investment_strategy_similarity_750pct_ios_below_quartile1","main_investment_strategy_similarity_900pct_ios_below_quartile1",
# "main_investment_strategy_similarity_050pct_ios_above_quartile3","main_investment_strategy_similarity_100pct_ios_above_quartile3",
# "main_investment_strategy_similarity_250pct_ios_above_quartile3","main_investment_strategy_similarity_500pct_ios_above_quartile3",
# "main_investment_strategy_similarity_750pct_ios_above_quartile3","main_investment_strategy_similarity_900pct_ios_above_quartile3"


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS", "\n")
###############################################################################

data_all_univariate_continuous <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_continuous)]

quantile_name_continuous <- "continuous"
quantile_type_continuous <- c("year","agg")
#quantile_nums_continuous <- c(5,4,3)
quantile_nums_continuous <- c(4)
#quantile_nums_continuous_group_var <- "yr"
quantile_nums_continuous_group_var <- "yr_month"

univariate_data_year_groups_continuous <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                     stringsAsFactors=FALSE)

univariate_data_year_groups_continuous[1,] <- c(start_year,end_year)
univariate_data_year_groups_continuous[2,] <- c(2000,2011)
univariate_data_year_groups_continuous[3,] <- c(2006,2011)
#univariate_data_year_groups_continuous[4,] <- c(1994,1999)
#univariate_data_year_groups_continuous[5,] <- c(2000,2005)

output_directory_univariate_continuous <- paste(output_directory,"univariate_continuous","\\",sep="")
create_directory(output_directory_univariate_continuous,remove=1)

for (l in 1:length(univariate_vars_dep))
{
  #l <- 1
  #l <- 2
  #l <- 3
  #l <- 4  
  
  cat("DEP VAR:",univariate_vars_dep[l], "\n")
  
  univariate_vars_indep_continuous <- colnames(data_all_univariate_continuous)[!(colnames(data_all_univariate_continuous) %in% c("yr","yr_month",univariate_vars_dep[l]))]
  
  for (i in 1:length(quantile_type_continuous))
  {
    #i <- 1
    #i <- 2
    
    #cat("I:",i, "\n")
    
    for (j in 1:length(quantile_nums_continuous))
    {
      #j <- 1
      #j <- 2
      #j <- 3
      
      #cat("J:",j, "\n")
      
      #       quantiles_pct_flow_temp <- lapply(univariate_vars_indep_continuous,quantile_cast,data=data_all_univariate_continuous,
      #                                         dep_var=univariate_vars_dep[l],group_var="yr",quantile_count=quantile_nums_continuous[j])
      #       quantiles_pct_flow_temp <- lapply(univariate_vars_indep_continuous,quantile_cast_by_continuous,data=data_all_univariate_continuous,
      #                                         dep_var=univariate_vars_dep[l],quantile_type="quantile",quantile_count=quantile_nums_continuous[j],
      #                                         group_var="yr",group=quantile_type_continuous[i])
      #       
      #       quantiles_pct_flow_temp <- lapply(univariate_vars_indep_continuous,quantile_cast_by_continuous2,data=data_all_univariate_continuous,
      #                                         dep_var=univariate_vars_dep[l],quantile_type="quantile",quantile_count_dep=quantile_nums_continuous[j],
      #                                         quantile_count_indep=1,group_var="yr",group=quantile_type_continuous[i])
      #       
      quantiles_pct_flow_temp <- lapply(univariate_vars_indep_continuous,quantile_cast_by_continuous2,data=data_all_univariate_continuous,
                                        dep_var=univariate_vars_dep[l],quantile_type="quantile",quantile_count_dep=quantile_nums_continuous[j],
                                        quantile_count_indep=1,group_var=quantile_nums_continuous_group_var,group=quantile_type_continuous[i])
      
      quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
      #quantiles_pct_flow <- quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% "quantile_var_indep2")]
      quantiles_pct_flow <- quantiles_pct_flow[,c(quantile_nums_continuous_group_var,"variable_indep","quantile_var_indep2",
                                                  colnames(quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% c(quantile_nums_continuous_group_var,"variable_indep","quantile_var_indep2"))]))]
      colnames(quantiles_pct_flow) <- c(quantile_nums_continuous_group_var,"cut_var","quantile_var_indep2",paste("X",seq(1,quantile_nums_continuous[j]),sep=""))
      #quantiles_pct_flow <- quantiles_pct_flow[order(quantiles_pct_flow[,quantile_nums_continuous_group_var],quantiles_pct_flow[,"cut_var"],quantiles_pct_flow[,"quantile_var_indep2"]),]
      row.names(quantiles_pct_flow) <- seq(nrow(quantiles_pct_flow))
      
      #Quantile by Year
      averages_yr_quan_all_cast <- diff_in_mean(quantiles_pct_flow,c("cut_var","quantile_var_indep2"),quantile_nums_continuous_group_var,"X1",paste("X",quantile_nums_continuous[j],sep=""))
      #averages_yr_quan_all_cast <- averages_yr_quan_all_cast[order(averages_yr_quan_all_cast[,"yr"]),]
      
      averages_yr_quan_all_cast <- ddply(.data=averages_yr_quan_all_cast, .variables=c(quantile_nums_continuous_group_var,"quantile_var_indep2"), 
                                         .fun = function(x,var_order){x[order(order(var_order)),] },var_order=univariate_vars_indep_continuous)
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[!(averages_yr_quan_all_cast[,"cut_var"] %in% univariate_vars_dep),]
      row.names(averages_yr_quan_all_cast) <- seq(nrow(averages_yr_quan_all_cast))
      
      averages_yr_quan_all_cast[,4:ncol(averages_yr_quan_all_cast)] <- format(round(averages_yr_quan_all_cast[,4:ncol(averages_yr_quan_all_cast)],  digits = 4))
      
      averages_yr_quan_all_cast[,"t_p_val"] <- ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .0100, paste(averages_yr_quan_all_cast[,"t_p_val"], "***", sep=""), 
                                                      ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .0500, paste(averages_yr_quan_all_cast[,"t_p_val"], "** ", sep=""), 
                                                             ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .1000, paste(averages_yr_quan_all_cast[,"t_p_val"], "*  ", sep=""), 
                                                                    averages_yr_quan_all_cast[,"t_p_val"])))           
      
      averages_yr_quan_all_cast[,"f_p_val"] <- ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .0100, paste(averages_yr_quan_all_cast[,"f_p_val"], "***", sep=""), 
                                                      ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .0500, paste(averages_yr_quan_all_cast[,"f_p_val"], "** ", sep=""), 
                                                             ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .1000, paste(averages_yr_quan_all_cast[,"f_p_val"], "*  ", sep=""), averages_yr_quan_all_cast[,"f_p_val"])))   
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[,c(quantile_nums_continuous_group_var,"quantile_var_indep2","cut_var",
                                                                colnames(averages_yr_quan_all_cast[,!(colnames(averages_yr_quan_all_cast) %in% c(quantile_nums_continuous_group_var,"quantile_var_indep2","cut_var"))]))]
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[!is.na(averages_yr_quan_all_cast[,quantile_nums_continuous_group_var]),]
      
      name1 <- paste("quantiles",quantile_type_continuous[i],univariate_vars_dep[l],"yearly",quantile_name_continuous,quantile_nums_continuous[j],sep="_")
      #assign(name1, averages_yr_quan_all_cast, envir = .GlobalEnv)
      write.csv(averages_yr_quan_all_cast,file=paste(output_directory_univariate_continuous,name1,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(averages_yr_quan_all_cast,name1)
      
      #Quantile by Year Groups
      for (k in 1:nrow(univariate_data_year_groups_continuous))
      {
        #k <- 1
        
        if (quantile_nums_continuous_group_var == "yr") {
          
          quantiles_pct_flow2 <- quantiles_pct_flow
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow2[(quantiles_pct_flow2[,"yr"]>=univariate_data_year_groups_continuous[k,1] 
                                                                & quantiles_pct_flow2[,"yr"]<=univariate_data_year_groups_continuous[k,2]),]
          quantiles_pct_flow_no_yr_temp[,"yr"] <- 9999
          
        } else if (quantile_nums_continuous_group_var == "yr_month") {
          
          quantiles_pct_flow2 <- data.frame(yr=NA,
                                            quantiles_pct_flow,
                                            stringsAsFactors=FALSE)
          
          quantiles_pct_flow2[,"yr"] <- as.integer(substr(as.character(quantiles_pct_flow2[,"yr_month"]),1,4))
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow2[(quantiles_pct_flow2[,"yr"]>=univariate_data_year_groups_continuous[k,1] 
                                                                & quantiles_pct_flow2[,"yr"]<=univariate_data_year_groups_continuous[k,2]),]
          quantiles_pct_flow_no_yr_temp[,"yr_month"] <- 9999
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow_no_yr_temp[,!(colnames(quantiles_pct_flow_no_yr_temp) %in% "yr")]
          
        } else {
          cat("ERROR IN GROUPS", "\n")
          
        }
        
        averages_quan_temp_cast <- diff_in_mean(quantiles_pct_flow_no_yr_temp,c("cut_var","quantile_var_indep2"),quantile_nums_continuous_group_var,"X1",paste("X",quantile_nums_continuous[j],sep=""))
        
        averages_quan_temp_cast <- ddply(.data=averages_quan_temp_cast, .variables=c(quantile_nums_continuous_group_var,"quantile_var_indep2"), 
                                         .fun = function(x,var_order){x[order(order(var_order)),] },var_order=univariate_vars_indep_continuous)
        
        averages_quan_temp_cast <- averages_quan_temp_cast[,!(colnames(averages_quan_temp_cast) %in% quantile_nums_continuous_group_var)]
        averages_quan_temp_cast <- averages_quan_temp_cast[!(averages_quan_temp_cast[,"cut_var"] %in% univariate_vars_dep),]
        row.names(averages_quan_temp_cast) <- seq(nrow(averages_quan_temp_cast))
        
        averages_quan_temp_cast[,3:ncol(averages_quan_temp_cast)] <- format(round(averages_quan_temp_cast[,3:ncol(averages_quan_temp_cast)],  digits = 4))
        
        averages_quan_temp_cast[,"t_p_val"] <- ifelse(averages_quan_temp_cast[,"t_p_val"] < .0100, paste(averages_quan_temp_cast[,"t_p_val"], "***", sep=""), 
                                                      ifelse(averages_quan_temp_cast[,"t_p_val"] < .0500, paste(averages_quan_temp_cast[,"t_p_val"], "** ", sep=""), 
                                                             ifelse(averages_quan_temp_cast[,"t_p_val"] < .1000, paste(averages_quan_temp_cast[,"t_p_val"], "*  ", sep=""), 
                                                                    paste(averages_quan_temp_cast[,"t_p_val"], "   ", sep=""))))           
        
        averages_quan_temp_cast[,"f_p_val"] <- ifelse(averages_quan_temp_cast[,"f_p_val"] < .0100, paste(averages_quan_temp_cast[,"f_p_val"], "***", sep=""), 
                                                      ifelse(averages_quan_temp_cast[,"f_p_val"] < .0500, paste(averages_quan_temp_cast[,"f_p_val"], "** ", sep=""), 
                                                             ifelse(averages_quan_temp_cast[,"f_p_val"] < .1000, paste(averages_quan_temp_cast[,"f_p_val"], "*  ", sep=""), 
                                                                    paste(averages_quan_temp_cast[,"f_p_val"], "   ", sep=""))))   
        
        
        averages_quan_temp_cast <- averages_quan_temp_cast[,c("quantile_var_indep2","cut_var",
                                                              colnames(averages_quan_temp_cast[,!(colnames(averages_quan_temp_cast) %in% c("quantile_var_indep2","cut_var"))]))]
        
        
        
        name_temp <- paste("quantiles",quantile_type_continuous[i],univariate_vars_dep[l],univariate_data_year_groups_continuous[k,1],univariate_data_year_groups_continuous[k,2],quantile_name_continuous,quantile_nums_continuous[j],sep="_")
        write.csv(averages_quan_temp_cast,file=paste(output_directory_univariate_continuous,name_temp,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
        #assign(name_temp, averages_quan_temp_cast, envir = .GlobalEnv)
        
        rm2(quantiles_pct_flow_no_yr_temp,averages_quan_temp_cast,name_temp)
      }
      rm2(quantiles_pct_flow_temp,quantiles_pct_flow2,k)
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(quantile_type_continuous), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(quantile_nums_continuous))
      
    }
    rm2(quantiles_pct_flow,j)
    
  }
  rm2(i,univariate_vars_indep_continuous)
  
}
rm2(l,quantile_type_continuous,quantile_nums_continuous,data_all_univariate_continuous,univariate_data_year_groups_continuous)
rm2(output_directory_univariate_continuous,quantile_nums_continuous_group_var)


###############################################################################
cat("UNIVARIATE ANALYSIS - BINARY", "\n")
###############################################################################

data_all_univariate_binary <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_binary)]

quantile_name_binary <- "binary"
quantile_type_binary <- c("year","agg") 
quantile_nums_binary <- c(2)
#quantile_nums_binary_group_var <- "yr"
quantile_nums_binary_group_var <- "yr_month"

univariate_data_year_groups_binary <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                 stringsAsFactors=FALSE)

univariate_data_year_groups_binary[1,] <- c(start_year,end_year)
univariate_data_year_groups_binary[2,] <- c(2000,2011)
univariate_data_year_groups_binary[3,] <- c(2006,2011)
#univariate_data_year_groups_binary[4,] <- c(1994,1999)
#univariate_data_year_groups_binary[5,] <- c(2000,2005)

output_directory_univariate_binary <- paste(output_directory,"univariate_binary","\\",sep="")
create_directory(output_directory_univariate_binary,remove=1)

for (l in 1:length(univariate_vars_dep))
{
  #l <- 1
  #l <- 2
  
  cat("DEP VAR:",univariate_vars_dep[l], "\n")
  
  #univariate_vars_indep_binary <- colnames(data_all_univariate_binary)[!(colnames(data_all_univariate_binary) %in% c("yr","yr_month",univariate_vars_dep[l]))]
  univariate_vars_indep_binary <- colnames(data_all_univariate_binary)[!(colnames(data_all_univariate_binary) %in% c("yr","yr_month",univariate_vars_dep))] 
  
  for (i in 1:length(quantile_type_binary))
  {
    #i <- 1
    #i <- 2
    
    #cat("I:",i, "\n")
    
    for (j in 1:length(quantile_nums_binary))
    {
      #j <- 1
      
      #cat("J:",j, "\n")
      
      
      #       quantiles_pct_flow_temp2 <- lapply(univariate_vars_indep_binary,quantile_cast_by_binary,data=data_all_univariate_binary,
      #                                          dep_var=univariate_vars_dep[l],quantile_type="dv",quantile_count=quantile_nums_binary[j],group_var="yr",group=quantile_type_binary[i])
      #       quantiles_pct_flow_temp3 <- lapply(univariate_vars_indep_binary,quantile_cast_by_continuous,data=data_all_univariate_binary,
      #                                         dep_var=univariate_vars_dep[l],quantile_type="dv",quantile_count=quantile_nums_binary[j],group_var="yr",group=quantile_type_binary[2])
      #       
      #       quantiles_pct_flow_temp4 <- lapply(univariate_vars_indep_binary,quantile_cast_by_binary,data=data_all_univariate_binary,
      #                                          dep_var=univariate_vars_dep[l],quantile_type="dv",quantile_count=quantile_nums_binary[j],group_var="yr",group=quantile_type_binary[2])
      #       
      
      #       
      #       quantiles_pct_flow_temp <- lapply(univariate_vars_indep_binary,quantile_cast_by_continuous,data=data_all_univariate_binary,
      #                                         dep_var=univariate_vars_dep[l],quantile_type="dv",quantile_count=quantile_nums_binary[j],group_var="yr",group=quantile_type_binary[i])
      #       
      #       quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
      #       quantiles_pct_flow <- quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% "variable")]
      #       quantiles_pct_flow <- quantiles_pct_flow[,c("cut_var","yr",paste("X",seq(0,(quantile_nums_binary[j]-1)),sep=""))]
      #       colnames(quantiles_pct_flow) <- c("cut_var","yr",paste("X",seq(1,(quantile_nums_binary[j])),sep=""))
      #       
      
      quantiles_pct_flow_temp <- lapply(univariate_vars_indep_binary,quantile_cast_by_continuous2,data=data_all_univariate_binary,
                                        dep_var=univariate_vars_dep[l],quantile_type="dv",quantile_count_dep=quantile_nums_binary[j],
                                        quantile_count_indep=1,group_var=quantile_nums_binary_group_var,group=quantile_type_binary[i])
      quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
      #quantiles_pct_flow <- quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% "quantile_var_indep2")]
      
      quantiles_pct_flow <- quantiles_pct_flow[,c(quantile_nums_binary_group_var,"variable_indep","quantile_var_indep2",
                                                  colnames(quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% c(quantile_nums_binary_group_var,"variable_indep","quantile_var_indep2"))]))]
      #colnames(quantiles_pct_flow) <- c(quantile_nums_binary_group_var,"cut_var","quantile_var_indep2",paste("X",seq(1,quantile_nums_binary[j]),sep=""))
      colnames(quantiles_pct_flow) <- c(quantile_nums_binary_group_var,"cut_var","quantile_var_indep2",paste("X",seq(0,(quantile_nums_binary[j]-1)),sep=""))
      #quantiles_pct_flow <- quantiles_pct_flow[order(quantiles_pct_flow[,quantile_nums_binary_group_var],quantiles_pct_flow[,"cut_var"],quantiles_pct_flow[,"quantile_var_indep2"]),]
      row.names(quantiles_pct_flow) <- seq(nrow(quantiles_pct_flow))
      
      
      #Quantile by Year
      averages_yr_quan_all_cast <- diff_in_mean(quantiles_pct_flow,c("cut_var","quantile_var_indep2"),quantile_nums_binary_group_var,"X0",paste("X",(quantile_nums_binary[j]-1),sep=""))
      #averages_yr_quan_all_cast <- averages_yr_quan_all_cast[order(averages_yr_quan_all_cast[,quantile_nums_binary_group_var]),]
      
      averages_yr_quan_all_cast <- ddply(.data=averages_yr_quan_all_cast, .variables=c(quantile_nums_binary_group_var,"quantile_var_indep2"), 
                                         .fun = function(x,var_order){x[order(order(var_order)),] },var_order=univariate_vars_indep_binary)
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[!(averages_yr_quan_all_cast[,"cut_var"] %in% univariate_vars_dep),]
      row.names(averages_yr_quan_all_cast) <- seq(nrow(averages_yr_quan_all_cast))
      
      
      averages_yr_quan_all_cast[,4:ncol(averages_yr_quan_all_cast)] <- format(round(averages_yr_quan_all_cast[,4:ncol(averages_yr_quan_all_cast)],  digits = 4))
      
      averages_yr_quan_all_cast[,"t_p_val"] <- ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .0100, paste(averages_yr_quan_all_cast[,"t_p_val"], "***", sep=""), 
                                                      ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .0500, paste(averages_yr_quan_all_cast[,"t_p_val"], "** ", sep=""), 
                                                             ifelse(averages_yr_quan_all_cast[,"t_p_val"] < .1000, paste(averages_yr_quan_all_cast[,"t_p_val"], "*  ", sep=""), 
                                                                    averages_yr_quan_all_cast[,"t_p_val"])))           
      
      averages_yr_quan_all_cast[,"f_p_val"] <- ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .0100, paste(averages_yr_quan_all_cast[,"f_p_val"], "***", sep=""), 
                                                      ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .0500, paste(averages_yr_quan_all_cast[,"f_p_val"], "** ", sep=""), 
                                                             ifelse(averages_yr_quan_all_cast[,"f_p_val"] < .1000, paste(averages_yr_quan_all_cast[,"f_p_val"], "*  ", sep=""), averages_yr_quan_all_cast[,"f_p_val"])))   
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[,c(quantile_nums_binary_group_var,"quantile_var_indep2","cut_var",
                                                                colnames(averages_yr_quan_all_cast[,!(colnames(averages_yr_quan_all_cast) %in% c(quantile_nums_binary_group_var,"quantile_var_indep2","cut_var"))]))]
      
      averages_yr_quan_all_cast <- averages_yr_quan_all_cast[!is.na(averages_yr_quan_all_cast[,quantile_nums_binary_group_var]),]
      
      name1 <- paste("quantiles",quantile_type_binary[i],univariate_vars_dep[l],"yearly",quantile_name_binary,quantile_nums_binary[j],sep="_")
      #assign(name1, averages_yr_quan_all_cast, envir = .GlobalEnv)
      write.csv(averages_yr_quan_all_cast,file=paste(output_directory_univariate_binary,name1,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(averages_yr_quan_all_cast,name1)
      
      #Quantile by Year Groups
      for (k in 1:nrow(univariate_data_year_groups_binary))
      {
        #k <- 1
        
        
        if (quantile_nums_binary_group_var == "yr") {
          
          quantiles_pct_flow2 <- quantiles_pct_flow
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow2[(quantiles_pct_flow2[,"yr"]>=univariate_data_year_groups_binary[k,1] 
                                                                & quantiles_pct_flow2[,"yr"]<=univariate_data_year_groups_binary[k,2]),]
          quantiles_pct_flow_no_yr_temp[,"yr"] <- 9999
          
        } else if (quantile_nums_binary_group_var == "yr_month") {
          
          quantiles_pct_flow2 <- data.frame(yr=NA,
                                            quantiles_pct_flow,
                                            stringsAsFactors=FALSE)
          
          quantiles_pct_flow2[,"yr"] <- as.integer(substr(as.character(quantiles_pct_flow2[,"yr_month"]),1,4))
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow2[(quantiles_pct_flow2[,"yr"]>=univariate_data_year_groups_binary[k,1] 
                                                                & quantiles_pct_flow2[,"yr"]<=univariate_data_year_groups_binary[k,2]),]
          quantiles_pct_flow_no_yr_temp[,"yr_month"] <- 9999
          
          quantiles_pct_flow_no_yr_temp <- quantiles_pct_flow_no_yr_temp[,!(colnames(quantiles_pct_flow_no_yr_temp) %in% "yr")]
          
        } else {
          cat("ERROR IN GROUPS", "\n")
          
        }
        
        averages_quan_temp_cast <- diff_in_mean(quantiles_pct_flow_no_yr_temp,c("cut_var","quantile_var_indep2"),quantile_nums_binary_group_var,"X0",paste("X",(quantile_nums_binary[j]-1),sep=""))
        
        averages_quan_temp_cast <- ddply(.data=averages_quan_temp_cast, .variables=c(quantile_nums_binary_group_var,"quantile_var_indep2"), 
                                         .fun = function(x,var_order){x[order(order(var_order)),] },var_order=univariate_vars_indep_binary)
        
        averages_quan_temp_cast <- averages_quan_temp_cast[,!(colnames(averages_quan_temp_cast) %in% quantile_nums_binary_group_var)]
        averages_quan_temp_cast <- averages_quan_temp_cast[!(averages_quan_temp_cast[,"cut_var"] %in% univariate_vars_dep),]
        row.names(averages_quan_temp_cast) <- seq(nrow(averages_quan_temp_cast))
        
        averages_quan_temp_cast[,3:ncol(averages_quan_temp_cast)] <- format(round(averages_quan_temp_cast[,3:ncol(averages_quan_temp_cast)],  digits = 4))
        
        averages_quan_temp_cast[,"t_p_val"] <- ifelse(averages_quan_temp_cast[,"t_p_val"] < .0100, paste(averages_quan_temp_cast[,"t_p_val"], "***", sep=""), 
                                                      ifelse(averages_quan_temp_cast[,"t_p_val"] < .0500, paste(averages_quan_temp_cast[,"t_p_val"], "** ", sep=""), 
                                                             ifelse(averages_quan_temp_cast[,"t_p_val"] < .1000, paste(averages_quan_temp_cast[,"t_p_val"], "*  ", sep=""), 
                                                                    paste(averages_quan_temp_cast[,"t_p_val"], "   ", sep=""))))           
        
        averages_quan_temp_cast[,"f_p_val"] <- ifelse(averages_quan_temp_cast[,"f_p_val"] < .0100, paste(averages_quan_temp_cast[,"f_p_val"], "***", sep=""), 
                                                      ifelse(averages_quan_temp_cast[,"f_p_val"] < .0500, paste(averages_quan_temp_cast[,"f_p_val"], "** ", sep=""), 
                                                             ifelse(averages_quan_temp_cast[,"f_p_val"] < .1000, paste(averages_quan_temp_cast[,"f_p_val"], "*  ", sep=""), 
                                                                    paste(averages_quan_temp_cast[,"f_p_val"], "   ", sep=""))))   
        
        averages_quan_temp_cast <- averages_quan_temp_cast[,c("quantile_var_indep2","cut_var",
                                                              colnames(averages_quan_temp_cast[,!(colnames(averages_quan_temp_cast) %in% c("quantile_var_indep2","cut_var"))]))]
        
        name_temp <- paste("quantiles",quantile_type_binary[i],univariate_vars_dep[l],univariate_data_year_groups_binary[k,1],univariate_data_year_groups_binary[k,2],quantile_name_binary,quantile_nums_binary[j],sep="_")
        write.csv(averages_quan_temp_cast,file=paste(output_directory_univariate_binary,name_temp,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
        #assign(name_temp, averages_quan_temp_cast, envir = .GlobalEnv)
        
        rm2(quantiles_pct_flow_no_yr_temp,averages_quan_temp_cast,name_temp)
      }
      rm2(quantiles_pct_flow_temp,quantiles_pct_flow2,k)
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(quantile_type_binary), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(quantile_nums_binary))
      
    }
    rm2(quantiles_pct_flow,j)
    
  }
  rm2(i,univariate_vars_indep_binary)
  
}
rm2(l,quantile_type_binary,quantile_nums_binary,data_all_univariate_binary,univariate_data_year_groups_binary)
rm2(output_directory_univariate_binary,quantile_nums_binary_group_var)


###############################################################################
cat("ALL POSSIBLE MODELS", "\n")
###############################################################################

#### NOT USING RIGHT NOW ####

#Create Different Independent Variable Groups
grade <- c("coleman_liau_XXX",
           "ari_XXX",
           "ari_XXX + flesch_kincaid_XXX + coleman_liau_XXX",
           "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
           "avg_grade_level_XXX",
           "avg_grade_level_ac_XXX")

similarity <- c("all_similarity_250pct_XXX + main_investment_strategy_similarity_250pct_XXX",
                "main_investment_strategy_similarity_250pct_XXX")

controls <- c("mktadjret_lag1 + mktadjret_lag1_sq + mktadjret_lag2 + mktadjret_lag2_sq  + age_y ",
              "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + age_y")

quantile <- c("main_investment_strategy_similarity_050pct_XXX_below_quartile1 + main_investment_strategy_similarity_050pct_XXX_above_quartile3",
              "avg_grade_level_XXX_below_quartile1 + avg_grade_level_XXX_above_quartile3 + main_investment_strategy_similarity_050pct_XXX_below_quartile1 + main_investment_strategy_similarity_050pct_XXX_above_quartile3",
              "avg_grade_level_ac_XXX_below_quartile1 + avg_grade_level_ac_XXX_above_quartile3 + main_investment_strategy_similarity_900pct_XXX_below_quartile1 + main_investment_strategy_similarity_900pct_XXX_above_quartile3",
              "coleman_liau_XXX_below_quartile1 + coleman_liau_XXX_above_quartile3 + main_investment_strategy_similarity_050pct_XXX_below_quartile1 + main_investment_strategy_similarity__050pct_XXX_above_quartile3",
              "")

# c("mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_below_quartile1 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_above_quartile3",
#   "mktadjret_lag1:avg_grade_level_XXX_below_quartile1 + mktadjret_lag1:avg_grade_level_XXX_above_quartile3 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_below_quartile1 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_above_quartile3",
#   "mktadjret_lag1:avg_grade_level_ac_XXX_below_quartile1 + mktadjret_lag1:avg_grade_level_ac_XXX_above_quartile3 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_below_quartile1 + mktadjret_lag1:main_investment_strategy_similarity__050pct_XXX_above_quartile3",
#   "mktadjret_lag1:coleman_liau_XXX_below_quartile1 + mktadjret_lag1:coleman_liau_XXX_above_quartile3 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_below_quartile1 + mktadjret_lag1:main_investment_strategy_similarity_050pct_XXX_above_quartile3",
#   "mktadjret_lag1:ari_XXX_below_quartile1 + mktadjret_lag1:ari_XXX_above_quartile3",
#   "")

fixed_effects <- c("factor(yr)",
                   "")

regression_equations_all <- expand.grid(grade,
                                        similarity,
                                        controls,
                                        quantile,
                                        fixed_effects)
regression_equations_all <- data.frame(lapply(regression_equations_all, as.character), 
                                       all_ind_vars=NA,
                                       stringsAsFactors=FALSE)
colnames(regression_equations_all) <- c("grade","similarity","controls","quantile","fixed_effects","full_independent_vars")

regression_equations_all <- unknown_to_NA(regression_equations_all,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations_all))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations_all[i,1:(ncol(regression_equations_all)-1)], use.names=FALSE))))
  regression_equations_all[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

rm2(grade,similarity,controls,quantile,fixed_effects,temp_char_vec,regression_equations_all)


###############################################################################
cat("PANEL REGRESSION - READBILITY", "\n")
###############################################################################

output_directory_reg_readability <- paste(output_directory,"reg_readability","\\",sep="")
create_directory(output_directory_reg_readability,remove=1)

#Regression equations
regression_equations1_1 <- list(dep_var=c("pflow"),
                                models=c("avg_grade_level_XXX",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + performance_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin"),
                                model_type=c("pooling"),
                                note=c("readability"))

regression_equations1_2 <- list(dep_var=c("mktadjret","exret","int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24","int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24"),
                                models=c("avg_grade_level_XXX",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                                         "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + performance_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin"),
                                model_type=c("pooling"),
                                note=c("readability"))

regression_equations1 <- list(regression_equations1_1,regression_equations1_2)

rm(regression_equations1_1,regression_equations1_2)

regression_temp_comb <-  ldply(regression_equations1, function(x){
  
  #x <- regression_equations1[[1]]
  #x <- regression_equations1[[2]]
  
  regression_dep_var_temp <- x["dep_var"]
  regression_dep_var_temp2 <- as.vector(unlist(regression_dep_var_temp))
  
  regression_models_temp <- x["models"]
  regression_models_temp2 <- as.vector(unlist(regression_models_temp))
  
  regression_model_type_temp <- x["model_type"]
  regression_model_type_temp2 <- as.vector(unlist(regression_model_type_temp))
  
  regression_note_temp <- x["note"]
  regression_note_temp2 <- as.vector(unlist(regression_note_temp))
  
  regression_temp_comb0 <- expand.grid(regression_dep_var_temp2,regression_models_temp2,regression_model_type_temp2,regression_note_temp2)
  colnames(regression_temp_comb0) <- c("dep_var","indep_var","model_type","note")
  
  regression_temp_comb0 <- regression_temp_comb0[do.call(order,regression_temp_comb0[c("dep_var")]),]
  row.names(regression_temp_comb0) <- seq(nrow(regression_temp_comb0))
  
  regression_temp_comb0 <- data.frame(lapply(regression_temp_comb0, as.character), stringsAsFactors=FALSE)
  
  return(regression_temp_comb0)})



# #Find correlation of model variables
# corr_decimals <- 3
# regression1_correlation1 <- c(na.omit(as.character(unlist(regression_equations1[,"full_independent_vars"], use.names=FALSE))))
# regression1_correlation2 <- paste(regression1_correlation1, sep="", collapse="+") 
# regression1_correlation3 <- unique(strsplit(gsub("\\+", "\\1 ", regression1_correlation2), " ")[[1]])
# regression1_correlation4 <- gsub("XXX","ios",regression1_correlation3,ignore.case = TRUE)
# regression1_correlation5 <- regression1_correlation4[!(regression1_correlation4=="")]
# regression1_correlation6 <- c(dep_var1,
#                               "pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#                               regression1_correlation5[!(regression1_correlation5 %in% c("factor(yr)"))])
# 
# regression1_correlation7 <- corstarsl(data_all[,regression1_correlation6],round=corr_decimals)
# 
# regression1_correlation8 <- matrix("", ncol=nrow(regression1_correlation7), nrow=nrow(regression1_correlation7), 
#                                    dimnames=list(rownames(regression1_correlation7), rownames(regression1_correlation7)))
# 
# regression1_correlation7 <- data.frame(lapply(regression1_correlation7, as.character), stringsAsFactors=FALSE)
# 
# for (i in 1:ncol(regression1_correlation7))
# {
#   
#   temp_col_name <- colnames(regression1_correlation7)[i]
#   regression1_correlation8[,temp_col_name] <- regression1_correlation7[,temp_col_name]
#   rm(temp_col_name)
# }
# 
# diag(regression1_correlation8) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")
# 
# regression1_correlation8 <- data.frame(var=row.names(regression1_correlation8),regression1_correlation8, stringsAsFactors=FALSE)
# row.names(regression1_correlation8) <- seq(nrow(regression1_correlation8))
# 
# rm2(corr_decimals,i)
# rm2(regression1_correlation1,regression1_correlation2,regression1_correlation3,regression1_correlation4)
# rm2(regression1_correlation5,regression1_correlation6,regression1_correlation7,regression1_correlation8)





#data_year_groups1 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), stringsAsFactors=FALSE)

#data_year_groups1[1,] <- c(start_year,end_year)
#data_year_groups1[2,] <- c(2000,2011)
#data_year_groups1[3,] <- c(2006,2011)
#data_year_groups1[4,] <- c(1994,1999)
#data_year_groups1[5,] <- c(2000,2005)

data_year_groups1_1 <- list(Start_yr=start_year,End_yr=end_year)
data_year_groups1_2 <- list(Start_yr=2000,End_yr=2011)
#data_year_groups1_3 <- list(Start_yr=2006,End_yr=2011)
#data_year_groups1_4 <- list(Start_yr=1994,End_yr=1999)
#data_year_groups1_5 <- list(Start_yr=2000,End_yr=2005)

#data_year_groups1 <- list(data_year_groups1_1,data_year_groups1_2,data_year_groups1_3,data_year_groups1_4,data_year_groups1_5)
data_year_groups1 <- list(data_year_groups1_1,data_year_groups1_2)




#data_year_groups1_Start_yr <- sapply(data_year_groups1, "[[", "Start_yr")
#data_year_groups1_End_yr <- sapply(data_year_groups1, "[[", "End_yr")


l_ply(data_year_groups1, function(x){
  
  #x <- data_year_groups1[[1]]
  #x <- data_year_groups1[[2]]
  
  Start_yr_temp <- x["Start_yr"]
  Start_yr_temp2 <- as.vector(unlist(Start_yr_temp))
  
  End_yr_temp <- x["End_yr"]
  End_yr_temp2 <- as.vector(unlist(End_yr_temp))
  
  #cat("STR X:", str(x),"\n")
  #cat("Start:", Start_yr_temp2,"\n")
  #cat("End:", End_yr_temp2,"\n")
  
  
  
})








for (k in 1:nrow(data_year_groups1))
{
  #k <- 1
  #k <- 2
  
  cat("START YEAR:", data_year_groups1[k,1], "END YEAR:", data_year_groups1[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups1[k,1] & data_all[,"yr"]<=data_year_groups1[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  
  
  ddply(.data=regression_temp_comb, .variables="dep_var", 
        .fun = function(x,datapd,data,name_short,model){
          
          #x <- regression_temp_comb[regression_temp_comb[,c("dep_var")]=="pflow",]
          #x <- regression_temp_comb[regression_temp_comb[,c("dep_var")]=="mktadjret",]
          #datapd <- data_temp.pd
          #data <- data_temp
          #name_short <- paste(data_year_groups1[k,1],data_year_groups1[k,2],note1,sep="_")
          #model <- model_type1
          
          out_file_name <- paste("reg_compare_plm",unique(x[,c("dep_var")]),name_short,sep="_")
          
          #models <- rep( list(list()), nrow(x) )
          se <- rep( list(list()), nrow(x) )
          pval <- rep( list(list()), nrow(x) )
          
          aa <- ddply(.data=x, .variables="indep_var", 
                      .fun = function(y,datapd2,data2,model2){ 
                        
                        #l <- 1
                        #l <- 2
                        #y <- x[l,]
                        #datapd2 <- datapd
                        #data2 <- data
                        #model2 <- model
                        
                        ind_vars_reg0 <- y[,"indep_var"]
                        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
                        reg0 <- plm(as.formula(paste(y[,"dep_var"],ind_vars_reg0,sep="~")),data=datapd2,model=model2)
                        #reg0 <- lm(as.formula(paste(y[,"dep_var"],ind_vars_reg0,sep="~")), data2)
                        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
                        #reg0_rse <- cl.plm(data2, reg0, data2[,identifier])
                        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
                        reg0_rse <- mcl.plm(data2, reg0, data2[,identifier], data2[,"month"])
                        #reg0_rse <- mcl(data2,reg0, data2[,identifier], data2[,"month"])
                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                        #models[[l]] <- reg0
                        
                        
                        #se[[l]] <- reg0_rse[,4]
                        #pval[[l]] <- reg0_rse[,4]
                        
                        #assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
                        
                        return(list(reg0,reg0_rse))
                        
                        
                      }, datapd2=datapd, data2=data, model2=model, 
                      .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
          
          aa1 <- aa[1]
          aa11 <- aa[[1]]
          aa2 <- aa[2]
          aa22 <- aa[[2]]
          
          bb <- sapply(aa, "[[",1)
          
        }, datapd=data_temp.pd, data=data_temp, name_short=paste(data_year_groups1[k,1],data_year_groups1[k,2],note1,sep="_"), model=model_type1,
        .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
  
  
  
  
  
  # }
  
  
  
  #for (i in 4:length(dep_var1))
  #for (i in 1:length(dep_var1))
  #{
  #i <- 1
  #i <- 2
  #i <- 3
  #i <- 4
  
  #out_file_name <- paste("reg_compare_plm",dep_var1[i],deparse(substitute(data_all)),note1,sep="_")
  out_file_name <- paste("reg_compare_plm",dep_var1[i],data_year_groups1[k,1],data_year_groups1[k,2],note1,sep="_")
  
  #models <- rep( list(list()), nrow(regression_equations1) )
  se <- rep( list(list()), nrow(regression_equations1) )
  pval <- rep( list(list()), nrow(regression_equations1) )
  
  
  
  for (l in 1:nrow(regression_equations1))
  {
    #l <- 1
    #l <- 2
    #l <- 3
    #l <- 4   
    #l <- 5
    #l <- 6
    #l <- 7
    #l <- 8     
    
    ind_vars_reg0 <- regression_equations1[l,"full_independent_vars"]
    ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
    reg0 <- plm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type1)
    #reg0 <- lm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")), data_temp)
    #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
    #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
    #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
    reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
    #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
    #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
    #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
    
    #models[[l]] <- reg0
    se[[l]] <- reg0_rse[,4]
    pval[[l]] <- reg0_rse[,4]
    
    assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
    #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
    
    rm2(ind_vars_reg0,reg0,reg0_rse)
    
  }
  
  htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep=""))), 
          model.names=paste("(",seq(1,nrow(regression_equations1)),")",sep=""),
          override.se=se,
          override.pval=pval,
          stars=c(0.01, 0.05, 0.1), digits=3, 
          caption="Effect of Readability on Hedge Fund Flows – Multivariate",
          file=paste(output_directory_reg_readability,out_file_name,".doc",sep=""))
  
  #custom.names
  
  progress_function(outer_loop_count=k, outer_loop_start_val=1, outer_loop_end_val=nrow(data_year_groups1), 
                    inner_loop_count=i, inner_loop_start_val=1, inner_loop_end_val=length(dep_var1))
  
  rm2(se,pval,out_file_name,l)
  eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep="")))
  
} 

rm2(data_temp,data_temp.pd,i)

}

rm2(data_year_groups1,dep_var1,model_type1,note1,temp_char_vec,regression_equations1,k)


###############################################################################
cat("PANEL REGRESSION - SIMILARITY", "\n")
###############################################################################

output_directory_reg_similarity <- paste(output_directory,"reg_similarity","\\",sep="")
create_directory(output_directory_reg_similarity,remove=1)

data_year_groups2 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups2[1,] <- c(start_year,end_year)
#data_year_groups2[2,] <- c(2000,2011)
#data_year_groups2[3,] <- c(2006,2011)
#data_year_groups2[4,] <- c(1994,1999)
#data_year_groups2[5,] <- c(2000,2005)


#dep_var2 <- c("pflow","nflow")
#dep_var2 <- c("pflow","mktadjret")
dep_var2 <- c("pflow","mktadjret","exret",
              "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
              "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")


model_type2 <- "pooling"

note2 <- "similarity"

#sim_type2 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type2 <- c("050pct","900pct")

#Regression equations
regression_equations2 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations2[1,] <- c(NA,
                               "all_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations2[2,] <- c(NA,
                               "main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations2[3,] <- c(NA,
                               "all_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations2[4,] <- c(NA,
                               "main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations2[5,] <- c(NA,
                               "all_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations2[6,] <- c(NA,
                               "main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations2[7,] <- c(NA,
                               "all_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations2[8,] <- c(NA,
                               "main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations2[9,] <- c(NA,
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations2 <- unknown_to_NA(regression_equations2,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations2))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations2[i,1:(ncol(regression_equations2)-1)], use.names=FALSE))))
  regression_equations2[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups2))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups2[k,1], "END YEAR:", data_year_groups2[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups2[k,1] & data_all[,"yr"]<=data_year_groups2[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var2))
    #for (i in 4:length(dep_var2))
  {
    #i <- 1
    
    for (j in 1:length(sim_type2))
    {
      #j <- 1
      #j <- 2
      #j <- 6
      
      out_file_name <- paste("reg_compare_plm",dep_var2[i],data_year_groups2[k,1],data_year_groups2[k,2],note2,sim_type2[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations2) )
      se <- rep( list(list()), nrow(regression_equations2) )
      pval <- rep( list(list()), nrow(regression_equations2) )
      
      for (l in 1:nrow(regression_equations2))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations2[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type2[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type2)
        #reg0 <- lm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep=""))), 
              model.names=paste("(",seq(1,nrow(regression_equations2)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Similarity on Hedge Fund Flows – Multivariate",
              file=paste(output_directory_reg_similarity,out_file_name,".doc",sep=""))
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var2), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type2))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups2,dep_var2,model_type2,note2,sim_type2,temp_char_vec,regression_equations2,k)


###############################################################################
cat("PANEL REGRESSION - READABILITY & SIMILARITY", "\n")
###############################################################################

output_directory_reg_readability_similarity <- paste(output_directory,"reg_readability_similarity","\\",sep="")
create_directory(output_directory_reg_readability_similarity,remove=1)

data_year_groups3 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups3[1,] <- c(start_year,end_year)
#data_year_groups3[2,] <- c(2000,2011)
#data_year_groups3[3,] <- c(2006,2011)
#data_year_groups3[4,] <- c(1994,1999)
#data_year_groups3[5,] <- c(2000,2005)

#dep_var3 <- c("pflow","nflow")
#dep_var3 <- c("pflow")
#dep_var3 <- c("pflow","mktadjret")
dep_var3 <- c("pflow","mktadjret","exret",
              "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
              "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")

model_type3 <- "pooling"

note3 <- "readbility_similarity_year"

#sim_type3 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type3 <- c("050pct","900pct")


#Regression equations
regression_equations3 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations3[1,] <- c("all_similarity_YYYpct_XXX",
                               "avg_grade_level_XXX",
                               NA,NA,NA,NA)
regression_equations3[2,] <- c("all_similarity_YYYpct_XXX",
                               "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               NA,NA,NA,NA)
regression_equations3[3,] <- c("main_investment_strategy_similarity_YYYpct_XXX",
                               "avg_grade_level_XXX",
                               NA,NA,NA,NA)
regression_equations3[4,] <- c("main_investment_strategy_similarity_YYYpct_XXX",
                               "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               NA,NA,NA,NA)
regression_equations3[5,] <- c("all_similarity_YYYpct_XXX",
                               "avg_grade_level_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations3[6,] <- c("all_similarity_YYYpct_XXX",
                               "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations3[7,] <- c("main_investment_strategy_similarity_YYYpct_XXX",
                               "avg_grade_level_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations3[8,] <- c("main_investment_strategy_similarity_YYYpct_XXX",
                               "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations3[9,] <- c("all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "coleman_liau_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)

regression_equations3 <- unknown_to_NA(regression_equations3,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations3))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations3[i,1:(ncol(regression_equations3)-1)], use.names=FALSE))))
  regression_equations3[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups3))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups3[k,1], "END YEAR:", data_year_groups3[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups3[k,1] & data_all[,"yr"]<=data_year_groups3[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var3))  
    #for (i in 4:length(dep_var3))
  {
    #i <- 1
    
    for (j in 1:length(sim_type3))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var3[i],data_year_groups3[k,1],data_year_groups3[k,2],note3,sim_type3[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations3) )
      se <- rep( list(list()), nrow(regression_equations3) )
      pval <- rep( list(list()), nrow(regression_equations3) )
      
      for (l in 1:nrow(regression_equations3))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations3[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type3[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type3)
        #reg0 <- lm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations3)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability & Similarity on Hedge Fund Flows – Multivariate",
              file=paste(output_directory_reg_readability_similarity,out_file_name,".doc",sep=""))
      
      #custom.names
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var3), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type3))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups3,dep_var3,model_type3,note3,sim_type3,temp_char_vec,regression_equations3,k)


###############################################################################
cat("PANEL REGRESSION - READABILITY & SIMILARITY QUARTILES", "\n")
###############################################################################

output_directory_reg_readability_similarity_quartiles <- paste(output_directory,"reg_readability_similarity_quartiles","\\",sep="")
create_directory(output_directory_reg_readability_similarity_quartiles,remove=1)

data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups4[1,] <- c(start_year,end_year)
#data_year_groups4[2,] <- c(2000,2011)
#data_year_groups4[3,] <- c(2006,2011)
#data_year_groups4[4,] <- c(1994,1999)
#data_year_groups4[5,] <- c(2000,2005)


#dep_var4 <- c("pflow","nflow")
#dep_var4 <- c("pflow")
#dep_var4 <- c("pflow","mktadjret")
dep_var4 <- c("pflow","mktadjret","exret",
              "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
              "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")

model_type4 <- "pooling"

note4 <- "readbility_similarity_year"

#sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type4 <- c("050pct","900pct")


#Regression equations
regression_equations4 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations4[1,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA,NA,NA)
regression_equations4[2,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA,NA,NA)
regression_equations4[3,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations4[4,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations4[5,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations4[6,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations4[7,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations4[8,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations4[9,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)

regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations4))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
  regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups4))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var4))  
    #for (i in 4:length(dep_var4))
  {
    #i <- 1
    
    for (j in 1:length(sim_type4))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations4) )
      se <- rep( list(list()), nrow(regression_equations4) )
      pval <- rep( list(list()), nrow(regression_equations4) )
      
      for (l in 1:nrow(regression_equations4))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
        #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability & Similarity on Hedge Fund Flows – Multivariate",
              file=paste(output_directory_reg_readability_similarity_quartiles,out_file_name,".doc",sep=""))
      
      #custom.names
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)





























###############################################################################
cat("YOUNG FUNDS", "\n")
###############################################################################

data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups4[1,] <- c(start_year,end_year)
data_year_groups4[2,] <- c(2000,2011)
data_year_groups4[3,] <- c(1994,1999)
data_year_groups4[4,] <- c(2000,2005)
data_year_groups4[5,] <- c(2006,2011)

#dep_var4 <- c("pflow","nflow")
dep_var4 <- c("pflow")

model_type4 <- "pooling"

note4 <- "young_readbility_similarity"

sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#sim_type4 <- c("050pct","900pct")


#Regression equations
regression_equations4 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations4[1,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations4[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations4[3,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               NA,NA,NA)
regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               NA,NA,NA)
regression_equations4[5,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               NA,NA)
regression_equations4[6,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations4[7,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations4[8,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "factor(yr)",
                               NA)
regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations4))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
  regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups4))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
  data_temp <- data_temp[data_temp[,"age_m"]<=36,]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var4))
  {
    #i <- 1
    
    for (j in 1:length(sim_type4))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations4) )
      se <- rep( list(list()), nrow(regression_equations4) )
      pval <- rep( list(list()), nrow(regression_equations4) )
      
      for (l in 1:nrow(regression_equations4))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
        #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability & Similarity on Young Hedge Fund Flows – Multivariate",
              file=paste(output_directory,out_file_name,".doc",sep=""))
      
      #custom.names
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)


###############################################################################
cat("FLOW VOLATILITY DATA", "\n")
###############################################################################

data_vol_other0 <- data_all[,c(identifier,"yr","sdpct_flow",
                               "age_y","total_fee","performance_fee","management_fee","other_fee",
                               "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                               "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                               "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                               "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                               "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios",
                               "ari_ios_below_quartile1","ari_ios_above_quartile3",
                               "coleman_liau_ios_below_quartile1","coleman_liau_ios_above_quartile3",
                               "flesch_kincaid_ios_below_quartile1","flesch_kincaid_ios_above_quartile3",
                               "fog_ios_below_quartile1","fog_ios_above_quartile3",
                               "smog_ios_below_quartile1","smog_ios_above_quartile3",
                               "avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3",
                               "avg_grade_level_acf_ios_below_quartile1","avg_grade_level_acf_ios_above_quartile3",
                               "avg_grade_level_ac_ios_below_quartile1","avg_grade_level_ac_ios_above_quartile3",
                               "all_similarity_050pct_ios_below_quartile1","all_similarity_100pct_ios_below_quartile1","all_similarity_250pct_ios_below_quartile1",
                               "all_similarity_500pct_ios_below_quartile1","all_similarity_750pct_ios_below_quartile1","all_similarity_900pct_ios_below_quartile1",
                               "all_similarity_050pct_ios_above_quartile3","all_similarity_100pct_ios_above_quartile3","all_similarity_250pct_ios_above_quartile3",
                               "all_similarity_500pct_ios_above_quartile3","all_similarity_750pct_ios_above_quartile3","all_similarity_900pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_050pct_ios_below_quartile1","main_investment_strategy_similarity_100pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_250pct_ios_below_quartile1","main_investment_strategy_similarity_500pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_750pct_ios_below_quartile1","main_investment_strategy_similarity_900pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_050pct_ios_above_quartile3","main_investment_strategy_similarity_100pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_250pct_ios_above_quartile3","main_investment_strategy_similarity_500pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_750pct_ios_above_quartile3","main_investment_strategy_similarity_900pct_ios_above_quartile3")]

data_vol_other_dt0 <- data.table(data_vol_other0, key = "fund_id,yr")
#data_vol_other_dt <- data_vol_other_dt0[, tail(.SD, 1), by = key(data_vol_other_dt0)]
data_vol_other_dt <- data_vol_other_dt0[unique(data_vol_other_dt0[,key(data_vol_other_dt0), with = FALSE]), mult = 'last']
data_vol_other <- data.frame(data_vol_other_dt,stringsAsFactors=FALSE)
data_vol_other <- unique(data_vol_other)

rm2(data_vol_other0,data_vol_other_dt0,data_vol_other_dt)

data_vol_dt <- data.table(data_all[c(identifier,"yr","aum","monthly_ret")])
setkeyv(data_vol_dt,c(identifier,"yr"))

ret_annualized1 <- data_vol_dt[, list(aret1=monthly_ret+1),by="fund_id,yr"]
ret_annualized2 <- ret_annualized1[, list(aret2=prod(aret1, na.rm=FALSE)),by="fund_id,yr"]
ret_annualized3 <- ret_annualized2[, list(aret=aret2-1),by="fund_id,yr"]
ret_annualized3 <- as.data.frame(ret_annualized3,stringsAsFactors=FALSE)
ret_annualized <- data.frame(ret_annualized3,
                             aret_lag1=NA,
                             stringsAsFactors=FALSE)
ret_annualized[,"aret_lag1"] <- create_lags2(ret_annualized,"aret",identifier,1)

rm2(ret_annualized1,ret_annualized2,ret_annualized3)

data_averaged0 <- data_vol_dt[, list(a_aum=mean(aum,na.rm=TRUE)),by="fund_id,yr"]
data_averaged0 <- as.data.frame(data_averaged0,stringsAsFactors=FALSE)
data_averaged <- data.frame(data_averaged0,
                            a_aum_log=suppressWarnings(log(data_averaged0[,"a_aum"])),
                            stringsAsFactors=FALSE)

rm2(data_averaged0)

data_vol0 <- merge(data_averaged, ret_annualized, 
                   by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                   all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

rm2(data_averaged,ret_annualized)

data_vol <- merge(data_vol0, data_vol_other, 
                  by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                  all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)

data_vol <- unknown_to_NA(data_vol,unknowns_strings)

rm2(data_vol0,data_vol_other,data_vol_dt)


###############################################################################
cat("FLOW VOLATILITY REGRESSION", "\n")
###############################################################################


data_year_groups5 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups5[1,] <- c(start_year,end_year)
data_year_groups5[2,] <- c(2000,2011)
data_year_groups5[3,] <- c(1994,1999)
data_year_groups5[4,] <- c(2000,2005)
data_year_groups5[5,] <- c(2006,2011)


dep_var5 <- c("sdpct_flow")

model_type5 <- "pooling"

note5 <- "vol_readability_similarity"

sim_type5 <- c("050pct","100pct","250pct","500pct","750pct","900pct")

#Regression equations
regression_equations5 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations5[1,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations5[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations5[3,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               NA,NA,NA)
regression_equations5[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               NA,NA,NA)
regression_equations5[5,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "ari_ios_below_quartile1 + ari_ios_above_quartile3 + coleman_liau_ios_below_quartile1 + coleman_liau_ios_above_quartile3 + flesch_kincaid_ios_below_quartile1 + flesch_kincaid_ios_above_quartile3 + fog_ios_below_quartile1 + fog_ios_above_quartile3 + smog_ios_below_quartile1 + smog_ios_above_quartile3",
                               NA,NA)
regression_equations5[6,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               NA,NA)
regression_equations5[7,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations5[7,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "factor(yr)",
                               NA)
regression_equations5 <- unknown_to_NA(regression_equations5,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations5))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations5[i,1:(ncol(regression_equations5)-1)], use.names=FALSE))))
  regression_equations5[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}


for (k in 1:nrow(data_year_groups5))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups5[k,1], "END YEAR:", data_year_groups5[k,2],"\n")
  
  data_temp <- data_vol[(data_vol[,"yr"]>=data_year_groups5[k,1] & data_vol[,"yr"]<=data_year_groups5[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var5))
  {
    #i <- 1
    
    for (j in 1:length(sim_type5))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var5[i],data_year_groups5[k,1],data_year_groups5[k,2],note5,sim_type5[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations5) )
      se <- rep( list(list()), nrow(regression_equations5) )
      pval <- rep( list(list()), nrow(regression_equations5) )
      
      for (l in 1:nrow(regression_equations5))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations5[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type5[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type5)
        #reg0 <- lm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"yr"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep=""))), 
              model.names=paste("(",seq(1,nrow(regression_equations5)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability Similarity on Hedge Fund Flow Volatility – Multivariate",
              file=paste(output_directory,out_file_name,".doc",sep=""))
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var5), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type5))
      
      rm2(se,pval,out_file_name,l)
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups5,dep_var5,model_type5,note5,sim_type5,temp_char_vec,regression_equations5,k)







###############################################################################
cat("PRINCIPAL COMPONENT - READABILITY", "\n")
###############################################################################

pc_scores_loadings <- function(data_in,variables,suffix,note,tol){
  
  #data_in <- data_all
  #variables <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")
  #suffix <- "ios"
  #note <- "Readability"
  #tol=NULL
  #tol=0.3
  
  title_full <- ifelse(is.null(tol), paste(note," (Tol = Null)",sep=""), paste(note," (Tol = ",tol,")",sep=""))
  
  pc_temp <- prcomp(~ ., data=data_in[,variables], na.action=na.omit, scale=TRUE)
  
  #Plotting the variances
  plot(pc_temp, main = title_full)
  barplot(pc_temp$sdev/pc_temp$sdev[1], main = title_full)
  
  pc_sum <- summary(pc_temp) 
  
  #Importance (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
  pc_importance <- pc_sum$importance
  pc_importance <- as.matrix(pc_importance)
  pc_importance <- data.frame(type="Importance",
                              var=row.names(pc_importance),
                              pc_importance,
                              PC_avg=rowMeans(pc_importance),
                              stringsAsFactors=FALSE)
  row.names(pc_importance)  <- seq(nrow(pc_importance))
  
  #Loadings (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
  pc_loadings <- pc_sum$rotation
  pc_loadings <- as.matrix(pc_loadings)
  pc_loadings <- data.frame(type="Loadings",
                            var=row.names(pc_loadings),
                            pc_loadings,
                            PC_avg=rowMeans(pc_loadings),
                            stringsAsFactors=FALSE)
  row.names(pc_loadings)  <- seq(nrow(pc_loadings))
  
  #Importance and Loadings
  pc_importance_loadings <- rbind(pc_importance,pc_loadings)
  row.names(pc_importance_loadings)  <- seq(nrow(pc_importance_loadings))
  rm(pc_importance,pc_loadings)
  
  #Center
  pc_center <- pc_sum$center
  pc_center <- as.matrix(pc_center)
  pc_center <- data.frame(var=row.names(pc_center),
                          pc_center,
                          stringsAsFactors=FALSE)
  row.names(pc_center)  <- seq(nrow(pc_center))
  
  #Scale
  pc_scale <- pc_sum$scale
  pc_scale <- as.matrix(pc_scale)
  pc_scale <- data.frame(var=row.names(pc_scale),
                         pc_scale,
                         stringsAsFactors=FALSE)
  row.names(pc_scale)  <- seq(nrow(pc_scale))
  
  #Center and Scale
  pc_center_scale <- merge(pc_center, pc_scale, 
                           by.x="var", by.y="var", 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)
  row.names(pc_center_scale) <- seq(nrow(pc_center_scale))
  rm(pc_center,pc_scale)
  
  #Scores
  pc_scores <- pc_sum$x
  pc_scores <- as.matrix(pc_scores)
  colnames(pc_scores) <- paste(colnames(pc_scores),suffix,sep="_")
  pc_scores <- data.frame(pc_scores,
                          stringsAsFactors=FALSE)
  row.names(pc_scores)  <- seq(nrow(pc_scores))
  
  #Combine data
  comb_list <- list("Importance_and_Loadings" = pc_importance_loadings, "Center_and_Scale" = pc_center_scale, "Scores" = pc_scores)
  
  rm(pc_importance_loadings,pc_center_scale,pc_scores)
  
  return(comb_list)
  
}

pca_text_read_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")

#pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", NULL)
pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", 0.2)

pc_read_ios_loadings <- pc_read_ios$Importance_and_Loadings
pc_read_ios_center <- pc_read_ios$Center_and_Scale
pc_read_ios_scores <- pc_read_ios$Scores

rm2(pca_text_read_vars_ios,pc_read_ios,pc_read_ios_loadings,pc_read_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT - SIMILARITY", "\n")
###############################################################################

#pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
#                           "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
#                           "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")
pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios")

#pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", NULL)
pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", 0.4 )

pc_sim_ios_loadings <- pc_sim_ios$Importance_and_Loadings
pc_sim_ios_center <- pc_sim_ios$Center_and_Scale
pc_sim_ios_scores <- pc_sim_ios$Scores

rm2(pca_text_sim_vars_ios,pc_sim_ios,pc_sim_ios_loadings,pc_sim_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT - READABILITY & SIMILARITY", "\n")
###############################################################################

# pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
#                             "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
#                             "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
#                             "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")
pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                            "all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios")

#pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", NULL)
pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", 0.2)

pc_both_ios_loadings <- pc_both_ios$Importance_and_Loadings
pc_both_ios_center <- pc_both_ios$Center_and_Scale
pc_both_ios_scores <- pc_both_ios$Scores

rm2(pca_text_both_vars_ios,pc_both_ios,pc_both_ios_loadings,pc_both_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT DATA - MERGE", "\n")
###############################################################################

data_all_pc_rn <- data.frame(rn=as.integer(row.names(data_all)),
                             data_all,
                             stringsAsFactors=FALSE)

ios_scores_rn <- data.frame(rn=as.integer(row.names(pc_both_ios_scores)),
                            pc_both_ios_scores,
                            stringsAsFactors=FALSE)

data_all_pc <- merge(data_all_pc_rn, ios_scores_rn, 
                     by.x="rn", by.y="rn", 
                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all_pc <- data_all_pc[order(data_all_pc[,"rn"]),]
data_all_pc <- data_all_pc[,!(colnames(data_all_pc) %in% "rn")]
data_all_pc <- data_all_pc[order(data_all_pc[,identifier],
                                 data_all_pc[,"yr"],
                                 data_all_pc[,"month"]),]
row.names(data_all_pc) <- seq(nrow(data_all_pc))

rm2(data_all_pc_rn,ios_scores_rn)
rm2(pc_read_ios_scores,pc_sim_ios_scores,pc_both_ios_scores)


###############################################################################
cat("PRINCIPAL COMPONENT REGRESSION", "\n")
###############################################################################


# data_pc.pd <- pdata.frame(data_all_pc, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
# dep_var_pc <- c("pflow")
# 
# index_vars_pc <- c(identifier, "yr_month")
# controls_pc <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y + log_mtna_agg"
# #fe_pc <- "factor(yr)"
# fe_pc <- "factor(branding_name) + factor(yr)"
# 
# model_type_pc <- "pooling"
# note <- "pca_sep_sim_good"
# 
# 
# for (i in 1:length(dep_var_pc))
# {
#   #i <- 1
#   
#   out_file_name <- paste("reg_compare_plm",dep_var_pc[i],deparse(substitute(data_all_pc)),note,sep="_")
#   
#   if (dep_var_pc[i]=="pflow")
#   {
#     #vars_ext <- "pflow_lag1 + pflow_lag2 + pflow_lag3"
#     vars_ext <- ""
#     
#   } else if (dep_var_pc[i]=="nflow")
#   {
#     #vars_ext <- "nflow_lag1 + nflow_lag2 + nflow_lag3"
#     vars_ext <- ""
#     
#   } else
#   {
#     cat("ERROR", "\n")
#   }
#   
#   
#   ind_vars_reg0_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios"
#   reg0_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg0_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg0_rse_pc <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#   reg0_rse_pc <- mcl.plm(data_all_pc,reg0_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg0_pc),digits=3,model.names=c("(1)"),override.se=list(reg0_rse_pc[,2]),override.pval=list(reg0_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg2_pc <- "PC1_pr + PC2_pr + PC3_pr + PC4_pr"
#   reg2_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg2_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg2_rse_pc <- coeftest(reg2, vcov=function(x) vcovDC(x, type="HC1"))
#   reg2_rse_pc <- mcl.plm(data_all_pc,reg2_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg2_pc),digits=3,model.names=c("(1)"),override.se=list(reg2_rse_pc[,2]),override.pval=list(reg2_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg3_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios + PC1_pr + PC2_pr + PC3_pr + PC4_pr"
#   reg3_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg3_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg3_rse_pc <- coeftest(reg3, vcov=function(x) vcovDC(x, type="HC1"))
#   reg3_rse_pc <- mcl.plm(data_all_pc,reg3_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg3_pc),digits=3,model.names=c("(1)"),override.se=list(reg3_rse_pc[,2]),override.pval=list(reg3_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg4_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
#   reg4_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg4_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   reg4_rse_pc <- mcl.plm(data_all_pc,reg4_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg4_pc),digits=3,model.names=c("(1)"),override.se=list(reg4_rse_pc[,2]),override.pval=list(reg4_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,fe_pc,sep="+")
#   #ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
#   #reg5_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg5_rse_pc <- mcl.plm(data_all_pc,reg5_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   reg5_pc <- lm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), data_all_pc)
#   reg5_rse_pc <- mcl(data_all_pc,reg5_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg5_pc),digits=3,model.names=c("(1)"),override.se=list(reg5_rse_pc[,2]),override.pval=list(reg5_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   htmlreg(list(reg0_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc), 
#           caption="The Importance of Clustering Standard Errors", digits=3, 
#           model.names=c("(1)","(2)","(3)","(4)","(5)"),
#           override.se=list(reg0_rse_pc[,2],reg2_rse_pc[,2],reg3_rse_pc[,2],reg4_rse_pc[,2],reg5_rse_pc[,2]),
#           override.pval=list(reg0_rse_pc[,4],reg2_rse_pc[,4],reg3_rse_pc[,4],reg4_rse_pc[,4],reg5_rse_pc[,4]),
#           stars=c(0.01, 0.05, 0.1),
#           file=paste(output_directory,out_file_name,".doc",sep=""))
#   
#   rm2(ind_vars_reg0_pc,ind_vars_reg2_pc,ind_vars_reg3_pc,ind_vars_reg4_pc,ind_vars_reg5_pc,out_file_name)
#   rm2(reg0_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc,reg0_rse_pc,reg2_rse_pc,reg3_rse_pc,reg4_rse_pc,reg5_rse_pc)
#   
# } 




# 
# data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
#                                 stringsAsFactors=FALSE)
# data_year_groups4[1,] <- c(1992,2012)
# data_year_groups4[2,] <- c(2000,2011)
# data_year_groups4[3,] <- c(1992,1998)
# data_year_groups4[4,] <- c(1999,2005)
# data_year_groups4[5,] <- c(2006,2012)
# 
# #dep_var4 <- c("pflow","nflow")
# dep_var4 <- c("pflow")
# 
# model_type4 <- "pooling"
# 
# note4 <- "pc"
# 
# #Regression equations
# regression_equations4 <- data.frame(grade=NA,
#                                     similarity=NA,
#                                     controls=NA,
#                                     quantile=NA,
#                                     fixed_effects=NA,
#                                     full_independent_vars=NA,
#                                     stringsAsFactors=FALSE)
# regression_equations4[1,] <- c("avg_grade_level_XXX",
#                                NA,NA,NA,NA,NA)
# regression_equations4[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,NA,NA,NA,NA)
# regression_equations4[3,] <- c("avg_grade_level_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[5,] <- c("avg_grade_level_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee",
#                                NA,NA,NA)
# regression_equations4[6,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee",
#                                NA,NA,NA)
# regression_equations4[7,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee + flagship_bin + closed_bin + dead_bin",
#                                NA,NA,NA)
# 
# regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)
# #Create Independent Variable Equation
# for (i in 1:nrow(regression_equations4))
# {
#   
#   temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
#   regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
#   
# }
# 
# for (k in 1:nrow(data_year_groups4))
# {
#   #k <- 1
#   #k <- 2
#   
#   cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
#   
#   data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
#   data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
#   
#   for (i in 1:length(dep_var4))
#   {
#     #i <- 1
#     
#     #out_file_name <- paste("reg_compare_plm",dep_var4[i],deparse(substitute(data_all)),note4,sep="_")
#     out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sep="_")
#     
#     #models <- rep( list(list()), nrow(regression_equations4) )
#     se <- rep( list(list()), nrow(regression_equations4) )
#     pval <- rep( list(list()), nrow(regression_equations4) )
#     
#     for (l in 1:nrow(regression_equations4))
#     {
#       #l <- 1
#       
#       ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
#       ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
#       reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
#       #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
#       #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
#       #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
#       #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#       reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
#       #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
#       #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#       #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#       
#       #models[[l]] <- reg0
#       se[[l]] <- reg0_rse[,4]
#       pval[[l]] <- reg0_rse[,4]
#       
#       assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
#       #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
#       
#       rm2(ind_vars_reg0,reg0,reg0_rse)
#       
#     }
#     
#     htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))), 
#             model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
#             override.se=se,
#             override.pval=pval,
#             stars=c(0.01, 0.05, 0.1), digits=3, 
#             caption="Effect of Readability on Hedge Fund Flows – Multivariate",
#             file=paste(output_directory,out_file_name,".doc",sep=""))
#     
#     #custom.names
#     
#     progress_function(outer_loop_count=k, outer_loop_start_val=1, outer_loop_end_val=nrow(data_year_groups4), 
#                       inner_loop_count=i, inner_loop_start_val=1, inner_loop_end_val=length(dep_var4))
#     
#     rm2(se,pval,out_file_name,l)
#     eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
#     
#   } 
#   
#   rm2(data_temp,data_temp.pd,i)
#   
# }
# 
# rm2(data_year_groups4,dep_var4,model_type4,note4,temp_char_vec,regression_equations4,k)
# 
# 
