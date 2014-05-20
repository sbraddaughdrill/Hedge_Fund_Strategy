# TODO: Add comment
# 
# Author: Brad
# File: Hedge_Fund_Strategy_Alphas.R
# Version: 1.0
# Date: 05.20.2014
# Purpose: Compute alphas
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

update.packages(ask=FALSE, checkBuilt=TRUE)

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

rm2(regression_equations_alpha,regression_equations_alpha_include)
#rm2(data1_factors)


###############################################################################
cat("MERGE IN FUNDS AND ALPHAS", "\n")
###############################################################################


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


###############################################################################
cat("OUTPUT DATA", "\n")
###############################################################################

ExportTable(data_fulll_db,"data_all",data_all)
write.csv(data_all,file=paste(output_directory,"data_all",".csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(quantile_vars_dv_temp2_ios,quantile_vars_dv,data2)
