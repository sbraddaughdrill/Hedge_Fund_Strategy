# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Data_Combination1.R
# Version: 1.0
# Date:    01.07.2014
# Purpose: Combine all data sources
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

#repo <- c("http://cran.us.r-project.org")
repo <- c("http://cran.rstudio.com/")
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
#unknowns_strings <- c("",".",NA,"na","n/a","n\a","NA","N/A","N\\A","<NA>","null","NULL",NULL,"nan","NaN",NaN,
#                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",NA_character_,
#                      "NA_character_",NA_real_,"NA_real_")
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  
  input_directory <- normalizePath("F:/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)   
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE)   
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)   
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)       
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)   
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)



###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_finance.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
#external_packages <- c("colbycol","compare","cwhmisc","DataCombine","fastmatch","foreign","formatR","gdata",
#                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
#                       "pander","pbapply","plm","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
#                       "sandwich","sqldf","stargazer","stringr","texreg","UsingR","xtable","zoo")

external_packages <- c("data.table","limma","plyr","R.utils","sqldf","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")


###############################################################################
cat("IMPORT READABILITY TEXT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

#Import .CSV files

#read_stats_ios_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f_full.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
read_stats_ios_f <- as.data.frame(read.csv(file=paste(output_directory,"read_stats_ios_f_full.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

#read_stats_ios_f <- trim_by_format(read_stats_ios_f,"character")
for (k in which(sapply(read_stats_ios_f,class)=="character")) 
{
  read_stats_ios_f[[k]] <= gsub("^\\s+|\\s+$", "", read_stats_ios_f[[k]], perl=TRUE)
}
rm(k)



read_stats_ios_f <- data.table(read_stats_ios_f)[, (colnames(read_stats_ios_f)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(read_stats_ios_f)]
read_stats_ios_f <- as.data.frame(read_stats_ios_f,stringsAsFactors=FALSE)

read_stats_ios_f <- read_stats_ios_f[,!(colnames(read_stats_ios_f) %in% "foreign_ios")]

#colnames(read_stats_ios_f) <- tolower(colnames(read_stats_ios_f))


sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"text_clean_trim.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

#sample_data_all <- trim_by_format(sample_data_all,"character")
for (k in which(sapply(sample_data_all,class)=="character")) 
{
  sample_data_all[[k]] <= gsub("^\\s+|\\s+$", "", sample_data_all[[k]], perl=TRUE)
}
rm(k)


sample_data_all <- data.table(sample_data_all)[, (colnames(sample_data_all)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(sample_data_all)]
sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)

sample_data_all <- sample_data_all[,!(colnames(sample_data_all) %in% "Strategy")]

#colnames(sample_data_all) <- tolower(colnames(sample_data_all))


###############################################################################
cat("IMPORT EUREKA HEDGE DATA", "\n")
###############################################################################

final_folder_expand3_path <- normalizePath("F:/Import_Data/Data/Eurekahedge/Final_Expand3",winslash="\\", mustWork=TRUE) 

#Import Fund Details

fund_details_path <- paste(final_folder_expand3_path,"\\","EurekahedgeHF_Fund_Details_part3",".csv",sep="")

fund_details_cols_all <- as.vector(t(read.csv(file=fund_details_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

#fund_details_ncol <- length(fund_details_cols_all)
#fund_details_nrow <- countLines(fund_details_path)

fund_details_cols_id1 <- c("Fund_ID","date","yr","month","pull_trim","pull_trim2")
fund_details_cols_id2 <- c("Fund_Name","Date_Added","Dead_Date","Dead_Reason")

fund_details_cols_nonid <- fund_details_cols_all[!(fund_details_cols_all %in% c(fund_details_cols_id1,fund_details_cols_id2))]
fund_details_cols_nonid_org1 <- fund_details_cols_nonid[(grepl("_org",fund_details_cols_nonid))]
fund_details_cols_nonid_org2 <- gsub("_org","",fund_details_cols_nonid_org1)
fund_details_cols_nonid_comments <- fund_details_cols_nonid[(grepl("_comments",fund_details_cols_nonid))]

fund_details_cols_nonid_drop1 <- c("Flagship","Closed","Limited","Dead","bad_min","bad_max", "min_date","max_date")
fund_details_cols_nonid_drop2 <- paste(c("Flagship","Closed","Limited","Dead","bad_min","bad_max"),"bin",sep="_")
fund_details_cols_nonid_keep <- fund_details_cols_nonid[!(fund_details_cols_nonid %in% c(fund_details_cols_nonid_org1,fund_details_cols_nonid_org2,fund_details_cols_nonid_comments,fund_details_cols_nonid_drop1,fund_details_cols_nonid_drop2))]

fund_details_cols_keep <- fund_details_cols_all[fund_details_cols_all %in% c(fund_details_cols_id1,fund_details_cols_nonid_keep)]

#q_fund_details <- ""
#q_fund_details <- paste(q_fund_details,"select     ",paste(fund_details_cols_keep,"",sep="",collapse=","),"     ", sep=" ")
#q_fund_details <- paste(q_fund_details,"select     *                                                            ", sep=" ")
#q_fund_details <- paste(q_fund_details,"from       file                                                         ", sep=" ")
#q_fund_details <- paste(q_fund_details,"where      yr > 2004                                                    ", sep=" ")
#q_fund_details <- gsub(" {2,}", " ", q_fund_details)


#fund_details <- read.csv.sql(file=fund_details_path, sql=q_fund_details, header=TRUE, sep = ",",stringsAsFactors=FALSE)
#fund_details <- read.csv2.sql(file=fund_details_path, sql=q_fund_details, filter=NULL, header=TRUE, sep = ",",stringsAsFactors=FALSE)
#fund_details <- read.csv.sql(file=fund_details_path, sql=q_fund_details, filter=list('gawk -f prog', prog = '{ gsub(/"/, ""); print }'), header=TRUE, sep = ",",stringsAsFactors=FALSE)
#fund_details <- read.csv.sql(file=fund_details_path, sql=q_fund_details, filter='tr.exe -d ^" ', header=TRUE, sep = ",",stringsAsFactors=FALSE)
#fund_details <- read.csv.sql(file=fund_details_path, sql=q_fund_details, sep=";", eol="\n", filter="gawk -v osep=; -f csv.awk", header=TRUE,stringsAsFactors=FALSE)

fund_details0 <- read.columns(file=fund_details_path,required.col=fund_details_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(fund_details_cols_all)
rm2(fund_details_cols_nonid,fund_details_cols_nonid_org1,fund_details_cols_nonid_org2,fund_details_cols_nonid_comments)
rm2(fund_details_cols_id1,fund_details_cols_id2,fund_details_cols_nonid_keep,fund_details_cols_nonid_drop1,fund_details_cols_nonid_drop2)
rm2(fund_details_cols_keep)

#fund_details <- fund_details0[fund_details0[,"yr"]>2004,]
fund_details <- fund_details0[fund_details0[,"yr"]>1993,]
rm2(fund_details0)

fund_details <- fund_details[order(fund_details[,identifier], 
                                   fund_details[,"pull_trim"],
                                   fund_details[,"pull_trim2"],
                                   fund_details[,"yr"],
                                   fund_details[,"month"]),]
row.names(fund_details) <- seq(nrow(fund_details))


#Import Fees and Redemption

fees_path <- paste(final_folder_expand3_path,"\\","EurekahedgeHF_Fee_and_Redemption_part3",".csv",sep="")

fees_cols_all <- as.vector(t(read.csv(file=fees_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

#fees_ncol <- length(fees_cols_all)
#fees_nrow <- countLines(fees_path)

fees_cols_id1 <- c("Fund_ID","date","yr","month","pull_trim","pull_trim2")
fees_cols_id2 <- c("Fund_Name","Date_Added","Dead_Date","Dead_Reason")

fees_cols_nonid <- fees_cols_all[!(fees_cols_all %in% c(fees_cols_id1,fees_cols_id2))]
fees_cols_nonid_org1 <- fees_cols_nonid[(grepl("_org",fees_cols_nonid))]
fees_cols_nonid_org2 <- gsub("_org","",fees_cols_nonid_org1)
fees_cols_nonid_comments <- fees_cols_nonid[(grepl("_comments",fees_cols_nonid))]

fees_cols_nonid_drop1 <- c("Flagship","Closed","Limited","Dead","bad_min","bad_max", "min_date","max_date")
fees_cols_nonid_drop2 <- paste(c("Flagship","Closed","Limited","Dead","bad_min","bad_max"),"bin",sep="_")
fees_cols_nonid_keep <- fees_cols_nonid[!(fees_cols_nonid %in% c(fees_cols_nonid_org1,fees_cols_nonid_org2,fees_cols_nonid_comments,fees_cols_nonid_drop1,fees_cols_nonid_drop2))]

fees_cols_keep <- fees_cols_all[fees_cols_all %in% c(fees_cols_id1,fees_cols_nonid_keep)]

fees0 <- read.columns(file=fees_path,required.col=fees_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(fees_cols_all)
rm2(fees_cols_nonid,fees_cols_nonid_org1,fees_cols_nonid_org2,fees_cols_nonid_comments)
rm2(fees_cols_id1,fees_cols_id2,fees_cols_nonid_keep,fees_cols_nonid_drop1,fees_cols_nonid_drop2)
rm2(fees_cols_keep)

#fees <- fees0[fees0[,"yr"]>2004,]
fees <- fees0[fees0[,"yr"]>1993,]
rm2(fees0)

fees <- fees[order(fees[,identifier], 
                   fees[,"pull_trim"],
                   fees[,"pull_trim2"],
                   fees[,"yr"],
                   fees[,"month"]),]
row.names(fees) <- seq(nrow(fees))


#Import Stats

stats_path <- paste(final_folder_expand3_path,"\\","EurekahedgeHF_Stats_noreturns_part3",".csv",sep="")

stats_cols_all <- as.vector(t(read.csv(file=stats_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

#stats_ncol <- length(stats_cols_all)
#stats_nrow <- countLines(stats_path)

stats_cols_id1 <- c("Fund_ID","date","yr","month","pull_trim","pull_trim2")
stats_cols_id2 <- c("Fund_Name","Date_Added","Dead_Date","Dead_Reason")

stats_cols_nonid <- stats_cols_all[!(stats_cols_all %in% c(stats_cols_id1,stats_cols_id2))]
stats_cols_nonid_org1 <- stats_cols_nonid[(grepl("_org",stats_cols_nonid))]
stats_cols_nonid_org2 <- gsub("_org","",stats_cols_nonid_org1)
stats_cols_nonid_comments <- stats_cols_nonid[(grepl("_comments",stats_cols_nonid))]

stats_cols_nonid_drop1 <- c("Flagship","Closed","Limited","Dead","bad_min","bad_max", "min_date","max_date")
stats_cols_nonid_drop2 <- paste(c("Flagship","Closed","Limited","Dead","bad_min","bad_max"),"bin",sep="_")
stats_cols_nonid_keep <- stats_cols_nonid[!(stats_cols_nonid %in% c(stats_cols_nonid_org1,stats_cols_nonid_org2,stats_cols_nonid_comments,stats_cols_nonid_drop1,stats_cols_nonid_drop2))]

stats_cols_keep <- stats_cols_all[stats_cols_all %in% c(stats_cols_id1,stats_cols_nonid_keep)]

stats0 <- read.columns(file=stats_path,required.col=stats_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(stats_cols_all)
rm2(stats_cols_nonid,stats_cols_nonid_org1,stats_cols_nonid_org2,stats_cols_nonid_comments)
rm2(stats_cols_id1,stats_cols_id2,stats_cols_nonid_keep,stats_cols_nonid_drop1,stats_cols_nonid_drop2)
rm2(stats_cols_keep)

#stats <- stats0[stats0[,"yr"]>2004,]
stats <- stats0[stats0[,"yr"]>1993,]
rm2(stats0)

stats <- stats[order(stats[,identifier], 
                     stats[,"pull_trim"],
                     stats[,"pull_trim2"],
                     stats[,"yr"],
                     stats[,"month"]),]
row.names(stats) <- seq(nrow(stats))


#Import Returns

rets_path <- paste(final_folder_expand3_path,"\\","EurekahedgeHF_NAV_AUM_Ret",".csv",sep="")

rets_cols_all <- as.vector(t(read.csv(file=rets_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

#rets_ncol <- length(rets_cols_all)
#rets_nrow <- countLines(rets_path)

rets_cols_id1 <- c("Fund_ID","date","yr","month","pull_trim","pull_trim2")
rets_cols_id2 <- c("Fund_Name","Date_Added","Dead_Date","Dead_Reason")

rets_cols_nonid <- rets_cols_all[!(rets_cols_all %in% c(rets_cols_id1,rets_cols_id2))]
rets_cols_nonid_org1 <- rets_cols_nonid[(grepl("_org",rets_cols_nonid))]
rets_cols_nonid_org2 <- gsub("_org","",rets_cols_nonid_org1)
rets_cols_nonid_comments <- rets_cols_nonid[(grepl("_comments",rets_cols_nonid))]

rets_cols_nonid_drop1 <- c("Flagship","Closed","Limited","Dead","bad_min","bad_max", "min_date","max_date")
rets_cols_nonid_drop2 <- paste(c("Flagship","Closed","Limited","Dead","bad_min","bad_max"),"bin",sep="_")
rets_cols_nonid_keep <- rets_cols_nonid[!(rets_cols_nonid %in% c(rets_cols_nonid_org1,rets_cols_nonid_org2,rets_cols_nonid_comments,rets_cols_nonid_drop1,rets_cols_nonid_drop2))]

ret_cols_keep <- rets_cols_all[rets_cols_all %in% c(rets_cols_id1,rets_cols_nonid_keep)]

rets0 <- read.columns(file=rets_path,required.col=ret_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(rets_cols_all)
rm2(rets_cols_nonid,rets_cols_nonid_org1,rets_cols_nonid_org2,rets_cols_nonid_comments)
rm2(rets_cols_id1,rets_cols_id2,rets_cols_nonid_keep,rets_cols_nonid_drop1,rets_cols_nonid_drop2)
rm2(ret_cols_keep)

#rets <- rets0[rets0[,"yr"]>2004,]
rets <- rets0[rets0[,"yr"]>1993,]
rm2(rets0)

# Convert pull to pull_trim2
colnames(rets)[match("pull",names(rets))] <- "pull_trim2"

rets[,"pull_trim2"] <- gsub("_NAV_AUM","",rets[,"pull_trim2"])

rets <- rets[order(rets[,identifier], 
                   rets[,"pull_trim"],
                   rets[,"pull_trim2"],
                   rets[,"yr"],
                   rets[,"month"]),]
row.names(rets) <- seq(nrow(rets))


# LEVERAGE IS IN BOTH FUND_DETAILS AND FEES SO RENAME IT IN BOTH THEN MERGE AND COMB #####
colnames(fund_details)[match("Leverage",names(fund_details))] <- "Leverage_fund_details"
colnames(fees)[match("Leverage",names(fees))] <- "Leverage_fees"

temp_allocate_cols <- c(unique(c(colnames(fund_details),colnames(fees),colnames(stats),colnames(rets))),"Leverage")
temp_allocate_rows_n <- max(c(nrow(fund_details),nrow(fees),nrow(stats),nrow(rets)))
temp_allocate <- data.frame(matrix(NA, ncol=length(temp_allocate_cols), nrow=temp_allocate_rows_n, dimnames=list(c(), temp_allocate_cols)),stringsAsFactors=FALSE)

invisible(gc(verbose = FALSE, reset = TRUE))

temp_allocate[,colnames(fund_details)] <- fund_details[,colnames(fund_details)]

invisible(gc(verbose = FALSE, reset = TRUE))

temp_allocate <- temp_allocate[,colnames(temp_allocate) %in% c(colnames(fund_details),"Leverage")]

rm2(fund_details)
invisible(gc(verbose = FALSE, reset = TRUE))

temp_allocate <- temp_allocate[!(rowSums(is.na(temp_allocate[,1:ncol(temp_allocate)]))==(ncol(temp_allocate))),]

invisible(gc(verbose = FALSE, reset = TRUE))


# Merge Data 1a

temp_allocate_key_a <- intersect(colnames(temp_allocate),colnames(fees))


temp_allocate <- merge(temp_allocate,fees, 
                       by.x=temp_allocate_key_a, by.y=temp_allocate_key_a, 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(fees,temp_allocate_key_a)

temp_allocate <- temp_allocate[order(temp_allocate[,identifier], 
                                     temp_allocate[,"pull_trim"],
                                     temp_allocate[,"pull_trim2"],
                                     temp_allocate[,"yr"],
                                     temp_allocate[,"month"]),]
row.names(temp_allocate) <- seq(nrow(temp_allocate))

invisible(gc(verbose = FALSE, reset = TRUE))


### FIX LEVERAGE

#temp_allocate <- data.frame(temp_allocate,Leverage="TEST",stringsAsFactors=FALSE)
temp_allocate[,"Leverage_fund_details"] <- ifelse(is.na(temp_allocate[,"Leverage_fund_details"]),"",temp_allocate[,"Leverage_fund_details"])
temp_allocate[,"Leverage_fees"] <- ifelse(is.na(temp_allocate[,"Leverage_fees"]),"",temp_allocate[,"Leverage_fees"])

temp_allocate[,"Leverage"] <- ifelse((temp_allocate[,"Leverage_fund_details"]=="" & temp_allocate[,"Leverage_fees"]==""),"",temp_allocate[,"Leverage"])
#temp_allocate[,"Leverage"] <- ifelse(temp_allocate[,"Leverage_fund_details"]==temp_allocate[,"Leverage_fees"],temp_allocate[,"Leverage_fund_details"],temp_allocate[,"Leverage"])

temp_allocate[,"Leverage"] <- ifelse((temp_allocate[,"Leverage_fund_details"]!="" & temp_allocate[,"Leverage_fees"]==""),temp_allocate[,"Leverage_fund_details"],temp_allocate[,"Leverage"])
temp_allocate[,"Leverage"] <- ifelse((temp_allocate[,"Leverage_fund_details"]=="" & temp_allocate[,"Leverage_fees"]!=""),temp_allocate[,"Leverage_fees"],temp_allocate[,"Leverage"])

temp_allocate[,"Leverage"] <- ifelse(temp_allocate[,"Leverage"]=="",NA,temp_allocate[,"Leverage"])

temp_allocate <- temp_allocate[,!(colnames(temp_allocate) %in% c("Leverage_fund_details","Leverage_fees"))]
invisible(gc(verbose = FALSE, reset = TRUE))


# Merge Data 1b

temp_allocate_key_b <- intersect(colnames(temp_allocate),colnames(stats))

temp_allocate <- merge(temp_allocate,stats, 
                       by.x=temp_allocate_key_b, by.y=temp_allocate_key_b, 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(stats,temp_allocate_key_b)

temp_allocate <- temp_allocate[order(temp_allocate[,identifier], 
                                     temp_allocate[,"pull_trim"],
                                     temp_allocate[,"pull_trim2"],
                                     temp_allocate[,"yr"],
                                     temp_allocate[,"month"]),]
row.names(temp_allocate) <- seq(nrow(temp_allocate))

invisible(gc(verbose = FALSE, reset = TRUE))


# Merge Data 1c

temp_allocate_key_c <- intersect(colnames(temp_allocate),colnames(rets))

temp_allocate <- merge(rets,temp_allocate,
                       by.x=temp_allocate_key_c, by.y=temp_allocate_key_c, 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(rets,temp_allocate_key_c)

temp_allocate <- temp_allocate[order(temp_allocate[,identifier], 
                                     temp_allocate[,"pull_trim"],
                                     temp_allocate[,"pull_trim2"],
                                     temp_allocate[,"yr"],
                                     temp_allocate[,"month"]),]
row.names(temp_allocate) <- seq(nrow(temp_allocate))

invisible(gc(verbose = FALSE, reset = TRUE))

#temp_allocate <- unique(temp_allocate)



# Merge Data 2

sample_data_all_trim <- merge(read_stats_ios_f[,c(identifier,"yr")],
                              sample_data_all[,!(colnames(sample_data_all) %in% c("Flagship","Closed","Limited","Dead"))], 
                              by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(read_stats_ios_f,sample_data_all)

sample_data_all_trim <- sample_data_all_trim[order(sample_data_all_trim[,identifier], 
                                                   sample_data_all_trim[,"yr"]),]
row.names(sample_data_all_trim) <- seq(nrow(sample_data_all_trim))

invisible(gc(verbose = FALSE, reset = TRUE))


# Merge Data 3

#a <- sample_data_all_trim[sample_data_all_trim[,"Fund_ID"]==5002,]
#b <- temp_allocate[temp_allocate[,"Fund_ID"]==5002,]

EurekahedgeHF_Excel_aca_full_key0 <- intersect(colnames(sample_data_all_trim),colnames(temp_allocate))

EurekahedgeHF_Excel_aca_full0 <- merge(temp_allocate,sample_data_all_trim,
                                       #sample_data_all_trim,temp_allocate,
                                       by.x=EurekahedgeHF_Excel_aca_full_key0, by.y=EurekahedgeHF_Excel_aca_full_key0, 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(sample_data_all_trim,temp_allocate,EurekahedgeHF_Excel_aca_full_key0)

EurekahedgeHF_Excel_aca_full0 <- EurekahedgeHF_Excel_aca_full0[order(EurekahedgeHF_Excel_aca_full0[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full0[,"pull_trim"],
                                                                     EurekahedgeHF_Excel_aca_full0[,"pull_trim2"],
                                                                     EurekahedgeHF_Excel_aca_full0[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full0[,"month"]),]
row.names(EurekahedgeHF_Excel_aca_full0) <- seq(nrow(EurekahedgeHF_Excel_aca_full0))

invisible(gc(verbose = FALSE, reset = TRUE))

write.csv(EurekahedgeHF_Excel_aca_full0, file=paste(output_directory,"Fund_details_merge",".csv",sep=""),row.names=FALSE)

