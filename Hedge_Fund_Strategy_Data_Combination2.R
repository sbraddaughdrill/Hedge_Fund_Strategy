# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Data_Combination2.R
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

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)
# options(max.print=999999)

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
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
#external_packages <- c("colbycol","compare","cwhmisc","DataCombine","fastmatch","foreign","formatR",
#                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
#                       "pander","pbapply","plm","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
#                       "sandwich","sqldf","stargazer","stringr","texreg","UsingR","xtable","zoo")

external_packages <- c("data.table","gdata","limma","plyr","R.utils","sqldf","zoo")
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

#read_stats_ios_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f_full.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
read_stats_ios_f <- as.data.frame(read.csv(file=paste(output_directory,"read_stats_ios_f_full.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

read_stats_ios_f <- trim_by_format(read_stats_ios_f,"character")

read_stats_ios_f <- data.table(read_stats_ios_f)
read_stats_ios_f <- read_stats_ios_f[, (colnames(read_stats_ios_f)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(read_stats_ios_f)]
read_stats_ios_f <- read_stats_ios_f[, c("avg_grade_level_ios"):=list(NA),by=NULL]
read_stats_ios_f <- read_stats_ios_f[, c("foreign_ios"):=list(NULL),by=NULL]

read_stats_ios_f <- as.data.frame(read_stats_ios_f,stringsAsFactors=FALSE)

read_stats_ios_f  <- read_stats_ios_f[((!is.na(read_stats_ios_f[,"ARI_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"Coleman_Liau_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"Flesch_Kincaid_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"FOG_ios"])) &   
                                         (!is.na(read_stats_ios_f[,"SMOG_ios"]))),]

read_stats_ios_f[,"avg_grade_level_ios"] <- rowMeans(read_stats_ios_f[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios")], na.rm=TRUE)

read_stats_ios_f <- read_stats_ios_f[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios",
                                        colnames(read_stats_ios_f)[!(colnames(read_stats_ios_f) %in% c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios"))])]

read_stats_ios_f <- read_stats_ios_f[,c(identifier,"yr","Strategy","Strat_ID",
                                        colnames(read_stats_ios_f)[!(colnames(read_stats_ios_f) %in% c(identifier,"yr","Strategy","Strat_ID"))])]

#colnames(read_stats_ios_f) <- tolower(colnames(read_stats_ios_f))


sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"text_clean_trim.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

sample_data_all <- trim_by_format(sample_data_all,"character")

sample_data_all <- data.table(sample_data_all)[, (colnames(sample_data_all)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(sample_data_all)]
sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)

sample_data_all <- sample_data_all[,!(colnames(sample_data_all) %in% "Strategy")]

sample_data_all_cols <- colnames(sample_data_all)

#colnames(sample_data_all) <- tolower(colnames(sample_data_all))


###############################################################################
cat("IMPORT SIMILARITY TEXT DATA", "\n")
###############################################################################

text_variables <- c("ios")
text_percentages <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#text_percentages <- c("900pct")

temp_input_data_name_short <- "year_id_unique_sim_cosine_normalized"

sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], 
                                                   sample_data_all_temp[,"yr"]),]
row.names(sample_data_all_temp) <- seq(nrow(sample_data_all_temp))

for (j in 1:length(text_variables))
{
  #j <- 1
  
  assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
  
}
rm2(j)


rm2(sample_data_all_temp)

for (i in 1:length(text_variables))
{
  #i <- 1
  
  for (j in 1:length(text_percentages))
  {
    
    #j <- 1
    
    temp_input_data_name_full <- paste(temp_input_data_name_short,text_percentages[j],text_variables[i],"avg",sep="_")
    
    #year_sim <- as.data.frame(fread(paste(output_directory,temp_input_data_name_full,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
    year_sim <- as.data.frame(read.csv(file=paste(output_directory,temp_input_data_name_full,".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
    
    #colnames(year_sim) <- tolower(colnames(year_sim))
    
    year_sim_char_cols <- colnames(year_sim)[laply(year_sim,class,.progress = "text",.drop = FALSE)=="character"]
    year_sim <- trim_dt(year_sim,year_sim_char_cols)
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    year_sim <- data.table(year_sim)[, (colnames(year_sim)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(year_sim)]
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    year_sim_years <- colnames(year_sim)
    year_sim_years <- year_sim_years[!(year_sim_years==identifier)] 
    year_sim_years <- gsub("[[:alpha:]]","",year_sim_years)
    
    colnames(year_sim) <- c(identifier,year_sim_years)
    
    assign(paste("years",text_variables[i],text_percentages[j],sep="_"), year_sim_years, envir=.GlobalEnv)
    
    year_sim_stacked0 <- lapply(year_sim_years, function(x,data){ data.frame(identifier=data[,identifier], yr=as.integer(x), similarity_all_ios=data[,x], stringsAsFactors=FALSE)}, data = year_sim)
    
    #year_sim_stacked <- do.call(rbind, year_sim_stacked0) 
    year_sim_stacked <- rbindlist(l=year_sim_stacked0, use.names=TRUE, fill=FALSE)
    year_sim_stacked <- as.data.frame(year_sim_stacked,stringsAsFactors=FALSE) 
    
    colnames(year_sim_stacked)[match("identifier",names(year_sim_stacked))] <- identifier
    
    year_sim_stacked <- year_sim_stacked[order(year_sim_stacked[,identifier],
                                               year_sim_stacked[,"yr"]),] 
    row.names(year_sim_stacked) <- seq(nrow(year_sim_stacked))
    
    colnames(year_sim_stacked) <- c(identifier,"yr",paste("similarity",text_percentages[j],text_variables[i],sep="_"))
    
    if (text_variables[i]=="ios")
    {
      year_sim_ios_all_stacked <- merge(year_sim_ios_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
      
    } else if (text_variables[i]=="pr")
    {
      year_sim_pr_all_stacked <- merge(year_sim_pr_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_variables), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_percentages))
    
    rm2(year_sim,year_sim_years,year_sim_stacked0,year_sim_stacked)
    
  }
  rm2(j)
} 
rm2(i)

year_sim_ios_all_stacked <- year_sim_ios_all_stacked[!(rowSums(is.na(year_sim_ios_all_stacked[,3:ncol(year_sim_ios_all_stacked)]))==(ncol(year_sim_ios_all_stacked)-2)),]
year_sim_ios_all_stacked <- year_sim_ios_all_stacked[order(year_sim_ios_all_stacked[,identifier], 
                                                           year_sim_ios_all_stacked[,"yr"]),]
row.names(year_sim_ios_all_stacked) <- seq(nrow(year_sim_ios_all_stacked))

colnames(year_sim_ios_all_stacked) <- paste("all",colnames(year_sim_ios_all_stacked),sep="_")
colnames(year_sim_ios_all_stacked)[1:2] <- c(identifier,"yr")

#colnames(year_sim_ios_all_stacked) <- tolower(colnames(year_sim_ios_all_stacked))


###############################################################################
cat("IMPORT CRSP DATA", "\n")
###############################################################################

#crsp_fundno_unique <- data.frame(crsp_fundno=unique(sample_data_all[,c("crsp_fundno")], incomparables=FALSE),stringsAsFactors=FALSE)

#ExportTable(crsp_db,"crsp_fundno_unique",crsp_fundno_unique)

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

Crspa_msi <- runsql("SELECT * FROM Crspa_msi",crsp_db)

Crspa_msi <- trim_by_format(Crspa_msi,"character")

Crspa_msi <- data.table(Crspa_msi)[, (colnames(Crspa_msi)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(Crspa_msi)]
Crspa_msi <- as.data.frame(Crspa_msi,stringsAsFactors=FALSE)

#DeleteTable(crsp_db,"crsp_fundno_unique")

colnames(Crspa_msi) <- tolower(colnames(Crspa_msi))

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

rm2(crsp_tables,crsp_fields)


###############################################################################
cat("CODE MISSING VARIABLES AS NA", "\n")
###############################################################################

Crspa_msi[,"vwretd"] <- ifelse(Crspa_msi[,"vwretd"]==-99, NA, Crspa_msi[,"vwretd"])

Crspa_msi[,"vwretx"] <- ifelse(Crspa_msi[,"vwretx"]==-99, NA, Crspa_msi[,"vwretx"])

###############################################################################
cat("CONVERT RETURNS TO MONTHLY", "\n")
###############################################################################

Crspa_msi_monthly <- data.frame(vwretd_annualized=Crspa_msi[,"vwretd"],
                                vwretx_annualized=Crspa_msi[,"vwretx"],
                                Crspa_msi,
                                stringsAsFactors=FALSE)

Crspa_msi_monthly[,"vwretd"] <- (((Crspa_msi_monthly[,"vwretd"] + 1)^(1/12))-1)
Crspa_msi_monthly[,"vwretx"] <- (((Crspa_msi_monthly[,"vwretx"] + 1)^(1/12))-1)

rm2(Crspa_msi)


###############################################################################
cat("CREATE YEAR AND MONTH VARIABLE FOR Crspa_msi", "\n")
###############################################################################

######## NEW 1/21/2015 #########
Crspa_msi_monthly[,"date"] <- as.Date(Crspa_msi_monthly[,"date"],origin="1970-01-01")
################################


crspa_msi_full <- transform(Crspa_msi_monthly,yr=year(as.IDate(date)),month=month(as.IDate(date)))

#crspa_msi_trim <- subset(crspa_msi_full,select=c(yr,month,vwretd,vwretx))
crspa_msi_trim <- crspa_msi_full[,(colnames(crspa_msi_full) %in% c("yr","month","vwretd_annualized","vwretd","vwretx_annualized","vwretx"))]
crspa_msi_trim <- crspa_msi_trim[,c("yr","month","vwretd_annualized","vwretd","vwretx_annualized","vwretx")]

rm2(Crspa_msi_monthly,crspa_msi_full)


###############################################################################
cat("COMPUTE MONTHLY FUND RETURN VOLATILITY", "\n")
###############################################################################

# daily_returns_full <- transform(daily_returns,yr=year(as.IDate(caldt)),month=month(as.IDate(caldt)))

# # daily_returns_trim <- subset(daily_returns_full,select=-c(caldt))
# daily_returns_trim <- daily_returns_full[,!(colnames(daily_returns_full) %in% "caldt")]

# fund_mret_volatility <- ddply(daily_returns_trim, c("crsp_fundno","yr","month"), summarize, sddret = sd(dret, na.rm = TRUE))


###############################################################################
cat("IMPORT EUREKA HEDGE DATA", "\n")
###############################################################################

missing_vars <-   c("AUM","Monthly_Ret")
missing_vars_expand <- c("group_freq","freq","idx","na_count",unlist(lapply(missing_vars,function(x){paste(x,c("_org","_new"),sep="")})))

age_vars <- c("chgdt","age_m","age_y")

aum_vars <- c("AUM")
aum_vars_lagged_count <- 4
aum_vars_lagged <- unlist(lapply(aum_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=aum_vars_lagged_count))

aum_vars_log <- c("AUM_log")
aum_vars_log_lagged_count <- 4
aum_vars_log_lagged <- unlist(lapply(aum_vars_log,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=aum_vars_log_lagged_count))

ret_vars <- c("Monthly_Ret","Monthly_Ret2","Yearly_Ret2","mktadjret")
ret_vars_lagged_count <- 4
ret_vars_lagged <- unlist(lapply(ret_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=ret_vars_lagged_count))

ret_sq_vars <- c("Monthly_Ret_sq","Monthly_Ret2_sq","Yearly_Ret2_sq","mktadjret_sq")
ret_sq_vars_lagged_count <- 4
ret_sq_vars_lagged <- unlist(lapply(ret_sq_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=ret_sq_vars_lagged_count))

flow_vars <- c("nflow","pflow")
flow_vars_lagged_count <- 4
flow_vars_lagged <- unlist(lapply(flow_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=flow_vars_lagged_count))

sdflow_vars <- c("sdnet_flow","sdpct_flow")
sdflow_vars_lagged_count <- 1
sdflow_vars_lagged <- unlist(lapply(sdflow_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=sdflow_vars_lagged_count))

crsp_msi_vars <- colnames(crspa_msi_trim)[!(colnames(crspa_msi_trim) %in% c("yr","month"))]

EurekahedgeHF_Excel_aca_full0_path <- paste(output_directory,"Fund_details_merge.csv",sep="")
EurekahedgeHF_Excel_aca_full0_cols <- as.vector(unlist(t(read.csv(file=EurekahedgeHF_Excel_aca_full0_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1))))

EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols0 <- c(missing_vars,missing_vars_expand,age_vars,aum_vars,aum_vars_lagged,aum_vars_log,aum_vars_log_lagged,ret_vars,ret_vars_lagged,
                                                             ret_sq_vars,ret_sq_vars_lagged, flow_vars,flow_vars_lagged,sdflow_vars,sdflow_vars_lagged,crsp_msi_vars)

EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols1 <- EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols0[!(EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols0 %in% EurekahedgeHF_Excel_aca_full0_cols)]

EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols <- c(EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols1)


EurekahedgeHF_Excel_aca_full0 <- data.frame(read.csv(file=EurekahedgeHF_Excel_aca_full0_path,header=TRUE,na.strings="NA",stringsAsFactors=FALSE),
                                            matrix(NA, ncol=length(EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols), nrow=1,dimnames=list(c(),EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols)),stringsAsFactors=FALSE)

#EurekahedgeHF_Excel_aca_full0 <- EurekahedgeHF_Excel_aca_full0[,!(colnames(EurekahedgeHF_Excel_aca_full0) %in% EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols)]
EurekahedgeHF_Excel_aca_full0 <- EurekahedgeHF_Excel_aca_full0[,!(colnames(EurekahedgeHF_Excel_aca_full0) %in% c(missing_vars_expand,crsp_msi_vars))]

EurekahedgeHF_Excel_aca_full0 <- as.data.table(EurekahedgeHF_Excel_aca_full0)

#for (k in colnames(EurekahedgeHF_Excel_aca_full0)) 
for(k in which(colnames(EurekahedgeHF_Excel_aca_full0) %in% EurekahedgeHF_Excel_aca_full0_cols))
{
  #k <- 1
  
  set(EurekahedgeHF_Excel_aca_full0, i=NULL, j=k, value=unknownToNA(EurekahedgeHF_Excel_aca_full0[[k]], unknown=unknowns_strings,force=TRUE))
  set(EurekahedgeHF_Excel_aca_full0, i=NULL, j=k, value=ifelse(is.na(EurekahedgeHF_Excel_aca_full0[[k]]),NA,EurekahedgeHF_Excel_aca_full0[[k]]))
}
rm2(k)


###############################################################################
cat("FILL IN MISSING INFO", "\n")
###############################################################################

## Make sure that there is only 1 Fund_ID-yr-month row

#EurekahedgeHF_Excel_aca_full_counts <- count(EurekahedgeHF_Excel_aca_full0,c(identifier,"pull_trim","pull_trim2","yr"))
EurekahedgeHF_Excel_aca_full_counts <- count(EurekahedgeHF_Excel_aca_full0,c(identifier,"pull_trim","pull_trim2"))
colnames(EurekahedgeHF_Excel_aca_full_counts)[match("freq",names(EurekahedgeHF_Excel_aca_full_counts))] <- "group_freq"

#EurekahedgeHF_Excel_aca_full_month_counts <- count(EurekahedgeHF_Excel_aca_full0,c(identifier,"yr","month"))
#EurekahedgeHF_Excel_aca_full_month_counts <- ddply(.data=EurekahedgeHF_Excel_aca_full0, .variables=c(identifier,"yr","month"), .fun = function(x){return(data.frame(x,freq=nrow(x),stringsAsFactors=FALSE))}, .progress = "text")


#EurekahedgeHF_Excel_aca_full_month_counts <- data.table(EurekahedgeHF_Excel_aca_full0)[,':='(freq=.N,idx=1:.N),by=c(identifier,"yr","month")]
EurekahedgeHF_Excel_aca_full0 <- EurekahedgeHF_Excel_aca_full0[,':='(freq=.N,idx=1:.N),by=c(identifier,"yr","month")]

EurekahedgeHF_Excel_aca_full_month_counts_good <- EurekahedgeHF_Excel_aca_full0[freq==1,]
EurekahedgeHF_Excel_aca_full_month_counts_bad <- EurekahedgeHF_Excel_aca_full0[freq>1,]

rm2(EurekahedgeHF_Excel_aca_full0)
invisible(gc(verbose = FALSE, reset = TRUE))

EurekahedgeHF_Excel_aca_full_month_counts_good <- EurekahedgeHF_Excel_aca_full_month_counts_good[,c("freq", "idx"):=NULL]
EurekahedgeHF_Excel_aca_full_month_counts_good <- as.data.frame(EurekahedgeHF_Excel_aca_full_month_counts_good,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full_month_counts_bad <- as.data.frame(EurekahedgeHF_Excel_aca_full_month_counts_bad,stringsAsFactors=FALSE)


#EurekahedgeHF_Excel_aca_full_month_counts_bad_merge0 <- merge(EurekahedgeHF_Excel_aca_full_month_counts_bad[,c(identifier,"pull_trim","pull_trim2","yr","month","freq","idx")],EurekahedgeHF_Excel_aca_full0,
#                                                             by.x=c(identifier,"pull_trim","pull_trim2","yr","month"), by.y=c(identifier,"pull_trim","pull_trim2","yr","month"), 
#                                                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

EurekahedgeHF_Excel_aca_full_month_counts_bad_merge <- merge(EurekahedgeHF_Excel_aca_full_month_counts_bad,EurekahedgeHF_Excel_aca_full_counts,
                                                             by.x=c(identifier,"pull_trim","pull_trim2"), by.y=c(identifier,"pull_trim","pull_trim2"), 
                                                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(EurekahedgeHF_Excel_aca_full_month_counts_bad,EurekahedgeHF_Excel_aca_full_counts)
invisible(gc(verbose = FALSE, reset = TRUE))

EurekahedgeHF_Excel_aca_full_month_counts_bad_merge <- EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[order(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,identifier], 
                                                                                                                 EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,"pull_trim"],
                                                                                                                 EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,"yr"],
                                                                                                                 EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,"month"],
                                                                                                                 EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,"pull_trim2"]),]
row.names(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge) <- seq(nrow(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge))

invisible(gc(verbose = FALSE, reset = TRUE))


EurekahedgeHF_Excel_aca_full_month_counts_bad_merge <- data.table(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge)
EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,na_count := rowSums(is.na(.SD)),.SDcols=colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge)]
EurekahedgeHF_Excel_aca_full_month_counts_bad_merge <- as.data.frame(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim <- EurekahedgeHF_Excel_aca_full_month_counts_bad_merge[,c(identifier,"pull_trim","pull_trim2","yr","month","freq","idx","group_freq","na_count")]

EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim <- EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[order(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,identifier], 
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"pull_trim"],
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"yr"],
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"month"],
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"group_freq"],
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"na_count"],
                                                                                                                           EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim[,"pull_trim2"]),]
row.names(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim) <- seq(nrow(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim))

EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim_keep <- ddply(.data=EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim, .variables=c(identifier,"pull_trim","yr","month"), .fun=function(x){return(head(x,1))}, .progress="none")

rm2(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim)
invisible(gc(verbose = FALSE, reset = TRUE))

EurekahedgeHF_Excel_aca_full_month_counts_bad_keep_key <- intersect(colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim_keep),colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge))

EurekahedgeHF_Excel_aca_full_month_counts_bad_keep <- merge(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim_keep,
                                                            EurekahedgeHF_Excel_aca_full_month_counts_bad_merge,
                                                            by.x=EurekahedgeHF_Excel_aca_full_month_counts_bad_keep_key, by.y=EurekahedgeHF_Excel_aca_full_month_counts_bad_keep_key, 
                                                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(EurekahedgeHF_Excel_aca_full_month_counts_bad_merge_trim_keep,EurekahedgeHF_Excel_aca_full_month_counts_bad_merge,EurekahedgeHF_Excel_aca_full_month_counts_bad_keep_key)
invisible(gc(verbose = FALSE, reset = TRUE))

EurekahedgeHF_Excel_aca_full_month_counts_bad_keep <- EurekahedgeHF_Excel_aca_full_month_counts_bad_keep[,!(colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_keep) %in% c("freq","idx"))]

EurekahedgeHF_Excel_aca_full_key <- intersect(colnames(EurekahedgeHF_Excel_aca_full_month_counts_good),colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_keep))

EurekahedgeHF_Excel_aca_full_month_counts_good <- EurekahedgeHF_Excel_aca_full_month_counts_good[,colnames(EurekahedgeHF_Excel_aca_full_month_counts_good) %in% EurekahedgeHF_Excel_aca_full_key]
EurekahedgeHF_Excel_aca_full_month_counts_good <- EurekahedgeHF_Excel_aca_full_month_counts_good[,EurekahedgeHF_Excel_aca_full_key]

EurekahedgeHF_Excel_aca_full_month_counts_bad_keep <- EurekahedgeHF_Excel_aca_full_month_counts_bad_keep[,colnames(EurekahedgeHF_Excel_aca_full_month_counts_bad_keep) %in% EurekahedgeHF_Excel_aca_full_key]
EurekahedgeHF_Excel_aca_full_month_counts_bad_keep <- EurekahedgeHF_Excel_aca_full_month_counts_bad_keep[,EurekahedgeHF_Excel_aca_full_key]

#EurekahedgeHF_Excel_aca_full1 <- rbind(EurekahedgeHF_Excel_aca_full_month_counts_good,EurekahedgeHF_Excel_aca_full_month_counts_bad_keep)
EurekahedgeHF_Excel_aca_full1 <- rbindlist(list(EurekahedgeHF_Excel_aca_full_month_counts_good,EurekahedgeHF_Excel_aca_full_month_counts_bad_keep),fill=TRUE)

rm2(EurekahedgeHF_Excel_aca_full_month_counts_good,EurekahedgeHF_Excel_aca_full_month_counts_bad_keep,EurekahedgeHF_Excel_aca_full_key)
#rm2(EurekahedgeHF_Excel_aca_full0)
invisible(gc(verbose = FALSE, reset = TRUE))


# EurekahedgeHF_Excel_aca_full1 <- as.data.frame(EurekahedgeHF_Excel_aca_full1,stringsAsFactors=FALSE)
# EurekahedgeHF_Excel_aca_full1 <- EurekahedgeHF_Excel_aca_full1[order(EurekahedgeHF_Excel_aca_full1[,identifier], 
#                                                                      EurekahedgeHF_Excel_aca_full1[,"pull_trim"],
#                                                                      EurekahedgeHF_Excel_aca_full1[,"pull_trim2"],
#                                                                      EurekahedgeHF_Excel_aca_full1[,"yr"],
#                                                                      EurekahedgeHF_Excel_aca_full1[,"month"]),]
# row.names(EurekahedgeHF_Excel_aca_full1) <- seq(nrow(EurekahedgeHF_Excel_aca_full1))
#
#colnames(EurekahedgeHF_Excel_aca_full1)[match("Monthly_Ret",names(EurekahedgeHF_Excel_aca_full1))] <- "Monthly_Ret_org"
#colnames(EurekahedgeHF_Excel_aca_full1)[match("AUM",names(EurekahedgeHF_Excel_aca_full1))] <- "AUM_org"

setorderv(EurekahedgeHF_Excel_aca_full1, c(identifier,"pull_trim","pull_trim2","yr","month"),c(1,1,1,1,1))

setnames(EurekahedgeHF_Excel_aca_full1,"Monthly_Ret","Monthly_Ret_org")
setnames(EurekahedgeHF_Excel_aca_full1,"AUM","AUM_org")

invisible(gc(verbose = FALSE, reset = TRUE))


## Fill in any missing ret Monthly Ret and AUM with most recent ret/aum data.

final_folder_NAV_AUM_path <- normalizePath("F:/Import_Data/Data/Eurekahedge/NAV_AUM_melt",winslash="\\", mustWork=TRUE) 


#Import NAV & AUM

NAV_AUM_path <- paste(final_folder_NAV_AUM_path,"\\","Eurekahedge_201404_HF_aca_NAV_AUM",".csv",sep="")

NAV_AUM_cols_all <- as.vector(t(read.csv(file=NAV_AUM_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

#NAV_AUM_ncol <- length(NAV_AUM_cols_all)
#NAV_AUM_nrow <- countLines(NAV_AUM_path)

NAV_AUM_cols_id1 <- c("Fund_ID","date","yr","month","pull","pull_trim","pull_trim2")
NAV_AUM_cols_id2 <- c("Fund_Name","Date_Added","Dead_Date","Dead_Reason")

NAV_AUM_cols_nonid <- NAV_AUM_cols_all[!(NAV_AUM_cols_all %in% c(NAV_AUM_cols_id1,NAV_AUM_cols_id2))]
NAV_AUM_cols_nonid_org1 <- NAV_AUM_cols_nonid[(grepl("_org",NAV_AUM_cols_nonid))]
NAV_AUM_cols_nonid_org2 <- gsub("_org","",NAV_AUM_cols_nonid_org1)
NAV_AUM_cols_nonid_comments <- NAV_AUM_cols_nonid[(grepl("_comments",NAV_AUM_cols_nonid))]

NAV_AUM_cols_nonid_drop1 <- c("Flagship","Closed","Limited","Dead","bad_min","bad_max", "min_date","max_date")
NAV_AUM_cols_nonid_keep <- NAV_AUM_cols_nonid[!(NAV_AUM_cols_nonid %in% c(NAV_AUM_cols_nonid_org1,NAV_AUM_cols_nonid_org2,NAV_AUM_cols_nonid_comments,NAV_AUM_cols_nonid_drop1))]

NAV_AUM_cols_keep <- NAV_AUM_cols_all[NAV_AUM_cols_all %in% c(NAV_AUM_cols_id1,NAV_AUM_cols_nonid_keep)]

NAV_AUM0 <- read.columns(file=NAV_AUM_path,required.col=NAV_AUM_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(NAV_AUM_cols_all)
rm2(NAV_AUM_cols_nonid,NAV_AUM_cols_nonid_org1,NAV_AUM_cols_nonid_org2,NAV_AUM_cols_nonid_comments)
rm2(NAV_AUM_cols_id1,NAV_AUM_cols_id2,NAV_AUM_cols_nonid_keep,NAV_AUM_cols_nonid_drop1)
rm2(NAV_AUM_cols_keep)

#NAV_AUM <- NAV_AUM0[NAV_AUM0[,"yr"]>2004,]
#NAV_AUM <- NAV_AUM0[NAV_AUM0[,"yr"]>1993,]

# NAV_AUM1 <- data.frame(NAV_AUM0,pull_trim=NA,pull_trim2=NA,yr=NA,month=NA,Monthly_Ret_temp=NA,AUM_temp=NA,stringsAsFactors=FALSE)
# 
# rm2(NAV_AUM0)
# 
# NAV_AUM1[,"pull_trim"] <- NAV_AUM1[,"pull"] 
# NAV_AUM1[,"pull_trim"] <- gsub("([[:alpha:]]|[[:punct:]])","",NAV_AUM1[,"pull_trim"])
# NAV_AUM1[,"pull_trim"] <- as.integer(NAV_AUM1[,"pull_trim"])
# 
# NAV_AUM1[,"pull_trim2"] <- NAV_AUM1[,"pull"] 
# NAV_AUM1[,"pull_trim2"] <- gsub("_NAV_AUM","",NAV_AUM1[,"pull_trim2"])
# 
# NAV_AUM1[,"date"] <- as.Date(as.yearmon(NAV_AUM1[,"date"],format="%b %Y"))
# 
# NAV_AUM1[,"yr"] <- year(NAV_AUM1[,"date"])
# NAV_AUM1[,"month"] <- month(NAV_AUM1[,"date"])
# 
# colnames(NAV_AUM1)[match("Monthly_Ret",names(NAV_AUM1))] <- "Monthly_Ret_new"
# colnames(NAV_AUM1)[match("AUM",names(NAV_AUM1))] <- "AUM_new"
# colnames(NAV_AUM1)[match("Monthly_Ret_temp",names(NAV_AUM1))] <- "Monthly_Ret"
# colnames(NAV_AUM1)[match("AUM_temp",names(NAV_AUM1))] <- "AUM"

NAV_AUM1 <- as.data.table(data.frame(NAV_AUM0,pull_trim=NA,pull_trim2=NA,yr=NA,month=NA,Monthly_Ret_temp=NA,AUM_temp=NA,stringsAsFactors=FALSE))

rm2(NAV_AUM0)

NAV_AUM1[, pull_trim:=pull,by=NULL]
set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("pull_trim")), value=gsub("([[:alpha:]]|[[:punct:]])","",NAV_AUM1[[which(colnames(NAV_AUM1)==c("pull_trim"))]]))
set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("pull_trim")), value=as.integer(NAV_AUM1[[which(colnames(NAV_AUM1)==c("pull_trim"))]]))

NAV_AUM1[, pull_trim2:=pull,by=NULL]
set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("pull_trim2")), value=gsub("_NAV_AUM","",NAV_AUM1[[which(colnames(NAV_AUM1)==c("pull_trim2"))]]))

set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("date")), value=as.Date(as.yearmon(NAV_AUM1[[which(colnames(NAV_AUM1)==c("date"))]],format="%b %Y")))

set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("yr")), value=year(NAV_AUM1[[which(colnames(NAV_AUM1)==c("date"))]]))
set(NAV_AUM1, i=NULL, j=which(colnames(NAV_AUM1)==c("month")), value=month(NAV_AUM1[[which(colnames(NAV_AUM1)==c("date"))]]))

setnames(NAV_AUM1,"Monthly_Ret","Monthly_Ret_new")
setnames(NAV_AUM1,"AUM","AUM_new")
setnames(NAV_AUM1,"Monthly_Ret_temp","Monthly_Ret")
setnames(NAV_AUM1,"AUM_temp","AUM")

NAV_AUM1_cols_keep <- c(identifier,"yr","month","Monthly_Ret_new","Monthly_Ret","AUM_new","AUM")

setcolorder(NAV_AUM1, c(NAV_AUM1_cols_keep,colnames(NAV_AUM1)[!(colnames(NAV_AUM1) %in% c(NAV_AUM1_cols_keep))]))
NAV_AUM1[,colnames(NAV_AUM1)[!(colnames(NAV_AUM1) %in% NAV_AUM1_cols_keep)]:=NULL]

rm(NAV_AUM1_cols_keep)


# Merge

EurekahedgeHF_Excel_aca_full_key <- c(identifier,"yr","month")

setkeyv(EurekahedgeHF_Excel_aca_full1,EurekahedgeHF_Excel_aca_full_key)
setkeyv(NAV_AUM1,EurekahedgeHF_Excel_aca_full_key)

EurekahedgeHF_Excel_aca_full <- merge(EurekahedgeHF_Excel_aca_full1,NAV_AUM1,
                                      by.x=EurekahedgeHF_Excel_aca_full_key, by.y=EurekahedgeHF_Excel_aca_full_key, 
                                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(EurekahedgeHF_Excel_aca_full1,NAV_AUM1,EurekahedgeHF_Excel_aca_full_key)

invisible(gc(verbose = FALSE, reset = TRUE))


### Monthly Ret

# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- "TEST"
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"]),"",EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"])
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"]),"",EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"])
# 
# #EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"]=="" & EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"]==""),"",EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"]==EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"],EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"],EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
# 
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"]!="" & EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"]==""),EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"],EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_org"]=="" & EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"]!=""),EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"],EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
# 
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"]=="TEST",EurekahedgeHF_Excel_aca_full[,"Monthly_Ret_new"],EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
# 
# EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"]=="",NA,EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=as.character(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value="TEST")
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org")), value=ifelse(is.na(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]),
                                                                                                                              "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new")), value=ifelse(is.na(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]),
                                                                                                                              "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]))

#set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]=="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]==""),
#                                                                                                                          "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]==EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]),
                                                                                                                          EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]!="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]==""),
                                                                                                                          EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_org"))]]=="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]]!=""),
                                                                                                                          EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]=="TEST",
                                                                                                                          EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret_new"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=ifelse(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]=="",
                                                                                                                          NA,EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))


### AUM

# EurekahedgeHF_Excel_aca_full[,"AUM"] <- "TEST"
# EurekahedgeHF_Excel_aca_full[,"AUM_org"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[,"AUM_org"]),"",EurekahedgeHF_Excel_aca_full[,"AUM_org"])
# EurekahedgeHF_Excel_aca_full[,"AUM_new"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[,"AUM_new"]),"",EurekahedgeHF_Excel_aca_full[,"AUM_new"])
# 
# #EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"AUM_org"]=="" & EurekahedgeHF_Excel_aca_full[,"AUM_new"]==""),"",EurekahedgeHF_Excel_aca_full[,"AUM"])
# EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"AUM_org"]==EurekahedgeHF_Excel_aca_full[,"AUM_new"],EurekahedgeHF_Excel_aca_full[,"AUM_org"],EurekahedgeHF_Excel_aca_full[,"AUM"])
# 
# EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"AUM_org"]!="" & EurekahedgeHF_Excel_aca_full[,"AUM_new"]==""),EurekahedgeHF_Excel_aca_full[,"AUM_org"],EurekahedgeHF_Excel_aca_full[,"AUM"])
# EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse((EurekahedgeHF_Excel_aca_full[,"AUM_org"]=="" & EurekahedgeHF_Excel_aca_full[,"AUM_new"]!=""),EurekahedgeHF_Excel_aca_full[,"AUM_new"],EurekahedgeHF_Excel_aca_full[,"AUM"])
# 
# EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"AUM"]=="TEST",EurekahedgeHF_Excel_aca_full[,"AUM_new"],EurekahedgeHF_Excel_aca_full[,"AUM"])
# 
# EurekahedgeHF_Excel_aca_full[,"AUM"] <- ifelse(EurekahedgeHF_Excel_aca_full[,"AUM"]=="",NA,EurekahedgeHF_Excel_aca_full[,"AUM"])


set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=as.character(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value="TEST")
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org")), value=ifelse(is.na(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]),
                                                                                                                      "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new")), value=ifelse(is.na(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]),
                                                                                                                      "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]))

#set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]=="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]==""),
#                                                                                                                  "",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]==EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]),
                                                                                                                  EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]!="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]==""),
                                                                                                                  EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse((EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_org"))]]=="" & EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]]!=""),
                                                                                                                  EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]=="TEST",
                                                                                                                  EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM_new"))]],EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=ifelse(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]=="",
                                                                                                                  NA,EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))


### Clean

#a <- EurekahedgeHF_Excel_aca_full[,c(identifier,"yr","month","Monthly_Ret_new","Monthly_Ret_org","Monthly_Ret","AUM_org","AUM_new","AUM")]
#unique(EurekahedgeHF_Excel_aca_full[,"Monthly_Ret"])
#unique(EurekahedgeHF_Excel_aca_full[,"AUM"])

# EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[,!(colnames(EurekahedgeHF_Excel_aca_full) %in% c("Monthly_Ret_org","Monthly_Ret_new","AUM_org","AUM_new"))]
# invisible(gc(verbose = FALSE, reset = TRUE))
# 
# EurekahedgeHF_Excel_aca_full_ids <- c("Fund_ID","Fund_Name","pull_trim","pull_trim2","date","yr","month","Date_Added","Dead_Date","Dead_Reason",
#                                       "Flagship_bin","Closed_bin","Limited_bin","Dead_bin","Monthly_Ret","Monthly_Ret2","Yearly_Ret2","AUM")
# 
# EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[,c(EurekahedgeHF_Excel_aca_full_ids,
#                                                                 colnames(EurekahedgeHF_Excel_aca_full)[!(colnames(EurekahedgeHF_Excel_aca_full) %in% EurekahedgeHF_Excel_aca_full_ids)])]
# 
# EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[order(EurekahedgeHF_Excel_aca_full[,identifier], 
#                                                                    EurekahedgeHF_Excel_aca_full[,"pull_trim"],
#                                                                    EurekahedgeHF_Excel_aca_full[,"pull_trim2"],
#                                                                    EurekahedgeHF_Excel_aca_full[,"date"],
#                                                                    EurekahedgeHF_Excel_aca_full[,"yr"],
#                                                                    EurekahedgeHF_Excel_aca_full[,"month"]),]
# row.names(EurekahedgeHF_Excel_aca_full) <- seq(nrow(EurekahedgeHF_Excel_aca_full))


EurekahedgeHF_Excel_aca_full[,c("Monthly_Ret_org","Monthly_Ret_new","AUM_org","AUM_new"):=NULL]

EurekahedgeHF_Excel_aca_full_ids <- c("Fund_ID","Fund_Name","pull_trim","pull_trim2","date","yr","month","Date_Added","Dead_Date","Dead_Reason",
                                      "Flagship_bin","Closed_bin","Limited_bin","Dead_bin","Monthly_Ret","Monthly_Ret2","Yearly_Ret2","AUM")

setcolorder(EurekahedgeHF_Excel_aca_full, c(EurekahedgeHF_Excel_aca_full_ids,colnames(EurekahedgeHF_Excel_aca_full)[!(colnames(EurekahedgeHF_Excel_aca_full) %in% c(EurekahedgeHF_Excel_aca_full_ids))]))

setorderv(EurekahedgeHF_Excel_aca_full, c(identifier,"pull_trim","pull_trim2","date","yr","month"),c(1,1,1,1,1,1))


###############################################################################
cat("CLEAN NUMBERS", "\n")
###############################################################################

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Monthly_Ret"))]]))
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("AUM"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Size_USm")), value=gsub(",","",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Size_USm"))]]))
#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Fund_Size_USm:=as.numeric(Fund_Size_USm)]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Size_USm")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Size_USm"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Capacity_USm")), value=gsub(",","",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Capacity_USm"))]]))
#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Fund_Capacity_USm:=as.numeric(Fund_Capacity_USm)]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Capacity_USm")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Fund_Capacity_USm"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Firms_Total_Asset_USm")), value=gsub(",","",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Firms_Total_Asset_USm"))]]))
#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Firms_Total_Asset_USm:=as.numeric(Firms_Total_Asset_USm)]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Firms_Total_Asset_USm")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Firms_Total_Asset_USm"))]]))

set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Total_Asset_in_Hedge_Funds_USm")), value=gsub(",","",EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Total_Asset_in_Hedge_Funds_USm"))]]))
#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Total_Asset_in_Hedge_Funds_USm:=as.numeric(Total_Asset_in_Hedge_Funds_USm)]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Total_Asset_in_Hedge_Funds_USm")), value=as.numeric(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Total_Asset_in_Hedge_Funds_USm"))]]))

#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, date:=as.Date(date,format="%Y-%m-%d")]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("date")), value=as.Date(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("date"))]],format="%Y-%m-%d"))

#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Date_Added:=as.Date(as.yearmon(Date_Added,format="%b %Y"))]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Date_Added")), value=as.Date(as.yearmon(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Date_Added"))]],format="%b %Y")))

#EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[, Inception_Date:=as.Date(as.yearmon(Inception_Date,format="%b %Y"))]
set(EurekahedgeHF_Excel_aca_full, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full)==c("Inception_Date")), value=as.Date(as.yearmon(EurekahedgeHF_Excel_aca_full[[which(colnames(EurekahedgeHF_Excel_aca_full)==c("Inception_Date"))]],format="%b %Y")))

EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full_drop_cols1a <- count(data.frame(col=gsub("_bin","",colnames(EurekahedgeHF_Excel_aca_full)),stringsAsFactors=FALSE),"col")
EurekahedgeHF_Excel_aca_full_drop_cols1b <- as.vector(EurekahedgeHF_Excel_aca_full_drop_cols1a[EurekahedgeHF_Excel_aca_full_drop_cols1a[,"freq"]>1,"col"])
EurekahedgeHF_Excel_aca_full_drop_cols2 <- c("pull_trim","pull_trim2","")

EurekahedgeHF_Excel_aca_full_trim1 <- EurekahedgeHF_Excel_aca_full[,!(colnames(EurekahedgeHF_Excel_aca_full) %in% c(EurekahedgeHF_Excel_aca_full_drop_cols1b,EurekahedgeHF_Excel_aca_full_drop_cols2))]

rm2(EurekahedgeHF_Excel_aca_full_drop_cols1a,EurekahedgeHF_Excel_aca_full_drop_cols1b,EurekahedgeHF_Excel_aca_full_drop_cols2)

rm2(EurekahedgeHF_Excel_aca_full)

EurekahedgeHF_Excel_aca_full_trim2 <- EurekahedgeHF_Excel_aca_full_trim1[EurekahedgeHF_Excel_aca_full_trim1[,identifier] %in% unique(c(read_stats_ios_f[,identifier], year_sim_ios_all_stacked[,identifier])),]

rm2(EurekahedgeHF_Excel_aca_full_trim1)


###############################################################################
cat("CONVERT PERCENTAGES TO DECIMAL", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full_trim2_cols <- colnames(EurekahedgeHF_Excel_aca_full_trim2)

EurekahedgeHF_Excel_aca_full6b <- EurekahedgeHF_Excel_aca_full_trim2

decimal_cols <- c("Yearly_Ret2","Monthly_Ret","Monthly_Ret2","Annualised_Return","Best_Monthly_Return","Worst_Monthly_Return",
                  "Rise_in_NAV_Since_Inception","Last_3_Months","One_Year_Rolling_Return","Two_Year_Rolling_Return","Five_Year_Rolling_Return",
                  "Annualised_Standard_Deviation","Downside_Deviation","Upside_Deviation","Maximum_Drawdown",
                  "Percentage_of_Positive_Months","VaR_90pct","VaR_95pct","VaR_99pct",
                  "Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin")

#colnames(EurekahedgeHF_Excel_aca_full6b)[which(sapply(EurekahedgeHF_Excel_aca_full6b,class)=="numeric")]

for (i in 1:length(decimal_cols))
{
  #i <- 1
  
  EurekahedgeHF_Excel_aca_full6b[,decimal_cols[i]] <- (EurekahedgeHF_Excel_aca_full6b[,decimal_cols[i]]/100)
  
  #progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(decimal_cols), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 
rm2(i)

EurekahedgeHF_Excel_aca_full6b <- EurekahedgeHF_Excel_aca_full6b[rowSums(is.na(EurekahedgeHF_Excel_aca_full6b[,1:ncol(EurekahedgeHF_Excel_aca_full6b)]))<ncol(EurekahedgeHF_Excel_aca_full6b),]

rm2(EurekahedgeHF_Excel_aca_full_trim2)


###############################################################################
cat("CONVERT AUM TO MILLIONS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full6c <- EurekahedgeHF_Excel_aca_full6b
EurekahedgeHF_Excel_aca_full6c[,"AUM"] <- as.numeric(EurekahedgeHF_Excel_aca_full6c[,"AUM"])*1000000

rm2(EurekahedgeHF_Excel_aca_full6b)


###############################################################################
cat("GET TNA FOR EACH CRSP_FUNDNO", "\n")
###############################################################################
# 
# monthly_tna_full <- merge(mflink1, monthly_tna_ret_nav2, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# # monthly_tna_full <- subset(monthly_tna_full,select=-c(caldt,mnav,mret))
# monthly_tna_full <- monthly_tna_full[,!(colnames(monthly_tna_full) %in% c("caldt","mnav","mret"))]
# 
# monthly_tna_full <- monthly_tna_full[c(identifier,"crsp_fundno","yr","month","mtna")]
# 
# monthly_tna_full <- monthly_tna_full[order(monthly_tna_full[,identifier], 
#                                            monthly_tna_full[,"crsp_fundno"], 
#                                            monthly_tna_full[,"yr"],
#                                            monthly_tna_full[,"month"]),]
# row.names(monthly_tna_full) <- seq(nrow(monthly_tna_full))

EurekahedgeHF_Excel_aca_full6d <- EurekahedgeHF_Excel_aca_full6c


#TRIM AUM
#aum_q <- 0.005
#aum_extrema <- quantile(EurekahedgeHF_Excel_aca_full6d[,"aum"], c(aum_q, 1-aum_q)) 
#EurekahedgeHF_Excel_aca_full6d <- EurekahedgeHF_Excel_aca_full6d[EurekahedgeHF_Excel_aca_full6d[,"aum"]<aum_extrema[2],]

EurekahedgeHF_Excel_aca_full6d[,"AUM"] <- winsorize_top(EurekahedgeHF_Excel_aca_full6d[,"AUM"],q=0.025)

rm2(EurekahedgeHF_Excel_aca_full6c)


###############################################################################
cat("MERGE MONTHLY_TNA_RET_NAV2 AND INDEX RETURNS/FUND VOLATILITY", "\n")
###############################################################################

crspa_msi_trim_cols <- colnames(crspa_msi_trim)

EurekahedgeHF_Excel_aca_full7 <- merge(EurekahedgeHF_Excel_aca_full6d, crspa_msi_trim, 
                                       by.x=c("yr","month"), by.y=c("yr","month"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#EurekahedgeHF_Excel_aca_full7 <- transform(EurekahedgeHF_Excel_aca_full7, mktadjret=monthly_ret-vwretx)
EurekahedgeHF_Excel_aca_full7 <- transform(EurekahedgeHF_Excel_aca_full7, mktadjret=Monthly_Ret-vwretd)

EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[rowSums(is.na(EurekahedgeHF_Excel_aca_full7[,1:ncol(EurekahedgeHF_Excel_aca_full7)]))<ncol(EurekahedgeHF_Excel_aca_full7),]

EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[order(EurekahedgeHF_Excel_aca_full7[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full7[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full7[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full7) <- seq(nrow(EurekahedgeHF_Excel_aca_full7))

rm2(EurekahedgeHF_Excel_aca_full6d,crspa_msi_trim)

#temp <- EurekahedgeHF_Excel_aca_full7[,c("yr","month","Fund_Id","Fund_Name","mktadjret","Monthly_Ret","vwretd")]


###############################################################################
cat("WINSORIZE VALUES OF RETURN", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full7b <- EurekahedgeHF_Excel_aca_full7

monthly_tna_ret_nav2_vars <- c("Monthly_Ret","Monthly_Ret2","Yearly_Ret2","mktadjret")
for (i in 1:length(monthly_tna_ret_nav2_vars))
{
  #i <- 1
  #i <- 2
  EurekahedgeHF_Excel_aca_full7b[,monthly_tna_ret_nav2_vars[i]] <- 
    winsorize_both(EurekahedgeHF_Excel_aca_full7b[,monthly_tna_ret_nav2_vars[i]],q=0.01)
  
} 
rm2(i)
rm2(EurekahedgeHF_Excel_aca_full7)

#temp <- EurekahedgeHF_Excel_aca_full7b[,c("yr","month","Fund_Id","Fund_Name","mktadjret","Monthly_Ret","vwretd")]


###############################################################################
cat("WINSORIZE VALUES OF FEES AND OTHER NUMBERS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full8 <- EurekahedgeHF_Excel_aca_full7b

fund_fees_month_vars <- c("Management_Fee_bin", "Performance_Fee_bin","Other_Fee_bin","Fund_Capacity_USm","Fund_Size_USm","Maximum_Drawdown")
#"Minimum_Investment_Size","Subsequent_Investment_Size"

for (i in 1:length(fund_fees_month_vars))
{
  #i <- 1
  #i <- 2
  EurekahedgeHF_Excel_aca_full8[,fund_fees_month_vars[i]] <- 
    winsorize_both(EurekahedgeHF_Excel_aca_full8[,fund_fees_month_vars[i]],q=0.01)
  
} 
rm2(i)
rm2(EurekahedgeHF_Excel_aca_full7b)

#colMeans(EurekahedgeHF_Excel_aca_full8[,c("mktadjret","Monthly_Ret","vwretd")], na.rm = TRUE)


###############################################################################
cat("ADD ALL DATES", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full8_min_date1 <- min(EurekahedgeHF_Excel_aca_full8[,"date"],na.rm = TRUE)
#EurekahedgeHF_Excel_aca_full8_min_date2 <- min(EurekahedgeHF_Excel_aca_full8[,"Dead_Date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_min_date3 <- min(EurekahedgeHF_Excel_aca_full8[,"Inception_Date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_min_date4 <- min(EurekahedgeHF_Excel_aca_full8[,"Date_Added"],na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_min_date <- min(EurekahedgeHF_Excel_aca_full8_min_date1,
                                              #EurekahedgeHF_Excel_aca_full8_min_date2,
                                              EurekahedgeHF_Excel_aca_full8_min_date3,
                                              EurekahedgeHF_Excel_aca_full8_min_date4,na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_max_date1 <- max(EurekahedgeHF_Excel_aca_full8[,"date"],na.rm = TRUE)
#EurekahedgeHF_Excel_aca_full8_max_date2 <- max(EurekahedgeHF_Excel_aca_full8[,"Dead_Date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_max_date3 <- max(EurekahedgeHF_Excel_aca_full8[,"Inception_Date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_max_date4 <- max(EurekahedgeHF_Excel_aca_full8[,"Date_Added"],na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_max_date <- max(EurekahedgeHF_Excel_aca_full8_max_date1,
                                              #EurekahedgeHF_Excel_aca_full8_max_date2,
                                              EurekahedgeHF_Excel_aca_full8_max_date3,
                                              EurekahedgeHF_Excel_aca_full8_max_date4,na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_all_fund_ids <- unique(EurekahedgeHF_Excel_aca_full8[,c(identifier)])

rm2(EurekahedgeHF_Excel_aca_full8_min_date1,EurekahedgeHF_Excel_aca_full8_max_date1)
#rm2(EurekahedgeHF_Excel_aca_full8_min_date2,EurekahedgeHF_Excel_aca_full8_max_date2)
rm2(EurekahedgeHF_Excel_aca_full8_min_date3,EurekahedgeHF_Excel_aca_full8_max_date3)
rm2(EurekahedgeHF_Excel_aca_full8_min_date4,EurekahedgeHF_Excel_aca_full8_max_date4)


###############################################################################
cat("PREALLOCATE NEW COLUMNS", "\n")
###############################################################################
# 
# age_vars <- c("chgdt","age_m","age_y")
# 
# aum_vars <- c("AUM")
# aum_vars_lagged_count <- 4
# aum_vars_lagged <- unlist(lapply(aum_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=aum_vars_lagged_count))
# 
# aum_vars_log <- c("AUM_log")
# aum_vars_log_lagged_count <- 4
# aum_vars_log_lagged <- unlist(lapply(aum_vars_log,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=aum_vars_log_lagged_count))
# 
# ret_vars <- c("Monthly_Ret","Monthly_Ret2","Yearly_Ret2","mktadjret")
# ret_vars_lagged_count <- 4
# ret_vars_lagged <- unlist(lapply(ret_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=ret_vars_lagged_count))
# 
# ret_sq_vars <- c("Monthly_Ret_sq","Monthly_Ret2_sq","Yearly_Ret2_sq","mktadjret_sq")
# ret_sq_vars_lagged_count <- 4
# ret_sq_vars_lagged <- unlist(lapply(ret_sq_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=ret_sq_vars_lagged_count))
# 
# flow_vars <- c("nflow","pflow")
# flow_vars_lagged_count <- 4
# flow_vars_lagged <- unlist(lapply(flow_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=flow_vars_lagged_count))
# 
# sdflow_vars <- c("sdnet_flow","sdpct_flow")
# sdflow_vars_lagged_count <- 1
# sdflow_vars_lagged <- unlist(lapply(sdflow_vars,function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=sdflow_vars_lagged_count))
# 
# 
# EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols0 <- c(age_vars,aum_vars,aum_vars_lagged,aum_vars_log,aum_vars_log_lagged,ret_vars,ret_vars_lagged,
#                                                              ret_sq_vars,ret_sq_vars_lagged, flow_vars,flow_vars_lagged,sdflow_vars,sdflow_vars_lagged)
# 
# EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols1 <- EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols0[!(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols0 %in% colnames(EurekahedgeHF_Excel_aca_full8))]
# 
# #EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols <- c("yr","month",aum_vars_lagged,ret_vars_lagged,flow_vars,flow_vars_lagged,sdflow_vars,sdflow_vars_lagged)
# EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols <- c("yr","month",EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols1)
# 
# EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- data.frame(expand.grid(identifier=EurekahedgeHF_Excel_aca_full8_all_fund_ids,date=seq(as.Date(EurekahedgeHF_Excel_aca_full8_min_date), as.Date(EurekahedgeHF_Excel_aca_full8_max_date),by="month"),stringsAsFactors=FALSE),
#                                                                 matrix(NA, ncol=length(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols),nrow=1,dimnames=list(c(),EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand_cols)),stringsAsFactors=FALSE)
# colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand)[match("identifier",names(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand))] <- identifier

EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- data.frame(expand.grid(identifier=EurekahedgeHF_Excel_aca_full8_all_fund_ids,date=seq(as.Date(EurekahedgeHF_Excel_aca_full8_min_date), as.Date(EurekahedgeHF_Excel_aca_full8_max_date),by="month"),stringsAsFactors=FALSE),
                                                                matrix(NA, ncol=2,nrow=1,dimnames=list(c(),c("yr","month"))),stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand)[match("identifier",names(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand))] <- identifier


EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[order(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,identifier], 
                                                                                                             EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"date"]),] 
row.names(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand) <- seq(nrow(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand))

rm2(EurekahedgeHF_Excel_aca_full8_min_date,EurekahedgeHF_Excel_aca_full8_max_date,EurekahedgeHF_Excel_aca_full8_all_fund_ids)

EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"yr"] <- year(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"date"])
EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"month"] <- month(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"date"])

EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,!(colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand) %in% "date")]

EurekahedgeHF_Excel_aca_full9 <- merge(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand, EurekahedgeHF_Excel_aca_full8, 
                                       by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                                       all.x=TRUE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"))

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[order(EurekahedgeHF_Excel_aca_full9[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full9[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full9[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full9) <- seq(nrow(EurekahedgeHF_Excel_aca_full9))

rm2(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand,EurekahedgeHF_Excel_aca_full8)


###############################################################################
cat("CREATE SQUARED VARIABLES", "\n")
###############################################################################

#EurekahedgeHF_Excel_aca_full9[,"mktadjret_sq"] <- (EurekahedgeHF_Excel_aca_full9[,"mktadjret_sq"])^2
#EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret_sq"] <- (EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret_sq"])^2
#EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret2_sq"] <- (EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret2_sq"])^2
#EurekahedgeHF_Excel_aca_full9[,"Yearly_Ret2_sq"] <- (EurekahedgeHF_Excel_aca_full9[,"Yearly_Ret2_sq"])^2

EurekahedgeHF_Excel_aca_full9[,"mktadjret_sq"] <- (((1+EurekahedgeHF_Excel_aca_full9[,"mktadjret_sq"])^2)-1)
EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret_sq"] <- (((1+EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret_sq"])^2)-1)
EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret2_sq"] <- (((1+EurekahedgeHF_Excel_aca_full9[,"Monthly_Ret2_sq"])^2)-1)
EurekahedgeHF_Excel_aca_full9[,"Yearly_Ret2_sq"] <- (((1+EurekahedgeHF_Excel_aca_full9[,"Yearly_Ret2_sq"])^2)-1)


###############################################################################
cat("CREATE LAG OF AUM AND RETURNS", "\n")
###############################################################################

# #duplicates1 <- sqldf("SELECT *, count(Fund_ID) as count FROM EurekahedgeHF_Excel_aca_full6 group by Fund_ID,date")
# #duplicates2 <- sqldf("SELECT * FROM duplicates1 where count<>1")
# #data_u <- unique(data_in[,c(group,time)])
# 
# #returns_short <- returns[(returns[,identifier]==5002 | returns[,identifier]==5003) ,]
# 
# lag_vars <- c("AUM","Monthly_Ret","mktadjret")
# lag_count <- 4
# 
# #[,c(identifier,"yr","month","date",lag_vars)]
# 
# EurekahedgeHF_Excel_aca_full9 <- data.frame(EurekahedgeHF_Excel_aca_full8_expand,
#                                             matrix(NA, ncol=length(lag_vars)*lag_count, nrow=nrow(EurekahedgeHF_Excel_aca_full8_expand), 
#                                                    dimnames=list(c(), c(paste(lag_vars[1],"_lag",seq(1,lag_count),sep=""),
#                                                                         paste(lag_vars[2],"_lag",seq(1,lag_count),sep=""),
#                                                                         paste(lag_vars[3],"_lag",seq(1,lag_count),sep="")))),stringsAsFactors=FALSE)
# 

aum_lookup <- data.frame(org_var=aum_vars_lagged,lag_var=aum_vars_lagged,lag_count=aum_vars_lagged,stringsAsFactors=FALSE)
aum_lookup[,"org_var"] <- gsub("_lag.*$","",aum_lookup[,"org_var"])
aum_lookup[,"lag_count"] <- as.integer(gsub(".*_lag","",aum_lookup[,"lag_count"]))

ret_lookup <- data.frame(org_var=ret_vars_lagged,lag_var=ret_vars_lagged,lag_count=ret_vars_lagged,stringsAsFactors=FALSE)
ret_lookup[,"org_var"] <- gsub("_lag.*$","",ret_lookup[,"org_var"])
ret_lookup[,"lag_count"] <- as.integer(gsub(".*_lag","",ret_lookup[,"lag_count"]))

ret_sq_lookup <- data.frame(org_var=ret_sq_vars_lagged,lag_var=ret_sq_vars_lagged,lag_count=ret_sq_vars_lagged,stringsAsFactors=FALSE)
ret_sq_lookup[,"org_var"] <- gsub("_lag.*$","",ret_sq_lookup[,"org_var"])
ret_sq_lookup[,"lag_count"] <- as.integer(gsub(".*_lag","",ret_sq_lookup[,"lag_count"]))

#aum_ret_lookup <- rbind(aum_lookup,ret_lookup)
aum_ret_lookup <- rbindlist(list(aum_lookup,ret_lookup,ret_sq_lookup))
aum_ret_lookup <- as.data.frame(aum_ret_lookup,stringsAsFactors=FALSE)

rm2(aum_lookup,ret_lookup,ret_sq_lookup)

EurekahedgeHF_Excel_aca_full9 <- ddply(.data=EurekahedgeHF_Excel_aca_full9,.variables=identifier,.fun=function(x,lookup){
  
  # x <- EurekahedgeHF_Excel_aca_full9[EurekahedgeHF_Excel_aca_full9[,identifier]==5002,]
  # lookup <- aum_ret_lookup
  
  for (i in 1:nrow(lookup))
  {
    #cat(lookup[i,"lag_var"], "\n")
    
    x[,lookup[i,"lag_var"]] <- shift(x[,lookup[i,"org_var"]],-lookup[i,"lag_count"])
  } 
  return(x)
},lookup=aum_ret_lookup,.progress="text")

rm2(aum_ret_lookup)

# EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,sort(colnames(EurekahedgeHF_Excel_aca_full9))]
# 
# starting_cols <- c(identifier,"Fund_Name","Date_Added","Flagship_bin","Closed_bin","Limited_bin","Dead_bin","Dead_Date","Dead_Reason",
#                    "eurekahedge_id","isin","sedol","valoren","cusip","bloomberg","reuters","date","yr","month",
#                    "aum",paste("aum","_lag",seq(1,lag_count),sep=""),
#                    "monthly_ret",paste("monthly_ret","_lag",seq(1,lag_count),sep=""),                  
#                    "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
#                    "monthly_ret2","yearly_ret")
# 
# all_cols <- colnames(EurekahedgeHF_Excel_aca_full9)
# 
# other_cols <- all_cols[-which(all_cols %in% starting_cols)]
# 
# EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,c(starting_cols,other_cols)]
# 
# EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[rowSums(is.na(EurekahedgeHF_Excel_aca_full9[,1:ncol(EurekahedgeHF_Excel_aca_full9)]))<ncol(EurekahedgeHF_Excel_aca_full9),]
# 
# EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[order(EurekahedgeHF_Excel_aca_full9[,identifier], 
#                                                                      EurekahedgeHF_Excel_aca_full9[,"yr"], 
#                                                                      EurekahedgeHF_Excel_aca_full9[,"month"]),] 
# row.names(EurekahedgeHF_Excel_aca_full9) <- seq(nrow(EurekahedgeHF_Excel_aca_full9))
# 
# rm2(EurekahedgeHF_Excel_aca_full8_expand)
# 
# #EurekahedgeHF_Excel_aca_full9[EurekahedgeHF_Excel_aca_full9[,c("aum")]==0,c("yr","month","fund_id","aum","aum_lag1","monthly_ret")]

###############################################################################
cat("CREATE FLOW AND LAG FLOW", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full10 <- EurekahedgeHF_Excel_aca_full9

EurekahedgeHF_Excel_aca_full10 <- data.table(EurekahedgeHF_Excel_aca_full10)

set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow")), 
    value=EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM"))]]-(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM_lag1"))]]*(1+EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("Monthly_Ret"))]])))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM_lag1"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("Monthly_Ret"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=ifelse(is.infinite(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]))

set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("nflow"))]]/EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM_lag1"))]])
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("AUM_lag1"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=ifelse(is.na(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("Monthly_Ret"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow"))]]))
set(EurekahedgeHF_Excel_aca_full10, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow")), 
    value=ifelse(is.infinite(EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow"))]]),NA,EurekahedgeHF_Excel_aca_full10[[which(colnames(EurekahedgeHF_Excel_aca_full10)==c("pflow"))]]))

EurekahedgeHF_Excel_aca_full10 <- as.data.frame(EurekahedgeHF_Excel_aca_full10,stringsAsFactors=FALSE)


flow_lookup <- data.frame(org_var=flow_vars_lagged,lag_var=flow_vars_lagged,lag_count=flow_vars_lagged,stringsAsFactors=FALSE)
flow_lookup[,"org_var"] <- gsub("_lag.*$","",flow_lookup[,"org_var"])
flow_lookup[,"lag_count"] <- as.integer(gsub(".*_lag","",flow_lookup[,"lag_count"]))

EurekahedgeHF_Excel_aca_full10 <- ddply(.data=EurekahedgeHF_Excel_aca_full10,.variables=identifier,.fun=function(x,lookup){
  
  # x <- EurekahedgeHF_Excel_aca_full10[EurekahedgeHF_Excel_aca_full10[,identifier]==5002,]
  # lookup <- flow_lookup
  
  for (i in 1:nrow(lookup))
  {
    x[,lookup[i,"lag_var"]] <- shift(x[,lookup[i,"org_var"]],-lookup[i,"lag_count"])
  } 
  return(x)
},lookup=flow_lookup,.progress="text")

rm2(flow_lookup)
rm2(EurekahedgeHF_Excel_aca_full9)


###############################################################################
cat("COMPUTE ANNUAL FUND FLOW VOLATILITY", "\n")
###############################################################################

# annual_flow_trim <- EurekahedgeHF_Excel_aca_full10[,c(identifier,"yr","nflow","pflow")]
# 
# fund_aflow_volatility <- ddply(annual_flow_trim, c(identifier,"yr"), summarize, 
#                                sdnet_flow=sd(nflow, na.rm = TRUE),sdpct_flow=sd(pflow, na.rm = TRUE))
# 
# fund_aflow_volatility_full <- data.frame(fund_aflow_volatility,
#                                          sdnet_flow_lag1=shift(fund_aflow_volatility$sdnet_flow,-1),
#                                          sdpct_flow_lag1=shift(fund_aflow_volatility$sdpct_flow,-1),stringsAsFactors=FALSE)
# 
# EurekahedgeHF_Excel_aca_full11 <- merge(EurekahedgeHF_Excel_aca_full10, fund_aflow_volatility_full, 
#                                         by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
#                                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# rm2(annual_flow_trim,fund_aflow_volatility,EurekahedgeHF_Excel_aca_full10)

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full10
EurekahedgeHF_Excel_aca_full11 <- data.table(EurekahedgeHF_Excel_aca_full11)
EurekahedgeHF_Excel_aca_full11[,c("sdnet_flow","sdpct_flow"):=list(NULL,NULL),by=NULL]
EurekahedgeHF_Excel_aca_full11 <- data.table(EurekahedgeHF_Excel_aca_full11)[,c("sdnet_flow","sdpct_flow"):=list(sd(nflow, na.rm = TRUE),sd(pflow, na.rm = TRUE)), by=c(identifier,"yr")]
EurekahedgeHF_Excel_aca_full11 <- as.data.frame(EurekahedgeHF_Excel_aca_full11,stringsAsFactors=FALSE)

sdflow_lookup <- data.frame(org_var=sdflow_vars_lagged,lag_var=sdflow_vars_lagged,lag_count=sdflow_vars_lagged,stringsAsFactors=FALSE)
sdflow_lookup[,"org_var"] <- gsub("_lag.*$","",sdflow_lookup[,"org_var"])
sdflow_lookup[,"lag_count"] <- as.integer(gsub(".*_lag","",sdflow_lookup[,"lag_count"]))

EurekahedgeHF_Excel_aca_full11 <- ddply(.data=EurekahedgeHF_Excel_aca_full11,.variables=identifier,.fun=function(x,lookup){
  
  # x <- EurekahedgeHF_Excel_aca_full11[EurekahedgeHF_Excel_aca_full11[,identifier]==5002,]
  # lookup <- sdflow_lookup
  for (i in 1:nrow(lookup))
  {
    x[,lookup[i,"lag_var"]] <- shift(x[,lookup[i,"org_var"]],-lookup[i,"lag_count"])
  } 
  return(x)
},lookup=sdflow_lookup,.progress="text")

rm2(sdflow_lookup)
rm2(EurekahedgeHF_Excel_aca_full10)


###############################################################################
cat("REORDER COLUMNS", "\n")
###############################################################################

#str(EurekahedgeHF_Excel_aca_full11,list.len=999)
#EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[,sort(colnames(EurekahedgeHF_Excel_aca_full11))]
#EurekahedgeHF_Excel_aca_full11_backup <- EurekahedgeHF_Excel_aca_full11

EurekahedgeHF_Excel_aca_full11_cols_id <- colnames(EurekahedgeHF_Excel_aca_full11)[(colnames(EurekahedgeHF_Excel_aca_full11) %in% c(sample_data_all_cols,"date","month"))]
EurekahedgeHF_Excel_aca_full11_cols_id <- EurekahedgeHF_Excel_aca_full11_cols_id[order(match(EurekahedgeHF_Excel_aca_full11_cols_id,sample_data_all_cols))]
EurekahedgeHF_Excel_aca_full11_cols_id <- c(EurekahedgeHF_Excel_aca_full11_cols_id[grepl("Dead",EurekahedgeHF_Excel_aca_full11_cols_id)],
                                            EurekahedgeHF_Excel_aca_full11_cols_id[!grepl("Dead",EurekahedgeHF_Excel_aca_full11_cols_id)])
EurekahedgeHF_Excel_aca_full11_cols_id <- c(identifier,"Fund_Name","date","yr","month",
                                            EurekahedgeHF_Excel_aca_full11_cols_id[!(EurekahedgeHF_Excel_aca_full11_cols_id %in% c(identifier,"Fund_Name","date","yr","month"))])

EurekahedgeHF_Excel_aca_full11_cols_nonid <- colnames(EurekahedgeHF_Excel_aca_full11)[!(colnames(EurekahedgeHF_Excel_aca_full11) %in% EurekahedgeHF_Excel_aca_full11_cols_id)]


EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- data.frame(non_id_cols=EurekahedgeHF_Excel_aca_full11_cols_nonid,
                                                              nonexpand_cols=NA,alpha_order=NA,org_order=NA,org_order_grouped=NA,sort_order=NA,new_order=NA,
                                                              final_nonfirst_cols=NA,final_order=NA,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"nonexpand_cols"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"] %in% c(EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols0,crspa_msi_trim_cols),0,1)

EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[order(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"]),]
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"alpha_order"] <- 1
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"alpha_order"] <- cumsum(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"alpha_order"])

EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[order(match(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"],EurekahedgeHF_Excel_aca_full_trim2_cols)),]
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"] %in% EurekahedgeHF_Excel_aca_full_trim2_cols,1,0)
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"] <- cumsum(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"])
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"] %in% EurekahedgeHF_Excel_aca_full_trim2_cols,EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"],0)

EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new1 <- alply(.data=EurekahedgeHF_Excel_aca_full11_cols_nonid_order, .margins=1, .fun = function(x,colnames){
  
  out1 <- sort(c(x[,"non_id_cols"],colnames[grep(paste("^",x[,"non_id_cols"],"_",sep=""),colnames,invert=FALSE)]))
  out2 <- colnames[!(colnames %in% out1)]
  return(data.frame(non_id_cols=c(out1),stringsAsFactors=FALSE))
},colnames=as.vector(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"]), .expand = FALSE,.progress = "none")

EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new2 <- rbindlist(EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new1,fill=TRUE)
EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new3 <- unique(data.frame(EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new2,stringsAsFactors=FALSE))

EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[order(match(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"],EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new3[,"non_id_cols"])),]
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"sort_order"] <- 1
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"sort_order"] <- cumsum(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"sort_order"])

EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"] %in% EurekahedgeHF_Excel_aca_full_trim2_cols,1,0)
EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"] <- cumsum(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"])
#EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order"]==0,0,EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"])

EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[order(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"nonexpand_cols"],
                                                                                                         EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"org_order_grouped"],
                                                                                                         EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"sort_order"]),]

EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"new_order"] <- seq(1,nrow(EurekahedgeHF_Excel_aca_full11_cols_nonid_order))

EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"final_nonfirst_cols"] <- ifelse(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"] %in% c(age_vars,crspa_msi_trim_cols),0,1)

EurekahedgeHF_Excel_aca_full11_cols_nonid_order <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[order(EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"final_nonfirst_cols"],
                                                                                                         EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"new_order"]),]

EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"final_order"] <- seq(1,nrow(EurekahedgeHF_Excel_aca_full11_cols_nonid_order))

EurekahedgeHF_Excel_aca_full11_cols_nonid <- EurekahedgeHF_Excel_aca_full11_cols_nonid_order[,"non_id_cols"]

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[,c(EurekahedgeHF_Excel_aca_full11_cols_id,EurekahedgeHF_Excel_aca_full11_cols_nonid)]

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[rowSums(is.na(EurekahedgeHF_Excel_aca_full11[,1:ncol(EurekahedgeHF_Excel_aca_full11)]))<ncol(EurekahedgeHF_Excel_aca_full11),]

rm2(EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new1,EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new2,EurekahedgeHF_Excel_aca_full11_cols_nonid_order_new3)
rm2(EurekahedgeHF_Excel_aca_full11_cols_nonid_order)
rm2(EurekahedgeHF_Excel_aca_full11_cols_id,EurekahedgeHF_Excel_aca_full11_cols_nonid)


###############################################################################
cat("FIND AGE FOR EVERY WFICN", "\n")
###############################################################################

#fund_age0 <- EurekahedgeHF_Excel_aca_full11[!is.na(EurekahedgeHF_Excel_aca_full11[,c("Inception_Date")]),c(identifier,"yr","month","Inception_Date")]
fund_age0 <- EurekahedgeHF_Excel_aca_full11[,c(identifier,"yr","month","Inception_Date")]

fund_age <- data.table(fund_age0)[,list(chgdt=min(Inception_Date,na.rm=TRUE)),by=identifier]
fund_age <- data.frame(fund_age,stringsAsFactors=FALSE)

fund_age_trim <- data.table(fund_age)[,list(chgdt=min(chgdt,na.rm=TRUE)),by=identifier]
fund_age_trim <- as.data.frame(fund_age_trim,stringsAsFactors=FALSE)

rm2(fund_age0,fund_age)

fund_age_trim2 <- fund_age_trim
fund_age_trim2[,c("chgdt")] <- as.IDate(fund_age_trim2[,c("chgdt")],origin = "1970-01-01",na.rm=TRUE)

fund_age_trim3 <- fund_age_trim2
fund_age_trim3 <- fund_age_trim3[!(is.na(fund_age_trim3[,identifier])),]
fund_age_trim3 <- fund_age_trim3[!(is.na(fund_age_trim3[,"chgdt"])),]
row.names(fund_age_trim3) <- seq(nrow(fund_age_trim3))

rm2(fund_age_trim,fund_age_trim2)

#fund_age_trim4 <- data.frame(fund_age_trim3,today=today(),stringsAsFactors=FALSE)
#fund_age_trim4[,"chgdt"] <- as.character(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"chgdt"] <- as.Date(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"today"] <- as.Date(fund_age_trim4[,"today"])
#fund_age_trim4[,"chgdt"] <- as.IDate(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"today"] <- as.IDate(fund_age_trim4[,"today"])
#fund_age_trim4[,"chgdt"] <- as.POSIXct(fund_age_trim4[,"chgdt"], format="%Y-%m-%d", origin = "1970-01-01")
#fund_age_trim4[,"today"] <- as.POSIXct(fund_age_trim4[,"today"], format="%Y-%m-%d", origin = "1970-01-01")


#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(chgdt,today(),"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                  by=eval(paste(identifier,",chgdt",sep=""))]
# fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(chgdt,
#                                                           today_temp,
#                                                           "days");
#                                                     list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                   by=eval(paste(identifier,",chgdt",sep=""))]

#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(from=chgdt,to=Sys.Date(),by="days")},
#                                 by=list(fund_id,chgdt)]

fund_age_month_temp <- data.table(fund_age_trim3)
fund_age_month_temp <- fund_age_month_temp[, seq(as.Date(chgdt, origin="1970-01-01"), Sys.Date(), by="days"),by=eval(paste(identifier,",chgdt",sep=""))]
fund_age_month_temp <- fund_age_month_temp[, c("yr","month"):=list(NA,NA),by=NULL]
setnames(fund_age_month_temp, c(colnames(fund_age_trim3),"data_expand","yr","month"))

#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(from=chgdt,
#                                                          to=today,
#                                                          by="days");
#                                                    list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                  by=eval(paste(identifier,",chgdt",sep=""))]



#test <- seq(as.POSIXct(fund_age_trim4[1,"chgdt"], format="%Y-%m-%d"),
#            as.POSIXct(today(), format="%Y-%m-%d"),
#            "days")
#test <- as.data.frame(test,stringsAsFactors=FALSE)  

rm2(fund_age_trim3)

set(fund_age_month_temp, i=NULL, j=which(colnames(fund_age_month_temp)==c("yr")), value=year(fund_age_month_temp[[which(colnames(fund_age_month_temp)==c("data_expand"))]]))
set(fund_age_month_temp, i=NULL, j=which(colnames(fund_age_month_temp)==c("month")), value=month(fund_age_month_temp[[which(colnames(fund_age_month_temp)==c("data_expand"))]]))
set(fund_age_month_temp, i=NULL, j=which(colnames(fund_age_month_temp)==c("data_expand")), value=NULL)

#fund_age_month_temp <- as.data.frame(fund_age_month_temp,stringsAsFactors=FALSE)
#fund_age_month_temp <- data.frame(fund_age_month_temp,yr=year(fund_age_month_temp[,"data_expand"]),month=month(fund_age_month_temp[,"data_expand"]),stringsAsFactors=FALSE)
#fund_age_month_temp2 <- fund_age_month_temp[,!(colnames(fund_age_month_temp) %in% "data_expand")]

setkey(fund_age_month_temp,NULL)
fund_age_month_temp <- unique(fund_age_month_temp)

#fund_age_month_temp2 <- as.data.frame(fund_age_month_temp,stringsAsFactors=FALSE)
#rm2(fund_age_month_temp)

#fund_age_month_temp2 <- unique(fund_age_month_temp2, incomparables=FALSE)
#row.names(fund_age_month_temp2) <- seq(nrow(fund_age_month_temp2))

#fund_age_month_temp2 <- transform(fund_age_month_temp2, chgdt=as.character(chgdt))

#fund_age_month_temp2 <- fund_age_month_temp[, chgdt:=as.character(chgdt)]
fund_age_month_temp2 <- fund_age_month_temp

rm2(fund_age_month_temp)

set(fund_age_month_temp2, i=NULL, j=which(colnames(fund_age_month_temp2)==c("chgdt")), value=as.character(fund_age_month_temp2[[which(colnames(fund_age_month_temp2)==c("chgdt"))]]))

fund_age_month <- fund_age_month_temp2[,list(yr,month,chgdt,age_m=(seq(1,.N)-1),age_y=((seq(1,.N)-1)/12)),by=identifier]
fund_age_month <- as.data.frame(fund_age_month,stringsAsFactors=FALSE)

rm2(fund_age_month_temp2)

EurekahedgeHF_Excel_aca_full12 <- merge(EurekahedgeHF_Excel_aca_full11[,!(colnames(EurekahedgeHF_Excel_aca_full11) %in% c("chgdt","age_m","age_y"))],fund_age_month, 
                                        by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

EurekahedgeHF_Excel_aca_full12 <- EurekahedgeHF_Excel_aca_full12[,colnames(EurekahedgeHF_Excel_aca_full11)]

rm2(fund_age_month,EurekahedgeHF_Excel_aca_full11)

EurekahedgeHF_Excel_aca_full12 <- EurekahedgeHF_Excel_aca_full12[order(EurekahedgeHF_Excel_aca_full12[,identifier],
                                                                       EurekahedgeHF_Excel_aca_full12[,"yr"],
                                                                       EurekahedgeHF_Excel_aca_full12[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full12) <- seq(nrow(EurekahedgeHF_Excel_aca_full12))



# age_vars <- c("age_m","age_y")
# for (i in 1:length(age_vars))
# {
#   #i <- 1
#   #i <- 2
#   EurekahedgeHF_Excel_aca_full12[,age_vars[i]] <- 
#     winsorize_both(EurekahedgeHF_Excel_aca_full12[,age_vars[i]],q=0.01)
#   
# } 


#colMeans(EurekahedgeHF_Excel_aca_full12[,c("nflow","pflow")], na.rm = TRUE)

# ###############################################################################
# cat("MERGE AGGREGATE DATA", "\n")
# ###############################################################################
# 
# monthly_agg2 <- merge(agg_mtna, agg_retail_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_open_to_inv_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_inst_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_m_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_vau_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_dead_flag_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, fund_age_month, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# rm2(agg_retail_fund_dv,agg_open_to_inv_dv,agg_inst_fund_dv,agg_m_fund_dv,agg_vau_fund_dv,agg_dead_flag_dv)
# 
# #monthly_agg2 <- subset(monthly_agg2,select=-c(mtna_agg))
# monthly_agg2 <- monthly_agg2[,!(colnames(monthly_agg2) %in% "mtna_agg")]

# ###############################################################################
# cat("MERGE MONTHLY_AGG_LAGS_FULL AND MONTHLY_AGG2", "\n")
# ###############################################################################
# 
# monthly_agg_full <- merge(monthly_agg_lags_full, monthly_agg2, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full <- monthly_agg_full[order(monthly_agg_full[,identifier],
#                                            monthly_agg_full[,"yr"],
#                                            monthly_agg_full[,"month"]),] 
# 
# #rm2(monthly_agg_lags_full,monthly_agg2)
# 
# 
# ###############################################################################
# cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN FUND_STYLE_MONTH", "\n")
# ###############################################################################
# 
# fund_style_full <- merge(mflink1,fund_style_month, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                          all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# # fund_style_full_trim <- subset(fund_style_full,select=-c(crsp_fundno,begdt,enddt,lipper_class_name,accrual_fund,sales_restrict))
# fund_style_full_trim <- fund_style_full[,!(colnames(fund_style_full) %in% c("crsp_fundno","begdt","enddt","lipper_class_name","accrual_fund","sales_restrict"))]
# 
# fund_style_full_trim <- fund_style_full_trim[c(identifier,"yr","month","crsp_obj_cd","lipper_class","lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")]
# 
# fund_style_full_trim <- fund_style_full_trim[order(fund_style_full_trim[,identifier], fund_style_full_trim[,"yr"], fund_style_full_trim[,"month"]),]
# 
# fund_style_full_trim <- unique(fund_style_full_trim, incomparables=FALSE)
# 
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"crsp_obj_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_class"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_name"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_asset_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_tax_cd"])),]
# 
# query_remove_styles <- "select distinct   wficn,yr, month, Count(wficn) Count
# from              fund_style_full_trim 
# group by          wficn, yr, month
# order by          wficn, yr, month"
# remove_styles <- sqldf(query_remove_styles)
# 
# remove_styles2 <- remove_styles[!(remove_styles[,"Count"]==1),]
# 
# colnames(remove_styles2)[match("Count",names(remove_styles2))] <- "Remove"
# remove_styles2[,"Remove"] <- 1
# 
# fund_style_full_trim2 <- merge(fund_style_full_trim,remove_styles2, 
#                                by.x=c(identifier, "yr", "month"), by.y=c(identifier, "yr", "month"), 
#                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_style_full_trim3 <- fund_style_full_trim2[is.na(fund_style_full_trim2[,"Remove"]),]
# # fund_style_full_trim3 <- subset(fund_style_full_trim3,select=-c(Remove))
# fund_style_full_trim3 <- fund_style_full_trim3[,!(colnames(fund_style_full_trim3) %in% "Remove")]
# 
# for(i in which(sapply(fund_style_full_trim3,class)=="character"))
# {
#   fund_style_full_trim3[[i]] = trim(fund_style_full_trim3[[i]])
# }
# for (i in 1:ncol(fund_style_full_trim3))
# {
#   #i <- 1
#   fund_style_full_trim3[,i] <- unknownToNA(fund_style_full_trim3[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#   fund_style_full_trim3[,i] <- ifelse(is.na(fund_style_full_trim3[,i]),NA, fund_style_full_trim3[,i])
# } 
# 
# #rm2(fund_style_full,fund_style_full_trim,query_remove_styles,remove_styles,remove_styles2,fund_style_full_trim2)
# 
# 
# ###############################################################################
# cat("MERGE FUND_STYLE_MONTH INTO MONTHLY_AGG_LAGS_FULL", "\n")
# ###############################################################################
# 
# monthly_agg_full2 <- merge(monthly_agg_full, fund_style_full_trim3, 
#                            by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full2 <- monthly_agg_full2[order(monthly_agg_full2[,identifier],
#                                              monthly_agg_full2[,"yr"],
#                                              monthly_agg_full2[,"month"]),] 
# 
# #rm2(monthly_agg_full,fund_style_full_trim3)
# 
# 
# ###############################################################################
# cat("TRIM YEARS IN MONTHLY_AGG_FULL", "\n")
# ###############################################################################
# 
# monthly_agg_full2_trim <- monthly_agg_full2[(monthly_agg_full2[,"yr"]>=1999 & monthly_agg_full2[,"yr"]<=2012),]



# ###############################################################################
# cat("CREATE LOG ASSETS, SQUARE RETURNS, AND STLYE DUMMIES", "\n")
# ###############################################################################

#EurekahedgeHF_Excel_aca_full13 <- data.frame(EurekahedgeHF_Excel_aca_full12,
#                                             log_AUM=NA,log_AUM_lag1=NA,log_AUM_lag2=NA,log_AUM_lag3=NA,log_AUM_lag4=NA,
#                                             mktadjret_sq=NA,mktadjret_lag1_sq=NA,mktadjret_lag2_sq=NA,mktadjret_lag3_sq=NA,mktadjret_lag4_sq=NA)

EurekahedgeHF_Excel_aca_full13 <- EurekahedgeHF_Excel_aca_full12

EurekahedgeHF_Excel_aca_full13[,"AUM_log"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"AUM"]))
EurekahedgeHF_Excel_aca_full13[,"AUM_log_lag1"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"AUM_lag1"]))
EurekahedgeHF_Excel_aca_full13[,"AUM_log_lag2"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"AUM_lag2"]))
EurekahedgeHF_Excel_aca_full13[,"AUM_log_lag3"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"AUM_lag3"]))
EurekahedgeHF_Excel_aca_full13[,"AUM_log_lag4"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"AUM_lag4"]))

EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq_lag1"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag1"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq_lag2"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag2"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq_lag3"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag3"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq_lag4"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag4"])^2

#monthly_data_all2[,"avg_mkt_exp"] <- (monthly_data_all2[,"actual_12b1_agg"]/monthly_data_all2[,"exp_ratio_agg"])
#monthly_data_all2[,"avg_mkt_explag1"] <- (monthly_data_all2[,"actual_12b1_agglag1"]/monthly_data_all2[,"exp_ratio_agglag1"])
#monthly_data_all2[,"avg_mkt_explag2"] <- (monthly_data_all2[,"actual_12b1_agglag2"]/monthly_data_all2[,"exp_ratio_agglag2"])
#monthly_data_all2[,"avg_mkt_explag3"] <- (monthly_data_all2[,"actual_12b1_agglag3"]/monthly_data_all2[,"exp_ratio_agglag3"])
#monthly_data_all2[,"avg_mkt_explag4"] <- (monthly_data_all2[,"actual_12b1_agglag4"]/monthly_data_all2[,"exp_ratio_agglag4"])

#lipper_class_u <- unique(monthly_data_all2[,"lipper_class"],incomparables=FALSE)
#lipper_obj_cd_u <- unique(monthly_data_all2[,"lipper_obj_cd"],incomparables=FALSE)
#lipper_asset_cd_u <- unique(monthly_data_all2[,"lipper_asset_cd"],incomparables=FALSE)
#lipper_tax_cd_u <- unique(monthly_data_all2[,"lipper_tax_cd"],incomparables=FALSE)
# 
# monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="EQ", 1, monthly_data_all2$lipper_asset_cd_eq_dv)
# monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="TX" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="MB"), 0, monthly_data_all2$lipper_asset_cd_eq_dv)
# 
# monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="TX", 1, monthly_data_all2$lipper_asset_cd_tx_dv)
# monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="MB" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="EQ"), 0, monthly_data_all2$lipper_asset_cd_tx_dv)
# 
# monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="MB", 1, monthly_data_all2$lipper_asset_cd_mb_dv)
# monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="EQ" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="TX"), 0, monthly_data_all2$lipper_asset_cd_mb_dv)
# 
# monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 1, monthly_data_all2$lipper_tax_cd_t_dv)
# monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 0, monthly_data_all2$lipper_tax_cd_t_dv)
# 
# monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 1, monthly_data_all2$lipper_tax_cd_te_dv)
# monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 0, monthly_data_all2$lipper_tax_cd_te_dv)

rm2(EurekahedgeHF_Excel_aca_full12)


# ###############################################################################
# cat("MERGE FUND_NAMES AND MDM_DATA_TRIM", "\n")
# ###############################################################################
# 
# fund_names_msd <- merge(fund_names, Mdmf_data_trim, 
#                         by.x=c("ncusip"), by.y=c("CUSIP"), all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_names_msd <- fund_names_msd[order(fund_names_msd[,"crsp_fundno"], 
#                                        fund_names_msd[,"chgdt"]),] 
# 
# 
# fund_names_msd2 <- merge(fund_names_msd, mflink1, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_names_msd2 <- fund_names_msd2[c(identifier,"crsp_fundno","Broad_Cat_Group","Global_Cat","MS_Cat","MS_Inst_Cat",
#                                      "MS_Rating_Overall", "US_Broad_Asset_Class","Equity_Style_Box_Long","MS_Anal_Rating",
#                                      "Firm_Name","Branding_Name","Prospectus_Objective")]
# 
# fund_names_msd2 <- fund_names_msd2[order(fund_names_msd2[,identifier], fund_names_msd2[,"crsp_fundno"]),]
# fund_names_msd2 <- unique(fund_names_msd2,comparables=FALSE)
# 
# #fund_names_msd3 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Firm_Name,Branding_Name,Global_Cat,
# #                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# fund_names_msd3 <- fund_names_msd2[,!(colnames(fund_names_msd2) %in% c("crsp_fundno","MS_Rating_Overall","Firm_Name","Branding_Name","Global_Cat",
#                                                                       "MS_Cat","MS_Inst_Cat","US_Broad_Asset_Class","MS_Anal_Rating","Prospectus_Objective"))]
# 
# #fund_names_msd3[fund_names_msd3[,identifier]==103259,"MS_Anal_Rating"] <- "Silver" 
# #fund_names_msd3[fund_names_msd3[,identifier]==103380,"MS_Anal_Rating"] <- "Bronze" 
# #fund_names_msd3[fund_names_msd3[,identifier]==400001,"MS_Anal_Rating"] <- "Bronze" 
# 
# fund_names_msd3 <- unique(fund_names_msd3,comparables=FALSE)
# 
# fund_names_msd3_row_count <- as.data.frame(data.table(fund_names_msd3)[, list(count=.N),by=identifier],stringsAsFactors=FALSE)
# fund_names_msd3_bad <- fund_names_msd3_row_count[fund_names_msd3_row_count[,"count"]>1,]
# 
# fund_names_msd4 <- fund_names_msd3[!(fund_names_msd3[,identifier] %in% fund_names_msd3_bad[,identifier]),]
# 
# #rm2(fund_names_msd,fund_names_msd2,fund_names_msd3,fund_names_msd3_row_count,fund_names_msd3_bad)
# 
# #fund_names_msd5 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Broad_Cat_Group,Equity_Style_Box_Long,Global_Cat,
# #                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# #fund_names_msd5 <- subset(fund_names_msd5,select=-c(Firm_Name))
# fund_names_msd5 <- fund_names_msd2[,!(colnames(fund_names_msd2) %in% c("crsp_fundno","MS_Rating_Overall","Broad_Cat_Group","Equity_Style_Box_Long","Global_Cat",
#                                                     "MS_Cat","MS_Inst_Cat","US_Broad_Asset_Class","MS_Anal_Rating","Prospectus_Objective"))]
# fund_names_msd5 <- fund_names_msd5[,!(colnames(fund_names_msd5) %in% c("Firm_Name"))]

# fund_names_msd5 <- unique(fund_names_msd5,comparables=FALSE)
# 
# fund_names_msd5_row_count <- as.data.frame(data.table(fund_names_msd5)[, list(count=.N),by=identifier],stringsAsFactors=FALSE)
# fund_names_msd5_bad <- fund_names_msd5_row_count[fund_names_msd5_row_count[,"count"]>1,]
# 
# fund_names_msd6 <- fund_names_msd5[!(fund_names_msd5[,identifier] %in% fund_names_msd5_bad[,identifier]),]
# 
# #rm2(fund_names_msd5,fund_names_msd5_row_count,fund_names_msd5_bad)
# 
# 
# #test <- as.data.frame(unique(fund_names_msd6[,"Branding_Name"],comparables=FALSE),stringsAsFactors=FALSE)
# #colnames(test) <- "Branding_Name"
# #test <- test[order(test[,"Branding_Name"]),]
# 
# 
# ###############################################################################
# cat("ADD DVS FOR STYLE AND RATING", "\n")
# ###############################################################################
# 
# #aa_Broad_Cat_Group_u <- as.data.frame(unique(fund_names_msd4[,"Broad_Cat_Group"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_Equity_Style_Box_Long_u <- as.data.frame(unique(fund_names_msd4[,"Equity_Style_Box_Long"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_MS_Anal_Rating_u <- as.data.frame(unique(fund_names_msd4[,"MS_Anal_Rating"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_Prospectus_Objective_u <- as.data.frame(unique(fund_names_msd4[,"Prospectus_Objective"],comparables=FALSE),stringsAsFactors=FALSE)
# 
# fund_names_msd_dv <- data.frame(fund_names_msd4,
#                                 Broad_Cat_Group_eq_dv=NA,Broad_Cat_Group_al_dv=NA,Broad_Cat_Group_fi_dv=NA,Broad_Cat_Group_alt_dv=NA,Broad_Cat_Group_tp_dv=NA,
#                                 Equity_Style_Box_Long_lg_dv=NA,Equity_Style_Box_Long_lv_dv=NA,Equity_Style_Box_Long_lb_dv=NA,
#                                 Equity_Style_Box_Long_mg_dv=NA,Equity_Style_Box_Long_mv_dv=NA,Equity_Style_Box_Long_mb_dv=NA,
#                                 Equity_Style_Box_Long_sg_dv=NA,Equity_Style_Box_Long_sv_dv=NA,Equity_Style_Box_Long_sb_dv=NA)
# #MS_Anal_Rating_gold_dv=NA,MS_Anal_Rating_silver_dv=NA,MS_Anal_Rating_bronze_dv=NA,
# #MS_Anal_Rating_neutral_dv=NA,MS_Anal_Rating_negative_dv=NA)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="EQUITY", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_eq_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALLOCATION", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_al_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="FIXED INCOME", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_fi_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALTERNATIVE", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_alt_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="TAX PREFERRED", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_tp_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lb_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mb_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sb_dv)
# 
# ####CANNOT USE THESE BECAUSE THERE AREN"T ACROSS TIME
# # fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="GOLD", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_gold_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="SILVER", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_silver_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="BRONZE", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_bronze_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEUTRAL", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_neutral_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEGATIVE", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_negative_dv)
# # 
# 
# monthly_data_all3 <- merge(monthly_data_all2, fund_names_msd_dv, 
#                            by.x=c(identifier), by.y=c(identifier), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_data_all4 <- data.frame(monthly_data_all3,FundRetMktNeg=NA,stringsAsFactors=FALSE)
# 
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(monthly_data_all4$mktadjret_agg<0, monthly_data_all4$mktadjret_agg, 0)
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(is.na(monthly_data_all4$mktadjret_agg), NA, monthly_data_all4$FundRetMktNeg)
# 
# monthly_data_all4 <- monthly_data_all4[order(monthly_data_all4[,identifier],
#                                              monthly_data_all4[,"yr"],
#                                              monthly_data_all4[,"month"]),]
# 
# #rm2(fund_names_msd_dv)


# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY BROAD_CAT_GROUP AND YEAR", "\n")
# ###############################################################################

#aa <- EurekahedgeHF_Excel_aca_full13[EurekahedgeHF_Excel_aca_full13[,identifier]==6131,c(identifier,"Fund_Name","yr","month","pflow","aum","aum_lag1","monthly_ret")]
#cc <- bb[bb[,identifier]==6131,c(identifier,"Fund_Name","yr","month","pflow","aum","aum_lag1","monthly_ret")]

EurekahedgeHF_Excel_aca_full14 <- EurekahedgeHF_Excel_aca_full13
#EurekahedgeHF_Excel_aca_full14 <- EurekahedgeHF_Excel_aca_full13[!is.na(EurekahedgeHF_Excel_aca_full13[,"Fund_Name"]),]

#similarity_db_tables <- ListTables(similarity_db)
#similarity_db_fields <- ListFields(similarity_db)

group_column <- "Primary_Investment_Strategy_combcol"

## Create hash table
EurekahedgeHF_Excel_aca_full14[,group_column] <- gsub(" {2,}", " ",EurekahedgeHF_Excel_aca_full14[,group_column], perl=TRUE)
EurekahedgeHF_Excel_aca_full14[,group_column] <- gsub("^\\s+|\\s+$", "",EurekahedgeHF_Excel_aca_full14[,group_column], perl=TRUE)
EurekahedgeHF_Excel_aca_full14[,group_column] <- toupper(EurekahedgeHF_Excel_aca_full14[,group_column])

group_column_u <- unique(EurekahedgeHF_Excel_aca_full14[,group_column])
group_column_u <- group_column_u[!is.na(group_column_u)]

hash_patterns1 <- list(id=1,pattern=c("([[:punct:]|/])"," {2,}","^\\s+|\\s+$"),replacement=c(" \\1 "," ",""))
hash_patterns2 <- list(id=2,pattern=c("[[:punct:]]"," {2,}","^\\s+|\\s+$"),replacement=c(" "," ",""))
#hash_patterns3 <- list(id=3,pattern=c("([[:punct:]|/])","(^.*$)","^.*?(?=(\\s+DEBT\\s+)).*$"," {2,}","^\\s+|\\s+$"),replacement=c(" \\1 "," \\1 "," \\1 "," ",""))
hash_patterns3 <- list(id=3,pattern=c("([[:punct:]|/])","(^.*$)","^.*(\\s+DEBT\\s+).*$"," {2,}","^\\s+|\\s+$"),replacement=c(" \\1 "," \\1 "," \\1 "," ",""))
#hash_patterns4 <- list(id=4,pattern=c("([[:punct:]|/])","(^.*$)","^.*?(?=(\\s+VALUE\\s+)).*$"," {2,}","^\\s+|\\s+$"),replacement=c(" \\1 "," \\1 "," \\1 "," ",""))
hash_patterns4 <- list(id=4,pattern=c("([[:punct:]|/])","(^.*$)","^.*(\\s+VALUE\\s+).*$"," {2,}","^\\s+|\\s+$"),replacement=c(" \\1 "," \\1 "," \\1 "," ",""))

hash_patterns <- ldply(.data=list(hash_patterns1,hash_patterns2,hash_patterns3,hash_patterns4), .fun = function(x){return(data.frame(g_id=as.vector(unlist(x["id"])),p_id=NA,pattern=as.vector(unlist(x["pattern"])),replacement=as.vector(unlist(x["replacement"])),stringsAsFactors=FALSE))})
hash_patterns_expand0 <- ddply(.data=hash_patterns, .variables=c("g_id"), .fun=function(x){x[,"p_id"] <- seq(1,nrow(x)) ; return(x)})
hash_patterns_expand1 <- ldply(.data=unique(hash_patterns[,"g_id"]), .fun = function(x,ids){return(head(expand.grid(seq=x,g_id=ids),x))},ids=unique(hash_patterns[,"g_id"]))

hash_patterns_expand <- merge(hash_patterns_expand0,hash_patterns_expand1, 
                              by.x=c("g_id"), by.y=c("g_id"), 
                              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

hash_patterns_expand <- hash_patterns_expand[,c("seq","g_id","p_id",colnames(hash_patterns_expand)[!(colnames(hash_patterns_expand) %in% c("seq","g_id","p_id"))])]

hash_patterns_expand <- hash_patterns_expand[order(hash_patterns_expand[,"seq"],hash_patterns_expand[,"g_id"],hash_patterns_expand[,"p_id"]),] 
row.names(hash_patterns_expand) <- seq(nrow(hash_patterns_expand))

hash_patterns_expand_trim <- unique(hash_patterns_expand[,!(colnames(hash_patterns_expand) %in% "seq")])

rm2(hash_patterns1,hash_patterns2,hash_patterns3,hash_patterns4)
rm2(hash_patterns,hash_patterns_expand0,hash_patterns_expand1)

hash_table0 <- dlply(.data=hash_patterns_expand,.variables="seq",.fun=function(y,x){
  
  require(data.table)
  require(gsubfn)
  
  x_org <- x
  for (i in 1:nrow(y)){  x <- gsubfn(y[i,"pattern"],paste0(y[i,"replacement"]),x,ignore.case=TRUE,perl=TRUE)}
  return(c(x_org,x))
  
},x=group_column_u)

hash_table <- data.frame(pattern=unique(as.vector(unlist(hash_table0))),replacement=unique(as.vector(unlist(hash_table0))),stringsAsFactors=FALSE)

for (i in 1:nrow(hash_patterns_expand_trim)){  
  
  hash_table[,"replacement"] <- gsubfn(hash_patterns_expand_trim[i,"pattern"],paste0(hash_patterns_expand_trim[i,"replacement"]),hash_table[,"replacement"],ignore.case=TRUE,perl=TRUE)
}
rm(i)

hash_table <- hash_table[hash_table[,"pattern"] != hash_table[,"replacement"],]
hash_table <- hash_table[order(hash_table[,"replacement"],hash_table[,"pattern"]),] 
row.names(hash_table) <- seq(nrow(hash_table))

rm2(hash_table0,hash_patterns_expand,hash_patterns_expand_trim)


#Clean group_column
EurekahedgeHF_Excel_aca_full14 <- as.data.table(EurekahedgeHF_Excel_aca_full14)

for (k in 1:nrow(hash_table)) 
{
  #k <- 1
  
  set(EurekahedgeHF_Excel_aca_full14, i=NULL, j=which(colnames(EurekahedgeHF_Excel_aca_full14)==c(group_column)), 
      value=gsub(hash_table[k,"pattern"],hash_table[k,"replacement"],EurekahedgeHF_Excel_aca_full14[[which(colnames(EurekahedgeHF_Excel_aca_full14)==c(group_column))]],ignore.case=TRUE,perl=TRUE))
}
rm(k)

EurekahedgeHF_Excel_aca_full14 <- as.data.frame(EurekahedgeHF_Excel_aca_full14,stringsAsFactors=FALSE)

rm(hash_table)


# Start Similarity

#sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
sample_data_all_temp <- sim_stats_ios[,c(identifier,"yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]

temp_stacked_id_group_temp <- EurekahedgeHF_Excel_aca_full14[,c(identifier,"yr",group_column)]
temp_stacked_id_group_temp <- unique(temp_stacked_id_group_temp,comparables=FALSE)
temp_stacked_id_group_temp <- temp_stacked_id_group_temp[!is.na(temp_stacked_id_group_temp[,c(group_column)]),]

temp_stacked_full <- merge(sample_data_all_temp[,c("yr",identifier)],temp_stacked_id_group_temp,
                           by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

temp_stacked_full <- temp_stacked_full[order(temp_stacked_full[,identifier], temp_stacked_full[,"yr"]),] 
temp_stacked_full <- temp_stacked_full[!is.na(temp_stacked_full[,c(group_column)]),]
temp_stacked_full <- unique(temp_stacked_full,comparables=FALSE)
row.names(temp_stacked_full) <- seq(nrow(temp_stacked_full))

text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
#text_group_vars <- toupper(text_group_vars)
#text_group_vars <- trim(text_group_vars)
text_group_vars <- gsub(" {2,}", " ",text_group_vars, perl=TRUE)
text_group_vars <- gsub("^\\s+|\\s+$", "",text_group_vars, perl=TRUE)
text_group_vars <- text_group_vars[!is.na(text_group_vars)]
text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]

# #Create empty data.frame for merging
# for (j in 1:length(text_variables))
# {
#   #j <- 1
#   
#   text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
#   text_percentages_temp_col <- empty.df(text_percentages_temp)
#   for (k in 1:ncol(text_percentages_temp_col))
#   {
#     text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
#   }
#   rm(k)
#   
#   header_col <- data.frame(temp=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
#   colnames(header_col)[1] <- identifier
#   assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked_OLD",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
#   
#   rm2(text_percentages_temp,text_percentages_temp_col,header_col)
# }
# rm(j)

# for (i in 1:length(text_group_vars))
# {
#   #i <- 1
#   
#   for (j in 1:length(text_variables))
#   {
#     #j <- 1
#     
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier,text_percentages,unknowns_strings,similarity_db)
# 
#     if (text_variables[j]=="ios")
#     {
#       year_sim_ios_primary_investment_strategy_combcol_stacked <- rbind(year_sim_ios_primary_investment_strategy_combcol_stacked,temp_sim_stacked)
#       
#     } else
#     {
#       cat("ERROR!", "\n")
#     }
#     
#     #rm2(temp_sim_stacked)
#     
#     progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
#   }
#   rm(j)
# }
# rm(i)
# 
# colnames(year_sim_ios_primary_investment_strategy_combcol_stacked) <- paste(group_column,colnames(year_sim_ios_primary_investment_strategy_combcol_stacked),sep="_")
# colnames(year_sim_ios_primary_investment_strategy_combcol_stacked)[1:3] <- c(identifier,"yr",group_column)
# 
# year_sim_ios_primary_investment_strategy_combcol_stacked <- year_sim_ios_primary_investment_strategy_combcol_stacked[order(year_sim_ios_primary_investment_strategy_combcol_stacked[,identifier], 
#                                                                                                                            year_sim_ios_primary_investment_strategy_combcol_stacked[,"yr"]),]


text_variables_expand <- expand.grid(text_variables,text_group_vars)
colnames(text_variables_expand) <- c("text_variables",group_column)
text_variables_expand[sapply(text_variables_expand, is.factor)] <- lapply(text_variables_expand[sapply(text_variables_expand, is.factor)], as.character)


year_sim_stacked0 <- dlply(.data=text_variables_expand, .variables=c("text_variables"),.fun=function(x,merged_data,group_var,id_var,percentiles_data,unknowns_strings,sim_db){
  
  # x <- text_variables_expand[text_variables_expand[,"text_variables"]=="ios",]
  # merged_data <- temp_stacked_full
  # group_var <- group_column
  # id_var <- identifier
  # percentiles_data <- text_percentages
  # unknowns_strings <- unknowns_strings
  # sim_db <- similarity_db
  
  #text_group_vars <- unique(unlist(x[,group_var]))
  #text_type <- unique(unlist(x[,c("text_variables")]))
  
  temp_sim_stacked <- llply(.data=unique(unlist(x[,group_var])), .fun = function(y,merged_data,group_var,text_type,id_var,percentiles_data,unknowns_strings,sim_db){
    
    # y <- unique(unlist(x[,group_var]))[[1]]
    # text_type <- unique(unlist(x[,c("text_variables")]))
    
    return(calculate_similarity_by_group(merged_data=merged_data,group_var=group_var,group_var_value=y,text_type=text_type,
                                         id_var=id_var,percentiles_data=percentiles_data,
                                         unknowns_strings=unknowns_strings,sim_db=sim_db))
    
  },merged_data=merged_data,group_var=group_var,text_type=unique(unlist(x[,c("text_variables")])),id_var=id_var,percentiles_data=percentiles_data,unknowns_strings=unknowns_strings,sim_db=sim_db, .progress = "text")
  
  temp_sim_stacked <- data.frame(text_variables=NA,rbindlist(l=temp_sim_stacked,fill=TRUE),stringsAsFactors=FALSE) 
  temp_sim_stacked[,"text_variables"] <- unique(unlist(x[,c("text_variables")]))
  
  return(temp_sim_stacked)
  
},merged_data=temp_stacked_full,group_var=group_column,id_var=identifier,percentiles_data=text_percentages,unknowns_strings=unknowns_strings,sim_db=similarity_db, .progress = "text")

year_sim_ios_primary_investment_strategy_combcol_stacked <- year_sim_stacked0[["ios"]]


year_sim_ios_primary_investment_strategy_combcol_stacked <- year_sim_ios_primary_investment_strategy_combcol_stacked[,!(colnames(year_sim_ios_primary_investment_strategy_combcol_stacked) %in% "text_variables")]
colnames(year_sim_ios_primary_investment_strategy_combcol_stacked) <- paste(group_column,colnames(year_sim_ios_primary_investment_strategy_combcol_stacked),sep="_")
colnames(year_sim_ios_primary_investment_strategy_combcol_stacked)[1:3] <- c(identifier,"yr",group_column)

year_sim_ios_primary_investment_strategy_combcol_stacked <- year_sim_ios_primary_investment_strategy_combcol_stacked[order(year_sim_ios_primary_investment_strategy_combcol_stacked[,identifier], 
                                                                                                                           year_sim_ios_primary_investment_strategy_combcol_stacked[,"yr"]),]

rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
rm2(EurekahedgeHF_Excel_aca_full13)


# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY EQUITY STYLE LONG BOX AND YEAR", "\n")
# ###############################################################################
# 
# group_column <- "Equity_Style_Box_Long"
# 
# sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd4[,c(identifier,group_column)], sample_data_all_temp[,c("yr",identifier)], 
#                            by.x=c(identifier), by.y=c(identifier), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
# text_group_vars <- toupper(text_group_vars)
# text_group_vars <- trim(text_group_vars)
# text_group_vars <- text_group_vars[!is.na(text_group_vars)]
# 
# #Create empty data.frame for merging
# for (j in 1:length(text_variables))
# {
#   #j <- 1
#   
#   text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
#   text_percentages_temp_col <- empty.df(text_percentages_temp)
#   for (k in 1:ncol(text_percentages_temp_col))
#   {
#     text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
#   }
#   header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
#   assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
#   
#   rm2(text_percentages_temp,text_percentages_temp_col,header_col)
# }
# 
# for (i in 1:length(text_group_vars))
# {
#   #i <- 1
#   
#   for (j in 1:length(text_variables))
#   {
#     #j <- 1
#     
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier)
#     
#     if (text_variables[j]=="ios")
#     {
#       year_sim_ios_equity_style_box_long_stacked <- rbind(year_sim_ios_equity_style_box_long_stacked,temp_sim_stacked)
#       
#     } else if (text_variables[j]=="pr")
#     {
#       year_sim_pr_equity_style_box_long_stacked <- rbind(year_sim_pr_equity_style_box_long_stacked,temp_sim_stacked)
#       
#     } else
#     {
#       cat("ERROR!", "\n")
#     }
#     
#     rm2(temp_sim_stacked)
#     
#     progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
#                       inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
#     
#     
#   }
#   
# }
# 
# colnames(year_sim_ios_equity_style_box_long_stacked) <- paste(group_column,colnames(year_sim_ios_equity_style_box_long_stacked),sep="_")
# colnames(year_sim_ios_equity_style_box_long_stacked)[1:3] <- c(identifier,"yr",group_column)
# 
# year_sim_ios_equity_style_box_long_stacked <- year_sim_ios_equity_style_box_long_stacked[order(year_sim_ios_equity_style_box_long_stacked[,identifier], 
#                                                                                                  year_sim_ios_equity_style_box_long_stacked[,"yr"]),]
# 
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
# 
# 
# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY FAMILY AND YEAR", "\n")
# ###############################################################################
# 
# group_column <- "Branding_Name"
# 
# sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd6[,c(identifier,group_column)], sample_data_all_temp[,c("yr",identifier)], 
#                            by.x=c(identifier), by.y=c(identifier), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
# text_group_vars <- toupper(text_group_vars)
# text_group_vars <- trim(text_group_vars)
# text_group_vars <- text_group_vars[!is.na(text_group_vars)]
# #text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]
# 
# #Create empty data.frame for merging
# for (j in 1:length(text_variables))
# {
#   #j <- 1
#   
#   text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
#   text_percentages_temp_col <- empty.df(text_percentages_temp)
#   for (k in 1:ncol(text_percentages_temp_col))
#   {
#     text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
#   }
#   header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
#   assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
#   
#   rm2(text_percentages_temp,text_percentages_temp_col,header_col)
# }
# 
# 
# for (i in 1:length(text_group_vars))
# {
#   #i <- 1
#   
#   for (j in 1:length(text_variables))
#   {
#     #j <- 1
#     
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier)
#     
#     if (text_variables[j]=="ios")
#     {
#       year_sim_ios_branding_name_stacked <- rbind(year_sim_ios_branding_name_stacked,temp_sim_stacked)
#       
#     } else if (text_variables[j]=="pr")
#     {
#       year_sim_pr_branding_name_stacked <- rbind(year_sim_pr_branding_name_stacked,temp_sim_stacked)
#       
#     } else
#     {
#       cat("ERROR!", "\n")
#     }
#     
#     rm2(temp_sim_stacked)
#     
#     progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
#                       inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
#     
#     
#   }
#   
# }
# 
# colnames(year_sim_ios_branding_name_stacked) <- paste(group_column,colnames(year_sim_ios_branding_name_stacked),sep="_")
# colnames(year_sim_ios_branding_name_stacked)[1:3] <- c(identifier,"yr",group_column)
# 
# year_sim_ios_branding_name_stacked <- year_sim_ios_branding_name_stacked[order(year_sim_ios_branding_name_stacked[,identifier], 
#                                                                                  year_sim_ios_branding_name_stacked[,"yr"]),]
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)


###############################################################################
cat("BACKUP SIMIALRITY DATA", "\n")
###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"year_sim_ios_all_stacked",year_sim_ios_all_stacked)
ExportTable(descriptive_stats_db,"year_sim_ios_primary_investment_strategy_combcol_stacked",year_sim_ios_primary_investment_strategy_combcol_stacked)
#ExportTable(descriptive_stats_db,"year_sim_ios_equity_style_box_long_stacked",year_sim_ios_equity_style_box_long_stacked)
#ExportTable(descriptive_stats_db,"year_sim_ios_branding_name_stacked",year_sim_ios_branding_name_stacked)

#year_sim_ios_all_stacked                   <- runsql("SELECT * FROM year_sim_ios_all_stacked",descriptive_stats_db)
#year_sim_ios_broad_cat_group_stacked       <- runsql("SELECT * FROM year_sim_ios_broad_cat_group_stacked",descriptive_stats_db)
#year_sim_ios_equity_style_box_long_stacked <- runsql("SELECT * FROM year_sim_ios_equity_style_box_long_stacked",descriptive_stats_db)
#year_sim_ios_branding_name_stacked         <- runsql("SELECT * FROM year_sim_ios_branding_name_stacked",descriptive_stats_db)


###############################################################################
cat("MERGE IOS TEXT DATA", "\n")
###############################################################################

read_stats_ios <- merge(EurekahedgeHF_Excel_aca_full14[,c(identifier,"yr","month")], read_stats_ios_f, 
                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                        #by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
read_stats_ios <- read_stats_ios[order(read_stats_ios[,identifier],read_stats_ios[,"yr"],read_stats_ios[,"month"]),]

sim_stats_ios <- merge(EurekahedgeHF_Excel_aca_full14[,c(identifier,"yr","month")], year_sim_ios_all_stacked,
                       by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_primary_investment_strategy_combcol_stacked, 
                       by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
#sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_equity_style_box_long_stacked, 
#                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
#                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
#sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_branding_name_stacked, 
#                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
#                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_ios <- sim_stats_ios[order(sim_stats_ios[,identifier],
                                     sim_stats_ios[,"yr"],
                                     sim_stats_ios[,"month"]),]

text_stats_ios <- merge(read_stats_ios, sim_stats_ios,
                        by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))


#rm2(year_sim_ios_all_stacked,year_sim_ios_broad_cat_group_stacked)
rm2(read_stats_ios,sim_stats_ios)


# ###############################################################################
# cat("BACKUP DATA", "\n")
# ###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"EurekahedgeHF_Excel_aca_full14",EurekahedgeHF_Excel_aca_full14)
ExportTable(descriptive_stats_db,"text_stats_ios",text_stats_ios)

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)


rm2(missing_vars,missing_vars_expand)
rm2(age_vars)
rm2(aum_vars,aum_vars_lagged_count,aum_vars_lagged)
rm2(aum_vars_log,aum_vars_log_lagged_count,aum_vars_log_lagged)
rm2(ret_vars,ret_vars_lagged_count,ret_vars_lagged)
rm2(ret_sq_vars,ret_sq_vars_lagged_count,ret_sq_vars_lagged)
rm2(flow_vars,flow_vars_lagged_count,flow_vars_lagged)
rm2(sdflow_vars,sdflow_vars_lagged_count,sdflow_vars_lagged)
rm2(crsp_msi_vars)

rm2(EurekahedgeHF_Excel_aca_full0_path,EurekahedgeHF_Excel_aca_full0_cols)
rm2(EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols0,EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols1,EurekahedgeHF_Excel_aca_full0_all_fund_ids_expand_cols)

rm2(descriptive_stats_tables,descriptive_stats_fields)