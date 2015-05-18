# TODO: Add comment
# 
# Author: Brad
# File: Hedge_Fund_Strategy_Multivariate.R
# Version: 1.0
# Date: 05.20.2014
# Purpose: Run regressions
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP","\n")
###############################################################################

# Clear workspace
rm(list=ls(all=T))
rm(list=ls(all.names=T))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE=500)

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=F)

# String as factors is F -- used for read.csv
options(StringsAsFactors=F)

# Default maxprint option
options(max.print=500)
# options(max.print=99999)

# Memory limit
#memory.limit(size=8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1


if (Location==1) {
  input_directory <- normalizePath("F:/Dropbox/Research/Fund_Strategies/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("F:/Research_temp2/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T) 
  
} else if (Location==3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Fund_Strategies/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Fund_Strategies/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=F)


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
# c("compare","cwhmisc","descr","fastmatch","formatR","gtools","Hmisc","installr",
#   "knitr","leaps","markdown","memisc","mitools","pander","pbapply",
#   "PerformanceAnalytics","psych","quantreg","R.oo","R2wd","reporttools","reshape2",
#   "rms","sqldf","stargazer","stringr","taRifx","UsingR","xtable","zoo")
external_packages <- c("data.table","gdata","limma","lmtest","MASS","plm","plyr","reshape2","sandwich","texreg")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=F,USE.NAMES=F)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("IMPORT DATA","\n")
###############################################################################

identifier <- "Fund_ID"

beg_year <- 1994
#beg_year <- 2007
end_year <- 2013

#strat_col <- "main_investment_strategy"
strat_col <- "Primary_Investment_Strategy_combcol"

output_directory_misreporting <- normalizePath("F:/Research_temp4/",winslash="\\",mustWork=T)
data_all_org <- read.csv(file=paste(output_directory_misreporting,"data_all",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)


###############################################################################
cat("TRIM DATA","\n")
###############################################################################

#data_all0 <- data_all_org[(data_all_org[,"yr"]>=beg_year & data_all_org[,"yr"]<=end_year),]
#data_all0 <- data_all_org[,!(colnames(data_all_org) %in% c("Fund_Name","Secondary_Investment_Strategy","Strategy","Strat_ID"))]
data_all0 <- data_all_org[,!(colnames(data_all_org) %in% c("Secondary_Investment_Strategy","Strat_ID"))]

rm2(data_all_org)


# ###############################################################################
# cat("MERGE DATA","\n")
# ###############################################################################
# 
# data_tone <- read.csv(file=paste(output_directory,"data_tone",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
# 
# data_all1 <- merge(data_all0,data_tone,
#                    by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
#                    all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# rm2(data_all0,data_tone)
# 
# data_all1 <- data_all1[order(data_all1[,identifier],
#                              data_all1[,"yr"],
#                              data_all1[,"month"]),]
# row.names(data_all1) <- seq(nrow(data_all1))

data_all1 <- data_all0
rm2(data_all0)


###############################################################################
cat("CLEAN DATA","\n")
###############################################################################

data_all1[,"date"] <- as.Date(data_all1[,"date"],format="%Y-%m-%d")
data_all1[,"Date_Added"] <- as.Date(data_all1[,"Date_Added"],format="%Y-%m-%d")
data_all1[,"chgdt"] <- as.Date(data_all1[,"chgdt"],format="%Y-%m-%d")
data_all1[,"Inception_Date"] <- as.Date(data_all1[,"Inception_Date"],format="%Y-%m-%d")

data_all1[,strat_col] <- ifelse(data_all1[,strat_col]=="",NA,data_all1[,strat_col])

for(k in which(sapply(data_all1,class)!="Date"))
{
  #k <- 1
  
  data_all1[[k]] <- unknownToNA(data_all1[[k]],unknown=unknowns_strings,force=T)
  data_all1[[k]] <- ifelse(is.na(data_all1[[k]]),NA,data_all1[[k]])
}
rm2(k)


###############################################################################
cat("WINSORIZE - READ AND SIM","\n")
###############################################################################

winsorize_vars_rs <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                    "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                    "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                    "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                    "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                    "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                    "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                    "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

data_all2 <- data_all1
for (i in 1:length(winsorize_vars_rs))
{
  #i <- 1
  #i <- 2
  data_all2[,winsorize_vars_rs[i]] <- winsorize_both(data_all2[,winsorize_vars_rs[i]],q=0.025)
  
}
rm(i)

rm2(winsorize_vars_rs)

rm2(data_all1)


###############################################################################
cat("WINSORIZE - TONE","\n")
###############################################################################

winsorize_vars_t <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")
""
data_all <- data_all2
for (i in 1:length(winsorize_vars_t))
{
  #i <- 1
  #i <- 2
  data_all[,winsorize_vars_t[i]] <- winsorize_both(data_all[,winsorize_vars_t[i]],q=0.025)
  
}
rm(i)

rm2(winsorize_vars_t)

rm2(data_all2)


###############################################################################
cat("CREATE ANNUALIZED RETURN AND FLOWS","\n")
###############################################################################

fund_ret_aum_path <- paste(output_directory_misreporting,"\\","data_prescreen",".csv",sep="")

fund_ret_aum_cols_all <- as.vector(t(read.csv(file=fund_ret_aum_path,header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))

fund_ret_aum_cols_keep <- fund_ret_aum_cols_all[fund_ret_aum_cols_all %in% c(identifier,"yr","month","Monthly_Ret","AUM","age_m","age_y")]

fund_ret_aum <- read.columns(file=fund_ret_aum_path,required.col=fund_ret_aum_cols_keep,sep=",",na.strings="NA",stringsAsFactors=FALSE)

rm2(fund_ret_aum_cols_all,fund_ret_aum_cols_keep)

# test <- data_all[data_all[,identifier]==5002,c(identifier,"yr_month",
#                                              "Monthly_Ret_lag1","Monthly_Ret_lag2","Monthly_Ret_lag3","Monthly_Ret_lag4",
#                                              #"Monthly_Ret2_lag1","Monthly_Ret2_lag2","Monthly_Ret2_lag3","Monthly_Ret2_lag4",
#                                              "Yearly_Ret2_lag1","Yearly_Ret2_lag2","Yearly_Ret2_lag3","Yearly_Ret2_lag4")]


#data_all_counts0 <- data_all[!is.na(data_all[,"Monthly_Ret"]),]
#data_all_counts1 <- count(data_all_counts0,c(identifier,"yr"))
#data_all_counts1_good <- data_all_counts1[data_all_counts1[,"freq"]==12,]
#data_all_counts1_bad <- data_all_counts1[data_all_counts1[,"freq"]!=12,]

#data_all_counts0 <- copy(as.data.table(data_all[,c(identifier,"yr","month","Monthly_Ret","AUM"),]))
data_all_counts0 <- copy(as.data.table(fund_ret_aum[,c(identifier,"yr","month","Monthly_Ret","AUM","age_m","age_y")]))

rm2(fund_ret_aum)

data_all_counts0_u <- copy(data_all_counts0)
data_all_counts0_u <- unique(data_all_counts0_u)

rm2(data_all_counts0)

data_all_counts1 <- copy(data_all_counts0_u)
#data_all_counts1[,droprow := is.na(Monthly_Ret)]
#data_all_counts1 <- data_all_counts1[!(droprow)][,droprow:=NULL][]
data_all_counts1 <- data_all_counts1[!is.na(Monthly_Ret)]

rm2(data_all_counts0_u)

data_all_counts2 <- copy(data_all_counts1)
#data_all_counts2[,droprow := is.na(AUM)]
#data_all_counts2 <- data_all_counts2[!(droprow)][,droprow:=NULL][]
data_all_counts2 <- data_all_counts2[!(is.na(AUM) & month==12)]

rm2(data_all_counts1)

data_all_counts3 <- copy(data_all_counts2)
setkeyv(data_all_counts3, c(identifier,"yr"))
data_all_counts3[,freq:=.N, by=c(identifier,"yr")]

rm2(data_all_counts2)

data_all_counts_good <- copy(data_all_counts3)
data_all_counts_good <- data_all_counts_good[freq==12,]

data_all_counts_bad <- copy(data_all_counts3)
data_all_counts_bad <- data_all_counts_bad[freq!=12,]

rm2(data_all_counts3)

data_all_counts_annual_ret0 <- copy(data_all_counts_good)
data_all_counts_annual_ret0[,ret_plus_one:=Monthly_Ret+1, by=NULL]
data_all_counts_annual_ret0 <- as.data.frame(data_all_counts_annual_ret0,stringsAsFactors=F)

rm2(data_all_counts_good,data_all_counts_bad)


data_all_counts_annual_ret1 <- ddply(.data=data_all_counts_annual_ret0, .variables=c(identifier,"yr"),.fun=function(x){
  
  # x <- data_all_counts_annual_ret0[data_all_counts_annual_ret0[,identifier]==5002 & data_all_counts_annual_ret0[,"yr"]==2007,]
  
  id_temp <- unique(x[,identifier])
  yr_temp <- unique(x[,"yr"])
  
  #cat(id_temp,",",yr_temp,"\n")
  
  age_m_temp0 <- unique(x[,"age_m"])
  if (length(age_m_temp0)==0){
    age_m_temp <- NA
  } else if (length(age_m_temp0)==1){
    age_m_temp <- age_m_temp0
  }  else {
    age_m_temp <- max(age_m_temp0,na.rm=T)
  }

  age_y_temp0 <- unique(x[,"age_y"])
  if (length(age_y_temp0)==0){
    age_y_temp <- NA
  } else if (length(age_y_temp0)==1){
    age_y_temp <- age_y_temp0
  }  else {
    age_y_temp <- max(age_y_temp0,na.rm=T)
  }

  x_out <- unique(data.frame(temp_id=id_temp,temp_yr=yr_temp,Yearly_Ret=prod(x[,"ret_plus_one"])-1,age_m_Yearly=age_m_temp,age_y_Yearly=age_y_temp,stringsAsFactors=F))
  colnames(x_out)[match("temp_id",names(x_out))] <- identifier
  colnames(x_out)[match("temp_yr",names(x_out))] <- "yr"
  
  return(x_out)
},.progress="text")

# Winsorize yearly ret
data_all_counts_annual_ret1_win <- data_all_counts_annual_ret1
data_all_counts_annual_ret1_win[,"Yearly_Ret"] <- winsorize_both(data_all_counts_annual_ret1_win[,"Yearly_Ret"],q=0.025)


data_all_counts_annual_ret2 <- merge(data_all_counts_annual_ret0,data_all_counts_annual_ret1_win,
                                     by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                                     all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

rm2(data_all_counts_annual_ret0,data_all_counts_annual_ret1,data_all_counts_annual_ret1_win)

data_all_counts_annual_ret2_trim <- data_all_counts_annual_ret2[data_all_counts_annual_ret2[,"month"]==12,]

rm2(data_all_counts_annual_ret2)

data_all_counts_annual_ret3 <- copy(as.data.table(data_all_counts_annual_ret2_trim))
setkeyv(data_all_counts_annual_ret3, c(identifier))
#data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,AUM_Yearly_lag1:=c(NA,AUM[-.N]),by=c(identifier)]
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,AUM_Yearly_lag1:=shift(AUM,-1),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,AUM_Yearly_lag2:=shift(AUM,-2),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,AUM_Yearly_lag3:=shift(AUM,-3),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,AUM_Yearly_lag4:=shift(AUM,-4),by=c(identifier)])
#data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_lag1:=c(NA,Yearly_Ret[-.N]),by=c(identifier)]
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_lag1:=shift(Yearly_Ret,-1),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_lag2:=shift(Yearly_Ret,-2),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_lag3:=shift(Yearly_Ret,-3),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_lag4:=shift(Yearly_Ret,-4),by=c(identifier)])

data_all_counts_annual_ret3[,Yearly_Ret_sq:=Yearly_Ret*Yearly_Ret,by=NULL]
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_sq_lag1:=shift(Yearly_Ret_sq,-1),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_sq_lag2:=shift(Yearly_Ret_sq,-2),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_sq_lag3:=shift(Yearly_Ret_sq,-3),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,Yearly_Ret_sq_lag4:=shift(Yearly_Ret_sq,-4),by=c(identifier)])

suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_m_Yearly_lag1:=shift(age_m_Yearly,-1),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_m_Yearly_lag2:=shift(age_m_Yearly,-2),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_m_Yearly_lag3:=shift(age_m_Yearly,-3),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_m_Yearly_lag4:=shift(age_m_Yearly,-4),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_y_Yearly_lag1:=shift(age_y_Yearly,-1),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_y_Yearly_lag2:=shift(age_y_Yearly,-2),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_y_Yearly_lag3:=shift(age_y_Yearly,-3),by=c(identifier)])
#suppressWarnings(data_all_counts_annual_ret3 <- data_all_counts_annual_ret3[,age_y_Yearly_lag4:=shift(age_y_Yearly,-4),by=c(identifier)])

rm2(data_all_counts_annual_ret2_trim)

data_all_counts_annual_ret4 <- copy(data_all_counts_annual_ret3)
data_all_counts_annual_ret4[,nflow_Yearly:=AUM-AUM_Yearly_lag1*(1+Yearly_Ret),by=NULL]
data_all_counts_annual_ret4[,pflow_Yearly:=nflow_Yearly/AUM_Yearly_lag1,by=NULL]

rm2(data_all_counts_annual_ret3)

# Winsorize yearly flow
data_all_counts_annual_ret5 <- as.data.frame(data_all_counts_annual_ret4,stringsAsFactors=F)

data_all_counts_annual_ret5[,"nflow_Yearly"] <- winsorize_both(data_all_counts_annual_ret5[,"nflow_Yearly"],q=0.025)
data_all_counts_annual_ret5[,"pflow_Yearly"] <- winsorize_both(data_all_counts_annual_ret5[,"pflow_Yearly"],q=0.025)

#max(as.data.frame(data_all_counts_annual_ret5)[,"nflow_Yearly"],na.rm=T)
#min(as.data.frame(data_all_counts_annual_ret5)[,"nflow_Yearly"],na.rm=T)

#max(as.data.frame(data_all_counts_annual_ret5)[,"pflow_Yearly"],na.rm=T)
#min(as.data.frame(data_all_counts_annual_ret5)[,"pflow_Yearly"],na.rm=T)

data_all_counts_annual_ret5 <- as.data.table(data_all_counts_annual_ret5)
setkeyv(data_all_counts_annual_ret5, c(identifier))

suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,nflow_Yearly_lag1:=shift(nflow_Yearly,-1),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,nflow_Yearly_lag2:=shift(nflow_Yearly,-2),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,nflow_Yearly_lag3:=shift(nflow_Yearly,-3),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,nflow_Yearly_lag4:=shift(nflow_Yearly,-4),by=c(identifier)])

suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,pflow_Yearly_lag1:=shift(pflow_Yearly,-1),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,pflow_Yearly_lag2:=shift(pflow_Yearly,-2),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,pflow_Yearly_lag3:=shift(pflow_Yearly,-3),by=c(identifier)])
suppressWarnings(data_all_counts_annual_ret5 <- data_all_counts_annual_ret5[,pflow_Yearly_lag4:=shift(pflow_Yearly,-4),by=c(identifier)])

rm2(data_all_counts_annual_ret4)

data_all_counts_annual_ret5 <- as.data.frame(data_all_counts_annual_ret5,stringsAsFactors=F)
colnames(data_all_counts_annual_ret5)[match("AUM",names(data_all_counts_annual_ret5))] <- "AUM_Yearly"

data_all_counts_annual_ret4_cols_drop <- c("month","Monthly_Ret","freq","ret_plus_one","age_m","age_y")
data_all_counts_annual_ret4_cols_keep <- colnames(data_all_counts_annual_ret5)[!(colnames(data_all_counts_annual_ret5) %in% data_all_counts_annual_ret4_cols_drop)]

data_all_counts_annual_ret4_cols_key <- intersect(colnames(data_all),data_all_counts_annual_ret4_cols_keep)
data_all_counts_annual_ret4_cols_new <- data_all_counts_annual_ret4_cols_keep[!(data_all_counts_annual_ret4_cols_keep %in% data_all_counts_annual_ret4_cols_key)]

data_all_annual_flow <- merge(data_all,data_all_counts_annual_ret5[,data_all_counts_annual_ret4_cols_keep],
                              by.x=data_all_counts_annual_ret4_cols_key,by.y=data_all_counts_annual_ret4_cols_key,
                              all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

rm2(data_all,data_all_counts_annual_ret5)
rm2(data_all_counts_annual_ret4_cols_drop,data_all_counts_annual_ret4_cols_keep)
rm2(data_all_counts_annual_ret4_cols_key)


###############################################################################
cat("MULTIVARIATE ANALYSIS - VARIABLES","\n")
###############################################################################

# pattern_cols_sub <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent",
#                       "string_percent","num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent",
#                       "kink_percent","quality_score_trim0","quality_score_trim1","quality_score_trim2","quality_score_trim3")
# 
# pattern_cols_all <- unique(unlist(lapply(pattern_cols_sub,function(x,cols){cols[grep(x,cols)]},cols=colnames(data_all))))
# 
# 
# ### All pattern cols
# 
# pattern_cols_99 <- pattern_cols_all[grep("_99_",pattern_cols_all)]
# pattern_cols_99_any <- pattern_cols_99[grep("_any_",pattern_cols_99)]
# pattern_cols_99_avg <- pattern_cols_99[grep("_avg_",pattern_cols_99)]
# 
# pattern_cols_95 <- pattern_cols_all[grep("_95_",pattern_cols_all)]
# pattern_cols_95_any <- pattern_cols_95[grep("_any_",pattern_cols_95)]
# pattern_cols_95_avg <- pattern_cols_95[grep("_avg_",pattern_cols_95)]
# 
# pattern_cols_90 <- pattern_cols_all[grep("_90_",pattern_cols_all)]
# pattern_cols_90_any <- pattern_cols_90[grep("_any_",pattern_cols_90)]
# pattern_cols_90_avg <- pattern_cols_90[grep("_avg_",pattern_cols_90)]
# 
# pattern_cols <- c(pattern_cols_99_any,pattern_cols_99_avg,pattern_cols_95_any,pattern_cols_95_avg,pattern_cols_90_any,pattern_cols_90_avg)
# 
# ### Quaity Score cols
# 
# quality_score_cols_trim0 <- pattern_cols_all[grep("quality_score_trim0",pattern_cols_all)]
# quality_score_cols_trim1 <- pattern_cols_all[grep("quality_score_trim1",pattern_cols_all)]
# quality_score_cols_trim2 <- pattern_cols_all[grep("quality_score_trim2",pattern_cols_all)]
# quality_score_cols_trim3 <- pattern_cols_all[grep("quality_score_trim3",pattern_cols_all)]
# 
# #quality_score_cols <- c(quality_score_cols_trim0,quality_score_cols_trim1,quality_score_cols_trim2,quality_score_cols_trim3)
# quality_score_cols <- c(quality_score_cols_trim0,quality_score_cols_trim1)

### Revision cols
revision_cols <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")


### Dep Vars (Text Vars)

#multivariate_vars_dep <- c(pattern_cols,revision_cols)
multivariate_vars_dep <- c(revision_cols,"pflow","sdpct_flow","sdpct_flow_lag1")

### Continuous Vars

multivariate_vars_continuous_fund <- c("mktadjret","mktadjret_lag1",
                                       "mktadjret_sq","mktadjret_sq_lag1",
                                       "AUM","AUM_lag1","AUM_log","AUM_log_lag1",
                                       "Fund_Size_USm","Fund_Capacity_USm","Firms_Total_Asset_USm","Total_Asset_in_Hedge_Funds_USm",
                                       "age_y","age_m","Sharpe_Ratio","Sortino_Ratio",
                                       "total_fee","Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin",
                                       "nflow","nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                                       "pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                       data_all_counts_annual_ret4_cols_new)

#"mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#"mktadjret_sq_lag1","mktadjret_sq_lag2","mktadjret_sq_lag3","mktadjret_sq_lag4",
#"AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",

multivariate_vars_continuous_text <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                       "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                                       "chars_no_space_ios","words_ios","sentences_ios",
                                       "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                                       "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                                       "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                                       "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                                       "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                                       "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

multivariate_vars_continuous_tone <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

multivariate_vars_continuous <- c(multivariate_vars_continuous_fund,multivariate_vars_continuous_text,multivariate_vars_continuous_tone)


### Binary Vars

multivariate_vars_binary_fund <- c("Listed_on_Exchange_bin","Hurdle_Rate_bin","Domicile_onshore_bin","Leverage_bin","Lockup_bin",
                                   "Flagship_bin","Closed_bin","Dead_bin")

multivariate_vars_binary_tone <- c("litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

multivariate_vars_binary <- c(multivariate_vars_binary_fund,multivariate_vars_binary_tone)

#rm2(quality_score_cols_trim0,quality_score_cols_trim1,quality_score_cols_trim2,quality_score_cols_trim3)
#rm2(pattern_cols_99,pattern_cols_99_any,pattern_cols_99_avg)
#rm2(pattern_cols_95,pattern_cols_95_any,pattern_cols_95_avg)
#rm2(pattern_cols_90,pattern_cols_90_any,pattern_cols_90_avg)
#rm2(pattern_cols_sub,pattern_cols_all)

#rm2(revision_cols,data_all_counts_annual_ret4_cols_new)


###############################################################################
cat("ADD NEW COLUMNS","\n")
###############################################################################

data_all_multivariate_new_cols <- c("age_y_log","age_m_log","AUM_USm","AUM_USm_log","Fund_Size_USm_log","Fund_Capacity_USm_log","Fund_Utilization_Ratio",
                                    "Firm_Size_combcol","Firm_Size_combcol_log","Fund_to_Firm_Size",paste(strat_col,"2",sep=""),
                                    "AUM_Yearly_log","AUM_Yearly_log_lag1","AUM_Yearly_log_lag2","AUM_Yearly_log_lag3","AUM_Yearly_log_lag4",
                                    "AUM_Yearly_USm","AUM_Yearly_lag1_USm","AUM_Yearly_lag2_USm","AUM_Yearly_lag3_USm","AUM_Yearly_lag4_USm",
                                    "AUM_Yearly_log_USm","AUM_Yearly_log_lag1_USm","AUM_Yearly_log_lag2_USm","AUM_Yearly_log_lag3_USm","AUM_Yearly_log_lag4_USm",
                                    "nflow_USm","nflow_lag1_USm","nflow_lag2_USm","nflow_lag3_USm","nflow_lag4_USm",
                                    "nflow_Yearly_USm","nflow_Yearly_lag1_USm","nflow_Yearly_lag2_USm","nflow_Yearly_lag3_USm","nflow_Yearly_lag4_USm",
                                    "age_y_Yearly_log","age_m_Yearly_log",
                                    "words_log_ios")

#data_all_multivariate_full_keep_trim0 <- data.frame(unique(data_all_annual_flow[,c(identifier,"yr",multivariate_vars_dep,multivariate_vars_continuous)]),
#                                                    matrix(NA,ncol=length(data_all_multivariate_new_cols),nrow=1,dimnames=list(c(),data_all_multivariate_new_cols)),stringsAsFactors=F)

data_all_multivariate_full_keep_trim0 <- data.frame(unique(data_all_annual_flow[,colnames(data_all_annual_flow)[colnames(data_all_annual_flow) %in% c(identifier,"Fund_Name","Strategy","yr",strat_col,multivariate_vars_dep,multivariate_vars_continuous,multivariate_vars_binary)]]),
                                                    matrix(NA,ncol=length(data_all_multivariate_new_cols),nrow=1,dimnames=list(c(),data_all_multivariate_new_cols)),
                                                    #matrix(NA,ncol=length(quality_score_cols),nrow=1,dimnames=list(c(),paste(quality_score_cols,"rv",sep="_"))),
                                                    stringsAsFactors=F)

data_all_multivariate_full_keep_trim0[,"age_m"] <- data_all_multivariate_full_keep_trim0[,"age_m"]+1
data_all_multivariate_full_keep_trim0[,"age_m_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_m"])

data_all_multivariate_full_keep_trim0[,"age_y"] <- data_all_multivariate_full_keep_trim0[,"age_m"]/12
data_all_multivariate_full_keep_trim0[,"age_y_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_y"])

data_all_multivariate_full_keep_trim0[,"AUM_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_USm"])

data_all_multivariate_full_keep_trim0[,"Fund_Size_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"])
data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm"])

data_all_multivariate_full_keep_trim0[,"Fund_Utilization_Ratio"] <- data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"]/data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm"]

data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"] <- ifelse((is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),NA,
                                                                      ifelse((!is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"],
                                                                             ifelse((is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & !is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"],data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"])))

data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol_log"] <- log(data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"])

data_all_multivariate_full_keep_trim0[,"Fund_to_Firm_Size"] <- data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"]/data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"]

data_all_multivariate_full_keep_trim0[,paste(strat_col,"2",sep="")] <- ifelse(data_all_multivariate_full_keep_trim0[,strat_col] %in% c("ARBITRAGE","DEBT","DUAL APPROACH","EVENT DRIVEN","TOP DOWN","VALUE","OTHERS"),"OTHER2",data_all_multivariate_full_keep_trim0[,strat_col])

data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag1"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag1"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag2"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag2"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag3"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag3"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag4"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag4"])

data_all_multivariate_full_keep_trim0[,"AUM_Yearly_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM_Yearly"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag1_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag1"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag2_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag2"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag3_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag3"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag4_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag4"]/1000000

data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_USm"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_USm"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag1_USm"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag1_USm"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag2_USm"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag2_USm"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag3_USm"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag3_USm"])
data_all_multivariate_full_keep_trim0[,"AUM_Yearly_log_lag4_USm"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_Yearly_lag4_USm"])

data_all_multivariate_full_keep_trim0[,"nflow_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_lag1_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_lag1"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_lag2_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_lag2"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_lag3_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_lag3"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_lag4_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_lag4"]/1000000

data_all_multivariate_full_keep_trim0[,"nflow_Yearly_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_Yearly"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag1_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag1"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag2_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag2"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag3_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag3"]/1000000
data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag4_USm"] <- data_all_multivariate_full_keep_trim0[,"nflow_Yearly_lag4"]/1000000

data_all_multivariate_full_keep_trim0[,"age_m_Yearly"] <- data_all_multivariate_full_keep_trim0[,"age_m_Yearly"]+1
data_all_multivariate_full_keep_trim0[,"age_m_Yearly_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_m_Yearly"])

data_all_multivariate_full_keep_trim0[,"age_y_Yearly"] <- data_all_multivariate_full_keep_trim0[,"age_m_Yearly"]/12
data_all_multivariate_full_keep_trim0[,"age_y_Yearly_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_y_Yearly"])

data_all_multivariate_full_keep_trim0[,"words_log_ios"] <- log(data_all_multivariate_full_keep_trim0[,"words_ios"])

for (i in which(colnames(data_all_multivariate_full_keep_trim0) %in% multivariate_vars_binary))
{
  #i <- 1
  #i <- 2
  data_all_multivariate_full_keep_trim0[[i]] <- ifelse(is.na(data_all_multivariate_full_keep_trim0[[i]]),0,data_all_multivariate_full_keep_trim0[[i]])
}
rm(i)
for (i in which(colnames(data_all_multivariate_full_keep_trim0) %in% revision_cols))
{
  #i <- 1
  #i <- 2
  data_all_multivariate_full_keep_trim0[[i]] <- ifelse(is.na(data_all_multivariate_full_keep_trim0[[i]]),0,data_all_multivariate_full_keep_trim0[[i]])
}
rm(i)

data_all_multivariate_full_keep_trim <- data_all_multivariate_full_keep_trim0

#rm2(data_all_multivariate_new_cols)
rm2(data_all_multivariate_full_keep_trim0)
rm2(data_all_annual_flow)


###############################################################################
cat("FIND FIRST MONTH FOR EACH FUND","\n")
###############################################################################

# data_all_multivariate_lookup0 <- data_all[!is.na(data_all[,strat_col]),]
# data_all_multivariate_lookup0 <- data_all[(!is.na(data_all[,strat_col]) & !is.na(data_all[,"AUM"]) & !is.na(data_all[,"age_y"]) & !is.na(data_all[,"total_fee"])),]
# 
# data_all_multivariate_lookup1 <- unique(data_all_multivariate_lookup0[,c(identifier,"yr")])
# data_all_multivariate_lookup1 <- data_all_multivariate_lookup1[order(data_all_multivariate_lookup1[,identifier],data_all_multivariate_lookup1[,"yr"]),]
# row.names(data_all_multivariate_lookup1) <- seq(nrow(data_all_multivariate_lookup1))
# 
# data_all_multivariate_lookup <- data_all_multivariate_lookup1
# 
# data_all_multivariate_lookup <- as.data.table(data_all_multivariate_lookup)
# setkeyv(data_all_multivariate_lookup,c(identifier,"yr"))
# setorderv(data_all_multivariate_lookup,c(identifier,"yr"),c(1,1))
# data_all_multivariate_lookup <- data_all_multivariate_lookup[,.SD[c(1)],by=c(identifier)]
# data_all_multivariate_lookup <- as.data.frame(data_all_multivariate_lookup,stringsAsFactors=F)
# 
# rm2(data_all_multivariate_lookup0,data_all_multivariate_lookup1)
# 
# data_all_multivariate_full1 <- merge(data_all_multivariate_lookup,data_all_multivariate_full_keep_trim,
#                                      by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
#                                      all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# data_all_multivariate_full1 <- data_all_multivariate_full1[order(data_all_multivariate_full1[,identifier],data_all_multivariate_full1[,"yr"]),]
# row.names(data_all_multivariate_full1) <- seq(nrow(data_all_multivariate_full1))
# 
# rm2(data_all_multivariate_lookup,data_all_multivariate_full_keep_trim)
# 
# data_all_multivariate_full2 <- as.data.table(data_all_multivariate_full1)
# setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
# setorderv(data_all_multivariate_full2,c(identifier,"yr"),c(1,1))
# 
# data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(Primary_Investment_Strategy_combcol)]
# data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(AUM)]
# data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(age_y)]
# data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(total_fee)]
# 
# data_all_multivariate_full2[,na_count:=rowSums(is.na(.SD)),.SDcols=colnames(data_all_multivariate_full2)]
# setorderv(data_all_multivariate_full2,c(identifier,"yr","na_count"),c(1,1,1))
# data_all_multivariate_full2[,na_count:=NULL,by=NULL]
# 
# setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
# data_all_multivariate_full2 <- data_all_multivariate_full2[,.SD[c(1)],by=c(identifier,"yr")]
# data_all_multivariate_full <- as.data.frame(data_all_multivariate_full2,stringsAsFactors=F)
# 
# rm2(data_all_multivariate_full1,data_all_multivariate_full2)


###############################################################################
cat("REMOVE MONTHLY VARIABLES","\n")
###############################################################################

data_all_multivariate_full_keep_trim <- data_all_multivariate_full_keep_trim[order(data_all_multivariate_full_keep_trim[,identifier],
                                                                                   data_all_multivariate_full_keep_trim[,"Fund_Name"],
                                                                                   data_all_multivariate_full_keep_trim[,"yr"],
                                                                                   data_all_multivariate_full_keep_trim[,"age_y"]),]
row.names(data_all_multivariate_full_keep_trim) <- seq(nrow(data_all_multivariate_full_keep_trim))


#data_all_multivariate_lookup0 <- data_all_multivariate_full_keep_trim[!is.na(data_all_multivariate_full_keep_trim[,strat_col]),]
data_all_multivariate_lookup0 <- data_all_multivariate_full_keep_trim[(!is.na(data_all_multivariate_full_keep_trim[,strat_col]) & !is.na(data_all_multivariate_full_keep_trim[,"AUM"]) & !is.na(data_all_multivariate_full_keep_trim[,"age_y"]) & !is.na(data_all_multivariate_full_keep_trim[,"total_fee"])),]
data_all_multivariate_lookup1 <- data_all_multivariate_lookup0[(!is.na(data_all_multivariate_lookup0[,"Fund_Name"])),]

data_all_multivariate_full_drop_cols <- c("age_m","age_m_log","age_y","age_y_log",
                                          "AUM","AUM_lag1","AUM_log","AUM_log_lag1","AUM_USm","AUM_USm_log",
                                          "mktadjret","mktadjret_lag1","mktadjret_sq","mktadjret_sq_lag1",
                                          "nflow","nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                                          "nflow_USm","nflow_lag1_USm","nflow_lag2_USm","nflow_lag3_USm","nflow_lag4_USm",
                                          "pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4")

data_all_multivariate_full <- unique(data_all_multivariate_lookup1[,!(colnames(data_all_multivariate_lookup1) %in% data_all_multivariate_full_drop_cols)])

rm2(data_all_multivariate_full_keep_trim,data_all_multivariate_full_drop_cols)
rm2(data_all_multivariate_lookup0,data_all_multivariate_lookup1)


###############################################################################
cat("FIND FUNDS WITH SAME TEXT MEASURES","\n")
###############################################################################

data_all_multivariate_full_trim <- data_all_multivariate_full

data_all_multivariate_full_trim <- data_all_multivariate_full_trim[!(data_all_multivariate_full_trim[,identifier] %in% c(38679,38680)),]

rm2(data_all_multivariate_full)


# ###############################################################################
# cat("FIND TOP-3 FOR EACH TEXT MEASURE - SETUP","\n")
# ###############################################################################
# 
# data_all_multivariate_full_cols_id <- c(identifier,"Fund_Name","Strategy")
# #data_all_multivariate_full_cols_nonid <- c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024",
# #                                           "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios",
# #                                           "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",multivariate_vars_continuous_tone)
# data_all_multivariate_full_cols_nonid <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios",
#                                            "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",multivariate_vars_continuous_tone)
# 
# data_summary_rank_lookup <- unique(data_all_multivariate_full_trim[,c(identifier,"Fund_Name","yr")])
# 
# top_bot_out_path <- paste(output_directory,"Top_Bottom_Stats",sep="//",collapse="//")  
# create_directory(top_bot_out_path,remove=1)
# 
# 
# ###############################################################################
# cat("FIND TOP-3 FOR EACH TEXT MEASURE - WINSORIZE","\n")
# ###############################################################################
# 
# data_summary_rank_winsorize <- merge(data_summary_rank_lookup,
#                                      data_all_multivariate_full_trim[,c(data_all_multivariate_full_cols_id,"yr",data_all_multivariate_full_cols_nonid)],
#                                      by.x=c(identifier,"yr","Fund_Name"),by.y=c(identifier,"yr","Fund_Name"),
#                                      all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize <- data_summary_rank_winsorize[order(data_summary_rank_winsorize[,identifier]),]
# row.names(data_summary_rank_winsorize) <- seq(nrow(data_summary_rank_winsorize))
# 
# data_summary_rank_winsorize <- data.frame(data_summary_rank_winsorize,
#                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"rank",sep="_"))),
#                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"top3",sep="_"))),
#                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"bot3",sep="_"))),
#                                           stringsAsFactors=F)
# 
# data_summary_rank_winsorize_trim0 <- ldply(.data=data_all_multivariate_full_cols_nonid,.fun=function(x,data,id_cols){
#   
#   # x <- data_all_multivariate_full_cols_nonid[[1]]
#   # data <- data_summary_rank_winsorize
#   # id_cols <- data_all_multivariate_full_cols_id
#   
#   #cat(data_all_multivariate_full_cols_nonid[[i]],"\n")
#   
#   temp_var <- unlist(x)
#   
#   temp_vector <- data.frame(temp_col=sort(unique(data[,temp_var])),matrix(NA,ncol=3,nrow=1,dimnames=list(c(),c("temp_rank","temp_top3","temp_bot3"))),stringsAsFactors=F)
#   temp_vector[,"temp_rank"] <- seq(1,nrow(temp_vector))
#   temp_vector[,"temp_top3"] <-ifelse(temp_vector[,"temp_rank"] %in% tail(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
#   temp_vector[,"temp_bot3"] <-ifelse(temp_vector[,"temp_rank"] %in% head(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
#   
#   temp_vector <- temp_vector[order(-temp_vector[,"temp_top3"]),]
#   temp_vector[,"temp_top3"] <- c("T1","T2","T3",rep(NA,nrow(temp_vector)-3))
#   
#   temp_vector <- temp_vector[order(-temp_vector[,"temp_bot3"]),]
#   temp_vector[,"temp_bot3"] <- c("B3","B2","B1",rep(NA,nrow(temp_vector)-3))
#   
#   temp_vector[,"temp_rank"] <- ifelse((is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),NA,
#                                       ifelse((!is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_top3"],
#                                              ifelse((is.na(temp_vector[,"temp_top3"]) & !is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_bot3"],
#                                                     paste(temp_vector[,"temp_top3"],temp_vector[,"temp_bot3"],sep=","))))
#   
#   colnames(temp_vector)[match("temp_col",names(temp_vector))] <- temp_var
#   colnames(temp_vector)[match("temp_rank",names(temp_vector))] <- paste(temp_var,"rank",sep="_")
#   colnames(temp_vector)[match("temp_top3",names(temp_vector))] <- paste(temp_var,"top3",sep="_")
#   colnames(temp_vector)[match("temp_bot3",names(temp_vector))] <- paste(temp_var,"bot3",sep="_")
#   
#   data_trim0 <- data[,!(colnames(data) %in% paste(temp_var,c("rank","top3","bot3"),sep="_"))]
#   
#   data2 <- merge(data_trim0,temp_vector,
#                 by.x=temp_var,by.y=temp_var,all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
#   
#   data2 <- data2[order(data2[,identifier]),]
#   row.names(data2) <- seq(nrow(data2))
#   
#   data2 <- data2[,c(id_cols,colnames(data2)[!(colnames(data2) %in% id_cols)])]
#   
#   data2 <- data2[,c(colnames(data2)[!(colnames(data2) %in% c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))],
#                   c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))]
#   
#   
#   data_trim <- data.frame(var_name=NA,data2[,c(id_cols,temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_"))],stringsAsFactors=F)
#   
#   colnames(data_trim)[match(temp_var,names(data_trim))] <- "var_value"
#   colnames(data_trim)[match(paste(temp_var,"rank",sep="_"),names(data_trim))] <- "rank"
#   colnames(data_trim)[match(paste(temp_var,"top3",sep="_"),names(data_trim))] <- "top3"
#   colnames(data_trim)[match(paste(temp_var,"bot3",sep="_"),names(data_trim))] <- "bot3"
#   
#   data_trim[,"var_name"] <- temp_var
#   
#   data_trim <- unique(data_trim[,c(id_cols,colnames(data_trim)[!(colnames(data_trim) %in% id_cols)])])
#   
#   rm(temp_var,temp_vector,data_trim0,data2)
#   
#   return(data_trim)
#   
# },data=data_summary_rank_winsorize,id_cols=data_all_multivariate_full_cols_id,.progress="none",.inform=F)
# 
# data_summary_rank_winsorize_trim1 <- data_summary_rank_winsorize_trim0[,colnames(data_summary_rank_winsorize_trim0)[!(colnames(data_summary_rank_winsorize_trim0) %in% c("top3","bot3"))]]
# data_summary_rank_winsorize_trim2 <- data_summary_rank_winsorize_trim1[!(data_summary_rank_winsorize_trim1[,"var_name"] %in% c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024","per_modalstrong","per_modalweak")),]
# data_summary_rank_winsorize_trim3 <- data_summary_rank_winsorize_trim2[!((data_summary_rank_winsorize_trim2[,"var_name"] %in% c("per_litigious","per_negative","per_positive","per_uncertainty")) & (data_summary_rank_winsorize_trim2[,"var_value"]==0)),]
# data_summary_rank_winsorize_trim4 <- data_summary_rank_winsorize_trim3[!is.na(data_summary_rank_winsorize_trim3[,"rank"]),]
# 
# rm(data_summary_rank_winsorize_trim0,data_summary_rank_winsorize_trim1,data_summary_rank_winsorize_trim2,data_summary_rank_winsorize_trim3)
# 
# 
# ## Readability
# 
# data_summary_rank_winsorize_readability_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios"),]
# 
# data_summary_rank_winsorize_readability_cast0a <- unique(data_summary_rank_winsorize_readability_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_winsorize_readability_cast_data_trim <- unique(data_summary_rank_winsorize_readability_cast_data[,!(colnames(data_summary_rank_winsorize_readability_cast_data) %in% c("Fund_Name","Strategy"))])
# 
# data_summary_rank_winsorize_readability_cast0b <- dcast(data=data_summary_rank_winsorize_readability_cast_data_trim[,!(colnames(data_summary_rank_winsorize_readability_cast_data_trim) %in% c("rank"))],
#                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_winsorize_readability_cast0c <- dcast(data=data_summary_rank_winsorize_readability_cast_data_trim[,!(colnames(data_summary_rank_winsorize_readability_cast_data_trim) %in% c("var_value"))],
#                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_winsorize_readability_cast0c) <- paste(colnames(data_summary_rank_winsorize_readability_cast0c),"rank",sep="_")
# colnames(data_summary_rank_winsorize_readability_cast0c)[1] <- identifier
# 
# data_summary_rank_winsorize_readability_cast1 <- merge(data_summary_rank_winsorize_readability_cast0a,data_summary_rank_winsorize_readability_cast0b,
#                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_readability_cast2 <- merge(data_summary_rank_winsorize_readability_cast1,data_summary_rank_winsorize_readability_cast0c,
#                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_readability_cast3 <- data_summary_rank_winsorize_readability_cast2[,sort(colnames(data_summary_rank_winsorize_readability_cast2))]
# 
# data_summary_rank_winsorize_readability_out <- data_summary_rank_winsorize_readability_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                                 colnames(data_summary_rank_winsorize_readability_cast3)[!(colnames(data_summary_rank_winsorize_readability_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_winsorize_readability_out <- data_summary_rank_winsorize_readability_out[order(data_summary_rank_winsorize_readability_out[,identifier]),]
# row.names(data_summary_rank_winsorize_readability_out) <- seq(nrow(data_summary_rank_winsorize_readability_out))
# 
# write.csv(data_summary_rank_winsorize_readability_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Readability",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_winsorize_readability_cast_data)
# rm2(data_summary_rank_winsorize_readability_cast0a,data_summary_rank_winsorize_readability_cast0b,data_summary_rank_winsorize_readability_cast0c)
# rm2(data_summary_rank_winsorize_readability_cast1,data_summary_rank_winsorize_readability_cast2,data_summary_rank_winsorize_readability_cast3)
# 
# ## Similarity
# 
# data_summary_rank_winsorize_similarity_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% c("all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios"),]
# 
# data_summary_rank_winsorize_similarity_cast0a <- unique(data_summary_rank_winsorize_similarity_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_winsorize_similarity_cast0b <- dcast(data=data_summary_rank_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_winsorize_similarity_cast_data) %in% c("rank"))],
#                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_winsorize_similarity_cast0c <- dcast(data=data_summary_rank_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_winsorize_similarity_cast_data) %in% c("var_value"))],
#                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_winsorize_similarity_cast0c) <- paste(colnames(data_summary_rank_winsorize_similarity_cast0c),"rank",sep="_")
# colnames(data_summary_rank_winsorize_similarity_cast0c)[1] <- identifier
# 
# data_summary_rank_winsorize_similarity_cast1 <- merge(data_summary_rank_winsorize_similarity_cast0a,data_summary_rank_winsorize_similarity_cast0b,
#                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_similarity_cast2 <- merge(data_summary_rank_winsorize_similarity_cast1,data_summary_rank_winsorize_similarity_cast0c,
#                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_similarity_cast3 <- data_summary_rank_winsorize_similarity_cast2[,sort(colnames(data_summary_rank_winsorize_similarity_cast2))]
# 
# data_summary_rank_winsorize_similarity_out <- data_summary_rank_winsorize_similarity_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                               colnames(data_summary_rank_winsorize_similarity_cast3)[!(colnames(data_summary_rank_winsorize_similarity_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_winsorize_similarity_out <- data_summary_rank_winsorize_similarity_out[order(data_summary_rank_winsorize_similarity_out[,identifier]),]
# row.names(data_summary_rank_winsorize_similarity_out) <- seq(nrow(data_summary_rank_winsorize_similarity_out))
# 
# write.csv(data_summary_rank_winsorize_similarity_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Similarity",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_winsorize_similarity_cast_data)
# rm2(data_summary_rank_winsorize_similarity_cast0a,data_summary_rank_winsorize_similarity_cast0b,data_summary_rank_winsorize_similarity_cast0c)
# rm2(data_summary_rank_winsorize_similarity_cast1,data_summary_rank_winsorize_similarity_cast2,data_summary_rank_winsorize_similarity_cast3)
# 
# ## Tone
# 
# data_summary_rank_winsorize_tone_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% multivariate_vars_continuous_tone,]
# 
# data_summary_rank_winsorize_tone_cast0a <- unique(data_summary_rank_winsorize_tone_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_winsorize_tone_cast0b <- dcast(data=data_summary_rank_winsorize_tone_cast_data[,!(colnames(data_summary_rank_winsorize_tone_cast_data) %in% c("rank"))],
#                                                  Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_winsorize_tone_cast0c <- dcast(data=data_summary_rank_winsorize_tone_cast_data[,!(colnames(data_summary_rank_winsorize_tone_cast_data) %in% c("var_value"))],
#                                                  Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_winsorize_tone_cast0c) <- paste(colnames(data_summary_rank_winsorize_tone_cast0c),"rank",sep="_")
# colnames(data_summary_rank_winsorize_tone_cast0c)[1] <- identifier
# 
# data_summary_rank_winsorize_tone_cast1 <- merge(data_summary_rank_winsorize_tone_cast0a,data_summary_rank_winsorize_tone_cast0b,
#                                                 by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_tone_cast2 <- merge(data_summary_rank_winsorize_tone_cast1,data_summary_rank_winsorize_tone_cast0c,
#                                                 by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_winsorize_tone_cast3 <- data_summary_rank_winsorize_tone_cast2[,sort(colnames(data_summary_rank_winsorize_tone_cast2))]
# 
# data_summary_rank_winsorize_tone_out <- data_summary_rank_winsorize_tone_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                   colnames(data_summary_rank_winsorize_tone_cast3)[!(colnames(data_summary_rank_winsorize_tone_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_winsorize_tone_out <- data_summary_rank_winsorize_tone_out[order(data_summary_rank_winsorize_tone_out[,identifier]),]
# row.names(data_summary_rank_winsorize_tone_out) <- seq(nrow(data_summary_rank_winsorize_tone_out))
# 
# write.csv(data_summary_rank_winsorize_tone_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Tone",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_winsorize_tone_cast_data)
# rm2(data_summary_rank_winsorize_tone_cast0a,data_summary_rank_winsorize_tone_cast0b,data_summary_rank_winsorize_tone_cast0c)
# rm2(data_summary_rank_winsorize_tone_cast1,data_summary_rank_winsorize_tone_cast2,data_summary_rank_winsorize_tone_cast3)
# 
# rm2(data_summary_rank_winsorize_trim4,data_summary_rank_winsorize)
# rm2(data_summary_rank_winsorize_readability_out,data_summary_rank_winsorize_similarity_out,data_summary_rank_winsorize_tone_out)
# 
# 
# ###############################################################################
# cat("FIND TOP-3 FOR EACH TEXT MEASURE - NON WINSORIZE","\n")
# ###############################################################################
# 
# data_summary_rank_non_winsorize <- merge(data_summary_rank_lookup,unique(data_all[,c(data_all_multivariate_full_cols_id,"yr",data_all_multivariate_full_cols_nonid)]),
#                                          by.x=c(identifier,"yr","Fund_Name"),by.y=c(identifier,"yr","Fund_Name"),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize <- data_summary_rank_non_winsorize[order(data_summary_rank_non_winsorize[,identifier]),]
# row.names(data_summary_rank_non_winsorize) <- seq(nrow(data_summary_rank_non_winsorize))
# 
# data_summary_rank_non_winsorize <- data.frame(data_summary_rank_non_winsorize,
#                                               matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"rank",sep="_"))),
#                                               matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"top3",sep="_"))),
#                                               matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"bot3",sep="_"))),
#                                               stringsAsFactors=F)
# 
# data_summary_rank_non_winsorize_trim0 <- ldply(.data=data_all_multivariate_full_cols_nonid,.fun=function(x,data,id_cols){
#   
#   # x <- data_all_multivariate_full_cols_nonid[[1]]
#   # data <- data_summary_rank_non_winsorize
#   # id_cols <- data_all_multivariate_full_cols_id
#   
#   #cat(data_all_multivariate_full_cols_nonid[[i]],"\n")
#   
#   temp_var <- unlist(x)
#   
#   temp_vector <- data.frame(temp_col=sort(unique(data[,temp_var])),matrix(NA,ncol=3,nrow=1,dimnames=list(c(),c("temp_rank","temp_top3","temp_bot3"))),stringsAsFactors=F)
#   temp_vector[,"temp_rank"] <- seq(1,nrow(temp_vector))
#   temp_vector[,"temp_top3"] <-ifelse(temp_vector[,"temp_rank"] %in% tail(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
#   temp_vector[,"temp_bot3"] <-ifelse(temp_vector[,"temp_rank"] %in% head(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
#   
#   temp_vector <- temp_vector[order(-temp_vector[,"temp_top3"]),]
#   temp_vector[,"temp_top3"] <- c("T1","T2","T3",rep(NA,nrow(temp_vector)-3))
#   
#   temp_vector <- temp_vector[order(-temp_vector[,"temp_bot3"]),]
#   temp_vector[,"temp_bot3"] <- c("B3","B2","B1",rep(NA,nrow(temp_vector)-3))
#   
#   temp_vector[,"temp_rank"] <- ifelse((is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),NA,
#                                       ifelse((!is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_top3"],
#                                              ifelse((is.na(temp_vector[,"temp_top3"]) & !is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_bot3"],
#                                                     paste(temp_vector[,"temp_top3"],temp_vector[,"temp_bot3"],sep=","))))
#   
#   colnames(temp_vector)[match("temp_col",names(temp_vector))] <- temp_var
#   colnames(temp_vector)[match("temp_rank",names(temp_vector))] <- paste(temp_var,"rank",sep="_")
#   colnames(temp_vector)[match("temp_top3",names(temp_vector))] <- paste(temp_var,"top3",sep="_")
#   colnames(temp_vector)[match("temp_bot3",names(temp_vector))] <- paste(temp_var,"bot3",sep="_")
#   
#   data <- merge(data[,!(colnames(data) %in% paste(temp_var,c("rank","top3","bot3"),sep="_"))],temp_vector,
#                 by.x=temp_var,by.y=temp_var,all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
#   
#   data <- data[order(data[,identifier]),]
#   row.names(data) <- seq(nrow(data))
#   
#   data <- data[,c(id_cols,colnames(data)[!(colnames(data) %in% id_cols)])]
#   
#   data <- data[,c(colnames(data)[!(colnames(data) %in% c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))],
#                   c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))]
#   
#   
#   data_trim <- data.frame(var_name=NA,data[,c(id_cols,temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_"))],stringsAsFactors=F)
#   
#   colnames(data_trim)[match(temp_var,names(data_trim))] <- "var_value"
#   colnames(data_trim)[match(paste(temp_var,"rank",sep="_"),names(data_trim))] <- "rank"
#   colnames(data_trim)[match(paste(temp_var,"top3",sep="_"),names(data_trim))] <- "top3"
#   colnames(data_trim)[match(paste(temp_var,"bot3",sep="_"),names(data_trim))] <- "bot3"
#   
#   data_trim[,"var_name"] <- temp_var
#   
#   data_trim <- data_trim[,c(id_cols,colnames(data_trim)[!(colnames(data_trim) %in% id_cols)])]
#   
#   rm(temp_var,temp_vector)
#   
#   return(data_trim)
#   
# },data=data_summary_rank_non_winsorize,id_cols=data_all_multivariate_full_cols_id,.progress="none",.inform=F)
# 
# data_summary_rank_non_winsorize_trim1 <- data_summary_rank_non_winsorize_trim0[,colnames(data_summary_rank_non_winsorize_trim0)[!(colnames(data_summary_rank_non_winsorize_trim0) %in% c("top3","bot3"))]]
# data_summary_rank_non_winsorize_trim2 <- data_summary_rank_non_winsorize_trim1[!(data_summary_rank_non_winsorize_trim1[,"var_name"] %in% c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024","per_modalstrong","per_modalweak")),]
# data_summary_rank_non_winsorize_trim3 <- data_summary_rank_non_winsorize_trim2[!((data_summary_rank_non_winsorize_trim2[,"var_name"] %in% c("per_litigious","per_negative","per_positive","per_uncertainty")) & (data_summary_rank_non_winsorize_trim2[,"var_value"]==0)),]
# data_summary_rank_non_winsorize_trim4 <- data_summary_rank_non_winsorize_trim3[!is.na(data_summary_rank_non_winsorize_trim3[,"rank"]),]
# 
# rm(data_summary_rank_non_winsorize_trim0,data_summary_rank_non_winsorize_trim1,data_summary_rank_non_winsorize_trim2,data_summary_rank_non_winsorize_trim3)
# 
# 
# ## Readability
# 
# data_summary_rank_non_winsorize_readability_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios"),]
# 
# data_summary_rank_non_winsorize_readability_cast0a <- unique(data_summary_rank_non_winsorize_readability_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_non_winsorize_readability_cast0b <- dcast(data=data_summary_rank_non_winsorize_readability_cast_data[,!(colnames(data_summary_rank_non_winsorize_readability_cast_data) %in% c("rank"))],
#                                                             Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_non_winsorize_readability_cast0c <- dcast(data=data_summary_rank_non_winsorize_readability_cast_data[,!(colnames(data_summary_rank_non_winsorize_readability_cast_data) %in% c("var_value"))],
#                                                             Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_non_winsorize_readability_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_readability_cast0c),"rank",sep="_")
# colnames(data_summary_rank_non_winsorize_readability_cast0c)[1] <- identifier
# 
# data_summary_rank_non_winsorize_readability_cast1 <- merge(data_summary_rank_non_winsorize_readability_cast0a,data_summary_rank_non_winsorize_readability_cast0b,
#                                                            by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_readability_cast2 <- merge(data_summary_rank_non_winsorize_readability_cast1,data_summary_rank_non_winsorize_readability_cast0c,
#                                                            by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_readability_cast3 <- data_summary_rank_non_winsorize_readability_cast2[,sort(colnames(data_summary_rank_non_winsorize_readability_cast2))]
# 
# data_summary_rank_non_winsorize_readability_out <- data_summary_rank_non_winsorize_readability_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                                         colnames(data_summary_rank_non_winsorize_readability_cast3)[!(colnames(data_summary_rank_non_winsorize_readability_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_non_winsorize_readability_out <- data_summary_rank_non_winsorize_readability_out[order(data_summary_rank_non_winsorize_readability_out[,identifier]),]
# row.names(data_summary_rank_non_winsorize_readability_out) <- seq(nrow(data_summary_rank_non_winsorize_readability_out))
# 
# write.csv(data_summary_rank_non_winsorize_readability_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Readability",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_non_winsorize_readability_cast_data)
# rm2(data_summary_rank_non_winsorize_readability_cast0a,data_summary_rank_non_winsorize_readability_cast0b,data_summary_rank_non_winsorize_readability_cast0c)
# rm2(data_summary_rank_non_winsorize_readability_cast1,data_summary_rank_non_winsorize_readability_cast2,data_summary_rank_non_winsorize_readability_cast3)
# 
# ## Similarity
# 
# data_summary_rank_non_winsorize_similarity_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% c("all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios"),]
# 
# data_summary_rank_non_winsorize_similarity_cast0a <- unique(data_summary_rank_non_winsorize_similarity_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_non_winsorize_similarity_cast0b <- dcast(data=data_summary_rank_non_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_non_winsorize_similarity_cast_data) %in% c("rank"))],
#                                                            Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_non_winsorize_similarity_cast0c <- dcast(data=data_summary_rank_non_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_non_winsorize_similarity_cast_data) %in% c("var_value"))],
#                                                            Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_non_winsorize_similarity_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_similarity_cast0c),"rank",sep="_")
# colnames(data_summary_rank_non_winsorize_similarity_cast0c)[1] <- identifier
# 
# data_summary_rank_non_winsorize_similarity_cast1 <- merge(data_summary_rank_non_winsorize_similarity_cast0a,data_summary_rank_non_winsorize_similarity_cast0b,
#                                                           by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_similarity_cast2 <- merge(data_summary_rank_non_winsorize_similarity_cast1,data_summary_rank_non_winsorize_similarity_cast0c,
#                                                           by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_similarity_cast3 <- data_summary_rank_non_winsorize_similarity_cast2[,sort(colnames(data_summary_rank_non_winsorize_similarity_cast2))]
# 
# data_summary_rank_non_winsorize_similarity_out <- data_summary_rank_non_winsorize_similarity_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                                       colnames(data_summary_rank_non_winsorize_similarity_cast3)[!(colnames(data_summary_rank_non_winsorize_similarity_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_non_winsorize_similarity_out <- data_summary_rank_non_winsorize_similarity_out[order(data_summary_rank_non_winsorize_similarity_out[,identifier]),]
# row.names(data_summary_rank_non_winsorize_similarity_out) <- seq(nrow(data_summary_rank_non_winsorize_similarity_out))
# 
# write.csv(data_summary_rank_non_winsorize_similarity_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Similarity",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_non_winsorize_similarity_cast_data)
# rm2(data_summary_rank_non_winsorize_similarity_cast0a,data_summary_rank_non_winsorize_similarity_cast0b,data_summary_rank_non_winsorize_similarity_cast0c)
# rm2(data_summary_rank_non_winsorize_similarity_cast1,data_summary_rank_non_winsorize_similarity_cast2,data_summary_rank_non_winsorize_similarity_cast3)
# 
# ## Tone
# 
# data_summary_rank_non_winsorize_tone_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% multivariate_vars_continuous_tone,]
# 
# data_summary_rank_non_winsorize_tone_cast0a <- unique(data_summary_rank_non_winsorize_tone_cast_data[,data_all_multivariate_full_cols_id])
# 
# data_summary_rank_non_winsorize_tone_cast0b <- dcast(data=data_summary_rank_non_winsorize_tone_cast_data[,!(colnames(data_summary_rank_non_winsorize_tone_cast_data) %in% c("rank"))],
#                                                      Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
# data_summary_rank_non_winsorize_tone_cast0c <- dcast(data=data_summary_rank_non_winsorize_tone_cast_data[,!(colnames(data_summary_rank_non_winsorize_tone_cast_data) %in% c("var_value"))],
#                                                      Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
# colnames(data_summary_rank_non_winsorize_tone_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_tone_cast0c),"rank",sep="_")
# colnames(data_summary_rank_non_winsorize_tone_cast0c)[1] <- identifier
# 
# data_summary_rank_non_winsorize_tone_cast1 <- merge(data_summary_rank_non_winsorize_tone_cast0a,data_summary_rank_non_winsorize_tone_cast0b,
#                                                     by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_tone_cast2 <- merge(data_summary_rank_non_winsorize_tone_cast1,data_summary_rank_non_winsorize_tone_cast0c,
#                                                     by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# data_summary_rank_non_winsorize_tone_cast3 <- data_summary_rank_non_winsorize_tone_cast2[,sort(colnames(data_summary_rank_non_winsorize_tone_cast2))]
# 
# data_summary_rank_non_winsorize_tone_out <- data_summary_rank_non_winsorize_tone_cast3[,c(data_all_multivariate_full_cols_id,
#                                                                                           colnames(data_summary_rank_non_winsorize_tone_cast3)[!(colnames(data_summary_rank_non_winsorize_tone_cast3) %in% data_all_multivariate_full_cols_id)])]
# 
# data_summary_rank_non_winsorize_tone_out <- data_summary_rank_non_winsorize_tone_out[order(data_summary_rank_non_winsorize_tone_out[,identifier]),]
# row.names(data_summary_rank_non_winsorize_tone_out) <- seq(nrow(data_summary_rank_non_winsorize_tone_out))
# 
# write.csv(data_summary_rank_non_winsorize_tone_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Tone",".csv",sep=""),row.names=FALSE)
# 
# rm2(data_summary_rank_non_winsorize_tone_cast_data)
# rm2(data_summary_rank_non_winsorize_tone_cast0a,data_summary_rank_non_winsorize_tone_cast0b,data_summary_rank_non_winsorize_tone_cast0c)
# rm2(data_summary_rank_non_winsorize_tone_cast1,data_summary_rank_non_winsorize_tone_cast2,data_summary_rank_non_winsorize_tone_cast3)
# 
# rm2(data_summary_rank_non_winsorize_trim4,data_summary_rank_non_winsorize)
# rm2(data_summary_rank_non_winsorize_readability_out,data_summary_rank_non_winsorize_similarity_out,data_summary_rank_non_winsorize_tone_out)
# 
# rm2(data_summary_rank_lookup)
# 

###############################################################################
cat("REGRESSION - FINALIZE DATA","\n")
###############################################################################

#data_reg0 <- data_all_multivariate_full_trim
data_reg0 <- data_all_multivariate_full_trim[,!(colnames(data_all_multivariate_full_trim) %in% c("Fund_Name","Strategy"))]

data_reg <- data_reg0
#data_reg <- data_reg0[data_reg0[,paste(strat_col,"2",sep="")]!="OTHER2",]

rm2(data_reg0)
#rm2(data_all_multivariate_full)


###############################################################################
cat("REGRESSION - VARIABLES","\n")
###############################################################################

#pattern_str <- "kink_percent_PCT_ANYAVG_CUTOFF + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
#quality_str <- "num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
#nonquality_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF"
#pb_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF"
#read_str <- "ARI_ios + Coleman_Liau_ios + Flesch_Kincaid_ios + FOG_ios + SMOG_ios + avg_grade_level_ios"

#sim_type_all <- c("050pct","500pct","750pct","900pct")

strat_dvs <- paste("factor(",strat_col,")",sep="")
#strat_dvs <- paste("factor(",paste(strat_col,"2",sep=""),")",sep="")
# 
# # Continuous Dependents
# #dep_input_cont_var <- c("quality_score_trim0","quality_score_trim1","quality_score_trim2","quality_score_trim3")
# #dep_input_cont_var <- c("quality_score_trim0","quality_score_trim1")
# dep_input_cont_var <- c("quality_score_trim1")
# dep_input_cont0 <- ldply(.data=dep_input_cont_var,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=c(90,95,99),cols_out=c("score","pct"))
# 
# dep_input_cont1 <- adply(.data=dep_input_cont0,.margins=1,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=c("any","avg"),cols_out=c(colnames(dep_input_cont0),"any_avg"))
# 
# dep_input_cont2 <- adply(.data=dep_input_cont1,.margins=1,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=sprintf("%03d",c(24,36,48,60)),cols_out=c(colnames(dep_input_cont1),"cutoff"))
# 
# dep_input_cont <- data.frame(dep_input_cont2,dep_var=NA,stringsAsFactors=F)
# dep_input_cont[,"dep_var"] <- paste(dep_input_cont[,"score"],dep_input_cont[,"pct"],dep_input_cont[,"any_avg"],dep_input_cont[,"cutoff"],sep="_")
# 
# rm2(dep_input_cont_var,dep_input_cont0,dep_input_cont1,dep_input_cont2)
# 
# 
# # Binary Dependents
# dep_input_bin_var <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent","string_percent",
#                        "num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent","kink_percent")
# dep_input_bin0 <- ldply(.data=dep_input_bin_var,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=c(90,95,99),cols_out=c("score","pct"))
# 
# dep_input_bin1 <- adply(.data=dep_input_bin0,.margins=1,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=c("any","avg"),cols_out=c(colnames(dep_input_bin0),"any_avg"))
# 
# dep_input_bin2 <- adply(.data=dep_input_bin1,.margins=1,.fun=function(x,expand_col,cols_out){
#   out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
#   colnames(out) <- cols_out
#   return(out)
# },expand_col=sprintf("%03d",c(24,36,48,60)),cols_out=c(colnames(dep_input_bin1),"cutoff"))
# 
# dep_input_bin3 <- data.frame(dep_input_bin2,dep_var=NA,stringsAsFactors=F)
# dep_input_bin3[,"dep_var"] <- paste(dep_input_bin3[,"score"],dep_input_bin3[,"pct"],dep_input_bin3[,"any_avg"],dep_input_bin3[,"cutoff"],sep="_")
# 
# dep_input_bin4 <- data.frame(matrix(NA,ncol=5,nrow=length(revision_cols),dimnames=list(c(),c("score","pct","any_avg","cutoff","dep_var"))),stringsAsFactors=F)
# dep_input_bin4[,"dep_var"] <- revision_cols
# 
# dep_input_bin <- rbind(dep_input_bin3,dep_input_bin4)                           
# 
# rm2(dep_input_bin_var,dep_input_bin0,dep_input_bin1,dep_input_bin2,dep_input_bin3,dep_input_bin4)


###############################################################################
cat("REGRESSION - DESCRIPTIVE STATS","\n")
###############################################################################

descript_stats_data0 <- data_reg[,colnames(data_reg) %in% unique(c(multivariate_vars_dep,data_all_multivariate_new_cols,multivariate_vars_continuous,multivariate_vars_binary))]
descript_stats_data1 <- descript_stats_data0[,!(colnames(descript_stats_data0) %in% c(strat_col,"Primary_Investment_Strategy_combcol2"))]

descript_stats <- describe2(descript_stats_data1)

write.csv(descript_stats,file=paste(output_directory,"//","descript_stats",".csv",sep=""),row.names=FALSE)

length(unique(data_reg[,identifier]))


###############################################################################
cat("UNIVARIATE","\n")
###############################################################################

data_reg_ari_q1 <- data_reg[data_reg[,"ARI_ios"]<=as.numeric(quantile(data_reg[,"ARI_ios"],c(.25))),"pflow_Yearly"]
data_reg_ari_q4 <- data_reg[data_reg[,"ARI_ios"]>as.numeric(quantile(data_reg[,"ARI_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_ari_q1,data_reg_ari_q4)
rm(data_reg_ari_q1,data_reg_ari_q4)

data_reg_cl_q1 <- data_reg[data_reg[,"Coleman_Liau_ios"]<=as.numeric(quantile(data_reg[,"Coleman_Liau_ios"],c(.25))),"pflow_Yearly"]
data_reg_cl_q4 <- data_reg[data_reg[,"Coleman_Liau_ios"]>as.numeric(quantile(data_reg[,"Coleman_Liau_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_cl_q1,data_reg_cl_q4)
rm(data_reg_cl_q1,data_reg_cl_q4)

data_reg_fk_q1 <- data_reg[data_reg[,"Flesch_Kincaid_ios"]<=as.numeric(quantile(data_reg[,"Flesch_Kincaid_ios"],c(.25))),"pflow_Yearly"]
data_reg_fk_q4 <- data_reg[data_reg[,"Flesch_Kincaid_ios"]>as.numeric(quantile(data_reg[,"Flesch_Kincaid_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_fk_q1,data_reg_fk_q4)
rm(data_reg_fk_q1,data_reg_fk_q4)

data_reg_fog_q1 <- data_reg[data_reg[,"FOG_ios"]<=as.numeric(quantile(data_reg[,"FOG_ios"],c(.25))),"pflow_Yearly"]
data_reg_fog_q4 <- data_reg[data_reg[,"FOG_ios"]>as.numeric(quantile(data_reg[,"FOG_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_fog_q1,data_reg_fog_q4)
rm(data_reg_fog_q1,data_reg_fog_q4)

data_reg_smog_q1 <- data_reg[data_reg[,"SMOG_ios"]<=as.numeric(quantile(data_reg[,"SMOG_ios"],c(.25))),"pflow_Yearly"]
data_reg_smog_q4 <- data_reg[data_reg[,"SMOG_ios"]>as.numeric(quantile(data_reg[,"SMOG_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_smog_q1,data_reg_smog_q4)
rm(data_reg_smog_q1,data_reg_smog_q4)

data_reg_avg_q1 <- data_reg[data_reg[,"avg_grade_level_ios"]<=as.numeric(quantile(data_reg[,"avg_grade_level_ios"],c(.25))),"pflow_Yearly"]
data_reg_avg_q4 <- data_reg[data_reg[,"avg_grade_level_ios"]>as.numeric(quantile(data_reg[,"avg_grade_level_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_avg_q1,data_reg_avg_q4)
rm(data_reg_avg_q1,data_reg_avg_q4)

data_reg_all_q1 <- data_reg[data_reg[,"all_similarity_900pct_ios"]<=as.numeric(quantile(data_reg[,"all_similarity_900pct_ios"],c(.25))),"pflow_Yearly"]
data_reg_all_q4 <- data_reg[data_reg[,"all_similarity_900pct_ios"]>as.numeric(quantile(data_reg[,"all_similarity_900pct_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_all_q1,data_reg_all_q4)
rm(data_reg_all_q1,data_reg_all_q4)

data_reg_cat_q1 <- data_reg[data_reg[,"Primary_Investment_Strategy_combcol_similarity_900pct_ios"]<=as.numeric(quantile(data_reg[,"Primary_Investment_Strategy_combcol_similarity_900pct_ios"],c(.25))),"pflow_Yearly"]
data_reg_cat_q4 <- data_reg[data_reg[,"Primary_Investment_Strategy_combcol_similarity_900pct_ios"]>as.numeric(quantile(data_reg[,"Primary_Investment_Strategy_combcol_similarity_900pct_ios"],c(.75))),"pflow_Yearly"]
t.test(data_reg_cat_q1,data_reg_cat_q4)
rm(data_reg_cat_q1,data_reg_cat_q4)

data_reg_pl_q1 <- data_reg[data_reg[,"per_litigious"]<=as.numeric(quantile(data_reg[,"per_litigious"],c(.25))),"pflow_Yearly"]
data_reg_pl_q4 <- data_reg[data_reg[,"per_litigious"]>as.numeric(quantile(data_reg[,"per_litigious"],c(.75))),"pflow_Yearly"]
t.test(data_reg_pl_q1,data_reg_pl_q4)
rm(data_reg_pl_q1,data_reg_pl_q4)

data_reg_pn_q1 <- data_reg[data_reg[,"per_negative"]<=as.numeric(quantile(data_reg[,"per_negative"],c(.25))),"pflow_Yearly"]
data_reg_pn_q4 <- data_reg[data_reg[,"per_negative"]>as.numeric(quantile(data_reg[,"per_negative"],c(.75))),"pflow_Yearly"]
t.test(data_reg_pn_q1,data_reg_pn_q4)
rm(data_reg_pn_q1,data_reg_pn_q4)

data_reg_pp_q1 <- data_reg[data_reg[,"per_positive"]<=as.numeric(quantile(data_reg[,"per_positive"],c(.25))),"pflow_Yearly"]
data_reg_pp_q4 <- data_reg[data_reg[,"per_positive"]>as.numeric(quantile(data_reg[,"per_positive"],c(.75))),"pflow_Yearly"]
t.test(data_reg_pp_q1,data_reg_pp_q4)
rm(data_reg_pp_q1,data_reg_pp_q4)

data_reg_pu_q1 <- data_reg[data_reg[,"per_uncertainty"]<=as.numeric(quantile(data_reg[,"per_uncertainty"],c(.25))),"pflow_Yearly"]
data_reg_pu_q4 <- data_reg[data_reg[,"per_uncertainty"]>as.numeric(quantile(data_reg[,"per_uncertainty"],c(.75))),"pflow_Yearly"]
t.test(data_reg_pu_q1,data_reg_pu_q4)
rm(data_reg_pu_q1,data_reg_pu_q4)



###############################################################################
cat("REGRESSION - CORRELATION AND PLOTS","\n")
###############################################################################

# a <- data_all[data_all[,identifier]==5445,]
# a <- data_reg[,c(identifier,"yr","age_y","AUM","total_fee",multivariate_vars_continuous_tone,multivariate_vars_binary_fund,multivariate_vars_binary_tone)]
# a_na <- a[rowSums(is.na(a)) > 0,]
# rm(a,a_na)

#cor_data <- data_reg[,c("age_y_log","age_y","age_m_log","age_m","AUM_USm","AUM_USm_log","Fund_Size_USm","Fund_Size_USm_log","Firm_Size_combcol","Firm_Size_combcol_log","Fund_to_Firm_Size")]
#cor_data <- data_reg[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios","per_litigious","per_negative","per_positive","per_uncertainty")]
#cor_test <- cor(as.matrix(cor_data),use="pairwise.complete.obs")

# b <-  data_reg[,c(identifier,"age_y_log","age_y","age_m_log","age_m")]
# c <-  data_reg[,c(identifier,revision_cols)]

#cor_data2 <- data_reg[,c("pflow_Yearly","pflow_Yearly_lag1","sdpct_flow","sdpct_flow_lag1")]
#cor_test2 <- cor(as.matrix(cor_data2),use="pairwise.complete.obs")

#cor_data3 <- data_reg[,c("Sharpe_Ratio","Sortino_Ratio")]
#cor_test3 <- cor(as.matrix(cor_data3),use="pairwise.complete.obs")

#cor_data4 <- data_reg[,c("Yearly_Ret_lag1","Yearly_Ret_lag2","Yearly_Ret_lag3","Yearly_Ret_lag4","Yearly_Ret_sq_lag1","Yearly_Ret_sq_lag2","Yearly_Ret_sq_lag3","Yearly_Ret_sq_lag4")]
#cor_test4 <- cor(as.matrix(cor_data4),use="pairwise.complete.obs")

# plot(data_reg[,c("Yearly_Ret")])
# plot(data_reg[,c("Yearly_Ret_lag1")])

# plot(data_reg[,"pflow_Yearly"],data_reg[,"age_m_Yearly"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"age_m_Yearly_log"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"age_y_Yearly"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"age_y_Yearly_log"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"AUM_Yearly_USm"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"AUM_Yearly_log_USm"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"total_fee"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Yearly_Ret_lag1"])

# plot(data_reg[,"pflow_Yearly"],data_reg[,"ARI_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Coleman_Liau_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Flesch_Kincaid_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"FOG_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"SMOG_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"avg_grade_level_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"words_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"words_log_ios"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_050pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_100pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_250pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_500pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_750pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"all_similarity_900pct_ios"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_050pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_100pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_250pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_500pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_750pct_ios"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"Primary_Investment_Strategy_combcol_similarity_900pct_ios"])
# 
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_litigious"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_modalstrong"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_modalweak"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_negative"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_positive"])
# plot(data_reg[,"pflow_Yearly"],data_reg[,"per_uncertainty"])



###############################################################################
cat("REGRESSION - YEARLY PERCENTAGE FLOW - SELECTION","\n")
###############################################################################

output_dir_reg0 <- paste(output_directory,"reg_selection_cont","\\",sep="")
create_directory(output_dir_reg0,remove=1)

data_year_groups0 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups0[1,] <- c(beg_year,end_year)

#dep_var0 <- unlist(dep_input_cont[,"dep_var"])
dep_var0 <- "pflow_Yearly"
#controls0 <- c("age_y","AUM","total_fee",multivariate_vars_continuous_tone,multivariate_vars_binary_fund,multivariate_vars_binary_tone)
controls0 <- c("age_m_Yearly_log","AUM_Yearly_log_lag1_USm",
               "total_fee","Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin",
               "sdpct_flow","Sharpe_Ratio",
               "Yearly_Ret_lag1","Yearly_Ret_lag2","Yearly_Ret_lag3","Yearly_Ret_lag4",
               "Yearly_Ret_sq_lag1","Yearly_Ret_sq_lag2","Yearly_Ret_sq_lag3","Yearly_Ret_sq_lag4",
               multivariate_vars_binary_fund,"Revision_DV")
model_type0 <- "pooling"
note0 <- "all_sim"
#sim_type0 <- c("900pct")
#sim_type0 <- c("050pct","900pct")
#sim_type0 <- c("050pct","750pct","900pct")
#sim_type0 <- c("050pct","500pct","750pct","900pct")
sim_type0 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
read1 <- c("ARI_XXX","Coleman_Liau_XXX","Flesch_Kincaid_XXX","FOG_XXX","SMOG_XXX","avg_grade_level_XXX","words_XXX","words_log_XXX")
strat1 <- c("all_similarity_YYYpct_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX")

# regression_equations0_1 <- c("avg_grade_level_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0)
# regression_equations0_2 <- c("avg_grade_level_XXX","all_similarity_YYYpct_XXX",controls0)
# regression_equations0_3 <- c("FOG_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0)
# #regression_equations0_3 <- c("Coleman_Liau_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0)
# regression_equations0_4 <- c("FOG_XXX","all_similarity_YYYpct_XXX",controls0)
# #regression_equations0_4 <- c("Coleman_Liau_XXX","all_similarity_YYYpct_XXX",controls0)
# regression_equations0_5 <- c("avg_grade_level_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0,strat_dvs)
# regression_equations0_6 <- c("avg_grade_level_XXX","all_similarity_YYYpct_XXX",controls0,strat_dvs)
# regression_equations0_7 <- c("FOG_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0,strat_dvs)
# #regression_equations0_7 <- c("Coleman_Liau_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0,strat_dvs)
# regression_equations0_8 <- c("FOG_XXX","all_similarity_YYYpct_XXX",controls0,strat_dvs)
# #regression_equations0_8 <- c("Coleman_Liau_XXX","all_similarity_YYYpct_XXX",controls0,strat_dvs)

regression_equations0_1 <- c(read1,controls0)
regression_equations0_2 <- c("all_similarity_YYYpct_XXX",controls0)
regression_equations0_3 <- c("Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0)
regression_equations0_4 <- c("all_similarity_YYYpct_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0)
regression_equations0_5 <- c(multivariate_vars_continuous_tone,controls0)
regression_equations0_6 <- c(multivariate_vars_binary_tone,controls0)
regression_equations0_7 <- c(read1,"all_similarity_YYYpct_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",multivariate_vars_binary_tone,multivariate_vars_continuous_tone,controls0)

regression_equations0_8 <- c(read1,controls0,strat_dvs)
regression_equations0_9 <- c("all_similarity_YYYpct_XXX",controls0,strat_dvs)
regression_equations0_10<- c("Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0,strat_dvs)
regression_equations0_11<- c("all_similarity_YYYpct_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls0,strat_dvs)
regression_equations0_12<- c(multivariate_vars_continuous_tone,controls0,strat_dvs)
regression_equations0_13<- c(multivariate_vars_binary_tone,controls0,strat_dvs)
regression_equations0_14<- c(read1,"all_similarity_YYYpct_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",multivariate_vars_binary_tone,multivariate_vars_continuous_tone,controls0,strat_dvs)

regression_equations0 <- list(regression_equations0_1,regression_equations0_2,regression_equations0_3,regression_equations0_4,regression_equations0_5,regression_equations0_6,regression_equations0_7,
                              regression_equations0_8,regression_equations0_9,regression_equations0_10,regression_equations0_11,regression_equations0_12,regression_equations0_13,regression_equations0_14)
rm2(regression_equations0_1,regression_equations0_2,regression_equations0_3,regression_equations0_4,regression_equations0_5,regression_equations0_6,regression_equations0_7)
rm2(regression_equations0_8,regression_equations0_9,regression_equations0_10,regression_equations0_11,regression_equations0_12,regression_equations0_13,regression_equations0_14)

for (k in 1:nrow(data_year_groups0))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups0[k,1],"END YEAR:",data_year_groups0[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups0[k,1] & data_all[,"yr"]<=data_year_groups0[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups0[k,1] & data_reg[,"yr"]<=data_year_groups0[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var0))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type0))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var0[i],data_year_groups0[k,1],data_year_groups0[k,2],note0,sim_type0[j],sep="_")
      
      #FORWARD SELECTION
      
      se_fs <- rep(list(list()),length(regression_equations0))
      pval_fs <- rep(list(list()),length(regression_equations0))
      
      for (l in 1:length(regression_equations0))
      {
        # l <- 1
        # l <- 8
        
        ind_vars_reg0 <- regression_equations0[[l]]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type0[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0 <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0,perl=T),ind_vars_reg0)
        
        model_data <- data_temp[,c(dep_var0[i],ind_vars_reg0)]
        model_data_trim <- model_data[complete.cases(model_data),]
        
        reg0_forward <- lm(as.formula(paste(dep_var0[i],"1",sep="~")),data=model_data_trim,na.action=na.omit)
        reg0 <- stepAIC(reg0_forward,direction="forward",scope=paste("~",paste(ind_vars_reg0,sep="",collapse=" + "),sep=""),trace=F)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var0[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type0)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #se_fs[[l]] <- reg0_rse[,2]
        se_fs[[l]] <- reg0_rse[,4]
        pval_fs[[l]] <- reg0_rse[,4]
        
        assign(paste("reg_fs",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm(ind_vars_reg0,model_data,model_data_trim,reg0_forward,reg0,reg0_rse)
      }
      rm(l)
      
      #BACKWARD ELIMINATION
      
      se_be <- rep(list(list()),length(regression_equations0))
      pval_be <- rep(list(list()),length(regression_equations0))
      
      for (l in 1:length(regression_equations0))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations0[[l]]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type0[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0 <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0,perl=T),ind_vars_reg0)
        
        model_data <- data_temp[,c(dep_var0[i],ind_vars_reg0)]
        model_data_trim <- model_data[complete.cases(model_data),]
        
        reg0_backward <- lm(as.formula(paste(dep_var0[i],paste(ind_vars_reg0,sep="",collapse=" + "),sep="~")),data=model_data_trim,na.action=na.omit)
        reg0 <- stepAIC(reg0_backward,direction="backward",scope=paste("~",paste(ind_vars_reg0,sep="",collapse=" + "),sep=""),trace=F)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var0[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type0)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #se_be[[l]] <- reg0_rse[,2]
        se_be[[l]] <- reg0_rse[,4]
        pval_be[[l]] <- reg0_rse[,4]
        
        assign(paste("reg_be",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm(ind_vars_reg0,model_data,model_data_trim,reg0_backward,reg0,reg0_rse)
      }
      rm(l)
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg_fs",seq(1,length(regression_equations0)),sep="",collapse=","),",",paste("reg_be",seq(1,length(regression_equations0)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",c(paste(seq(1,length(regression_equations0)),"FS",sep="_"),paste(seq(1,length(regression_equations0)),"BE",sep="_")),")",sep=""),
              override.se=c(se_fs,se_be),override.pval=c(pval_fs,pval_be),stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg0,out_file_name,".doc",sep=""))
      
      rm(se_fs,se_be,pval_fs,pval_be,out_file_name)
      eval(parse(text=paste("rm(",paste("reg_fs",seq(1,length(regression_equations0)),sep="",collapse=","),")",sep="")))
      eval(parse(text=paste("rm(",paste("reg_be",seq(1,length(regression_equations0)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg0,data_year_groups0,dep_var0,model_type0,note0,sim_type0,controls0,regression_equations0)


###############################################################################
cat("REGRESSION -  YEARLY PERCENTAGE FLOW - READABILITY","\n")
###############################################################################

output_dir_reg1 <- paste(output_directory,"reg_read_cont","\\",sep="")
create_directory(output_dir_reg1,remove=1)

data_year_groups1 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups1[1,] <- c(beg_year,end_year)

dep_var1 <- "pflow_Yearly"
#controls1 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin + Flagship_bin + Listed_on_Exchange_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls1 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls1 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls1 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls1  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls1  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
controls1  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"

model_type1 <- "pooling"
note1 <- "read"
sim_type1 <- c("500pct")
#read1 <- "avg_grade_level_XXX"
#read1 <- "FOG_XXX"
read1 <- "Coleman_Liau_XXX"
#read1 <- "words_ios"
#read1 <- "words_log_ios"
strat1 <- "all_similarity_YYYpct_XXX"

regression_equations1 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
# regression_equations1[01,] <- c("ARI_XXX",NA,NA,NA,NA,NA)
# regression_equations1[02,] <- c("Coleman_Liau_XXX",NA,NA,NA,NA,NA)
# regression_equations1[03,] <- c("Flesch_Kincaid_XXX",NA,NA,NA,NA,NA)
# regression_equations1[04,] <- c("FOG_XXX",NA,NA,NA,NA,NA)
# regression_equations1[05,] <- c("SMOG_XXX",NA,NA,NA,NA,NA)
# regression_equations1[06,] <- c("avg_grade_level_XXX",NA,NA,NA,NA,NA)
# regression_equations1[07,] <- c("words_XXX",NA,NA,NA,NA,NA)
# regression_equations1[08,] <- c("words_log_XXX",NA,NA,NA,NA,NA)
# regression_equations1[09,] <- c("ARI_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[10,] <- c("Coleman_Liau_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[11,] <- c("Flesch_Kincaid_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[12,] <- c("FOG_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[13,] <- c("SMOG_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[14,] <- c("avg_grade_level_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[15,] <- c("words_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[16,] <- c("words_log_XXX",NA,controls1,NA,NA,NA)
# regression_equations1[17,] <- c("ARI_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[18,] <- c("Coleman_Liau_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[19,] <- c("Flesch_Kincaid_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[20,] <- c("FOG_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[21,] <- c("SMOG_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[22,] <- c("avg_grade_level_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[23,] <- c("words_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[24,] <- c("words_log_XXX",NA,controls1,NA,strat_dvs,NA)
# regression_equations1[25,] <- c("ARI_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[26,] <- c("Coleman_Liau_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[27,] <- c("Flesch_Kincaid_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[28,] <- c("FOG_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[29,] <- c("SMOG_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[30,] <- c("avg_grade_level_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[31,] <- c("words_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")
# regression_equations1[32,] <- c("words_log_XXX",NA,controls1,NA,strat_dvs,"factor(yr)")

regression_equations1[01,] <- c("ARI_XXX",NA,NA,NA,NA,NA)
regression_equations1[02,] <- c("Coleman_Liau_XXX",NA,NA,NA,NA,NA)
regression_equations1[03,] <- c("Flesch_Kincaid_XXX",NA,NA,NA,NA,NA)
regression_equations1[04,] <- c("FOG_XXX",NA,NA,NA,NA,NA)
regression_equations1[05,] <- c("SMOG_XXX",NA,NA,NA,NA,NA)
regression_equations1[06,] <- c("avg_grade_level_XXX",NA,NA,NA,NA,NA)
regression_equations1[07,] <- c("ARI_XXX",NA,controls1,NA,NA,NA)
regression_equations1[08,] <- c("Coleman_Liau_XXX",NA,controls1,NA,NA,NA)
regression_equations1[09,] <- c("Flesch_Kincaid_XXX",NA,controls1,NA,NA,NA)
regression_equations1[10,] <- c("FOG_XXX",NA,controls1,NA,NA,NA)
regression_equations1[11,] <- c("SMOG_XXX",NA,controls1,NA,NA,NA)
regression_equations1[12,] <- c("avg_grade_level_XXX",NA,controls1,NA,NA,NA)
regression_equations1[13,] <- c("ARI_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[14,] <- c("Coleman_Liau_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[15,] <- c("Flesch_Kincaid_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[16,] <- c("FOG_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[17,] <- c("SMOG_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[18,] <- c("avg_grade_level_XXX",NA,controls1,NA,strat_dvs,NA)
regression_equations1[19,] <- c("ARI_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)
regression_equations1[20,] <- c("Coleman_Liau_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)
regression_equations1[21,] <- c("Flesch_Kincaid_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)
regression_equations1[22,] <- c("FOG_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)
regression_equations1[23,] <- c("SMOG_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)
regression_equations1[24,] <- c("avg_grade_level_XXX",NA,controls1,"factor(yr)",strat_dvs,NA)

for (i in 1:ncol(regression_equations1))
{
  #regression_equations1[i,] <-  gsub("PCT","90",regression_equations1[i,])
  #regression_equations1[i,] <-  gsub("ANYAVG","any",regression_equations1[i,])
  #regression_equations1[i,] <-  gsub("CUTOFF","024",regression_equations1[i,])
  regression_equations1[i,] <-  unknown_to_NA(regression_equations1[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations1))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations1[i,1:(ncol(regression_equations1)-1)],use.names=F))))
  regression_equations1[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}

for (k in 1:nrow(data_year_groups1))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups1[k,1],"END YEAR:",data_year_groups1[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups1[k,1] & data_all[,"yr"]<=data_year_groups1[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups1[k,1] & data_reg[,"yr"]<=data_year_groups1[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var1))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type1))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var1[i],data_year_groups1[k,1],data_year_groups1[k,2],note1,sim_type1[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations1) )
      se <- rep(list(list()),nrow(regression_equations1))
      pval <- rep(list(list()),nrow(regression_equations1))
      
      for (l in 1:nrow(regression_equations1))
      {
        # l <- 1
        # l <- 7
        # l <- 8
        # l <- 24
        
        ind_vars_reg0 <- regression_equations1[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type1[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0_list <- gsub("^\\s+|\\s+$","",unlist(strsplit(ind_vars_reg0,split="\\+")),perl=T)
        ind_vars_reg0_list <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0_list,perl=T),ind_vars_reg0_list)
        
        model_data <- data_temp[,c(dep_var1[i],ind_vars_reg0_list)]
        
        reg0 <- lm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),model_data)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data=model_data,model=model_type1)
        #reg0_rse <- cl.plm(model_data,reg0,model_data[,identifier])
        #reg0_rse <- mcl.plm(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        #reg0_rse <- mcl(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,model_data,ind_vars_reg0_list,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations1)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg1,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg1,data_year_groups1,dep_var1,model_type1,note1,sim_type1,controls1,read1,strat1,regression_equations1,temp_char_vec,k)


###############################################################################
cat("REGRESSION -  YEARLY PERCENTAGE FLOW - ALL SIM","\n")
###############################################################################

output_dir_reg2 <- paste(output_directory,"reg_all_sim_cont","\\",sep="")
create_directory(output_dir_reg2,remove=1)

data_year_groups2 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups2[1,] <- c(beg_year,end_year)

dep_var2 <- "pflow_Yearly"
#controls2 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin + Flagship_bin + Listed_on_Exchange_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls2 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls2 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls2 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls2 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls2  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
controls2  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"

model_type2 <- "pooling"
note2 <- "all_sim"
#sim_type2 <- c("900pct")
#sim_type2 <- c("050pct","900pct")
#sim_type2 <- c("050pct","750pct","900pct")
#sim_type2 <- c("050pct","500pct","750pct","900pct")
#sim_type2 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type2 <- c("050pct","500pct","900pct")
#read2 <- "avg_grade_level_XXX"
#read2 <- "FOG_XXX"
read2 <- "Coleman_Liau_XXX"
#read2 <- "words_ios"
#read2 <- "words_log_ios"
strat2 <- "all_similarity_YYYpct_XXX"

regression_equations2 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations2[01,] <- c(read2,NA,NA,NA,NA,NA)
regression_equations2[02,] <- c(strat2,NA,NA,NA,NA,NA)
regression_equations2[03,] <- c(paste(read2,strat2,sep=" + "),NA,NA,NA,NA,NA)
regression_equations2[04,] <- c(read2,NA,controls2,NA,NA,NA)
regression_equations2[05,] <- c(strat2,NA,controls2,NA,NA,NA)
regression_equations2[06,] <- c(paste(read2,strat2,sep=" + "),NA,controls2,NA,NA,NA)
regression_equations2[07,] <- c(read2,NA,controls2,NA,strat_dvs,NA)
regression_equations2[08,] <- c(strat2,NA,controls2,NA,strat_dvs,NA)
regression_equations2[09,] <- c(paste(read2,strat2,sep=" + "),NA,controls2,NA,strat_dvs,NA)
regression_equations2[10,] <- c(read2,NA,controls2,"factor(yr)",strat_dvs,NA)
regression_equations2[11,] <- c(strat2,NA,controls2,"factor(yr)",strat_dvs,NA)
regression_equations2[12,] <- c(paste(read2,strat2,sep=" + "),NA,controls2,"factor(yr)",strat_dvs,NA)


for (i in 1:ncol(regression_equations2))
{
  #regression_equations2[i,] <-  gsub("PCT","90",regression_equations2[i,])
  #regression_equations2[i,] <-  gsub("ANYAVG","any",regression_equations2[i,])
  #regression_equations2[i,] <-  gsub("CUTOFF","024",regression_equations2[i,])
  regression_equations2[i,] <-  unknown_to_NA(regression_equations2[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations2))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations2[i,1:(ncol(regression_equations2)-1)],use.names=F))))
  regression_equations2[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}

for (k in 1:nrow(data_year_groups2))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups2[k,1],"END YEAR:",data_year_groups2[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups2[k,1] & data_all[,"yr"]<=data_year_groups2[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups2[k,1] & data_reg[,"yr"]<=data_year_groups2[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var2))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type2))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var2[i],data_year_groups2[k,1],data_year_groups2[k,2],note2,sim_type2[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations2) )
      se <- rep(list(list()),nrow(regression_equations2))
      pval <- rep(list(list()),nrow(regression_equations2))
      
      for (l in 1:nrow(regression_equations2))
      {
        # l <- 1
        # l <- 7
        # l <- 8
        
        ind_vars_reg0 <- regression_equations2[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type2[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0_list <- gsub("^\\s+|\\s+$","",unlist(strsplit(ind_vars_reg0,split="\\+")),perl=T)
        ind_vars_reg0_list <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0_list,perl=T),ind_vars_reg0_list)
        
        model_data <- data_temp[,c(dep_var2[i],ind_vars_reg0_list)]
        
        reg0 <- lm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")),model_data)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")),data=model_data,model=model_type2)
        #reg0_rse <- cl.plm(model_data,reg0,model_data[,identifier])
        #reg0_rse <- mcl.plm(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        #reg0_rse <- mcl(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,model_data,ind_vars_reg0_list,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations2)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg2,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg2,data_year_groups2,dep_var2,model_type2,note2,sim_type2,controls2,read2,strat2,regression_equations2,temp_char_vec,k)


###############################################################################
cat("REGRESSION - YEARLY PERCENTAGE FLOW - CAT SIM","\n")
###############################################################################

output_dir_reg3 <- paste(output_directory,"reg_cat_sim_cont","\\",sep="")
create_directory(output_dir_reg3,remove=1)

data_year_groups3 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups3[1,] <- c(beg_year,end_year)

dep_var3 <- "pflow_Yearly"
#controls3 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin + Flagship_bin + Listed_on_Exchange_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls3 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls3 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls3 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls3 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls3  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
controls3  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"

model_type3 <- "pooling"
note3 <- "cat_sim"
#sim_type3 <- c("900pct")
#sim_type3 <- c("050pct","900pct")
#sim_type3 <- c("050pct","750pct","900pct")
#sim_type3 <- c("050pct","500pct","750pct","900pct")
#sim_type3 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type3 <- c("050pct","500pct","900pct")
#read3 <- "avg_grade_level_XXX"
#read3 <- "FOG_XXX"
read3 <- "Coleman_Liau_XXX"
#read3 <- "words_ios"
#read3 <- "words_log_ios"
strat3 <- "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX"

regression_equations3 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations3[01,] <- c(read3,NA,NA,NA,NA,NA)
regression_equations3[02,] <- c(strat3,NA,NA,NA,NA,NA)
regression_equations3[03,] <- c(paste(read3,strat3,sep=" + "),NA,NA,NA,NA,NA)
regression_equations3[04,] <- c(read3,NA,controls3,NA,NA,NA)
regression_equations3[05,] <- c(strat3,NA,controls3,NA,NA,NA)
regression_equations3[06,] <- c(paste(read3,strat3,sep=" + "),NA,controls3,NA,NA,NA)
regression_equations3[07,] <- c(read3,NA,controls3,NA,strat_dvs,NA)
regression_equations3[08,] <- c(strat3,NA,controls3,NA,strat_dvs,NA)
regression_equations3[09,] <- c(paste(read3,strat3,sep=" + "),NA,controls3,NA,strat_dvs,NA)
regression_equations3[10,] <- c(read3,NA,controls3,"factor(yr)",strat_dvs,NA)
regression_equations3[11,] <- c(strat3,NA,controls3,"factor(yr)",strat_dvs,NA)
regression_equations3[12,] <- c(paste(read3,strat3,sep=" + "),NA,controls3,"factor(yr)",strat_dvs,NA)

for (i in 1:ncol(regression_equations3))
{
  #regression_equations3[i,] <-  gsub("PCT","90",regression_equations3[i,])
  #regression_equations3[i,] <-  gsub("ANYAVG","any",regression_equations3[i,])
  #regression_equations3[i,] <-  gsub("CUTOFF","024",regression_equations3[i,])
  regression_equations3[i,] <-  unknown_to_NA(regression_equations3[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations3))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations3[i,1:(ncol(regression_equations3)-1)],use.names=F))))
  regression_equations3[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}


for (k in 1:nrow(data_year_groups3))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups3[k,1],"END YEAR:",data_year_groups3[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups3[k,1] & data_all[,"yr"]<=data_year_groups3[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups3[k,1] & data_reg[,"yr"]<=data_year_groups3[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var3))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type3))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var3[i],data_year_groups3[k,1],data_year_groups3[k,2],note3,sim_type3[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations3) )
      se <- rep(list(list()),nrow(regression_equations3))
      pval <- rep(list(list()),nrow(regression_equations3))
      
      for (l in 1:nrow(regression_equations3))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations3[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type3[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type3)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations3)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg3,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg3,data_year_groups3,dep_var3,model_type3,note3,sim_type3,controls3,read3,strat3,regression_equations3,temp_char_vec,k)


###############################################################################
cat("REGRESSION - YEARLY PERCENTAGE FLOW - ALL AND CAT SIM","\n")
###############################################################################

output_dir_reg4 <- paste(output_directory,"reg_all_and_cat_sim_cont","\\",sep="")
create_directory(output_dir_reg4,remove=1)

data_year_groups4 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups4[1,] <- c(beg_year,end_year)

dep_var4 <- "pflow_Yearly"
#controls4 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin + Flagship_bin + Listed_on_Exchange_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls4 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls4 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls4 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls4  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls4  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
controls4  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"

model_type4 <- "pooling"
note4 <- "all_cat_sim"
#sim_type4 <- c("900pct")
#sim_type4 <- c("050pct","900pct")
#sim_type4 <- c("050pct","750pct","900pct")
#sim_type4 <- c("050pct","500pct","750pct","900pct")
#sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type4 <- c("050pct","500pct","900pct")
read4 <- "all_similarity_YYYpct_XXX"
strat4 <- "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX"

regression_equations4 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations4[01,] <- c(read4,NA,NA,NA,NA,NA)
regression_equations4[02,] <- c(strat4,NA,NA,NA,NA,NA)
regression_equations4[03,] <- c(paste(read4,strat4,sep=" + "),NA,NA,NA,NA,NA)
regression_equations4[04,] <- c(read4,NA,controls4,NA,NA,NA)
regression_equations4[05,] <- c(strat4,NA,controls4,NA,NA,NA)
regression_equations4[06,] <- c(paste(read4,strat4,sep=" + "),NA,controls4,NA,NA,NA)
regression_equations4[07,] <- c(read4,NA,controls4,NA,strat_dvs,NA)
regression_equations4[08,] <- c(strat4,NA,controls4,NA,strat_dvs,NA)
regression_equations4[09,] <- c(paste(read4,strat4,sep=" + "),NA,controls4,NA,strat_dvs,NA)
regression_equations4[10,] <- c(read4,NA,controls4,"factor(yr)",strat_dvs,NA)
regression_equations4[11,] <- c(strat4,NA,controls4,"factor(yr)",strat_dvs,NA)
regression_equations4[12,] <- c(paste(read4,strat4,sep=" + "),NA,controls4,"factor(yr)",strat_dvs,NA)


for (i in 1:ncol(regression_equations4))
{
  #regression_equations4[i,] <-  gsub("PCT","90",regression_equations4[i,])
  #regression_equations4[i,] <-  gsub("ANYAVG","any",regression_equations4[i,])
  #regression_equations4[i,] <-  gsub("CUTOFF","024",regression_equations4[i,])
  regression_equations4[i,] <-  unknown_to_NA(regression_equations4[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations4))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)],use.names=F))))
  regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}


for (k in 1:nrow(data_year_groups4))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups4[k,1],"END YEAR:",data_year_groups4[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups4[k,1] & data_reg[,"yr"]<=data_year_groups4[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var4))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type4))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations4) )
      se <- rep(list(list()),nrow(regression_equations4))
      pval <- rep(list(list()),nrow(regression_equations4))
      
      for (l in 1:nrow(regression_equations4))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg4,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg4,data_year_groups4,dep_var4,model_type4,note4,sim_type4,controls4,read4,strat4,regression_equations4,temp_char_vec,k)



###############################################################################
cat("REGRESSION - YEARLY PERCENTAGE FLOW - TONE","\n")
###############################################################################

output_dir_reg5 <- paste(output_directory,"reg_tone_cont","\\",sep="")
create_directory(output_dir_reg5,remove=1)

data_year_groups5 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups5[1,] <- c(beg_year,end_year)

dep_var5 <- "pflow_Yearly"
#controls5 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin + Flagship_bin + Listed_on_Exchange_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls5 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Revision_DV + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_lag3 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls5 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls5 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls5 <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls5  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + total_fee + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2 + Performance_Fee_bin"
#controls5  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Hurdle_Rate_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
#controls5  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Sharpe_Ratio + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"
controls5  <- "age_m_Yearly_log + AUM_Yearly_log_lag1_USm + Lockup_bin + Yearly_Ret_lag1 + Yearly_Ret_lag2 + Yearly_Ret_sq_lag1 + Yearly_Ret_sq_lag2"

model_type5 <- "pooling"
note5 <- "tone"
sim_type5 <- c("500pct")

regression_equations5 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)

regression_equations5[01,] <- c("per_litigious",NA,NA,NA,NA,NA)
regression_equations5[02,] <- c("per_negative",NA,NA,NA,NA,NA)
regression_equations5[03,] <- c("per_positive",NA,NA,NA,NA,NA)
regression_equations5[04,] <- c("per_uncertainty",NA,NA,NA,NA,NA)
regression_equations5[05,] <- c("per_litigious",NA,controls5,NA,NA,NA)
regression_equations5[06,] <- c("per_negative",NA,controls5,NA,NA,NA)
regression_equations5[07,] <- c("per_positive",NA,controls5,NA,NA,NA)
regression_equations5[08,] <- c("per_uncertainty",NA,controls5,NA,NA,NA)
regression_equations5[09,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls5,NA,NA,NA)
regression_equations5[10,] <- c("per_litigious",NA,controls5,NA,strat_dvs,NA)
regression_equations5[11,] <- c("per_negative",NA,controls5,NA,strat_dvs,NA)
regression_equations5[12,] <- c("per_positive",NA,controls5,NA,strat_dvs,NA)
regression_equations5[13,] <- c("per_uncertainty",NA,controls5,NA,strat_dvs,NA)
regression_equations5[14,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls5,NA,strat_dvs,NA)
regression_equations5[15,] <- c("per_litigious",NA,controls5,"factor(yr)",strat_dvs,NA)
regression_equations5[16,] <- c("per_negative",NA,controls5,"factor(yr)",strat_dvs,NA)
regression_equations5[17,] <- c("per_positive",NA,controls5,"factor(yr)",strat_dvs,NA)
regression_equations5[18,] <- c("per_uncertainty",NA,controls5,"factor(yr)",strat_dvs,NA)
regression_equations5[19,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls5,"factor(yr)",strat_dvs,NA)


regression_equations5 <- unknown_to_NA(regression_equations5,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations5))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations5[i,1:(ncol(regression_equations5)-1)],use.names=F))))
  regression_equations5[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}
# for (i in 1:ncol(regression_equations5))
# {
#   regression_equations5[i,] <-  gsub("PCT","90",regression_equations5[i,])
#   regression_equations5[i,] <-  gsub("ANYAVG","any",regression_equations5[i,])
#   regression_equations5[i,] <-  gsub("CUTOFF","024",regression_equations5[i,])
# }

for (k in 1:nrow(data_year_groups5))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups5[k,1],"END YEAR:",data_year_groups5[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups5[k,1] & data_all[,"yr"]<=data_year_groups5[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups5[k,1] & data_reg[,"yr"]<=data_year_groups5[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var5))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type5))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var5[i],data_year_groups5[k,1],data_year_groups5[k,2],note5,sim_type5[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations5) )
      se <- rep(list(list()),nrow(regression_equations5))
      pval <- rep(list(list()),nrow(regression_equations5))
      
      for (l in 1:nrow(regression_equations5))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations5[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type5[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type5)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations5)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg5,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg5,data_year_groups5,dep_var5,model_type5,note5,sim_type5,controls5,regression_equations5,temp_char_vec,k)

