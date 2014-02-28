# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Stats.R
# Version: 1.0
# Date:    02.27.2014
# Purpose: Compute descriptive statistics and regressions
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

rm(list=ls(all=TRUE))                              #Clear workspace

Sys.setenv(R_HISTSIZE=500)                         #Limit History so that it never contains more than 50 lines

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=FALSE)
options(StringsAsFactors=FALSE)                    #String as factors is False -- used for read.csv
#options(max.print=99999)                                   
options(max.print=500)                               #Default maxprint option

#memory.limit(size=2047)                              #Memory limit default
#memory.limit(size=3000)                              #Increase memory limit to 3000 mb (3 gb)


# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1

if (Location == 1) {
  #setwd("C:/Research_temp2/")
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp2/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
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
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

describe2 <- function(x){
  
  #x <- descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))]
  
  var <- colnames(x)
  var <- as.data.frame(var, stringsAsFactors=FALSE)
  
  get_stats <- function(column,data){
    
    #column <- "sentences_ios"
    #data <- x
    #group_var <- group
    
    text01 <- paste0("var='",column,"',")
    text02 <- paste0("n=sum(!is.na(",column,")),") 
    text03 <- paste0("mean=mean(",column,",na.rm=TRUE),")
    text04 <- paste0("sd=sd(",column,",na.rm=TRUE),")
    text05 <- paste0("mode=names(sort(-table(",column,")))[1],")
    text06 <- paste0("mad=mad(",column,",na.rm=TRUE),")
    text07 <- paste0("range=max(",column,",na.rm=TRUE)-min(",column,",na.rm=TRUE),")
    text08 <- paste0("skew=skew(",column,", na.rm=TRUE,type=3),")
    text09 <- paste0("kurtosis=kurtosi(",column,", na.rm=TRUE,type=3),")
    text10 <- paste0("se=(sd(",column,",na.rm=TRUE)/sqrt(sum(!is.na(",column,")))),")
    text11 <- paste0("min=min(",column,",na.rm=TRUE),")
    text12 <- paste0("decile1=quantile(",  column,", probs=0.10,na.rm=TRUE),")
    text13 <- paste0("quintile1=quantile(",column,", probs=0.20,na.rm=TRUE),")
    text14 <- paste0("quartile1=quantile(",column,", probs=0.25,na.rm=TRUE),")
    text15 <- paste0("decile3=quantile(",  column,", probs=0.30,na.rm=TRUE),")
    text16 <- paste0("quintile2=quantile(",column,", probs=0.40,na.rm=TRUE),")
    text17 <- paste0("median=quantile(",   column,", probs=0.50,na.rm=TRUE),")
    text18 <- paste0("quintile3=quantile(",column,", probs=0.60,na.rm=TRUE),")
    text19 <- paste0("decile7=quantile(",  column,", probs=0.70,na.rm=TRUE),")
    text20 <- paste0("quartile3=quantile(",column,", probs=0.75,na.rm=TRUE),")
    text21 <- paste0("quintile4=quantile(",column,", probs=0.80,na.rm=TRUE),")
    text22 <- paste0("decile9=quantile(",  column,", probs=0.90,na.rm=TRUE),")
    text23 <- paste0("max=max(",column,",na.rm=TRUE)")
    
    str <-  paste0("list(",text01,text02,text03,text04,text05,text06,text07,text08,text09,text10,
                   text11,text12,text13,text14,text15,text16,text17,text18,text19,text20,
                   text21,text22,text23,")")
    expr <- parse(text=str)
    
    a_dt <- data.table(data)
    b <- as.data.frame(a_dt[,eval(expr)], stringsAsFactors=FALSE)
    
    return(b)
    
  }
  
  cc <- apply(var, 1,get_stats, data=x)
  dd <- do.call("rbind", cc)
  
  return(dd)
  
}

describeBy2 <- function(x,group){
  
  #x <- descrip_stats_ios2
  #group <- "yr"
  
  var <- colnames(x[,-match(group,names(x))])
  var <- as.data.frame(var, stringsAsFactors=FALSE)
  
  get_stats_yr <- function(column,data,group_var){
    
    #column <- "sentences_ios"
    #data <- x
    #group_var <- group
    
    text01 <- paste0("var='",column,"',")
    text02 <- paste0("n=sum(!is.na(",column,")),") 
    text03 <- paste0("mean=mean(",column,",na.rm=TRUE),")
    text04 <- paste0("sd=sd(",column,",na.rm=TRUE),")
    text05 <- paste0("mode=names(sort(-table(",column,")))[1],")
    text06 <- paste0("mad=mad(",column,",na.rm=TRUE),")
    text07 <- paste0("range=max(",column,",na.rm=TRUE)-min(",column,",na.rm=TRUE),")
    text08 <- paste0("skew=skew(",column,", na.rm=TRUE,type=3),")
    text09 <- paste0("kurtosis=kurtosi(",column,", na.rm=TRUE,type=3),")
    text10 <- paste0("se=(sd(",column,",na.rm=TRUE)/sqrt(sum(!is.na(",column,")))),")
    text11 <- paste0("min=min(",column,",na.rm=TRUE),")
    text12 <- paste0("decile1=quantile(",  column,", probs=0.10,na.rm=TRUE),")
    text13 <- paste0("quintile1=quantile(",column,", probs=0.20,na.rm=TRUE),")
    text14 <- paste0("quartile1=quantile(",column,", probs=0.25,na.rm=TRUE),")
    text15 <- paste0("decile3=quantile(",  column,", probs=0.30,na.rm=TRUE),")
    text16 <- paste0("quintile2=quantile(",column,", probs=0.40,na.rm=TRUE),")
    text17 <- paste0("median=quantile(",   column,", probs=0.50,na.rm=TRUE),")
    text18 <- paste0("quintile3=quantile(",column,", probs=0.60,na.rm=TRUE),")
    text19 <- paste0("decile7=quantile(",  column,", probs=0.70,na.rm=TRUE),")
    text20 <- paste0("quartile3=quantile(",column,", probs=0.75,na.rm=TRUE),")
    text21 <- paste0("quintile4=quantile(",column,", probs=0.80,na.rm=TRUE),")
    text22 <- paste0("decile9=quantile(",  column,", probs=0.90,na.rm=TRUE),")
    text23 <- paste0("max=max(",column,",na.rm=TRUE)")
    
    str <-  paste0("list(",text01,text02,text03,text04,text05,text06,text07,text08,text09,text10,
                   text11,text12,text13,text14,text15,text16,text17,text18,text19,text20,
                   text21,text22,text23,")")
    expr <- parse(text=str)
    
    a_dt <- data.table(data,c(group_var))
    b <- as.data.frame(a_dt[,eval(expr),by=group_var], stringsAsFactors=FALSE)
    
    return(b)
    
  }
  
  cc <- apply(var, 1,get_stats_yr, data=x,group_var=group)
  dd <- do.call("rbind", cc)
  
  return(dd[order(dd[,group]),])
  
}

quantile_dvs <- function(w,data,group_var,quantile_data,quantile_col_low,quantile_col_high){
  #w <- quintile_vars_ios[1]
  #data <- text_stats_ios
  #group_var <- c(identifier,"yr","month")
  #quantile_data <- quintile_vars_data_ios
  #quantile_col_low <- "quintile1"
  #quantile_col_high <- "quintile4"
  
  data_trim <- data[,append(group_var,w)]
  
  quintile_data_trim <- quantile_data[quantile_data[,"var"]==w,]
  quintile_data_trim <- quintile_data_trim[,-match("var",names(quintile_data_trim))]
  quintile_data_trim <- data.frame(quintile_data_trim,temp_q_low=NA,temp_q_high=NA)
  
  data_full <- merge(data_trim, quintile_data_trim, by.x=c("yr"), by.y=c("yr"), 
                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)
  
  data_full[,"temp_q_low"] <- ifelse((data_full[,w]<data_full[,quantile_col_low]), 1, 0)
  data_full[,"temp_q_low"] <- ifelse(is.na(data_full[,w]), NA, data_full[,"temp_q_low"])
  data_full[,"temp_q_high"] <- ifelse((data_full[,w]>data_full[,quantile_col_high]), 1, 0)
  data_full[,"temp_q_high"] <- ifelse(is.na(data_full[,w]), NA, data_full[,"temp_q_high"])
  
  colnames(data_full)[match("temp_q_low",names(data_full))]  <- paste(w,"below",quantile_col_low,sep="_")
  colnames(data_full)[match("temp_q_high",names(data_full))] <- paste(w,"above",quantile_col_high,sep="_")
  
  return(data_full[,-match(c(w,quantile_col_low,quantile_col_high),names(data_full))])
  
}

quantile_cast_merge <- function(w,quantile_num){
  #w <- quintiles_melt[quintiles_melt[,"yr"]==1999,]
  #quantile_num <- 5
  
  merge_table <- unique(w[,c("temp_id","yr","variable")])
  quintile_u <- sort(unique(w[,"quantile"]))
  
  for (i in 1:length(quintile_u))
  {
    #i <- 1
    
    v <- w[w[,"quantile"]==quintile_u[i],]
    v <- v[!is.na(v[,"value"]),]
    colnames(v)[match(c("value"),names(v))] <- quintile_u[i]
    v[,"temp_id"] <- seq(1,nrow(v))
    merge_table <- merge(merge_table,v[,-match(c("quantile"),names(v))], 
                         by.x=c("temp_id","yr","variable"), by.y=c("temp_id","yr","variable"), 
                         all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)
    
  }
  merge_table <- merge_table[!(rowSums(is.na(merge_table[,4:ncol(merge_table)]))==quantile_num),]
  return(merge_table)
}

quantile_cast_cuts <- function(z,split_var,quantile_num){
  #z <- data_trim[data_trim[,"yr"]==1999,]
  #split_var <- x
  #quantile_num <- 5
  eps <- .Machine$double.eps 
  df <- data.frame(z,quantile=as.integer(with(z, cut(z[,split_var], breaks=quantile(z[,split_var], probs=(0:quantile_num)/quantile_num,na.rm=TRUE)+eps*(0:quantile_num),include.lowest=TRUE))),stringsAsFactors=FALSE)
  return(df)
}

quantile_yr_cast <- function(x,data,dep_var,group_var,quantile_count){
  
  #x <- univariate_vars[21]
  #x <- univariate_vars[27]
  
  #data <- data_all_univariate
  #dep_var <- "pct_flow"
  #group_var <- "yr"
  #quantile_count <- 5
  
  data_trim <- data[,c(dep_var,group_var,x)]
  data_trim <- data_trim[!is.na(data_trim[,x]),]
  
  quantiles <- ddply(data_trim, group_var,quantile_cast_cuts ,split_var=x,quantile_num=quantile_count)
  
  quantiles_melt <- melt(quantiles,c(group_var,"quantile"),dep_var)
  quantiles_melt <- ddply(quantiles_melt, c("yr","variable"), function(y){data.frame(temp_id=seq(1,nrow(y)),y,stringsAsFactors=FALSE)})
  
  quantiles_melt_cast <- ddply(quantiles_melt, c("yr"), quantile_cast_merge,quantile_num=quantile_count)
  quantiles_melt_cast <- subset(quantiles_melt_cast,select=-temp_id)
  
  return(data.frame(cut_var=x,quantiles_melt_cast,stringsAsFactors=FALSE))
  
}

quantile_cast <- function(x,data,dep_var,group_var,quantile_count){
  
  #x <- univariate_vars[21]
  #x <- univariate_vars[27]
  
  #data <- data_all_univariate
  #dep_var <- "pct_flow"
  #group_var <- "yr"
  #quantile_count <- 5
  
  data_trim <- data[,c(dep_var,group_var,x)]
  data_trim <- data_trim[!is.na(data_trim[,x]),]
  
  quantiles <- quantile_cast_cuts(data_trim,split_var=x,quantile_num=quantile_count)
  
  quantiles_melt <- melt(quantiles,c(group_var,"quantile"),dep_var)
  quantiles_melt <- ddply(quantiles_melt, c("yr","variable"), function(y){data.frame(temp_id=seq(1,nrow(y)),y,stringsAsFactors=FALSE)})
  
  quantiles_melt_cast <- ddply(quantiles_melt, c("yr"), quantile_cast_merge,quantile_num=quantile_count)
  quantiles_melt_cast <- subset(quantiles_melt_cast,select=-temp_id)
  
  return(data.frame(cut_var=x,quantiles_melt_cast,stringsAsFactors=FALSE))
  
}

diff_in_mean <- function(x,var_col,yr_col,quantile_first_col,quantile_last_col){
  #x <- quintiles_pct_flow
  #var_col <- "cut_var"
  #yr_col <- "yr"
  #quantile_first_col <- "X1"
  #quantile_last_col <- "X5"
  
  averages_quantile_cast <- ddply(x, c(yr_col,var_col), function(z){
    stats <- suppressWarnings(as.data.frame(describe(z[,-match(c("yr",var_col),names(z))], 
                                                     na.rm=TRUE,skew=FALSE,range=FALSE),stringsAsFactors=FALSE))
    stats[,"var"] <- row.names(stats)
    return(stats)
  })
  
  colnames(averages_quantile_cast)[match(c("var"),names(averages_quantile_cast))] <- "quantile"
  averages_quantile_cast2 <- ddply(averages_quantile_cast, c("yr"), function(z){
    return(suppressMessages(dcast(z[c(yr_col,"quantile",var_col,"mean")], cut_var~quantile)))
  })
  
  averages_quantile_cast_ttest <- ddply(x, c(yr_col,var_col), function(z){
    #z <- x[x[,"variable"]=="pct_flow",]
    
    ftest_results <- var.test(z[,quantile_first_col], z[,quantile_last_col])
    ttest_results <- t.test(z[,quantile_first_col], z[,quantile_last_col])
    test_data <- data.frame(t_minus_b=(ttest_results$estimate[2]-ttest_results$estimate[1]),
                            t_stat=ttest_results$statistic,
                            t_p_val=ttest_results$p.value,
                            f_stat=ftest_results$statistic,
                            f_p_val=ftest_results$p.value)
    return(test_data)
  })
  
  combined_table <- merge(averages_quantile_cast2, averages_quantile_cast_ttest, 
                          by.x=c(yr_col,var_col), by.y=c(yr_col,var_col), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)
  
  return(combined_table)
  
}

kmo.test <- function(df){ 
  ### 
  ## Calculate the Kaiser-Meyer-Olkin Measure of Sampling Adequacy. 
  ## Input should be a data frame or matrix, output is the KMO statistic. 
  ## Formula derived from Hutcheson et al, 1999, 
  ## "The multivariate social scientist," page 224, ISBN 0761952012 
  ## see <http://www2.chass.ncsu.edu/garson/pa765/hutcheson.htm><http://www2.chass.ncsu.edu/garson/pa765/hutcheson.htm%3E> 
  ### 
  
  #df <- data_all[,pca_text_both_vars_all]
  
  require(corpcor) 
  
  cor.sq = cor(df)^2 
  cor.sumsq = (sum(cor.sq)-dim(cor.sq)[1])/2 
  
  pcor.sq = cor2pcor(cor(df))^2 
  pcor.sumsq = (sum(pcor.sq)-dim(pcor.sq)[1])/2 
  kmo = sus.cor.ss/(sus.cor.ss+sus.pcor.ss) 
  return(kmo) 
} 


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

update.packages(ask=FALSE, checkBuilt=TRUE)

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","descr","fastmatch","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
                       "pander","pbapply","PerformanceAnalytics","plm","plyr","psych","quantreg","R.oo","R2wd",
                       "reporttools","reshape2","rms","RSQLite","sandwich","sqldf","stargazer","stringr","SWordInstaller",
                       "texreg","UsingR","xtable")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

EurekahedgeHF_Excel_aca_full11 <- runsql("SELECT * FROM EurekahedgeHF_Excel_aca_full11",descriptive_stats_db)
text_stats_ios <- runsql("SELECT * FROM text_stats_ios",descriptive_stats_db)


###############################################################################
cat("MAKE ALL COLUMNS LOWER CASE", "\n")
###############################################################################

colnames(EurekahedgeHF_Excel_aca_full11) <- tolower(colnames(EurekahedgeHF_Excel_aca_full11))
colnames(text_stats_ios) <- tolower(colnames(text_stats_ios))


###############################################################################
cat("FIX DATA", "\n")
###############################################################################

#fund_type_no_na <- EurekahedgeHF_Excel_aca_full11[!(is.na(EurekahedgeHF_Excel_aca_full11[,"main_investment_strategy"])),]
#fund_type_remove <- fund_type_no_na[((fund_type_no_na[,"broad_cat_group"]=="Alternative") |
#                                       (fund_type_no_na[,"broad_cat_group"]=="Tax Preferred")),]
fund_type_remove <- EurekahedgeHF_Excel_aca_full11[is.na(EurekahedgeHF_Excel_aca_full11[,"main_investment_strategy"]),]
fund_type_remove2 <- unique(fund_type_remove[,identifier])

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[!(EurekahedgeHF_Excel_aca_full11[,identifier] %in% fund_type_remove2),]
text_stats_ios   <- text_stats_ios[!(text_stats_ios[,identifier] %in% fund_type_remove2),]

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[(EurekahedgeHF_Excel_aca_full11[,"yr"]>=1992 & EurekahedgeHF_Excel_aca_full11[,"yr"]<=2012),]
text_stats_ios   <- text_stats_ios[(text_stats_ios[,"yr"]>=1992 & text_stats_ios[,"yr"]<=2012),]

monthly_data_all4_na_cols <- c("yr","pct_flow","net_flow","mktadjret_agg",
                               "mktadjret_agglag1","mktadjret_agglag2","mktadjret_agglag3","mktadjret_agglag4",
                               "mktadjret_agg_sq","mnav_agg","mtna_agg","log_mtna_agg","age_y","sddret_agg",
                               "sddret_agg","turn_ratio_agg","exp_ratio_agg","mgmt_fee_agg",
                               "equity_style_box_long_lg_dv","equity_style_box_long_lv_dv","equity_style_box_long_lb_dv",
                               "equity_style_box_long_mg_dv","equity_style_box_long_mv_dv","equity_style_box_long_mb_dv",
                               "equity_style_box_long_sg_dv","equity_style_box_long_sv_dv","equity_style_box_long_sb_dv",
                               "broad_cat_group_eq_dv","broad_cat_group_al_dv","broad_cat_group_fi_dv")
for (i in 1:length(monthly_data_all4_na_cols))
{
  #i <- 1
  monthly_data_all4 <- monthly_data_all4[!(is.na(monthly_data_all4[,monthly_data_all4_na_cols[i]])),]
  
}
monthly_data_all4 <- subset(monthly_data_all4,select=-c(broad_cat_group,equity_style_box_long))

text_stats_ios_sim_cols <- names(text_stats_ios)[grep("_similarity", names(text_stats_ios))] 
for (i in 1:length(text_stats_ios_sim_cols))
{
  #i <- 1
  
  text_stats_ios <- text_stats_ios[!(is.na(text_stats_ios[,text_stats_ios_sim_cols[i]])),]
  
}
text_stats_ios <- text_stats_ios[((!is.na(text_stats_ios[,"ari_ios"])) & 
                                      (!is.na(text_stats_ios[,"coleman_liau_ios"])) & 
                                      (!is.na(text_stats_ios[,"flesch_kincaid_ios"])) & 
                                      (!is.na(text_stats_ios[,"fog_ios"])) &   
                                      (!is.na(text_stats_ios[,"smog_ios"]))),]

rm2(fund_type_no_na,fund_type_remove,fund_type_remove2)
rm2(monthly_data_all4_na_cols,text_stats_ios_sim_cols)

#fund_fee_no_na <- monthly_data_all4_trim[!(is.na(monthly_data_all4_trim[,"mgmt_fee_agg"])),]
#fund_fee_small <- fund_fee_no_na[fund_fee_no_na[,"mgmt_fee_agg"]<0.1,]
#fund_fee_small <- fund_fee_small[,c(identifier,"yr","month","mgmt_fee_agg")]


###############################################################################
cat("MERGE FUND AND TEXT DATA", "\n")
###############################################################################

data_all0 <- merge(text_stats_ios, text_stats_pr, 
                   by.x=c(identifier,"yr","month","branding_name","equity_style_box_long","broad_cat_group"),
                   by.y=c(identifier,"yr","month","branding_name","equity_style_box_long","broad_cat_group"),
                   all.x=FALSE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all <- merge(monthly_data_all4, data_all0, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                  all.x=FALSE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all <- data_all[order(data_all[,identifier],data_all[,"yr"],data_all[,"month"]),]
data_all <- data.frame(data_all,yr_month=paste(data_all$yr,data_all$month,sep="_"),stringsAsFactors=FALSE)

rm2(data_all0)

###############################################################################
cat("CREATE ADDITIONAL AVERAGE READABILITY MEASURES", "\n")
###############################################################################

data_all  <- data.frame(data_all,
                        avg_grade_level_acf_ios=rowMeans(data_all[,c("ari_ios","coleman_liau_ios","flesch_kincaid_ios")],na.rm=TRUE),
                        avg_grade_level_ac_ios=rowMeans(data_all[,c("ari_ios","coleman_liau_ios")],na.rm=TRUE),
                        avg_grade_level_acf_pr=rowMeans(data_all[,c("ari_pr","coleman_liau_pr","flesch_kincaid_pr")],na.rm=TRUE),
                        avg_grade_level_ac_pr=rowMeans(data_all[,c("ari_pr","coleman_liau_pr")],na.rm=TRUE),
                        stringsAsFactors=FALSE)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PANEL A)", "\n")
###############################################################################

fund_count <- as.numeric(length(unique(data_all[,identifier],comparables=FALSE)))

descrip_stats_fund <- subset(data_all,select=-c(wficn,month,
                                                mnav_agglag1,mnav_agglag2,mnav_agglag3,mnav_agglag4,
                                                mtna_agglag1,mtna_agglag2,mtna_agglag3,mtna_agglag4,
                                                mret_agglag1,mret_agglag2,mret_agglag3,mret_agglag4,
                                                mktadjret_agglag1,mktadjret_agglag2,mktadjret_agglag3,mktadjret_agglag4,
                                                sddret_agglag1,sddret_agglag2,sddret_agglag3,sddret_agglag4,
                                                actual_12b1_agglag1,actual_12b1_agglag2,actual_12b1_agglag3,actual_12b1_agglag4,
                                                max_12b1_agglag1,max_12b1_agglag2,max_12b1_agglag3,max_12b1_agglag4,
                                                exp_ratio_agglag1,exp_ratio_agglag2,exp_ratio_agglag3,exp_ratio_agglag4,
                                                mgmt_fee_agglag1,mgmt_fee_agglag2,mgmt_fee_agglag3,mgmt_fee_agglag4,
                                                turn_ratio_agglag1,turn_ratio_agglag2,turn_ratio_agglag3,turn_ratio_agglag4,
                                                nflowlag1,nflowlag2,nflowlag3,nflowlag4,
                                                pflowlag1,pflowlag2,pflowlag3,pflowlag4,
                                                crsp_obj_cd,age_m,chgdt,
                                                lipper_class,lipper_obj_cd,lipper_obj_name,lipper_asset_cd,lipper_tax_cd,
                                                mktadjret_agglag1_sq,mktadjret_agglag2_sq,mktadjret_agglag3_sq,mktadjret_agglag4_sq,
                                                log_mtna_agglag1,log_mtna_agglag2,log_mtna_agglag3,log_mtna_agglag4,
                                                avg_mkt_explag1,avg_mkt_explag2,avg_mkt_explag3,avg_mkt_explag4,
                                                lipper_asset_cd_eq_dv,lipper_asset_cd_tx_dv,lipper_asset_cd_mb_dv,lipper_tax_cd_t_dv,lipper_tax_cd_te_dv,
                                                sdnet_flow,sdpct_flow,sdnet_flowlag1,sdpct_flowlag1,
                                                broad_cat_group_alt_dv,broad_cat_group_tp_dv))

descrip_stats_fund2 <- descrip_stats_fund[c("yr","pct_flow","mktadjret_agg","mktadjret_agg_sq","fundretmktneg","mnav_agg","mtna_agg","age_y",
                                            "sddret_agg","turn_ratio_agg","actual_12b1_agg","exp_ratio_agg","avg_mkt_exp","mgmt_fee_agg",
                                            "equity_style_box_long_lg_dv","equity_style_box_long_lv_dv","equity_style_box_long_lb_dv",
                                            "equity_style_box_long_mg_dv","equity_style_box_long_mv_dv","equity_style_box_long_mb_dv",
                                            "equity_style_box_long_sg_dv","equity_style_box_long_sv_dv","equity_style_box_long_sb_dv",
                                            "broad_cat_group_eq_dv","broad_cat_group_al_dv","broad_cat_group_fi_dv")]
#descriptive_stats_PA <- as.data.frame(describe(descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))], 
#                                               na.rm=TRUE,skew=TRUE,range=TRUE),stringsAsFactors=FALSE)
#descriptive_stats_PA[,"var"] <- row.names(descriptive_stats_PA)
#descriptive_stats_PA <- descriptive_stats_PA[c("var","n","min","median","mean","max","sd")]
descriptive_stats_PA <- describe2(descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))])
descriptive_stats_PA <- descriptive_stats_PA[c("var","n","quartile1","median","mean","quartile3","sd")]
descriptive_stats_PA <- rbind(descriptive_stats_PA,c("number_of_funds",fund_count,"","","","",""))
write.csv(descriptive_stats_PA,file=paste(output_directory,"descriptive_stats_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(descrip_stats_fund)

# cap4 <- "Descriptive Statistics"
# tableContinuous(vars=descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))], 
#                 stats=c("n","min","median","mean","max","s"), prec = 3,
#                 print.pval="anova", cap=cap4, lab="tab: cont1", longtable=FALSE)
#tableContinuous(vars=descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))],prec=3, 
#                print.pval="anova", cap=cap4,lab="tab: cont1", longtable=FALSE)


###############################################################################
cat("DESCRIPTIVE STATISTICS - IOS (PANEL B)", "\n")
###############################################################################

descrip_stats_ios <- subset(data_all,select=-c(wficn,month,punct_ios,conjunctions_ios,prepositions_ios,normalized_space_ios, 
                                                pronouns_ios,ttr_ios,broad_cat_group,equity_style_box_long,branding_name))

#descrip_stats_ios_sim_cols <- names(descrip_stats_ios)[grep("_050pct_ios", names(descrip_stats_ios))] 
descrip_stats_ios_sim_cols <- names(descrip_stats_ios)[grep("pct_ios", names(descrip_stats_ios))] 
descrip_stats_ios_vars <- c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                             "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                             "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                             "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                             "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",descrip_stats_ios_sim_cols)

#descriptive_stats_PB <- as.data.frame(describe(descrip_stats_ios[,descrip_stats_ios_vars],na.rm=TRUE,skew=TRUE,range=TRUE),stringsAsFactors=FALSE)
#descriptive_stats_PB[,"var"] <- row.names(descriptive_stats_PB)
#descriptive_stats_PB <- descriptive_stats_PB[c("var","n","min","median","mean","max","sd")]
descriptive_stats_PB <- describe2(descrip_stats_ios[,descrip_stats_ios_vars])
descriptive_stats_PB <- descriptive_stats_PB[c("var","n","quartile1","median","mean","quartile3","sd")]
descriptive_stats_PB <- descriptive_stats_PB[descriptive_stats_PB[,"var"] %in% descrip_stats_ios_vars,]
descriptive_stats_PB <- descriptive_stats_PB[match(descrip_stats_ios_vars, descriptive_stats_PB$var),]
write.csv(descriptive_stats_PB,file=paste(output_directory,"descriptive_stats_PB.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

#rm2(descrip_stats_ios)

###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES BY YEAR (PANEL A)", "\n")
###############################################################################

fund_count_yr <- ddply(data_all, "yr", function(x) {data.frame(var="number_of_funds", 
                                                               count=as.numeric(length(unique(x$wficn,comparables=FALSE))),
                                                               stringsAsFactors=FALSE)})

#descriptive_stats_yr_temp_PA <- describeBy(descrip_stats_fund2[,-match("yr",names(descrip_stats_fund2))], 
#                                           group=descrip_stats_fund2$yr, na.rm=TRUE,skew=TRUE, range=TRUE, mat=TRUE)
#descriptive_stats_yr_temp_PA[,"var"] <- row.names(descriptive_stats_yr_temp_PA)
#descriptive_stats_yr_temp_PA <- descriptive_stats_yr_temp_PA[order(descriptive_stats_yr_temp_PA[,"group1"]),]
#colnames(descriptive_stats_yr_temp_PA)[match("group1",names(descriptive_stats_yr_temp_PA))] <- "yr"
#descriptive_stats_yr_temp_PA[,"var"] <- gsub("\\s*\\d*$", "", descriptive_stats_yr_temp_PA[,"var"])
#descriptive_stats_yr_temp_PA <- unfactorize2(descriptive_stats_yr_temp_PA)
#descriptive_stats_yr_temp_PA[,"yr"] <- as.integer(descriptive_stats_yr_temp_PA[,"yr"])

descriptive_stats_yr_stats_fund <- describeBy2(descrip_stats_fund2,"yr")

fund_count_yr_mean <- fund_count_yr
colnames(fund_count_yr_mean)[match("count",names(fund_count_yr_mean))] <- "mean"
descriptive_stats_yr_mean_full_PA <- rbind(descriptive_stats_yr_stats_fund[c("yr","var","mean")],fund_count_yr_mean)

descriptive_stats_yr_mean_PA <- suppressMessages(dcast(descriptive_stats_yr_mean_full_PA, var~yr))
descriptive_stats_yr_mean_PA <- descriptive_stats_yr_mean_PA[order(order(descriptive_stats_PA[,"var"])),] 
descriptive_stats_yr_mean_PA <- cbind(descriptive_stats_yr_mean_PA,data.frame(Full=descriptive_stats_PA[,"mean"],stringsAsFactors=FALSE))
descriptive_stats_yr_mean_PA[nrow(descriptive_stats_yr_mean_PA),"Full"] <- fund_count
write.csv(descriptive_stats_yr_mean_PA,file=paste(output_directory,"descriptive_stats_yr_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

fund_count_yr_median <- fund_count_yr
colnames(fund_count_yr_median)[match("count",names(fund_count_yr_median))] <- "median"
descriptive_stats_yr_median_full_PA <- rbind(descriptive_stats_yr_stats_fund[c("yr","var","median")],fund_count_yr_median)

descriptive_stats_yr_median_PA <- suppressMessages(dcast(descriptive_stats_yr_median_full_PA, var~yr))
descriptive_stats_yr_median_PA <- descriptive_stats_yr_median_PA[order(order(descriptive_stats_PA[,"var"])),] 
descriptive_stats_yr_median_PA <- cbind(descriptive_stats_yr_median_PA,data.frame(Full=descriptive_stats_PA[,"median"],stringsAsFactors=FALSE))
descriptive_stats_yr_median_PA[nrow(descriptive_stats_yr_median_PA),"Full"] <- fund_count
#write.csv(descriptive_stats_yr_median_PA,file=paste(output_directory,"descriptive_stats_yr_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(fund_count_yr_mean,fund_count_yr_median)


###############################################################################
cat("DESCRIPTIVE STATISTICS - IOS BY YEAR (PANEL B)", "\n")
###############################################################################

descriptive_stats_yr_stats_ios0 <- describeBy2(data_all[,c("yr",descrip_stats_ios_vars)],"yr")
descriptive_stats_yr_stats_ios <- descriptive_stats_yr_stats_ios0[tolower(descriptive_stats_yr_stats_ios0[,"var"]) %in% descrip_stats_ios_vars,]

descriptive_stats_yr_mean_PB <- suppressMessages(dcast(descriptive_stats_yr_stats_ios[c("yr","var","mean")], var~yr))
descriptive_stats_yr_mean_PB <- descriptive_stats_yr_mean_PB[order(order(descriptive_stats_PB[,"var"])),] 
descriptive_stats_yr_mean_PB <- cbind(descriptive_stats_yr_mean_PB,data.frame(Full=descriptive_stats_PB[,"mean"],stringsAsFactors=FALSE))
write.csv(descriptive_stats_yr_mean_PB,file=paste(output_directory,"descriptive_stats_yr_PB.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

descriptive_stats_yr_median_PB <- suppressMessages(dcast(descriptive_stats_yr_stats_ios[c("yr","var","median")], var~yr))
descriptive_stats_yr_median_PB <- descriptive_stats_yr_median_PB[order(order(descriptive_stats_PB[,"var"])),] 
descriptive_stats_yr_median_PB <- cbind(descriptive_stats_yr_median_PB,data.frame(Full=descriptive_stats_PB[,"median"],stringsAsFactors=FALSE))
#write.csv(descriptive_stats_yr_median_PB,file=paste(output_directory,"descriptive_stats_yr_PB.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

###############################################################################
cat("CORRELATION MATRIX (PANEL A & B)", "\n")
###############################################################################


descrip_stats_ios_sim_cols_trim <- names(descrip_stats_ios)[grep("_900pct_ios", names(descrip_stats_ios))] 


#corr_variables <- c("pct_flow","sentences_ios","words_ios","num_syll_ios","sntc_per_word_ios",
#                    "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
#                    "syll_per100_ios","lett_per100_ios","ari_ios","coleman_liau_ios","flesch_kincaid_ios","smog_ios",
#                    "similarity_ios")

corr_text_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",descrip_stats_ios_sim_cols_trim)

#correlation <- as.matrix(cor(cbind(monthly_data_all4[,"pct_flow"],text_stats_ios[,corr_text_vars_ios]),
#                             use="na.or.complete", method=c("pearson", "kendall", "spearman")))

correlation_stars1_PA <- corstar(text_stats_ios[,corr_text_vars_ios],round=3)
write.csv(correlation_stars1_PA,file=paste(output_directory,"correlation_stars1_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

#correlation_stars2_PA <- corstarsl(text_stats_ios[,corr_text_vars_ios],round=3)
#write.csv(correlation_stars2_PA,file=paste(output_directory,"correlation_stars2_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)


###############################################################################
cat("COMPUTE DV FOR ABOVE AND BELOW SIMILARITY/READABILITY QUANTILE - IOS", "\n")
###############################################################################

quintile_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                        "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                        "all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
                        "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios",
                        "all_similarity_100pct_ios","broad_cat_group_similarity_100pct_ios",
                        "equity_style_box_long_similarity_100pct_ios","branding_name_similarity_100pct_ios",
                        "all_similarity_250pct_ios","broad_cat_group_similarity_250pct_ios",
                        "equity_style_box_long_similarity_250pct_ios","branding_name_similarity_250pct_ios",
                        "all_similarity_500pct_ios","broad_cat_group_similarity_500pct_ios",
                        "equity_style_box_long_similarity_500pct_ios","branding_name_similarity_500pct_ios",
                        "all_similarity_750pct_ios","broad_cat_group_similarity_750pct_ios",
                        "equity_style_box_long_similarity_750pct_ios","branding_name_similarity_750pct_ios",
                        "all_similarity_900pct_ios","broad_cat_group_similarity_900pct_ios",
                        "equity_style_box_long_similarity_900pct_ios","branding_name_similarity_900pct_ios")

quintile_vars_data_ios <- descriptive_stats_yr_stats_ios0[tolower(descriptive_stats_yr_stats_ios0[,"var"]) %in% quintile_vars_ios,
                                                            c("yr","var","quartile1","quartile3")] 

quintile_vars_dv_temp_ios <- lapply(quintile_vars_ios,quantile_dvs,
                                     data=data_all,
                                     group_var=c(identifier,"yr","month"),quantile_data=quintile_vars_data_ios,
                                     quantile_col_low="quartile1",quantile_col_high="quartile3")

quintile_vars_dv_temp2_ios <- do.call(cbind, quintile_vars_dv_temp_ios)
quintile_vars_dv_temp2_ios <- quintile_vars_dv_temp2_ios[order(quintile_vars_dv_temp2_ios[,identifier],
                                                                 quintile_vars_dv_temp2_ios[,"yr"],
                                                                 quintile_vars_dv_temp2_ios[,"month"]),]

quintile_vars_dv_temp2_ios <- quintile_vars_dv_temp2_ios[,unique(colnames(quintile_vars_dv_temp2_ios))]

rm2(quintile_vars_ios,quintile_vars_data_ios,quintile_vars_dv_temp_ios)


###############################################################################
cat("MERGE QUANTILE DVs", "\n")
###############################################################################

quintile_vars_dv <- merge(quintile_vars_dv_temp2_ios, quintile_vars_dv_temp2_pr, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                          all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

quintile_vars_dv <- quintile_vars_dv[order(quintile_vars_dv[,identifier],
                                           quintile_vars_dv[,"yr"],
                                           quintile_vars_dv[,"month"]),]

data_all <- merge(data_all, quintile_vars_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all <- data_all[order(data_all[,identifier],
                           data_all[,"yr"],
                           data_all[,"month"]),]


###############################################################################
cat("UNIVARIATE ANALYSIS", "\n")
###############################################################################

data_all_univariate <- data_all[,c("yr","pct_flow",
                                   "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                                   "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                                   "all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
                                   "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios",
                                   "all_similarity_100pct_ios","broad_cat_group_similarity_100pct_ios",
                                   "equity_style_box_long_similarity_100pct_ios","branding_name_similarity_100pct_ios",
                                   "all_similarity_250pct_ios","broad_cat_group_similarity_250pct_ios",
                                   "equity_style_box_long_similarity_250pct_ios","branding_name_similarity_250pct_ios",
                                   "all_similarity_500pct_ios","broad_cat_group_similarity_500pct_ios",
                                   "equity_style_box_long_similarity_500pct_ios","branding_name_similarity_500pct_ios",
                                   "all_similarity_750pct_ios","broad_cat_group_similarity_750pct_ios",
                                   "equity_style_box_long_similarity_750pct_ios","branding_name_similarity_750pct_ios",
                                   "all_similarity_900pct_ios","broad_cat_group_similarity_900pct_ios",
                                   "equity_style_box_long_similarity_900pct_ios","branding_name_similarity_900pct_ios",
                                   "mktadjret_agglag1","sddret_agglag1","age_y","log_mtna_agg","mnav_agg",
                                   "turn_ratio_agg","actual_12b1_agg","exp_ratio_agg","avg_mkt_exp")]

univariate_vars <- colnames(data_all_univariate)[!(colnames(data_all_univariate)=="yr" | 
                                                     colnames(data_all_univariate)=="pct_flow" |
                                                     colnames(data_all_univariate)=="actual_12b1_agg" | 
                                                     colnames(data_all_univariate)=="avg_mkt_exp")]


###############################################################################
cat("UNIVARIATE ANALYSIS QUANTILES BY YEAR (QUINTILES,QUARTILES,TERCILES)", "\n")
###############################################################################

quantile_type <- c("year","agg")
quantile_nums <- c(5,4,3)

for (i in 1:length(quantile_type))
{
  #i <- 1
  #i <- 2
  
  for (j in 1:length(quantile_nums))
  {
    #j <- 1
    #j <- 2
    #j <- 3
    
    if (quantile_type[i]=="year")
    {
      quantiles_pct_flow_temp <- lapply(univariate_vars,quantile_yr_cast,data=data_all_univariate,
                                        dep_var="pct_flow",group_var="yr",quantile_count=quantile_nums[j])
      
    } else if (quantile_type[i]=="agg")
    {
      quantiles_pct_flow_temp <- lapply(univariate_vars,quantile_cast,data=data_all_univariate,
                                        dep_var="pct_flow",group_var="yr",quantile_count=quantile_nums[j])
    } else
    {
      cat("ERROR!!", "\n")
      
    }
    
    quantiles_pct_flow <- do.call(rbind, quantiles_pct_flow_temp)
    quantiles_pct_flow <- subset(quantiles_pct_flow,select=-variable)
    
    #quantile by year
    averages_yr_quan_all_cast <- diff_in_mean(quantiles_pct_flow,"cut_var","yr","X1",paste("X",quantile_nums[j],sep=""))
    
    name1 <- paste("quantiles",quantile_type[i],quantile_nums[j],"yr_all",sep="_")
    assign(name1, averages_yr_quan_all_cast, envir = .GlobalEnv)
    write.csv(averages_yr_quan_all_cast,file=paste(output_directory,name1,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    #quantile no year (all data)
    quantiles_pct_flow_no_yr <- quantiles_pct_flow
    quantiles_pct_flow_no_yr[,"yr"] <- 9999
    
    averages_quan_all_cast <- diff_in_mean(quantiles_pct_flow_no_yr,"cut_var","yr","X1",paste("X",quantile_nums[j],sep=""))
    averages_quan_all_cast <- subset(averages_quan_all_cast,select=-c(yr))
    averages_quan_all_cast <- averages_quan_all_cast[order(order(univariate_vars)),] 
    
    name2 <- paste("quantiles",quantile_type[i],quantile_nums[j],"all",sep="_")
    assign(name2, averages_quan_all_cast, envir = .GlobalEnv)
    write.csv(averages_quan_all_cast,file=paste(output_directory,name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    #quantile no year (1999-2000 data)
    quantiles_pct_flow_no_yr_99_00 <- quantiles_pct_flow[quantiles_pct_flow[,"yr"]<=2000,]
    quantiles_pct_flow_no_yr_99_00[,"yr"] <- 9999
    
    averages_quan_99_00_cast <- diff_in_mean(quantiles_pct_flow_no_yr_99_00,"cut_var","yr","X1",paste("X",quantile_nums[j],sep=""))
    averages_quan_99_00_cast <- subset(averages_quan_99_00_cast,select=-c(yr))
    averages_quan_99_00_cast <- averages_quan_99_00_cast[order(order(univariate_vars)),] 
    
    name3 <- paste("quantiles",quantile_type[i],quantile_nums[j],"99_00",sep="_")
    assign(name3, averages_quan_99_00_cast, envir = .GlobalEnv)
    write.csv(averages_quan_99_00_cast,file=paste(output_directory,name3,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    #quantile no year (2001-2007 data)
    quantiles_pct_flow_no_yr_01_07 <- quantiles_pct_flow[(quantiles_pct_flow[,"yr"]>=2001 & quantiles_pct_flow[,"yr"]<=2007),]
    quantiles_pct_flow_no_yr_01_07[,"yr"] <- 9999
    
    averages_quan_01_07_cast <- diff_in_mean(quantiles_pct_flow_no_yr_01_07,"cut_var","yr","X1",paste("X",quantile_nums[j],sep=""))
    averages_quan_01_07_cast <- subset(averages_quan_01_07_cast,select=-c(yr))
    averages_quan_01_07_cast <- averages_quan_01_07_cast[order(order(univariate_vars)),] 
    
    name4 <- paste("quantiles",quantile_type[i],quantile_nums[j],"01_07",sep="_")
    assign(name4, averages_quan_01_07_cast, envir = .GlobalEnv)
    write.csv(averages_quan_01_07_cast,file=paste(output_directory,name4,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    #quantile no year (2008-2009 data)
    quantiles_pct_flow_no_yr_08_09 <- quantiles_pct_flow[quantiles_pct_flow[,"yr"]>=2008,]
    quantiles_pct_flow_no_yr_08_09[,"yr"] <- 9999
    
    averages_quan_08_09_cast <- diff_in_mean(quantiles_pct_flow_no_yr_08_09,"cut_var","yr","X1",paste("X",quantile_nums[j],sep=""))
    averages_quan_08_09_cast <- subset(averages_quan_08_09_cast,select=-c(yr))
    averages_quan_08_09_cast <- averages_quan_08_09_cast[order(order(univariate_vars)),]
    
    name5 <- paste("quantiles",quantile_type[i],quantile_nums[j],"08_09",sep="_")
    assign(name5, averages_quan_08_09_cast, envir = .GlobalEnv)
    write.csv(averages_quan_08_09_cast,file=paste(output_directory,name5,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    rm2(quantiles_pct_flow_temp,quantiles_pct_flow)
    rm2(name1,name2,name3,name4,name5)
    rm2(quantiles_pct_flow_no_yr,quantiles_pct_flow_no_yr_99_00,quantiles_pct_flow_no_yr_01_07,quantiles_pct_flow_no_yr_08_09)
    rm2(averages_yr_quan_all_cast,averages_quan_all_cast,averages_quan_99_00_cast,averages_quan_01_07_cast,averages_quan_08_09_cast)
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(quantile_type), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(quantile_nums))
    
  }
  
}

rm2(quantile_type,quantile_nums,data_all_univariate,univariate_vars)


###############################################################################
cat("PANEL REGRESSION", "\n")
###############################################################################

data_all.pd <- pdata.frame(data_all, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)


#dep_var <- c("pct_flow","net_flow")
dep_var <- c("pct_flow")

index_vars <- c(identifier, "yr_month")

model_type <- "pooling"
#note <- "all_grade"
note <- "xxx_test"

#vars_grade <- "ari_XXX"
#vars_grade <- "ari_XXX + flesch_kincaid_XXX + coleman_liau_XXX"
#vars_grade <- "ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX"
#vars_grade <- "avg_grade_level_XXX"
#vars_grade <- "avg_grade_level_ac_XXX"
vars_grade <- "coleman_liau_XXX"
#vars_sim   <- "all_similarity_250pct_XXX + broad_cat_group_similarity_250pct_XXX + 
#               equity_style_box_long_similarity_250pct_XXX + branding_name_similarity_250pct_XXX"
#vars_sim   <- "equity_style_box_long_similarity_050pct_XXX"
#vars_sim   <- "broad_cat_group_similarity_050pct_XXX"
vars_sim   <- "broad_cat_group_similarity_050pct_XXX"
#controls  <- "mktadjret_agglag1 + mktadjret_agglag1_sq + mktadjret_agglag2 + mktadjret_agglag2_sq + sddret_agglag1 + age_y + log_mtna_agg"
controls  <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y + log_mtna_agg"
#controls  <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y"
#dv_style  <- "equity_style_box_long_lg_dv + equity_style_box_long_lv_dv + equity_style_box_long_lb_dv + 
#              equity_style_box_long_mg_dv + equity_style_box_long_mv_dv + equity_style_box_long_mb_dv + 
#              equity_style_box_long_sg_dv + equity_style_box_long_sv_dv + equity_style_box_long_sb_dv"
#dv_cat    <- "broad_cat_group_eq_dv + broad_cat_group_al_dv"
dv_quin <- "broad_cat_group_similarity_050pct_XXX_below_quartile1 + broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin <- "avg_grade_level_XXX_below_quartile1 + avg_grade_level_XXX_above_quartile3 + 
#            broad_cat_group_similarity_050pct_XXX_below_quartile1 + broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin <- "avg_grade_level_ac_XXX_below_quartile1 + avg_grade_level_ac_XXX_above_quartile3 + 
#            broad_cat_group_similarity_900pct_XXX_below_quartile1 + broad_cat_group_similarity_900pct_XXX_above_quartile3"
#dv_quin <- "coleman_liau_XXX_below_quartile1 + coleman_liau_XXX_above_quartile3 + 
#            broad_cat_group_similarity_050pct_XXX_below_quartile1 + broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin <- "below_quintile1_avggrdlvl_XXX + above_quintile4_avggrdlvl_XXX + 
#            below_quintile1_all_XXX  + above_quintile4_all_XXX + 
#            below_quintile1_bcg_XXX  + above_quintile4_bcg_XXX + 
#            below_quintile1_esbl_XXX + above_quintile4_esbl_XXX + 
#            below_quintile1_bn_XXX   + above_quintile4_bn_XXX"
#dv_quin <- "below_quintile1_avggrdlvl_XXX + above_quintile4_avggrdlvl_XXX +
#            below_quintile1_bcg_XXX  + above_quintile4_bcg_XXX"
#dv_quin <- "below_quintile1_bcg_XXX  + above_quintile4_bcg_XXX"
#dv_quin <- "below_quintile1_esbl_XXX + above_quintile4_esbl_XXX"
#dv_quin <- ""
dv_quin_int <- "mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1:avg_grade_level_XXX_below_quartile1 + 
#                mktadjret_agglag1:avg_grade_level_XXX_above_quartile3 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1:avg_grade_level_ac_XXX_below_quartile1 + 
#                mktadjret_agglag1:avg_grade_level_ac_XXX_above_quartile3 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1:coleman_liau_XXX_below_quartile1 + 
#                mktadjret_agglag1:coleman_liau_XXX_above_quartile3 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
#                mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1:ari_XXX_below_quartile1 + 
#                mktadjret_agglag1:ari_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1*below_quintile1_avggrdlvl_XXX + mktadjret_agglag1*above_quintile4_avggrdlvl_XXX + 
#                mktadjret_agglag1*below_quintile1_all_XXX + mktadjret_agglag1*above_quintile4_all_XXX + 
#                mktadjret_agglag1*below_quintile1_bcg_XXX + mktadjret_agglag1*above_quintile4_bcg_XXX + 
#                mktadjret_agglag1*below_quintile1_esbl_XXX + mktadjret_agglag1*above_quintile4_esbl_XXX + 
#                mktadjret_agglag1*below_quintile1_bn_XXX + mktadjret_agglag1*above_quintile4_bn_XXX"
#dv_quin_int <- "mktadjret_agglag1*below_quintile1_avggrdlvl_XXX + mktadjret_agglag1*above_quintile4_avggrdlvl_XXX + 
#                mktadjret_agglag1*below_quintile1_bcg_XXX + mktadjret_agglag1*above_quintile4_bcg_XXX"
#dv_quin_int <- "mktadjret_agglag1*below_quintile1_bcg_XXX + mktadjret_agglag1*above_quintile4_bcg_XXX"
#dv_quin_int <- "mktadjret_agglag1*below_quintile1_esbl_XXX + mktadjret_agglag1*above_quintile4_esbl_XXX"
#dv_quin_int <- ""
fe <- "factor(yr)"
#fe <- "factor(branding_name) + factor(yr)"



reg_test_ind_vars <- 
  "coleman_liau_ios + 
broad_cat_group_similarity_050pct_ios + 
mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + age_y + log_mtna_agg + mnav_agg + 
broad_cat_group_similarity_050pct_ios_below_quartile1 + broad_cat_group_similarity_050pct_ios_above_quartile3 + 
mktadjret_agglag1:broad_cat_group_similarity_050pct_ios_below_quartile1 + 
mktadjret_agglag1:broad_cat_group_similarity_050pct_ios_above_quartile3"
reg_test <- plm(as.formula(paste(dep_var[i],reg_test_ind_vars,sep="~")), data=data_all.pd,model=model_type)
reg_test_rse <- mcl.plm(data_all,reg_test, data_all[,identifier], data_all[,"month"])
#screenreg(list(reg_test),digits=3,model.names=c("(1)"),override.se=list(reg_test_rse[,4]),override.pval=list(reg_test_rse[,4]),stars=c(0.01,0.05,0.1))

reg_test2 <- lm(as.formula(paste(dep_var[i],paste(reg_test_ind_vars,"factor(yr)",sep="+"),sep="~")), data_all)
reg_test_rse2 <- mcl(data_all,reg_test2, data_all[,identifier], data_all[,"month"])
#screenreg(list(reg_test2),digits=3,model.names=c("(1)"),override.se=list(reg_test_rse2[,4]),override.pval=list(reg_test_rse2[,4]),stars=c(0.01,0.05,0.1))


for (i in 1:length(dep_var))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var[i],deparse(substitute(data_all)),note,sep="_")
  
  if (dep_var[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  ind_vars_reg1 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),sep="+")
  #reg1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg1,sep="~")), data=data.pd,model=model_type)
  reg1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg1,sep="~")),data=data_all.pd,model=model_type)
  #reg1_rse <- coeftest(reg1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
  #reg1_rse <- cl.plm(data_all, reg1, data_all$wficn)
  #reg1_rse <- coeftest(reg1, vcov=function(x) vcovDC(x, type="HC1"))
  reg1_rse <- mcl.plm(data_all,reg1, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,2]),override.pval=list(reg1_rse[,4]),stars=c(0.01,0.05,0.1))
  #screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,4]),override.pval=list(reg1_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg2 <- paste(gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg2 <- plm(as.formula(paste(dep_var[i],ind_vars_reg2,sep="~")), data=data_all.pd,model=model_type)
  reg2_rse <- mcl.plm(data_all,reg2, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg2),digits=3,model.names=c("(1)"),override.se=list(reg2_rse[,4]),override.pval=list(reg2_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg3 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","pr",vars_grade),sep="+")
  reg3 <- plm(as.formula(paste(dep_var[i],ind_vars_reg3,sep="~")), data=data_all.pd,model=model_type)
  reg3_rse <- mcl.plm(data_all,reg3, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg3),digits=3,model.names=c("(1)"),override.se=list(reg3_rse[,4]),override.pval=list(reg3_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg4 <- paste(gsub("XXX","ios",vars_sim),gsub("XXX","pr",vars_sim),sep="+")
  reg4 <- plm(as.formula(paste(dep_var[i],ind_vars_reg4,sep="~")), data=data_all.pd,model=model_type)
  reg4_rse <- mcl.plm(data_all,reg4, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg4),digits=3,model.names=c("(1)"),override.se=list(reg4_rse[,4]),override.pval=list(reg4_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg5 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg5 <- plm(as.formula(paste(dep_var[i],ind_vars_reg5,sep="~")), data=data_all.pd,model=model_type)
  reg5_rse <- mcl.plm(data_all,reg5, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg5),digits=3,model.names=c("(1)"),override.se=list(reg5_rse[,4]),override.pval=list(reg5_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg6 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,
                         #dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         sep="+")
  reg6 <- plm(as.formula(paste(dep_var[i],ind_vars_reg6,sep="~")), data=data_all.pd,model=model_type)
  reg6_rse <- mcl.plm(data_all,reg6, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg6),digits=3,model.names=c("(1)"),override.se=list(reg6_rse[,4]),override.pval=list(reg6_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg7 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,
                         #dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         fe,sep="+")
  #reg7 <- plm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data=data_all.pd,model=model_type)
  #reg7_rse <- mcl.plm(data_all,reg7, data_all[,identifier], data_all[,"month"])
  reg7 <- lm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data_all)
  reg7_rse <- mcl(data_all,reg7, data_all[,identifier], data_all[,"month"])
  #screenreg(list(reg7),digits=3,model.names=c("(1)"),override.se=list(reg7_rse[,4]),override.pval=list(reg7_rse[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
          override.se=list(reg1_rse[,4],reg2_rse[,4],reg3_rse[,4],
                           reg4_rse[,4],reg5_rse[,4],reg6_rse[,4],reg7_rse[,4]),
          override.pval=list(reg1_rse[,4],reg2_rse[,4],reg3_rse[,4],
                             reg4_rse[,4],reg5_rse[,4],reg6_rse[,4],reg7_rse[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  #custom.names
  
  rm2(ind_vars_reg1,ind_vars_reg2,ind_vars_reg3,ind_vars_reg4,ind_vars_reg5,ind_vars_reg6,ind_vars_reg7)
  rm2(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg1_rse,reg2_rse,reg3_rse,reg4_rse,reg5_rse,reg6_rse,reg7_rse,out_file_name)
  
} 


#reg1 <- lm(as.formula(equation), data=data_all)
#reg1 <- plm(as.formula(equation), data=data.pd,model="pooling")
#summary(reg1)
#reg1_rse <- coeftest(reg1, vcov=function(x) vcovDC(x, type="HC1"))
#summary(reg1_rse)
#screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,2]),override.pval=list(reg1_rse[,4]),stars=c(0.01, 0.05, 0.1))



###############################################################################
cat("POOLED OLS REGRESSION - (1999-2000)", "\n")
###############################################################################

#controls  <- "mktadjret_agglag1 + sddret_agglag1 + age_y + log_mtna_agg"

data_99_00 <- data_all[data_all[,"yr"]<=2000,]
data_99_00.pd <- pdata.frame(data_99_00, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)


for (i in 1:length(dep_var))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var[i],deparse(substitute(data_99_00)),note,sep="_")
  
  if (dep_var[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  ind_vars_reg1 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),sep="+")
  reg1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg1,sep="~")), data=data_99_00.pd,model=model_type)
  reg1_rse <-   mcl.plm(data_99_00,reg1, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,2]),override.pval=list(reg1_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg2 <- paste(gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg2 <- plm(as.formula(paste(dep_var[i],ind_vars_reg2,sep="~")), data=data_99_00.pd,model=model_type)
  reg2_rse <-   mcl.plm(data_99_00,reg2, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg2),digits=3,model.names=c("(1)"),override.se=list(reg2_rse[,2]),override.pval=list(reg2_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg3 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","pr",vars_grade),sep="+")
  reg3 <- plm(as.formula(paste(dep_var[i],ind_vars_reg3,sep="~")), data=data_99_00.pd,model=model_type)
  reg3_rse <-   mcl.plm(data_99_00,reg3, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg3),digits=3,model.names=c("(1)"),override.se=list(reg3_rse[,2]),override.pval=list(reg3_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg4 <- paste(gsub("XXX","ios",vars_sim),gsub("XXX","pr",vars_sim),sep="+")
  reg4 <- plm(as.formula(paste(dep_var[i],ind_vars_reg4,sep="~")), data=data_99_00.pd,model=model_type)
  reg4_rse <-   mcl.plm(data_99_00,reg4, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg4),digits=3,model.names=c("(1)"),override.se=list(reg4_rse[,2]),override.pval=list(reg4_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg5 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg5 <- plm(as.formula(paste(dep_var[i],ind_vars_reg5,sep="~")), data=data_99_00.pd,model=model_type)
  reg5_rse <-   mcl.plm(data_99_00,reg5, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg5),digits=3,model.names=c("(1)"),override.se=list(reg5_rse[,2]),override.pval=list(reg5_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg6 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         sep="+")
  reg6 <- plm(as.formula(paste(dep_var[i],ind_vars_reg6,sep="~")), data=data_99_00.pd,model=model_type)
  reg6_rse <-   mcl.plm(data_99_00,reg6, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg6),digits=3,model.names=c("(1)"),override.se=list(reg6_rse[,2]),override.pval=list(reg6_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg7 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         fe,sep="+")
  #reg7 <- plm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data=data_99_00.pd,model=model_type)
  #reg7_rse <-   mcl.plm(data_99_00,reg7, data_99_00[,identifier], data_99_00[,"month"])
  reg7 <- lm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data_99_00)
  reg7_rse <- mcl(data_99_00,reg7, data_99_00[,identifier], data_99_00[,"month"])
  #screenreg(list(reg7),digits=3,model.names=c("(1)"),override.se=list(reg7_rse[,2]),override.pval=list(reg7_rse[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
          override.se=list(reg1_rse[,2],reg2_rse[,2],reg3_rse[,2],
                           reg4_rse[,2],reg5_rse[,2],reg6_rse[,2],reg7_rse[,2]),
          override.pval=list(reg1_rse[,4],reg2_rse[,4],reg3_rse[,4],
                             reg4_rse[,4],reg5_rse[,4],reg6_rse[,4],reg7_rse[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  rm2(ind_vars_reg1,ind_vars_reg2,ind_vars_reg3,ind_vars_reg4,ind_vars_reg5,ind_vars_reg6,ind_vars_reg7)
  rm2(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg1_rse,reg2_rse,reg3_rse,reg4_rse,reg5_rse,reg6_rse,reg7_rse,out_file_name)
  
} 



###############################################################################
cat("POOLED OLS REGRESSION - (2001-2007)", "\n")
###############################################################################

data_01_07 <- data_all[(data_all[,"yr"]>=2001 & data_all[,"yr"]<=2007),]
data_01_07.pd <- pdata.frame(data_01_07, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)

for (i in 1:length(dep_var))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var[i],deparse(substitute(data_01_07)),note,sep="_")
  
  if (dep_var[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  ind_vars_reg1 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),sep="+")
  reg1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg1,sep="~")), data=data_01_07.pd,model=model_type)
  reg1_rse <-   mcl.plm(data_01_07,reg1, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,2]),override.pval=list(reg1_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg2 <- paste(gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg2 <- plm(as.formula(paste(dep_var[i],ind_vars_reg2,sep="~")), data=data_01_07.pd,model=model_type)
  reg2_rse <-   mcl.plm(data_01_07,reg2, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg2),digits=3,model.names=c("(1)"),override.se=list(reg2_rse[,2]),override.pval=list(reg2_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg3 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","pr",vars_grade),sep="+")
  reg3 <- plm(as.formula(paste(dep_var[i],ind_vars_reg3,sep="~")), data=data_01_07.pd,model=model_type)
  reg3_rse <-   mcl.plm(data_01_07,reg3, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg3),digits=3,model.names=c("(1)"),override.se=list(reg3_rse[,2]),override.pval=list(reg3_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg4 <- paste(gsub("XXX","ios",vars_sim),gsub("XXX","pr",vars_sim),sep="+")
  reg4 <- plm(as.formula(paste(dep_var[i],ind_vars_reg4,sep="~")), data=data_01_07.pd,model=model_type)
  reg4_rse <-   mcl.plm(data_01_07,reg4, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg4),digits=3,model.names=c("(1)"),override.se=list(reg4_rse[,2]),override.pval=list(reg4_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg5 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg5 <- plm(as.formula(paste(dep_var[i],ind_vars_reg5,sep="~")), data=data_01_07.pd,model=model_type)
  reg5_rse <-   mcl.plm(data_01_07,reg5, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg5),digits=3,model.names=c("(1)"),override.se=list(reg5_rse[,2]),override.pval=list(reg5_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg6 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         sep="+")
  reg6 <- plm(as.formula(paste(dep_var[i],ind_vars_reg6,sep="~")), data=data_01_07.pd,model=model_type)
  reg6_rse <-   mcl.plm(data_01_07,reg6, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg6),digits=3,model.names=c("(1)"),override.se=list(reg6_rse[,2]),override.pval=list(reg6_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg7 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         fe,sep="+")
  #reg7 <- plm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data=data_01_07.pd,model=model_type)
  #reg7_rse <-   mcl.plm(data_01_07,reg7, data_01_07[,identifier], data_01_07[,"month"])
  reg7 <- lm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data_01_07)
  reg7_rse <- mcl(data_01_07,reg7, data_01_07[,identifier], data_01_07[,"month"])
  #screenreg(list(reg7),digits=3,model.names=c("(1)"),override.se=list(reg7_rse[,2]),override.pval=list(reg7_rse[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
          override.se=list(reg1_rse[,2],reg2_rse[,2],reg3_rse[,2],
                           reg4_rse[,2],reg5_rse[,2],reg6_rse[,2],reg7_rse[,2]),
          override.pval=list(reg1_rse[,4],reg2_rse[,4],reg3_rse[,4],
                             reg4_rse[,4],reg5_rse[,4],reg6_rse[,4],reg7_rse[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  rm2(ind_vars_reg1,ind_vars_reg2,ind_vars_reg3,ind_vars_reg4,ind_vars_reg5,ind_vars_reg6,ind_vars_reg7)
  rm2(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg1_rse,reg2_rse,reg3_rse,reg4_rse,reg5_rse,reg6_rse,reg7_rse,out_file_name)  
} 



###############################################################################
cat("POOLED OLS REGRESSION - (2008-2009)", "\n")
###############################################################################

data_08_09 <- data_all[data_all[,"yr"]>=2008,]
data_08_09.pd <- pdata.frame(data_08_09, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)

for (i in 1:length(dep_var))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var[i],deparse(substitute(data_08_09)),note,sep="_")
  
  if (dep_var[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  ind_vars_reg1 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),sep="+")
  reg1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg1,sep="~")), data=data_08_09.pd,model=model_type)
  reg1_rse <-   mcl.plm(data_08_09,reg1, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg1),digits=3,model.names=c("(1)"),override.se=list(reg1_rse[,2]),override.pval=list(reg1_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg2 <- paste(gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg2 <- plm(as.formula(paste(dep_var[i],ind_vars_reg2,sep="~")), data=data_08_09.pd,model=model_type)
  reg2_rse <-   mcl.plm(data_08_09,reg2, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg2),digits=3,model.names=c("(1)"),override.se=list(reg2_rse[,2]),override.pval=list(reg2_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg3 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","pr",vars_grade),sep="+")
  reg3 <- plm(as.formula(paste(dep_var[i],ind_vars_reg3,sep="~")), data=data_08_09.pd,model=model_type)
  reg3_rse <-   mcl.plm(data_08_09,reg3, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg3),digits=3,model.names=c("(1)"),override.se=list(reg3_rse[,2]),override.pval=list(reg3_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg4 <- paste(gsub("XXX","ios",vars_sim),gsub("XXX","pr",vars_sim),sep="+")
  reg4 <- plm(as.formula(paste(dep_var[i],ind_vars_reg4,sep="~")), data=data_08_09.pd,model=model_type)
  reg4_rse <-   mcl.plm(data_08_09,reg4, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg4),digits=3,model.names=c("(1)"),override.se=list(reg4_rse[,2]),override.pval=list(reg4_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg5 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg5 <- plm(as.formula(paste(dep_var[i],ind_vars_reg5,sep="~")), data=data_08_09.pd,model=model_type)
  reg5_rse <-   mcl.plm(data_08_09,reg5, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg5),digits=3,model.names=c("(1)"),override.se=list(reg5_rse[,2]),override.pval=list(reg5_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg6 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         sep="+")
  reg6 <- plm(as.formula(paste(dep_var[i],ind_vars_reg6,sep="~")), data=data_08_09.pd,model=model_type)
  reg6_rse <-   mcl.plm(data_08_09,reg6, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg6),digits=3,model.names=c("(1)"),override.se=list(reg6_rse[,2]),override.pval=list(reg6_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg7 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                         gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                         #controls,dv_style,dv_cat,vars_ext,
                         controls,dv_cat,vars_ext,
                         gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                         #gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                         fe,sep="+")
  #reg7 <- plm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data=data_08_09.pd,model=model_type)
  #reg7_rse <-   mcl.plm(data_08_09,reg7, data_08_09[,identifier], data_08_09[,"month"])
  reg7 <- lm(as.formula(paste(dep_var[i],ind_vars_reg7,sep="~")), data_08_09)
  reg7_rse <- mcl(data_08_09,reg7, data_08_09[,identifier], data_08_09[,"month"])
  #screenreg(list(reg7),digits=3,model.names=c("(1)"),override.se=list(reg7_rse[,2]),override.pval=list(reg7_rse[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
          override.se=list(reg1_rse[,2],reg2_rse[,2],reg3_rse[,2],
                           reg4_rse[,2],reg5_rse[,2],reg6_rse[,2],reg7_rse[,2]),
          override.pval=list(reg1_rse[,4],reg2_rse[,4],reg3_rse[,4],
                             reg4_rse[,4],reg5_rse[,4],reg6_rse[,4],reg7_rse[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  rm2(ind_vars_reg1,ind_vars_reg2,ind_vars_reg3,ind_vars_reg4,ind_vars_reg5,ind_vars_reg6,ind_vars_reg7)
  rm2(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg1_rse,reg2_rse,reg3_rse,reg4_rse,reg5_rse,reg6_rse,reg7_rse,out_file_name)  
} 


###############################################################################
cat("PRINCIPAL COMPONENT - IOS", "\n")
###############################################################################

#IOS READABILITY

#pca_text_read_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")

#pc_read_ios <- prcomp(~ ., data=text_stats_ios[,pca_text_read_vars_ios], na.action=na.omit, scale=TRUE)
#pc_read_ios
#plot(pc_read_ios) #Plotting te variances
#barplot(pc_read_ios$sdev/pc_read_ios$sdev[1])
#pc_read_ios2 <- prcomp(~ ., data=text_stats_ios[,pca_text_read_vars_ios], na.action=na.omit, scale=TRUE,tol=0.2)
#pc_read_ios2
#plot(pc_read_ios2) #Plotting te variances
#barplot(pc_read_ios2$sdev/pc_read_ios2$sdev[1])

#pc_read_ios_sd <- pc_read_ios2$sdev
#pc_read_ios_loadings <- pc_read_ios2$rotation
#rownames(pc_read_ios_loadings) <- colnames(pc_read_ios2)
#pc_read_ios_scores <- pc_read_ios2$x

#ios SIMILARITY

#pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
#                           "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios")

#pc_sim_ios <- prcomp(~ ., data=text_stats_ios[,pca_text_sim_vars_ios], na.action=na.omit, scale=TRUE)
#pc_sim_ios
#plot(pc_sim_ios) #Plotting te variances
#barplot(pc_sim_ios$sdev/pc_sim_ios$sdev[1])
#pc_sim_ios2 <- prcomp(~ ., data=text_stats_ios[,pca_text_sim_vars_ios], na.action=na.omit, scale=TRUE,tol=0.2)
#pc_sim_ios2
#plot(pc_sim_ios2) #Plotting te variances
#barplot(pc_sim_ios2$sdev/pc_sim_ios2$sdev[1])

#pc_sim_ios_sd <- pc_sim_ios2$sdev
#pc_sim_ios_loadings <- pc_sim_ios2$rotation
#rownames(pc_sim_ios_loadings) <- colnames(pc_sim_ios2)
#pc_sim_ios_scores <- pc_sim_ios2$x

#ios ALL READ AND BCG SIM

#pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
#                            "broad_cat_group_similarity_050pct_ios")
pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                            "all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
                            "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios")

pc_both_ios <- prcomp(~ ., data=text_stats_ios[,pca_text_both_vars_ios], na.action=na.omit, scale=TRUE)
pc_both_ios
#summary(pc_both_ios) 
plot(pc_both_ios) #Plotting te variances
barplot(pc_both_ios$sdev/pc_both_ios$sdev[1])
pc_both_ios2 <- prcomp(~ ., data=text_stats_ios[,pca_text_both_vars_ios], na.action=na.omit, scale=TRUE,tol=0.3)
pc_both_ios2
#summary(pc_both_ios2) 
plot(pc_both_ios2) #Plotting te variances
barplot(pc_both_ios2$sdev/pc_both_ios2$sdev[1])

pc_both_ios_sd <- pc_both_ios2$sdev

pc_both_ios_scores <- pc_both_ios2$x

#NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign
pc_both_ios_loadings0 <- pc_both_ios2$rotation
pc_both_ios_loadings <- data.frame(var=row.names(pc_both_ios_loadings0),pc_both_ios_loadings0,
                                   PC_avg=rowMeans(pc_both_ios_loadings0),stringsAsFactors=FALSE)

rm2(pc_both_ios_loadings0)

###############################################################################
cat("PRINCIPAL COMPONENT - PR", "\n")
###############################################################################

#PR READABILITY

#pca_text_read_vars_pr <- c("ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr")

#pc_read_pr <- prcomp(~ ., data=text_stats_pr[,pca_text_read_vars_pr], na.action=na.omit, scale=TRUE)
#pc_read_pr
#plot(pc_read_pr) #Plotting te variances
#barplot(pc_read_pr$sdev/pc_read_pr$sdev[1])
#pc_read_pr2 <- prcomp(~ ., data=text_stats_pr[,pca_text_read_vars_pr], na.action=na.omit, scale=TRUE,tol=0.2)
#pc_read_pr2
#plot(pc_read_pr2) #Plotting te variances
#barplot(pc_read_pr2$sdev/pc_read_pr2$sdev[1])

#pc_read_pr_sd <- pc_read_pr2$sdev
#pc_read_pr_loadings <- pc_read_pr2$rotation
#rownames(pc_read_pr_loadings) <- colnames(pc_read_pr2)
#pc_read_pr_scores <- pc_read_pr2$x

#pr SIMILARITY

#pca_text_sim_vars_pr <- c("all_similarity_050pct_pr","broad_cat_group_similarity_050pct_pr",
#                           "equity_style_box_long_similarity_050pct_pr","branding_name_similarity_050pct_pr")

#pc_sim_pr <- prcomp(~ ., data=text_stats_pr[,pca_text_sim_vars_pr], na.action=na.omit, scale=TRUE)
#pc_sim_pr
#plot(pc_sim_pr) #Plotting te variances
#barplot(pc_sim_pr$sdev/pc_sim_pr$sdev[1])
#pc_sim_pr2 <- prcomp(~ ., data=text_stats_pr[,pca_text_sim_vars_pr], na.action=na.omit, scale=TRUE,tol=0.2)
#pc_sim_pr2
#plot(pc_sim_pr2) #Plotting te variances
#barplot(pc_sim_pr2$sdev/pc_sim_pr2$sdev[1])

#pc_sim_pr_sd <- pc_sim_pr2$sdev
#pc_sim_pr_loadings <- pc_sim_pr2$rotation
#rownames(pc_sim_pr_loadings) <- colnames(pc_sim_pr2)
#pc_sim_pr_scores <- pc_sim_pr2$x

#pr ALL READ AND SIM

#pca_text_both_vars_pr <- c("ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr",
#                           "broad_cat_group_similarity_050pct_pr")
pca_text_both_vars_pr <- c("ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr",
                           "all_similarity_050pct_pr","broad_cat_group_similarity_050pct_pr",
                           "equity_style_box_long_similarity_050pct_pr","branding_name_similarity_050pct_pr")

pc_both_pr <- prcomp(~ ., data=text_stats_pr[,pca_text_both_vars_pr], na.action=na.omit, scale=TRUE)
pc_both_pr
#summary(pc_both_pr)
plot(pc_both_pr) #Plotting the variances
barplot(pc_both_pr$sdev/pc_both_pr$sdev[1])
pc_both_pr2 <- prcomp(~ ., data=text_stats_pr[,pca_text_both_vars_pr], na.action=na.omit, scale=TRUE,tol=0.3)
pc_both_pr2
#summary(pc_both_pr2)
plot(pc_both_pr2) #Plotting te variances
barplot(pc_both_pr2$sdev/pc_both_pr2$sdev[1])

pc_both_pr_sd <- pc_both_pr2$sdev
pc_both_pr_scores <- pc_both_pr2$x

#NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign
pc_both_pr_loadings0 <- pc_both_pr2$rotation
pc_both_pr_loadings <- data.frame(var=row.names(pc_both_pr_loadings0),pc_both_pr_loadings0,
                                  PC_avg=rowMeans(pc_both_pr_loadings0),stringsAsFactors=FALSE)

rm2(pc_both_pr_loadings0)


###############################################################################
cat("PRINCIPAL COMPONENT - ALL", "\n")
###############################################################################

#ALL READABILITY

pca_text_read_vars_all <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                            "ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr")

pc_read_all <- prcomp(~ ., data=data_all[,pca_text_read_vars_all], na.action=na.omit, scale=TRUE)
pc_read_all
plot(pc_read_all) #Plotting the variances
barplot(pc_read_all$sdev/pc_read_all$sdev[1])
pc_read_all2 <- prcomp(~ ., data=data_all[,pca_text_read_vars_all], na.action=na.omit, scale=TRUE,tol=0.2)
pc_read_all2
plot(pc_read_all2) #Plotting te variances
barplot(pc_read_all2$sdev/pc_read_all2$sdev[1])

pc_read_all_sd <- pc_read_all2$sdev
pc_read_all_loadings <- pc_read_all2$rotation
rownames(pc_read_all_loadings) <- colnames(pc_read_all2)
pc_read_all_scores <- pc_read_all2$x

#all SIMILARITY

pca_text_sim_vars_all <- c("all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
                           "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios",
                           "all_similarity_050pct_pr","broad_cat_group_similarity_050pct_pr",
                           "equity_style_box_long_similarity_050pct_pr","branding_name_similarity_050pct_pr")

pc_sim_all <- prcomp(~ ., data=data_all[,pca_text_sim_vars_all], na.action=na.omit, scale=TRUE)
pc_sim_all
plot(pc_sim_all) #Plotting te variances
barplot(pc_sim_all$sdev/pc_sim_all$sdev[1])
pc_sim_all2 <- prcomp(~ ., data=data_all[,pca_text_sim_vars_all], na.action=na.omit, scale=TRUE,tol=0.2)
pc_sim_all2
plot(pc_sim_all2) #Plotting te variances
barplot(pc_sim_all2$sdev/pc_sim_all2$sdev[1])

pc_sim_all_sd <- pc_sim_all2$sdev
pc_sim_all_loadings <- pc_sim_all2$rotation
rownames(pc_sim_all_loadings) <- colnames(pc_sim_all2)
pc_sim_all_scores <- pc_sim_all2$x

#all ALL READ AND BCG SIM

#pca_text_both_vars_all <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
#                            "ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr",
#                            "broad_cat_group_similarity_050pct_ios","broad_cat_group_similarity_050pct_pr")
pca_text_both_vars_all <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                            "ari_pr","coleman_liau_pr","flesch_kincaid_pr","fog_pr","smog_pr",
                            "all_similarity_050pct_ios","broad_cat_group_similarity_050pct_ios",
                            "equity_style_box_long_similarity_050pct_ios","branding_name_similarity_050pct_ios",
                            "all_similarity_050pct_pr","broad_cat_group_similarity_050pct_pr",
                            "equity_style_box_long_similarity_050pct_pr","branding_name_similarity_050pct_pr")

pc_both_all <- prcomp(~ ., data=data_all[,pca_text_both_vars_all], na.action=na.omit, scale=TRUE)
#summary(pc_both_all)
pc_both_all
plot(pc_both_all) #Plotting te variances
barplot(pc_both_all$sdev/pc_both_all$sdev[1])
pc_both_all2 <- prcomp(~ ., data=data_all[,pca_text_both_vars_all], na.action=na.omit, scale=TRUE,tol=0.4)
#summary(pc_both_all2) 
pc_both_all2
plot(pc_both_all2) #Plotting te variances
barplot(pc_both_all2$sdev/pc_both_all2$sdev[1])

pc_both_all_sd <- pc_both_all2$sdev
pc_both_all_var <- pc_both_all_sd^2
pc_both_all_scores <- pc_both_all2$x

#NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign
pc_both_all_loadings0 <- pc_both_all2$rotation
pc_both_all_loadings <- data.frame(var=row.names(pc_both_all_loadings0),pc_both_all_loadings0,
                                   PC_avg=rowMeans(pc_both_all_loadings0),stringsAsFactors=FALSE)

#kmo <- kmo.test(data_all[,pca_text_both_vars_all])
#which(!is.finite(as.matrix(data_all[,pca_text_both_vars_all])))

rm2(pc_both_all_loadings0)


###############################################################################
cat("PRINCIPAL COMPONENT DATA - MERGE", "\n")
###############################################################################

colnames(pc_both_ios_scores) <- paste(colnames(pc_both_ios_scores),"_ios",sep="")
colnames(pc_both_pr_scores) <- paste(colnames(pc_both_pr_scores),"_pr",sep="")
colnames(pc_both_all_scores) <- paste(colnames(pc_both_all_scores),"_all",sep="")

dat_all_pc_rn <- data.frame(rn=as.integer(row.names(data_all)),data_all)
ios_scores_rn <- data.frame(rn=as.integer(row.names(pc_both_ios_scores)),pc_both_ios_scores)
pr_scores_rn  <- data.frame(rn=as.integer(row.names(pc_both_pr_scores)),pc_both_pr_scores)
all_scores_rn <- data.frame(rn=as.integer(row.names(pc_both_all_scores)),pc_both_all_scores)

scores_rn <- merge(ios_scores_rn, pr_scores_rn, by.x="rn", by.y="rn", 
                   all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)
scores_rn <- merge(scores_rn, all_scores_rn, by.x="rn", by.y="rn", 
                   all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

dat_all_pc <- merge(dat_all_pc_rn, scores_rn, by.x="rn", by.y="rn", 
                    all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

dat_all_pc <- dat_all_pc[order(dat_all_pc[,"rn"]),]
dat_all_pc <- subset(dat_all_pc,select=-c(rn))
dat_all_pc <- dat_all_pc[order(dat_all_pc[,identifier],
                               dat_all_pc[,"yr"],
                               dat_all_pc[,"month"]),]
row.names(dat_all_pc) <- seq(nrow(dat_all_pc))


###############################################################################
cat("PRINCIPAL COMPONENT REGRESSION", "\n")
###############################################################################

data_pc.pd <- pdata.frame(dat_all_pc, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
dep_var_pc <- c("pct_flow")

index_vars_pc <- c(identifier, "yr_month")
controls_pc  <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y + log_mtna_agg"
#fe_pc <- "factor(yr)"
fe_pc <- "factor(branding_name) + factor(yr)"

model_type_pc <- "pooling"
note <- "pca_sep_sim_good"


for (i in 1:length(dep_var_pc))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var_pc[i],deparse(substitute(dat_all_pc)),note,sep="_")
  
  if (dep_var_pc[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var_pc[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  
  ind_vars_reg1_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios"
  reg1_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg1_pc,sep="~")), data_pc.pd,model=model_type_pc)
  #reg1_rse_pc <- coeftest(reg1, vcov=function(x) vcovDC(x, type="HC1"))
  reg1_rse_pc <- mcl.plm(dat_all_pc,reg1_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  #screenreg(list(reg1_pc),digits=3,model.names=c("(1)"),override.se=list(reg1_rse_pc[,2]),override.pval=list(reg1_rse_pc[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg2_pc <- "PC1_pr + PC2_pr + PC3_pr + PC4_pr"
  reg2_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg2_pc,sep="~")), data_pc.pd,model=model_type_pc)
  #reg2_rse_pc <- coeftest(reg2, vcov=function(x) vcovDC(x, type="HC1"))
  reg2_rse_pc <- mcl.plm(dat_all_pc,reg2_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  #screenreg(list(reg2_pc),digits=3,model.names=c("(1)"),override.se=list(reg2_rse_pc[,2]),override.pval=list(reg2_rse_pc[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg3_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios + PC1_pr + PC2_pr + PC3_pr + PC4_pr"
  reg3_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg3_pc,sep="~")), data_pc.pd,model=model_type_pc)
  #reg3_rse_pc <- coeftest(reg3, vcov=function(x) vcovDC(x, type="HC1"))
  reg3_rse_pc <- mcl.plm(dat_all_pc,reg3_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  #screenreg(list(reg3_pc),digits=3,model.names=c("(1)"),override.se=list(reg3_rse_pc[,2]),override.pval=list(reg3_rse_pc[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg4_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
  reg4_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg4_pc,sep="~")), data_pc.pd,model=model_type_pc)
  reg4_rse_pc <- mcl.plm(dat_all_pc,reg4_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  #screenreg(list(reg4_pc),digits=3,model.names=c("(1)"),override.se=list(reg4_rse_pc[,2]),override.pval=list(reg4_rse_pc[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,fe_pc,sep="+")
  #ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
  #reg5_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), data_pc.pd,model=model_type_pc)
  #reg5_rse_pc <- mcl.plm(dat_all_pc,reg5_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  reg5_pc <- lm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), dat_all_pc)
  reg5_rse_pc <- mcl(dat_all_pc,reg5_pc, dat_all_pc[,identifier], dat_all_pc[,"month"])
  #screenreg(list(reg5_pc),digits=3,model.names=c("(1)"),override.se=list(reg5_rse_pc[,2]),override.pval=list(reg5_rse_pc[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg1_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)"),
          override.se=list(reg1_rse_pc[,2],reg2_rse_pc[,2],reg3_rse_pc[,2],reg4_rse_pc[,2],reg5_rse_pc[,2]),
          override.pval=list(reg1_rse_pc[,4],reg2_rse_pc[,4],reg3_rse_pc[,4],reg4_rse_pc[,4],reg5_rse_pc[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  rm2(ind_vars_reg1_pc,ind_vars_reg2_pc,ind_vars_reg3_pc,ind_vars_reg4_pc,ind_vars_reg5_pc,out_file_name)
  rm2(reg1_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc,reg1_rse_pc,reg2_rse_pc,reg3_rse_pc,reg4_rse_pc,reg5_rse_pc)
  
} 




###############################################################################
cat("FLOW VOLATILITY DATA", "\n")
###############################################################################

data_vol_other <- data_all[,c(identifier,"yr","sdpct_flow",
                              "avg_grade_level_ios","broad_cat_group_similarity_050pct_ios",
                              "avg_grade_level_pr","broad_cat_group_similarity_050pct_pr",
                              "branding_name","equity_style_box_long","broad_cat_group")]
data_vol_other <- unique(data_vol_other)

data_vol_test_dt <- data.table(data_all[c(identifier,"yr","mtna_agg","mret_agg","sddret_agg")])
setkeyv(data_vol_test_dt,c(identifier,"yr"))

ret_annualized1 <- data_vol_test_dt[, list(aret1=mret_agg+1),by="wficn,yr"]
ret_annualized2 <- ret_annualized1[, list(aret2=prod(aret1, na.rm=FALSE)),by="wficn,yr"]
ret_annualized <- ret_annualized2[, list(aret=aret2-1),by="wficn,yr"]
ret_annualized <- as.data.frame(ret_annualized)

data_averaged <- data_vol_test_dt[, list(amtna_agg=mean(mtna_agg,na.rm=TRUE),
                                         asddret_agg=mean(sddret_agg,na.rm=TRUE)),by="wficn,yr"]
data_averaged <- as.data.frame(data_averaged)


data_vol0 <- merge(data_averaged, ret_annualized, by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                   all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_vol <- merge(data_vol0, data_vol_other, by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_vol <- data.frame(data_vol,
                       asddret_agg_lag1=shift(asddret_agg,-1),
                       aret_lag1=shift(aret,-1),
                       amtna_agg_log=suppressWarnings(log(amtna_agg)))

for (i in 1:ncol(data_vol))
{
  data_vol[,i] <- unknownToNA(data_vol[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  data_vol[,i] <- ifelse(is.na(data_vol[,i]),NA, data_vol[,i])
} 

rm2(data_vol_other,data_vol_test_dt,data_averaged,data_vol0)
rm2(ret_annualized1,ret_annualized2,ret_annualized)


###############################################################################
cat("FLOW VOLATILITY REGRESSION", "\n")
###############################################################################

data_vol.pd <- pdata.frame(data_vol, index=c(identifier, "yr"), drop.index=TRUE, row.names=TRUE)

dep_var_vol <- "sdpct_flow"
index_vars_vol <- c(identifier,"yr")
model_type_vol <- "pooling"
note <- "volatility_050"

out_file_name_vol <- paste("reg_compare_plm",dep_var_vol,deparse(substitute(data_vol)),note,sep="_")


ind_vars_reg1vol <- "avg_grade_level_ios + broad_cat_group_similarity_050pct_ios"
#reg1vol <- lm(as.formula(paste(dep_var_vol,ind_vars_reg1vol,sep="~")), data=data_vol)
#reg1vol_rse <- mcl(data_vol,reg1vol, data_vol$wficn, data_vol$yr)

reg1vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg1vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg1vol_rse <- mcl.plm(data_vol,reg1vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg1vol),digits=3,model.names=c("(1)"),override.se=list(reg1vol_rse[,2]),override.pval=list(reg1vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg2vol <- "avg_grade_level_pr + broad_cat_group_similarity_050pct_pr"
reg2vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg2vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg2vol_rse <- mcl.plm(data_vol,reg2vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg2vol),digits=3,model.names=c("(1)"),override.se=list(reg2vol_rse[,2]),override.pval=list(reg2vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg3vol <- "avg_grade_level_ios + avg_grade_level_pr"
reg3vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg3vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg3vol_rse <- mcl.plm(data_vol,reg3vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg3vol),digits=3,model.names=c("(1)"),override.se=list(reg3vol_rse[,2]),override.pval=list(reg3vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg4vol <- "broad_cat_group_similarity_050pct_ios + broad_cat_group_similarity_050pct_pr"
reg4vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg4vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg4vol_rse <- mcl.plm(data_vol,reg4vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg4vol),digits=3,model.names=c("(1)"),override.se=list(reg4vol_rse[,2]),override.pval=list(reg4vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg5vol <- "avg_grade_level_ios + broad_cat_group_similarity_050pct_ios + avg_grade_level_pr + broad_cat_group_similarity_050pct_pr"
reg5vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg5vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg5vol_rse <- mcl.plm(data_vol,reg5vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg5vol),digits=3,model.names=c("(1)"),override.se=list(reg5vol_rse[,2]),override.pval=list(reg5vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg6vol <- "avg_grade_level_ios + broad_cat_group_similarity_050pct_ios + 
avg_grade_level_pr + broad_cat_group_similarity_050pct_pr + aret_lag1 + asddret_agg_lag1 + amtna_agg_log"
reg6vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg6vol,sep="~")), data=data_vol.pd,model=model_type_vol)
reg6vol_rse <- mcl.plm(data_vol,reg6vol, data_vol[,identifier], data_vol[,"yr"])
#screenreg(list(reg6vol),digits=3,model.names=c("(1)"),override.se=list(reg6vol_rse[,2]),override.pval=list(reg6vol_rse[,4]),stars=c(0.01,0.05,0.1))

ind_vars_reg7vol <- "avg_grade_level_ios + broad_cat_group_similarity_050pct_ios + 
avg_grade_level_pr + broad_cat_group_similarity_050pct_pr + aret_lag1 + asddret_agg_lag1 + amtna_agg_log + 
factor(branding_name) + factor(yr)"
#ind_vars_reg7vol <- "avg_grade_level_ios + broad_cat_group_similarity_050pct_ios + 
#                     avg_grade_level_pr + broad_cat_group_similarity_050pct_pr + amtna_agg_log + asddret_agg + aret + 
#                     factor(branding_name)"
#reg7vol <- plm(as.formula(paste(dep_var_vol,ind_vars_reg7vol,sep="~")), data=data_vol.pd,model=model_type_vol)
#reg7vol_rse <- mcl.plm(data_vol,reg7vol, data_vol[,identifier], data_vol[,"yr"])
reg7vol <- lm(as.formula(paste(dep_var_vol,ind_vars_reg7vol,sep="~")), data=data_vol)
reg7vol_rse <- mcl(data_vol,reg7vol, data_vol[,identifier], data_vol[,"yr"])
#reg7vol_rse <- cl(data_vol,reg7vol, data_vol[,identifier])

reg7vol_rse <- ifelse(is.na(reg7vol_rse),0.0, reg7vol_rse)

#screenreg(list(reg7vol),digits=3,model.names=c("(1)"),override.se=list(reg7vol_rse[,2]),override.pval=list(reg7vol_rse[,4]),stars=c(0.01,0.05,0.1))

htmlreg(list(reg1vol,reg2vol,reg3vol,reg4vol,reg5vol,reg6vol,reg7vol), 
        caption="The Importance of Clustering Standard Errors", digits=3, 
        model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
        override.se=list(reg1vol_rse[,2],reg2vol_rse[,2],reg3vol_rse[,2],
                         reg4vol_rse[,2],reg5vol_rse[,2],reg6vol_rse[,2],reg7vol_rse[,2]),
        override.pval=list(reg1vol_rse[,4],reg2vol_rse[,4],reg3vol_rse[,4],
                           reg4vol_rse[,4],reg5vol_rse[,4],reg6vol_rse[,4],reg7vol_rse[,4]),
        stars=c(0.01, 0.05, 0.1),
        file=paste(output_directory,out_file_name_vol,".doc",sep=""))


rm2(ind_vars_reg1vol,ind_vars_reg2vol,ind_vars_reg3vol,ind_vars_reg4vol,ind_vars_reg5vol,ind_vars_reg6vol,ind_vars_reg7vol)
rm2(reg1vol,reg2vol,reg3vol,reg4vol,reg5vol,reg6vol,reg7vol,out_file_name_vol)
rm2(reg1vol_rse,reg2vol_rse,reg3vol_rse,reg4vol_rse,reg5vol_rse,reg6vol_rse,reg7vol_rse)



###############################################################################
cat("YOUNG FUNDS", "\n")
###############################################################################

funds_young <- data_all[data_all[,"age_m"]<=36,]

funds_young.pd <- pdata.frame(funds_young, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)

dep_var <- c("pct_flow")

index_vars <- c(identifier, "yr_month")

model_type <- "pooling"

note <- "xxx_young_funds"

#vars_grade <- "avg_grade_level_XXX"
vars_grade <- "coleman_liau_XXX"
vars_sim   <- "broad_cat_group_similarity_050pct_XXX"
controls  <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y + log_mtna_agg"
#dv_cat    <- "broad_cat_group_eq_dv + broad_cat_group_al_dv"
#dv_quin <- "ari_XXX_below_quartile1 + ari_XXX_above_quartile3 + 
#broad_cat_group_similarity_050pct_XXX_below_quartile1 + broad_cat_group_similarity_050pct_XXX_above_quartile3"
#dv_quin_int <- "mktadjret_agglag1:ari_XXX_below_quartile1 + 
#mktadjret_agglag1:ari_XXX_above_quartile3 + 
#mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
#mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"

dv_quin <- "broad_cat_group_similarity_050pct_XXX_below_quartile1 + broad_cat_group_similarity_050pct_XXX_above_quartile3"
dv_quin_int <- "mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_below_quartile1 + 
mktadjret_agglag1:broad_cat_group_similarity_050pct_XXX_above_quartile3"
#fe <- "factor(branding_name) + factor(yr)"
fe <- "factor(yr)"
#fe <- "factor(branding_name)"

for (i in 1:length(dep_var))
{
  #i <- 1
  
  out_file_name <- paste("reg_compare_plm",dep_var[i],deparse(substitute(funds_young)),note,sep="_")
  
  if (dep_var[i]=="pct_flow")
  {
    #vars_ext  <- "pflowlag1 + pflowlag2 + pflowlag3"
    vars_ext  <- ""
    
  } else if (dep_var[i]=="net_flow")
  {
    #vars_ext  <- "nflowlag1 + nflowlag2 + nflowlag3"
    vars_ext  <- ""
    
  } else
  {
    cat("ERROR", "\n")
  }
  
  ind_vars_reg_young_f1 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),sep="+")
  reg_young_f1 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f1,sep="~")),data=funds_young.pd,model=model_type)
  reg_young_f1_rse <- mcl.plm(funds_young,reg_young_f1, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f1),digits=3,model.names=c("(1)"),override.se=list(reg_young_f1_rse[,4]),override.pval=list(reg_young_f1_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f2 <- paste(gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg_young_f2 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f2,sep="~")), data=funds_young.pd,model=model_type)
  reg_young_f2_rse <- mcl.plm(funds_young,reg_young_f2, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f2),digits=3,model.names=c("(1)"),override.se=list(reg_young_f2_rse[,4]),override.pval=list(reg_young_f2_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f3 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","pr",vars_grade),sep="+")
  reg_young_f3 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f3,sep="~")), data=funds_young.pd,model=model_type)
  reg_young_f3_rse <- mcl.plm(funds_young,reg_young_f3, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f3),digits=3,model.names=c("(1)"),override.se=list(reg_young_f3_rse[,4]),override.pval=list(reg_young_f3_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f4 <- paste(gsub("XXX","ios",vars_sim),gsub("XXX","pr",vars_sim),sep="+")
  reg_young_f4 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f4,sep="~")), data=funds_young.pd,model=model_type)
  reg_young_f4_rse <- mcl.plm(funds_young,reg_young_f4, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f4),digits=3,model.names=c("(1)"),override.se=list(reg_young_f4_rse[,4]),override.pval=list(reg_young_f4_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f5 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                                 gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),sep="+")
  reg_young_f5 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f5,sep="~")), data=funds_young.pd,model=model_type)
  reg_young_f5_rse <- mcl.plm(funds_young,reg_young_f5, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f5),digits=3,model.names=c("(1)"),override.se=list(reg_young_f5_rse[,4]),override.pval=list(reg_young_f5_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f6 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                                 gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                                 #controls,dv_style,dv_cat,vars_ext,
                                 controls,
                                 #dv_cat,vars_ext,
                                 gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                                 gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                                 sep="+")
  reg_young_f6 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f6,sep="~")), data=funds_young.pd,model=model_type)
  reg_young_f6_rse <- mcl.plm(funds_young,reg_young_f6, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f6),digits=3,model.names=c("(1)"),override.se=list(reg_young_f6_rse[,4]),override.pval=list(reg_young_f6_rse[,4]),stars=c(0.01,0.05,0.1))
  
  ind_vars_reg_young_f7 <- paste(gsub("XXX","ios",vars_grade),gsub("XXX","ios",vars_sim),
                                 gsub("XXX","pr",vars_grade),gsub("XXX","pr",vars_sim),
                                 #controls,dv_style,dv_cat,vars_ext,
                                 controls,
                                 #dv_cat,vars_ext,
                                 gsub("XXX","ios",dv_quin),gsub("XXX","pr",dv_quin),
                                 gsub("XXX","ios",dv_quin_int),gsub("XXX","pr",dv_quin_int),
                                 fe,sep="+")
  #reg_young_f7 <- plm(as.formula(paste(dep_var[i],ind_vars_reg_young_f7,sep="~")), data=funds_young.pd,model=model_type)
  #reg_young_f7_rse <- mcl.plm(funds_young,reg_young_f7, funds_young[,identifier], funds_young[,"month"])
  reg_young_f7 <- lm(as.formula(paste(dep_var[i],ind_vars_reg_young_f7,sep="~")), funds_young)
  reg_young_f7_rse <- mcl(funds_young,reg_young_f7, funds_young[,identifier], funds_young[,"month"])
  #screenreg(list(reg_young_f7),digits=3,model.names=c("(1)"),override.se=list(reg_young_f7_rse[,4]),override.pval=list(reg_young_f7_rse[,4]),stars=c(0.01,0.05,0.1))
  
  htmlreg(list(reg_young_f1,reg_young_f2,reg_young_f3,reg_young_f4,reg_young_f5,reg_young_f6,reg_young_f7), 
          caption="The Importance of Clustering Standard Errors", digits=3, 
          model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
          override.se=list(reg_young_f1_rse[,4],reg_young_f2_rse[,4],reg_young_f3_rse[,4],
                           reg_young_f4_rse[,4],reg_young_f5_rse[,4],reg_young_f6_rse[,4],reg_young_f7_rse[,4]),
          override.pval=list(reg_young_f1_rse[,4],reg_young_f2_rse[,4],reg_young_f3_rse[,4],
                             reg_young_f4_rse[,4],reg_young_f5_rse[,4],reg_young_f6_rse[,4],reg_young_f7_rse[,4]),
          stars=c(0.01, 0.05, 0.1),
          file=paste(output_directory,out_file_name,".doc",sep=""))
  
  #custom.names
  
  rm2(ind_vars_reg_young_f1,ind_vars_reg_young_f2,ind_vars_reg_young_f3,ind_vars_reg_young_f4,ind_vars_reg_young_f5,ind_vars_reg_young_f6,ind_vars_reg_young_f7)
  rm2(reg_young_f1,reg_young_f2,reg_young_f3,reg_young_f4,reg_young_f5,reg_young_f6,reg_young_f7)
  rm2(reg_young_f1_rse,reg_young_f2_rse,reg_young_f3_rse,reg_young_f4_rse,reg_young_f5_rse,reg_young_f6_rse,reg_young_f7_rse,out_file_name)
  
} 