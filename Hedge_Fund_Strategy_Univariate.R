# TODO: Add comment
# 
# Author: Brad
# File: Hedge_Fund_Strategy_Univariate.R
# Version: 1.0
# Date: 05.20.2014
# Purpose: Run Univariate Analysis
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

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_all <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("UNIVARIATE ANALYSIS - VARIABLES", "\n")
###############################################################################

univariate_vars_dep <- c("pflow","mktadjret","exret")

#univariate_vars_dep <- c("pflow","mktadjret","exret",
#                         "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
#                         "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")

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

univariate_data_year_groups_continuous <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                     stringsAsFactors=FALSE)

univariate_data_year_groups_continuous[1,] <- c(start_year,end_year)
#univariate_data_year_groups_continuous[2,] <- c(2000,2011)
#univariate_data_year_groups_continuous[3,] <- c(2006,2011)
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

univariate_data_year_groups_binary <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                 stringsAsFactors=FALSE)

univariate_data_year_groups_binary[1,] <- c(start_year,end_year)
#univariate_data_year_groups_binary[2,] <- c(2000,2011)
#univariate_data_year_groups_binary[3,] <- c(2006,2011)
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

