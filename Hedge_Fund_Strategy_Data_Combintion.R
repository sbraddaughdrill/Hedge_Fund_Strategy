# TODO: Add comment
# 
# Author:  Brad
# File:    Data_Combination.R
# Version: 1.0
# Date:    03.17.2013
# Purpose: Combine all data sources
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
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

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 3


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


unknowns_strings <- c("",".",NA,"na","n/a","n\a","NA","N/A","N\\A","null","NULL",NULL,"nan","NaN",
                      NaN,NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",NA_character_,
                      "NA_character_",NA_real_,"NA_real_")
vector_clean_na <- function(x,unknowns){
  x <- unknownToNA(x, unknown=unknowns,force=TRUE)
  x <- ifelse(is.na(x),NA, x)
  return(x)
}

trim_dt <- function(x, cols) {
  DT <- data.table(x)
  for (j in cols) {
    set(DT, i=NULL, j=j, value=gsub("^\\s+|\\s+$", "", DT[[j]], perl=TRUE))
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}

char_to_date_dt <- function(x, cols,format) {
  DT <- data.table(x)
  for (j in cols) {
    set(DT, i=NULL, j=j, value=as.Date(DT[[j]],format=format))
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}

import_crsp_file_no_year <- function(table_name,fundnos){
  q_import_crsp_table <- ""
  q_import_crsp_table <- paste(q_import_crsp_table, "select       a.*                                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "from         ",table_name," as a                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "where        a.crsp_fundno in (select distinct    b.crsp_fundno           ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "                               from               ", fundnos ," as b)     ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "order by     a.crsp_fundno                                                ", sep=" ")
  q_import_crsp_table <- gsub(" {2,}", " ", q_import_crsp_table)
  table <- runsql(q_import_crsp_table,crsp_db)
  for (i in 1:ncol(table))
  {
    table[,i] <- unknownToNA(table[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
    table[,i] <- ifelse(is.na(table[,i]),NA, table[,i])
  } 
  return(table)
}

import_crsp_file_year <- function(table_name,fundnos,year_cutoff_low,year_cutoff_high){
  q_import_crsp_table <- ""
  q_import_crsp_table <- paste(q_import_crsp_table, "select       a.*                                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "from         ",table_name," as a                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "where        a.crsp_fundno in (select distinct    b.crsp_fundno           ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "                               from               ", fundnos ," as b)     ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "and          yr >=  " , year_cutoff_low,"                                 ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "and          yr <=  " , year_cutoff_high,"                                ", sep=" ") 
  q_import_crsp_table <- paste(q_import_crsp_table, "order by     a.crsp_fundno                                                ", sep=" ")
  q_import_crsp_table <- gsub(" {2,}", " ", q_import_crsp_table)
  table <- runsql(q_import_crsp_table,crsp_db)
  for (i in 1:ncol(table))
  {
    table[,i] <- unknownToNA(table[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
    table[,i] <- ifelse(is.na(table[,i]),NA, table[,i])
  } 
  return(table)
}

aggregate_crsp_variable <- function(tna_data,other_variable_data,variable){
  
  #tna_data <- monthly_tna_full
  #other_variable_data <- monthly_ret
  #variable <- "mret"
  #variable <- "mnav"
  
  
  other_variable_data <- other_variable_data[,c("crsp_fundno","yr","month",variable)]
  
  temp_data <- merge(tna_data, other_variable_data, 
                     by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  temp_data <- temp_data[c(identifier,"crsp_fundno","yr","month","mtna",variable)]
  
  temp_data <- temp_data[order(temp_data[,identifier], 
                               temp_data[,"crsp_fundno"],                                         
                               temp_data[,"yr"], 
                               temp_data[,"month"]),] 
  
  temp_data_trim <- subset(temp_data,select=-c(crsp_fundno))
  
  temp_data_trim <- temp_data_trim[c(identifier,"yr","month","mtna",variable)]
  
  temp_data_trim <- temp_data_trim[order(temp_data_trim[,identifier], 
                                         temp_data_trim[,"yr"], 
                                         temp_data_trim[,"month"]),] 
  
  #test0 <- temp_data_trim[temp_data_trim[,identifier]==100166 & temp_data_trim[,"yr"]==2000,]
  
  sumtna <- as.data.frame(data.table(temp_data_trim)[, list(summtna=sum(mtna)),by="Fund_ID,yr,month"],stringsAsFactors=FALSE)
  
  #rm2(temp_data_trim)
  
  temp_data_sum_tna <- merge(temp_data[,-match(variable,names(temp_data))], sumtna, 
                             by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  #rm2(sumtna)
  
  temp_data_sum_tna <- temp_data_sum_tna[c(identifier,"crsp_fundno","yr","month","mtna","summtna")]
  
  temp_data_sum_tna <- temp_data_sum_tna[order(temp_data_sum_tna[,identifier], 
                                               temp_data_sum_tna[,"crsp_fundno"], 
                                               temp_data_sum_tna[,"yr"], 
                                               temp_data_sum_tna[,"month"]),] 
  
  #test1 <- temp_data_sum_tna[temp_data_sum_tna[,identifier]==100166 & temp_data_sum_tna[,"yr"]==2000,]
  
  temp_data_weights0 <- as.data.frame(data.table(temp_data_sum_tna)[, list(weight=mtna/summtna),by="crsp_fundno,yr,month"],stringsAsFactors=FALSE)
  
  #rm2(temp_data_sum_tna)
  
  temp_data_weights <- merge(subset(temp_data,select=-c(mtna)), temp_data_weights0, 
                             by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  #rm2(temp_data,temp_data_weights0)
  
  temp_data_weights <- temp_data_weights[c(identifier,"crsp_fundno","yr","month","weight",variable)]
  
  temp_data_weights <- temp_data_weights[order(temp_data_weights[,identifier], 
                                               temp_data_weights[,"crsp_fundno"], 
                                               temp_data_weights[,"yr"], 
                                               temp_data_weights[,"month"]),] 
  
  #test2 <- temp_data_weights[temp_data_weights[,identifier]==100166 & temp_data_weights[,"yr"]==2000,]
  
  temp_weighted <- temp_data_weights[,"weight"]* temp_data_weights[,variable]
  
  temp_data_agg1 <- cbind(temp_data_weights,temp_weighted)
  
  #test3 <- temp_data_agg1[temp_data_agg1[,identifier]==100166 & temp_data_agg1[,"yr"]==2000,]
  
  #rm2(temp_data_weights,temp_weighted)
  
  temp_data_agg1_trim <- temp_data_agg1[,-match(c("crsp_fundno","weight",variable),names(temp_data_agg1))]
  
  #rm2(temp_data_agg1)
  
  temp_data_agg1_trim <- temp_data_agg1_trim[order(temp_data_agg1_trim[,identifier], 
                                                   temp_data_agg1_trim[,"yr"], 
                                                   temp_data_agg1_trim[,"month"]),] 
  
  temp_data_agg <- as.data.frame(data.table(temp_data_agg1_trim)[, list(agg_temp=sum(temp_weighted)),
                                                                 by="Fund_ID,yr,month"],stringsAsFactors=FALSE)
  
  colnames(temp_data_agg)[4] <- paste(variable,"_agg",sep="")
  
  #test4 <- temp_data_agg[temp_data_agg[,identifier]==100166 & temp_data_agg[,"yr"]==2000,]
  
  #rm2(temp_data_agg1_trim)
  
  return(temp_data_agg)
  
}


create_lags2 <- function(data_in,variable,group,lags){
  
  #data_in <- EurekahedgeHF_Excel_aca_full6[,c("Fund_ID","yr","month","date","AUM","Monthly_Ret","mktadjret")]
  #variable <- "AUM"
  #group <- identifier
  #lags <- 2
  
  lagmatrix <- function(x,max.lag){
    embed(c(rep(NA,max.lag),x),max.lag+1)
  }
  
  lag_df <- ddply(data_in, group, .fun = function(x,maxlag=lags,col=variable){lagmatrix(x[,col],maxlag)},
                  .progress = "text", .inform = FALSE, .drop = TRUE,
                  .parallel = FALSE, .paropts = NULL)
  colnames(lag_df) <- c(group,variable,paste(variable,"_lag",seq(1,lags),sep=""))
  
  return(lag_df[,(ncol(lag_df)-lags+1):ncol(lag_df)])
  
}


calculate_similarity_by_group <- function(merged_data,group_var,group_var_value,text_type){
  
  
  #i <- 1
  #i <- 4
  #j <- 1
  
  #merged_data <- temp_stacked_full
  #group_var <- "Broad_Cat_Group"
  #group_var_value <- text_group_vars[i]
  #text_type <- text_variables[j]
  
  temp_stacked <- merged_data[toupper(merged_data[,group_var])==group_var_value,]
  temp_stacked <- temp_stacked[order(temp_stacked[,"yr"],temp_stacked[,identifier]),] 
  
  for (k in 1:length(text_percentages))
  {
    #k <- 1
    temp_years <- get(paste("years",text_type,text_percentages[k],sep="_"))
    
    temp_avg <- data.frame(yr=integer(),group=character(),Fund_ID=integer(),avg_similarity=numeric(),stringsAsFactors=FALSE) 
    
    for (l in 1:length(temp_years))
    {
      #l <- 1
      #l <- 10
      
      temp_input_df_name <- paste(temp_input_data_name_short,text_percentages[k],text_type,temp_years[l],sep="_")
      temp_sql_str <- paste("SELECT * FROM",temp_input_df_name,sep=" ")
      temp_data <- runsql(temp_sql_str,similarity_db)
      
      for(m in which(sapply(temp_data,class)=="character"))
      {
        temp_data[[m]] = trim(temp_data[[m]])
      }
      for (m in 1:ncol(temp_data))
      {
        temp_data[,m] <- unknownToNA(temp_data[,m], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
        temp_data[,m] <- ifelse(is.na(temp_data[,m]),NA, temp_data[,m])
      } 
      
      temp_data <- temp_data[order(temp_data[,"yr"],temp_data[,identifier]),] 
      
      temp_col_mat_df <- as.matrix(temp_data[,3:ncol(temp_data)])
      
      #Make diagonal NA
      diag(temp_col_mat_df) <- NA
      
      temp_id_cat <- unique(temp_stacked[,c(identifier,group_var)],comparables=FALSE)
      temp_id_cat <- temp_id_cat[!(is.na(temp_id_cat[,group_var])),]
      
      temp_data_obj <- merge(temp_id_cat, temp_data[,c("yr",identifier)], 
                             by.x=c(identifier), by.y=c(identifier), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      #temp_data_obj <- merge(temp_id_cat, temp_data[,c("yr",identifier)], 
      #                       by.x=c(identifier), by.y=c(identifier), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
      temp_data_obj <- temp_data_obj[order(temp_data_obj[,"yr"],temp_data_obj[,identifier]),] 
      
      temp_data_full <- data.frame(yr=temp_data_obj[,"yr"],
                                   group=temp_data_obj[,group_var],
                                   Fund_ID=temp_data_obj[,identifier],
                                   temp_col_mat_df,stringsAsFactors=FALSE)
      
      temp_data_full <- temp_data_full[order(temp_data_full[,"yr"],temp_data_full[,identifier]),] 
      
      #Seperate by group
      temp_data_full_no_na <- temp_data_full[!(is.na(temp_data_full[,"group"])),]
      
      if(nrow(temp_data_full_no_na)>0)
      {
        temp_data_full_no_na_ids <- paste("X",temp_data_full_no_na[,identifier],sep="")
        temp_data_full_no_na_trim <- temp_data_full_no_na[,c("yr","group",identifier,temp_data_full_no_na_ids)]
        temp_data_full_no_na_trim[,identifier] <- as.integer(temp_data_full_no_na_trim[,identifier])
        
        temp_data_full_cat_temp <- temp_data_full_no_na_trim[toupper(temp_data_full_no_na_trim[,"group"])==group_var_value,]
        
        
        if(nrow(temp_data_full_cat_temp)==1)
        {
          temp_data_full_cat_temp_ids <- paste("X",temp_data_full_cat_temp[,identifier],sep="")
          temp_data_full_cat_temp_trim <- temp_data_full_cat_temp[,c("yr","group",identifier,temp_data_full_cat_temp_ids)]
          
          temp_data_full_cat_temp_avg <- data.frame(yr=temp_data_full_cat_temp_trim[,"yr"],
                                                    group=temp_data_full_cat_temp_trim[,"group"],
                                                    Fund_ID=temp_data_full_cat_temp_trim[,identifier],
                                                    avg_similarity=temp_data_full_cat_temp_trim[,4],
                                                    stringsAsFactors=FALSE)
          
          temp_avg <- rbind(temp_avg,temp_data_full_cat_temp_avg)
          
          rm2(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
        } else if (nrow(temp_data_full_cat_temp)>1)
        {
          temp_data_full_cat_temp_ids <- paste("X",temp_data_full_cat_temp[,identifier],sep="")
          temp_data_full_cat_temp_trim <- temp_data_full_cat_temp[,c("yr","group",identifier,temp_data_full_cat_temp_ids)]
          
          temp_data_full_cat_temp_avg <- data.frame(yr=temp_data_full_cat_temp_trim[,"yr"],
                                                    group=temp_data_full_cat_temp_trim[,"group"],
                                                    Fund_ID=temp_data_full_cat_temp_trim[,identifier],
                                                    avg_similarity=rowMeans(temp_data_full_cat_temp_trim[,4:ncol(temp_data_full_cat_temp_trim)], na.rm = TRUE),
                                                    stringsAsFactors=FALSE)
          
          temp_avg <- rbind(temp_avg,temp_data_full_cat_temp_avg)
          
          rm2(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
        } else
        {
          rm2(temp_data_full_cat_temp)
          
        }
        
        rm2(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm2(temp_data_full_no_na,temp_data_full_no_na_ids,temp_data_full_no_na_trim)
        
      } else
      {
        rm2(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm2(temp_data_full_no_na)
        
      }
      
    }
    
    colnames(temp_avg)[4] <- paste("similarity",text_percentages[k],text_type,sep="_")
    
    temp_stacked <- merge(temp_stacked, subset(temp_avg,select=-c(group)), by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
    
    #rm2(temp_years,temp_avg)
    
  }
  
  temp_stacked <- temp_stacked[!(rowSums(is.na(temp_stacked[,4:ncol(temp_stacked)]))==(ncol(temp_stacked)-3)),]
  temp_stacked <- temp_stacked[order(temp_stacked[,identifier],temp_stacked[,"yr"]),] 
  
  return(temp_stacked)
  
}

empty.df<- function(header){
  
  df<-data.frame(matrix(matrix(rep(1,length(header)),1),1))
  colnames(df)<-header
  return(df[NULL,])
  
}





###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

update.packages(ask=FALSE, checkBuilt=TRUE)

#Load External Packages
#external_packages <- c("arules","bigalgebra","biganalytics","bigmemory","bigtabulate","clv","ff",
#                       "foreach","pracma","rJava","snow","Snowball","snowfall","synchronicity","XML")
external_packages <- c("compare","cwhmisc","data.table","DataCombine","fastmatch","foreign","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
                       "pander","pbapply","plm","plyr","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
                       "sandwich","sqldf","stargazer","stringr","SWordInstaller","texreg","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


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

read_stats_ios_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

#for(i in which(sapply(read_stats_ios_f,class)=="character"))
#{
#  read_stats_ios_f[[i]] = trim(read_stats_ios_f[[i]])
#}

read_stats_ios_f_char_cols <- colnames(read_stats_ios_f)[laply(read_stats_ios_f,class,.progress = "text",.drop = FALSE)=="character"]
read_stats_ios_f <- trim_dt(read_stats_ios_f,read_stats_ios_f_char_cols)
read_stats_ios_f <- as.data.frame(read_stats_ios_f,stringsAsFactors=FALSE)

#for (i in 1:ncol(read_stats_ios_f))
#{
#  read_stats_ios_f[,i] <- unknownToNA(read_stats_ios_f[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#  read_stats_ios_f[,i] <- ifelse(is.na(read_stats_ios_f[,i]),NA, read_stats_ios_f[,i])
#}
read_stats_ios_f <- data.table(read_stats_ios_f)[, (colnames(read_stats_ios_f)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(read_stats_ios_f)]
read_stats_ios_f <- as.data.frame(read_stats_ios_f,stringsAsFactors=FALSE)

sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

#for(i in which(sapply(sample_data_all,class)=="character"))
#{
#  sample_data_all[[i]] = trim(sample_data_all[[i]])
#}
sample_data_all_char_cols <- colnames(sample_data_all)[laply(sample_data_all,class,.progress = "text",.drop = FALSE)=="character"]
sample_data_all <- trim_dt(sample_data_all,sample_data_all_char_cols)
sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)

#for (i in 1:ncol(sample_data_all))
#{
#  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
#} 
sample_data_all <- data.table(sample_data_all)[, (colnames(sample_data_all)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(sample_data_all)]
sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)

read_stats_ios_f <- subset(read_stats_ios_f,select=-c(foreign_ios))
sample_data_all <- subset(sample_data_all,select=-c(Strategy))


###############################################################################
cat("IMPORT SIMILARITY TEXT DATA", "\n")
###############################################################################

text_variables <- c("ios")
text_percentages <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#text_percentages <- c("900pct")

temp_input_data_name_short <- "year_id_unique_sim_cosine_normalized"

sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]

for (j in 1:length(text_variables))
{
  #j <- 1
  
  assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
  
}
rm2(sample_data_all_temp)

for (i in 1:length(text_variables))
{
  #i <- 1
  
  for (j in 1:length(text_percentages))
  {
    
    #j <- 1
    
    temp_input_data_name_full <- paste(temp_input_data_name_short,text_percentages[j],text_variables[i],"avg",sep="_")
    
    year_sim <- as.data.frame(fread(paste(output_directory,temp_input_data_name_full,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
    
    #assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
    
    #for(k in which(sapply(year_sim,class)=="character"))
    #{
    #  year_sim[[k]] = trim(year_sim[[k]])
    #}
    
    year_sim_char_cols <- colnames(year_sim)[laply(year_sim,class,.progress = "text",.drop = FALSE)=="character"]
    year_sim <- trim_dt(year_sim,year_sim_char_cols)
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    #for (k in 1:ncol(year_sim))
    #{
    #  year_sim[,k] <- unknownToNA(year_sim[,k], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
    #  year_sim[,k] <- ifelse(is.na(year_sim[,k]),NA, year_sim[,k])
    #}
    
    year_sim <- data.table(year_sim)[, (colnames(year_sim)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(year_sim)]
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    year_sim_years <- colnames(year_sim)
    year_sim_years <- year_sim_years[!(year_sim_years==identifier)] 
    
    assign(paste("years",text_variables[i],text_percentages[j],sep="_"), year_sim_years, envir=.GlobalEnv)
    
    year_sim_stacked0 <- lapply(year_sim_years, function(x,data){ data.frame(Fund_ID=data[,identifier], 
                                                                             yr=as.integer(x), 
                                                                             similarity_all_ios=data[,x], stringsAsFactors=FALSE) }, data = year_sim)
    
    year_sim_stacked <- do.call(rbind, year_sim_stacked0) 
    year_sim_stacked <- year_sim_stacked[order(year_sim_stacked[,identifier],year_sim_stacked[,"yr"]),] 
    colnames(year_sim_stacked) <- c(identifier,"yr",paste("similarity",text_percentages[j],text_variables[i],sep="_"))
    
    if (text_variables[i]=="ios")
    {
      year_sim_ios_all_stacked <- merge(year_sim_ios_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else if (text_variables[i]=="pr")
    {
      year_sim_pr_all_stacked <- merge(year_sim_pr_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_variables), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_percentages))
    
    rm2(year_sim,year_sim_years,year_sim_stacked0,year_sim_stacked)
    
  }
  
} 

year_sim_ios_all_stacked <- year_sim_ios_all_stacked[!(rowSums(is.na(year_sim_ios_all_stacked[,3:ncol(year_sim_ios_all_stacked)]))==(ncol(year_sim_ios_all_stacked)-2)),]
year_sim_ios_all_stacked <- year_sim_ios_all_stacked[order(year_sim_ios_all_stacked[,identifier], 
                                                           year_sim_ios_all_stacked[,"yr"]),]
colnames(year_sim_ios_all_stacked) <- paste("all",colnames(year_sim_ios_all_stacked),sep="_")
colnames(year_sim_ios_all_stacked)[1:2] <- c(identifier,"yr")

# 
# 
# ###############################################################################
# cat("IMPORT MFLINKS DATA", "\n")
# ###############################################################################
# 
# mflinks_tables <- ListTables(mflinks_db)
# mflinks_fields <- ListFields(mflinks_db)
# 
# mflink1_fields <- mflinks_fields[mflinks_fields[,1]=="mflink1",]
# mflink1 <- runsql("SELECT * FROM mflink1",mflinks_db)
# 
# for(i in which(sapply(mflink1,class)=="character"))
# {
#   mflink1[[i]] = trim(mflink1[[i]])
# }
# 
# for (i in 1:ncol(mflink1))
# {
#   mflink1[,i] <- unknownToNA(mflink1[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#   mflink1[,i] <- ifelse(is.na(mflink1[,i]),NA, mflink1[,i])
# } 
# 
# mflink1 <- mflink1[!(is.na(mflink1[,"crsp_fundno"])),]
# 
# 
# 
# 
# ###############################################################################
# cat("IMPORT MDMF DATA", "\n")
# ###############################################################################
# 
# mdmf_tables <- ListTables(msd_db)
# mdmf_fields <- ListFields(msd_db)
# 
# mdmf_fields <- mdmf_fields[mdmf_fields[,1]=="Mdmf_data_raw",]
# Mdmf_data_raw <- runsql("SELECT * FROM Mdmf_data_raw",msd_db)
# 
# for(i in which(sapply(Mdmf_data_raw,class)=="character"))
# {
#   Mdmf_data_raw[[i]] = trim(Mdmf_data_raw[[i]])
# }
# 
# for (i in 1:ncol(Mdmf_data_raw))
# {
#   Mdmf_data_raw[,i] <- unknownToNA(Mdmf_data_raw[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#   Mdmf_data_raw[,i] <- ifelse(is.na(Mdmf_data_raw[,i]),NA, Mdmf_data_raw[,i])
# } 
# 
# Mdmf_data_trim <- subset(Mdmf_data_raw,select=c(CUSIP, Broad_Cat_Group, Global_Cat, MS_Cat, MS_Inst_Cat, 
#                                                 MS_Rating_Overall, US_Broad_Asset_Class, Equity_Style_Box_Long, MS_Anal_Rating, 
#                                                 Firm_Name, Branding_Name, Prospectus_Objective))
# Mdmf_data_trim <- Mdmf_data_trim[order(Mdmf_data_trim[,"CUSIP"]),]
# # 
# 
# 
# 
# 
# ###############################################################################
# cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN SMAPLE_DATA_ALL", "\n")
# ###############################################################################
# 
# sample_data_all <- merge(sample_data_all, mflink1, by.x=c("wficn"), by.y=c("wficn"), 
#                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# sample_data_all <- sample_data_all[c("wficn","crsp_fundno","yr")]
# sample_data_all <- sample_data_all[order(sample_data_all[,"wficn"], sample_data_all[,"crsp_fundno"], sample_data_all[,"yr"]),]
# 
# 
###############################################################################
cat("IMPORT CRSP DATA", "\n")
###############################################################################

#crsp_fundno_unique <- data.frame(crsp_fundno=unique(sample_data_all[,c("crsp_fundno")], incomparables=FALSE),stringsAsFactors=FALSE)

#ExportTable(crsp_db,"crsp_fundno_unique",crsp_fundno_unique)

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

Crspa_msi <- runsql("SELECT * FROM Crspa_msi",crsp_db)

# for(i in which(sapply(Crspa_msi,class)=="character"))
# {
#   Crspa_msi[[i]] <- trim(Crspa_msi[[i]])
# }

#test<- colnames(Crspa_msi)[which(sapply(Crspa_msi,class)=="character")]
#test<- colnames(Crspa_msi)[laply(Crspa_msi,class,.progress = "text",.drop = FALSE)=="character"]
#test<- Crspa_msi[,laply(Crspa_msi,class,.progress = "text",.drop = FALSE)=="character"]
#Crspa_msi_char_cols <- colnames(Crspa_msi[,laply(Crspa_msi,class,.progress = "text",.drop = FALSE)=="character"])
#Crspa_msi_char_cols <- colnames(Crspa_msi[,laply(Crspa_msi,class,.progress = "text",.drop = FALSE)=="character"])

Crspa_msi_char_cols <- colnames(Crspa_msi)[laply(Crspa_msi,class,.progress = "text",.drop = FALSE)=="character"]
#Crspa_msi <- data.table(Crspa_msi)[, (Crspa_msi_char_cols) := trim(.SD), .SDcols = Crspa_msi_char_cols]
Crspa_msi <- trim_dt(Crspa_msi,Crspa_msi_char_cols)
Crspa_msi <- as.data.frame(Crspa_msi,stringsAsFactors=FALSE)

#for (i in 1:ncol(Crspa_msi))
# {
#   Crspa_msi[,i] <- unknownToNA(Crspa_msi[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#   Crspa_msi[,i] <- ifelse(is.na(Crspa_msi[,i]),NA, Crspa_msi[,i])
# } 

#Crspa_msi <- sapply(Crspa_msi, vector_clean_na, unknowns=unknowns_strings)
#Crspa_msi <- as.data.frame(Crspa_msi, stringsAsFactors=FALSE)

Crspa_msi <- data.table(Crspa_msi)[, (colnames(Crspa_msi)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(Crspa_msi)]
Crspa_msi <- as.data.frame(Crspa_msi,stringsAsFactors=FALSE)

#crsp_tables_to_import <- c("fund_hdr", "fund_hdr_hist", "Fund_names", "fund_summary", "fund_summary2","monthly_tna_ret_nav")
#crsp_tables_no_year_import <- c("Daily_returns","fund_hdr","fund_hdr_hist","Fund_names","monthly_tna_ret_nav")
#for (i in 1:length(crsp_tables_no_year_import))
#{

#  assign(tolower(crsp_tables_no_year_import[i]),import_crsp_file_no_year(crsp_tables_no_year_import[i],"crsp_fundno_unique"), envir=.GlobalEnv)

#  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(crsp_tables_no_year_import), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)

#} 

#crsp_tables_year_import <- c("fund_fees_month","fund_style_month")
#for (i in 1:length(crsp_tables_year_import))
#{

#  assign(tolower(crsp_tables_year_import[i]),import_crsp_file_year(crsp_tables_year_import[i],"crsp_fundno_unique","1998","2012"), envir=.GlobalEnv)

#  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(crsp_tables_year_import), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)

#} 

#DeleteTable(crsp_db,"crsp_fundno_unique")

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

###############################################################################
cat("IMPORT EUREKA HEDGE DATA", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full <- read.csv(file=paste(output_directory,"EurekahedgeHF_Excel_aca_merge",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[rowSums(is.na(EurekahedgeHF_Excel_aca_full[,1:ncol(EurekahedgeHF_Excel_aca_full)]))<ncol(EurekahedgeHF_Excel_aca_full),]

colnames(EurekahedgeHF_Excel_aca_full)[match("Fund.ID",names(EurekahedgeHF_Excel_aca_full))] <- identifier
colnames(EurekahedgeHF_Excel_aca_full)[match("Fund.Name",names(EurekahedgeHF_Excel_aca_full))] <- "Fund_Name"
EurekahedgeHF_Excel_aca_full  <- EurekahedgeHF_Excel_aca_full[order(EurekahedgeHF_Excel_aca_full[,identifier],EurekahedgeHF_Excel_aca_full[,"Fund_Name"],EurekahedgeHF_Excel_aca_full[,"yr"]),]

#for(i in which(sapply(EurekahedgeHF_Excel_aca_full,class)=="character"))
#{
#  EurekahedgeHF_Excel_aca_full[[i]] = trim(EurekahedgeHF_Excel_aca_full[[i]])
#}

EurekahedgeHF_Excel_aca_full_char_cols <- colnames(EurekahedgeHF_Excel_aca_full)[laply(EurekahedgeHF_Excel_aca_full,class,.progress = "text",.drop = FALSE)=="character"]

#EurekahedgeHF_Excel_aca_full <- data.table(EurekahedgeHF_Excel_aca_full)[, (EurekahedgeHF_Excel_aca_full_char_cols) := trim(.SD), .SDcols = EurekahedgeHF_Excel_aca_full_char_cols]
EurekahedgeHF_Excel_aca_full <- trim_dt(EurekahedgeHF_Excel_aca_full,EurekahedgeHF_Excel_aca_full_char_cols)
EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)


#for (i in 1:ncol(EurekahedgeHF_Excel_aca_full))
#{
#  EurekahedgeHF_Excel_aca_full[,i] <- unknownToNA(EurekahedgeHF_Excel_aca_full[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#  EurekahedgeHF_Excel_aca_full[,i] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[,i]),NA, EurekahedgeHF_Excel_aca_full[,i])
#} 

#EurekahedgeHF_Excel_aca_full <- sapply(EurekahedgeHF_Excel_aca_full, function(x,unknowns){
#  x <- unknownToNA(x, unknown=unknowns,force=TRUE)
#  x <- ifelse(is.na(x),NA, x)
#},unknowns=unknowns_strings)
#EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full, stringsAsFactors=FALSE)

#EurekahedgeHF_Excel_aca_full <- llply(EurekahedgeHF_Excel_aca_full, function(x,unknowns){
#  x <- unknownToNA(x, unknown=unknowns,force=TRUE)
#  x <- ifelse(is.na(x),NA, x)
#},unknowns=unknowns_strings,.progress = "text")
#EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full, stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full <- data.table(EurekahedgeHF_Excel_aca_full)[, (colnames(EurekahedgeHF_Excel_aca_full)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(EurekahedgeHF_Excel_aca_full)]
EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)


#test <- EurekahedgeHF_Excel_aca_full[,c("Date.Added","Dead.Date","Inception.Date")] 
#test1 <- unique(EurekahedgeHF_Excel_aca_full[,c("Date.Added","Dead.Date","Inception.Date")])
#test1[,"Date.Added"] <- as.Date(test1[,"Date.Added"],format="%Y-%m-%d")

EurekahedgeHF_Excel_aca_full_char_to_date_cols <- c("Date.Added","Dead.Date","Inception.Date","date")
#for (i in 1:length(EurekahedgeHF_Excel_aca_full_char_to_date_cols))
#{
#  EurekahedgeHF_Excel_aca_full[,EurekahedgeHF_Excel_aca_full_char_to_date_cols[i]] <- as.Date(EurekahedgeHF_Excel_aca_full[,EurekahedgeHF_Excel_aca_full_char_to_date_cols[i]],format="%Y-%m-%d")
#} 
EurekahedgeHF_Excel_aca_full <- char_to_date_dt(EurekahedgeHF_Excel_aca_full,EurekahedgeHF_Excel_aca_full_char_to_date_cols,"%Y-%m-%d")
EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[rowSums(is.na(EurekahedgeHF_Excel_aca_full[,1:ncol(EurekahedgeHF_Excel_aca_full)]))<ncol(EurekahedgeHF_Excel_aca_full),]


###############################################################################
cat("CONVERT PERCENTAGES TO DECIMAL", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full

decimal_cols <- c("Annualised.Return","Best.Monthly.Return","Worst.Monthly.Return","Yearly_Ret","Rise.in.NAV.Since.Inception",
                  "Last.3.Months","One.Year.Rolling.Return","Two.Year.Rolling.Return","Five.Year.Rolling.Return",
                  "Annualised.Standard.Deviation","Downside.Deviation","Upside.Deviation","Maximum.Drawdown",
                  "Percentage.of.Positive.Months")

#test <- EurekahedgeHF_Excel_aca_full2[,decimal_cols]

for (i in 1:length(decimal_cols))
{
  #i <- 1
  
  EurekahedgeHF_Excel_aca_full2[,decimal_cols[i]] <- (EurekahedgeHF_Excel_aca_full2[,decimal_cols[i]]/100)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(decimal_cols), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[rowSums(is.na(EurekahedgeHF_Excel_aca_full2[,1:ncol(EurekahedgeHF_Excel_aca_full2)]))<ncol(EurekahedgeHF_Excel_aca_full2),]


###############################################################################
cat("CODE MISSING VARIABLES AS NA", "\n")
###############################################################################

Crspa_msi[,"VWRETD"] <- ifelse((Crspa_msi$VWRETD==-99), NA, Crspa_msi$VWRETD)

Crspa_msi[,"VWRETX"] <- ifelse((Crspa_msi$VWRETX==-99), NA, Crspa_msi$VWRETX)


# ###############################################################################
# cat("CREATE YEAR AND MONTH VARIABLE FOR Crspa_msi", "\n")
# ###############################################################################

crspa_msi_full <- transform(Crspa_msi,yr=year(as.IDate(CALDT)),month=month(as.IDate(CALDT)))

crspa_msi_trim <- subset(crspa_msi_full,select=c(yr,month,VWRETD,VWRETX))


# ###############################################################################
# cat("COMPUTE MONTHLY FUND RETURN VOLATILITY", "\n")
# ###############################################################################
# 
# daily_returns_full <- transform(daily_returns,yr=year(as.IDate(caldt)),month=month(as.IDate(caldt)))
# 
# daily_returns_trim <- subset(daily_returns_full,select=-c(caldt))
# 
# fund_mret_volatility <- ddply(daily_returns_trim, c("crsp_fundno","yr","month"), summarize, sddret = sd(dret, na.rm = TRUE))
# 
# 


###############################################################################
cat("MERGE MONTHLY_TNA_RET_NAV2 AND INDEX RETURNS/FUND VOLATILITY", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full3 <- merge(EurekahedgeHF_Excel_aca_full2, crspa_msi_trim, 
                                       by.x=c("yr","month"), by.y=c("yr","month"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

#EurekahedgeHF_Excel_aca_full3 <- transform(EurekahedgeHF_Excel_aca_full3, mktadjret=Monthly_Ret-VWRETX)
EurekahedgeHF_Excel_aca_full3 <- transform(EurekahedgeHF_Excel_aca_full3, mktadjret=Monthly_Ret-VWRETD)

EurekahedgeHF_Excel_aca_full3 <- EurekahedgeHF_Excel_aca_full3[rowSums(is.na(EurekahedgeHF_Excel_aca_full3[,1:ncol(EurekahedgeHF_Excel_aca_full3)]))<ncol(EurekahedgeHF_Excel_aca_full3),]

EurekahedgeHF_Excel_aca_full3 <- EurekahedgeHF_Excel_aca_full3[order(EurekahedgeHF_Excel_aca_full3[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full3[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full3[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full3) <- seq(nrow(EurekahedgeHF_Excel_aca_full3))


###############################################################################
cat("CREATE LAG OF TNA AND RETURNS", "\n")
###############################################################################


#duplicates1 <- sqldf("SELECT *, count(Fund_ID) as count FROM EurekahedgeHF_Excel_aca_full6 group by Fund_ID,date")
#duplicates2 <- sqldf("SELECT * FROM duplicates1 where count<>1")
#data_u <- unique(data_in[,c(group,time)])

#returns_short <- returns[(returns[,identifier]==5002 | returns[,identifier]==5003) ,]

lag_vars <- c("AUM","Monthly_Ret","mktadjret")
lag_count <- 4

#[,c("Fund_ID","yr","month","date",lag_vars)]

EurekahedgeHF_Excel_aca_full4 <- data.frame(EurekahedgeHF_Excel_aca_full3,
                      matrix(NA, ncol=length(lag_vars)*lag_count, nrow=nrow(EurekahedgeHF_Excel_aca_full3)),
                      stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full4) <- c(colnames(EurekahedgeHF_Excel_aca_full3),
                       paste(lag_vars[1],"_lag",seq(1,lag_count),sep=""),
                       paste(lag_vars[2],"_lag",seq(1,lag_count),sep=""),
                       paste(lag_vars[3],"_lag",seq(1,lag_count),sep=""))
                      

for (i in 1:length(lag_vars))
{
  #i <- 1
  #i <- 2
  
  EurekahedgeHF_Excel_aca_full4[,paste(lag_vars[i],"_lag",seq(1,lag_count),sep="")] <- create_lags2(EurekahedgeHF_Excel_aca_full4,lag_vars[i],identifier,lag_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(lag_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[,sort(colnames(EurekahedgeHF_Excel_aca_full4))]

starting_cols <- c(identifier,"Fund_Name","Date.Added","Flagship","Closed","Limited","Dead","Dead.Date","Dead.Reason",
                   "Eurekahedge.ID","ISIN","SEDOL","Valoren","CUSIP","Bloomberg","Reuters","date","yr","month",
                   "AUM",paste("AUM","_lag",seq(1,lag_count),sep=""),
                   "Monthly_Ret",paste("Monthly_Ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "Monthly_Ret2","Yearly_Ret")

all_cols <- colnames(EurekahedgeHF_Excel_aca_full4)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[rowSums(is.na(EurekahedgeHF_Excel_aca_full4[,1:ncol(EurekahedgeHF_Excel_aca_full4)]))<ncol(EurekahedgeHF_Excel_aca_full4),]

EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[order(EurekahedgeHF_Excel_aca_full4[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full4[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full4[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full4) <- seq(nrow(EurekahedgeHF_Excel_aca_full4))


###############################################################################
cat("CREATE FLOW AND LAG FLOW", "\n")
###############################################################################

lag_flow_vars <- c("nflow","pflow")
lag_flow_count <- 4

EurekahedgeHF_Excel_aca_full5 <- data.frame(EurekahedgeHF_Excel_aca_full4,
                                            nflow=NA,
                                            pflow=NA,
                                            matrix(NA, ncol=length(lag_flow_vars)*lag_flow_count, nrow=nrow(EurekahedgeHF_Excel_aca_full4)),
                                            stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full5) <- c(colnames(EurekahedgeHF_Excel_aca_full4),
                                             lag_flow_vars[1],
                                             lag_flow_vars[2],
                                             paste(lag_flow_vars[1],"lag",seq(1,lag_flow_count),sep=""),
                                             paste(lag_flow_vars[2],"lag",seq(1,lag_flow_count),sep=""))

EurekahedgeHF_Excel_aca_full5[,"nflow"] <- as.data.frame(data.table(EurekahedgeHF_Excel_aca_full5)[,list(nflow=AUM-(AUM_lag1*(1+Monthly_Ret))),by=identifier], stringsAsFactors=FALSE)["nflow"]
EurekahedgeHF_Excel_aca_full5[,"pflow"] <- as.data.frame(data.table(EurekahedgeHF_Excel_aca_full5)[,list(pflow=((AUM-(AUM_lag1*(1+Monthly_Ret)))/AUM_lag1)),by=identifier], stringsAsFactors=FALSE)["pflow"]

for (i in 1:length(lag_flow_vars))
{
  #i <- 1
  #i <- 2
  
  EurekahedgeHF_Excel_aca_full5[,paste(lag_flow_vars[i],"lag",seq(1,lag_flow_count),sep="")] <- create_lags2(EurekahedgeHF_Excel_aca_full5,lag_flow_vars[i],identifier,lag_flow_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(lag_flow_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 


# nflow_lags <- as.data.frame(data.table(nflow)[,list(nflow,
#                                                           nflowlag1=shift(nflow,-1),
#                                                           nflowlag2=shift(nflow,-2),
#                                                           nflowlag3=shift(nflow,-3),
#                                                           nflowlag4=shift(nflow,-4)),by=identifier], stringsAsFactors=FALSE) 
# 
# pflow_lags <- as.data.frame(data.table(pflow)[,list(pflow,
#                                                           pflowlag1=shift(pflow,-1),
#                                                           pflowlag2=shift(pflow,-2),
#                                                           pflowlag3=shift(pflow,-3),
#                                                           pflowlag4=shift(pflow,-4)),by=identifier], stringsAsFactors=FALSE) 
# 
# EurekahedgeHF_Excel_aca_full5 <- data.frame(EurekahedgeHF_Excel_aca_full4,
#                                     nflow_lags[,c("nflow","nflowlag1","nflowlag2","nflowlag3","nflowlag4")], 
#                                     pflow_lags[,c("pflow","pflowlag1","pflowlag2","pflowlag3","pflowlag4")],stringsAsFactors=FALSE)



EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[,sort(colnames(EurekahedgeHF_Excel_aca_full5))]

starting_cols <- c(identifier,"Fund_Name","Date.Added","Flagship","Closed","Limited","Dead","Dead.Date","Dead.Reason",
                   "Eurekahedge.ID","ISIN","SEDOL","Valoren","CUSIP","Bloomberg","Reuters","date","yr","month",
                   "AUM",paste("AUM","_lag",seq(1,lag_count),sep=""),
                   "Monthly_Ret",paste("Monthly_Ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "Monthly_Ret2","Yearly_Ret",
                   "nflow",paste("nflow","lag",seq(1,lag_flow_count),sep=""),  
                   "pflow",paste("pflow","lag",seq(1,lag_flow_count),sep=""))

all_cols <- colnames(EurekahedgeHF_Excel_aca_full5)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[rowSums(is.na(EurekahedgeHF_Excel_aca_full5[,1:ncol(EurekahedgeHF_Excel_aca_full5)]))<ncol(EurekahedgeHF_Excel_aca_full5),]

EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[order(EurekahedgeHF_Excel_aca_full5[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full5[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full5[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full5) <- seq(nrow(EurekahedgeHF_Excel_aca_full5))

#rm2(EurekahedgeHF_Excel_aca_full4)


# ###############################################################################
# cat("COMPUTE ANNUAL FUND FLOW VOLATILITY", "\n")
# ###############################################################################

annual_flow_trim <- EurekahedgeHF_Excel_aca_full5[,c(identifier,"yr","nflow","pflow")]

fund_aflow_volatility <- ddply(annual_flow_trim, c(identifier,"yr"), summarize, 
                               sdnet_flow=sd(nflow, na.rm = TRUE),sdpct_flow=sd(pflow, na.rm = TRUE))

fund_aflow_volatility_full <- data.frame(fund_aflow_volatility,
                                         sdnet_flowlag1=shift(fund_aflow_volatility$sdnet_flow,-1),
                                         sdpct_flowlag1=shift(fund_aflow_volatility$sdpct_flow,-1),stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full6 <- merge(EurekahedgeHF_Excel_aca_full5, fund_aflow_volatility_full, 
                                       by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(annual_flow_trim,fund_aflow_volatility)


###############################################################################
cat("STRIP COMMENTS FROM VARIABLES", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full_strip_comments_cols <- c("Dividend.Policy","Exchange.Name","Fund.Closed","High.Water.Mark","Hurdle.Rate","Listed.on.Exchange")

#Rename original columns
EurekahedgeHF_Excel_aca_full6 <- rename.vars(EurekahedgeHF_Excel_aca_full6, EurekahedgeHF_Excel_aca_full_strip_comments_cols, paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols,"_org",sep=""))

strip_cols <- c(EurekahedgeHF_Excel_aca_full_strip_comments_cols, paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols,"_comments",sep=""))

#TEMP!!
#test_strip <- EurekahedgeHF_Excel_aca_full6[,c("Dividend.Policy_org","Exchange.Name_org","Fund.Closed_org","High.Water.Mark_org","Hurdle.Rate_org","Listed.on.Exchange_org")]

EurekahedgeHF_Excel_aca_full7 <-  data.frame(EurekahedgeHF_Excel_aca_full6, matrix(NA, ncol=length(strip_cols), nrow=nrow(EurekahedgeHF_Excel_aca_full6), dimnames=list(c(), strip_cols)), stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[,sort(colnames(EurekahedgeHF_Excel_aca_full7), decreasing = FALSE)]


#Strip out comments in parenetheses
#for (i in 1:length(EurekahedgeHF_Excel_aca_full_strip_comments_cols))
#{
#i <- 1

#  EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_comments",sep = "")] <- 
#    gsub("\\(([^()]+)\\)", "\\1", str_extract_all(EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_org",sep = "")], "\\(([^()]+)\\)"))
#  EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_comments",sep = "")] <- 
#    ifelse(toupper(EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_comments",sep = "")])=="CHARACTER0", NA, EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_comments",sep = "")])

#  cat("Loop: ",i," of ",length(EurekahedgeHF_Excel_aca_full_strip_comments_cols), "\n")
#}

strip_comments <- function(x, cols) {
  DT <- data.table(x)
  for (j in cols) {
    set(DT, i=NULL, j=paste(j,"_comments",sep = ""), value=gsub("\\(([^()]+)\\)", "\\1", str_extract_all(DT[[paste(j,"_org",sep = "")]], "\\(([^()]+)\\)"), perl=TRUE))
    set(DT, i=NULL, j=paste(j,"_comments",sep = ""), value=gsub("^\\s+|\\s+$", "", DT[[paste(j,"_comments",sep = "")]], perl=TRUE))
    set(DT, i=which(toupper(DT[[paste(j,"_comments",sep = "")]]) == "CHARACTER0"), j=paste(j,"_comments",sep = ""), value=NA_character_)
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}
EurekahedgeHF_Excel_aca_full7 <- strip_comments(EurekahedgeHF_Excel_aca_full7,EurekahedgeHF_Excel_aca_full_strip_comments_cols)
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)


#Create Y/N
#for (i in 1:length(EurekahedgeHF_Excel_aca_full_strip_comments_cols))
#{
#  #i <- 1
#  
#  EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- gsub("[(].*$", "", EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"_org",sep = "")],perl=TRUE)
#  
#  EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <-
#    trim(EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])
#  
#  EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- 
#    ifelse(toupper(EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])=="CHARACTER0", NA, EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])
#  
#  cat("Loop: ",i," of ",length(EurekahedgeHF_Excel_aca_full_strip_comments_cols), "\n")
#} 
create_yn <- function(x, cols) {
  DT <- data.table(x)
  for (j in cols) {
    set(DT, i=NULL, j=j, value=gsub("[(].*$", "", DT[[paste(j,"_org",sep = "")]], "\\(([^()]+)\\)", perl=TRUE))
    set(DT, i=NULL, j=j, value=gsub("^\\s+|\\s+$", "", DT[[j]], perl=TRUE))
    set(DT, i=which(toupper(DT[[j]]) == "CHARACTER0"), j=j, value=NA_character_)
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}
EurekahedgeHF_Excel_aca_full7 <- create_yn(EurekahedgeHF_Excel_aca_full7,EurekahedgeHF_Excel_aca_full_strip_comments_cols)
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)

#Check for uknowns
EurekahedgeHF_Excel_aca_full7 <- data.table(EurekahedgeHF_Excel_aca_full7)[, (EurekahedgeHF_Excel_aca_full_strip_comments_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = EurekahedgeHF_Excel_aca_full_strip_comments_cols]
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)


#Change not specificied to NA

NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","DISCUSS WITH MANAGER","UPON REQUEST")

#for (i in 1:length(EurekahedgeHF_Excel_aca_full_strip_comments_cols))
#{
#i <- 1

#  for (j in 1:length(NA_Phrases))
#  {
#j <- 1

#    EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- 
#      ifelse(grepl(NA_Phrases[j], EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")], ignore.case = TRUE,perl=TRUE), NA, EurekahedgeHF_Excel_aca_full2[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])
#    
#  }
#  
#  cat("Loop: ",i," of ",length(EurekahedgeHF_Excel_aca_full_strip_comments_cols), "\n")
#} 

not_specified_to_na <- function(x, cols, phrases) {
  DT <- data.table(x)
  for (j in cols) {
    for (k in 1:length(phrases)) {
      #k <- 1
      set(DT, i=grep(phrases[k], DT[[j]], ignore.case = TRUE, perl=TRUE), j=j, value=NA_character_)
    }
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}
EurekahedgeHF_Excel_aca_full7 <- not_specified_to_na(EurekahedgeHF_Excel_aca_full7,EurekahedgeHF_Excel_aca_full_strip_comments_cols,NA_Phrases)
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)
#test <- unique(EurekahedgeHF_Excel_aca_full7[,EurekahedgeHF_Excel_aca_full_strip_comments_cols])

#Change no phrases to NO

NO_Phrases <- c("NIL","NONE","NONE AFTER 12 MONTHS","NONE AFTER 1ST YEAR","NO DIVIDEND","NON DIVIDEND","LITTLE OR NO")

#for (i in 1:length(EurekahedgeHF_Excel_aca_full_strip_comments_cols))
#{
#i <- 1

#  for (j in 1:length(NO_Phrases)){
#j <- 1

#    EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- 
#      ifelse(grepl(NO_Phrases[j], EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")], ignore.case = TRUE), "No", EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])

#  }

#  cat("Loop: ",i," of ",length(EurekahedgeHF_Excel_aca_full_strip_comments_cols), "\n")

#}

no_to_no <- function(x, cols, phrases) {
  DT <- data.table(x)
  for (j in cols) {
    for (k in 1:length(phrases)) {
      #k <- 1
      set(DT, i=grep(phrases[k], DT[[j]], ignore.case = TRUE, perl=TRUE), j=j, value="No")
    }
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}  
EurekahedgeHF_Excel_aca_full7 <- no_to_no(EurekahedgeHF_Excel_aca_full7,EurekahedgeHF_Excel_aca_full_strip_comments_cols,NO_Phrases)
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)


#Change yes phrases to YES

YES_Phrases <- c("RARELY","OCCASIONALLY")

#for (i in 1:length(EurekahedgeHF_Excel_aca_full_strip_comments_cols))
#{
#i <- 1

#  for (j in 1:length(YES_Phrases))
#  {
#j <- 1

#EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- 
#  ifelse(toupper(EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])==YES_Phrases[j], "YES", EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])

#    EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")] <- 
#      ifelse(grepl(YES_Phrases[j], EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")], ignore.case = TRUE), "Yes", EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"",sep = "")])

#  }

#  cat("Loop: ",i," of ",length(EurekahedgeHF_Excel_aca_full_strip_comments_cols), "\n")

#} 

yes_to_yes <- function(x, cols, phrases) {
  DT <- data.table(x)
  for (j in cols) {
    for (k in 1:length(phrases)) {
      #k <- 1
      set(DT, i=grep(phrases[k], DT[[j]], ignore.case = TRUE, perl=TRUE), j=j, value="Yes")
    }
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}  
EurekahedgeHF_Excel_aca_full7 <- yes_to_yes(EurekahedgeHF_Excel_aca_full7,EurekahedgeHF_Excel_aca_full_strip_comments_cols,YES_Phrases)
EurekahedgeHF_Excel_aca_full7 <- as.data.frame(EurekahedgeHF_Excel_aca_full7,stringsAsFactors=FALSE)


#EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"yn",sep = "_")] <- gsub(" .*$", "", EurekahedgeHF_Excel_aca_full7[,EurekahedgeHF_Excel_aca_full_strip_comments_cols[i]])
#EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"comments",sep = "_")] <- 
#  ifelse(toupper(EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"comments",sep = "_")])=="CHARACTER0", NA, EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"comments",sep = "_")])

#test <- unique(EurekahedgeHF_Excel_aca_full7[,colnames(EurekahedgeHF_Excel_aca_full7)])
#test <- unique(EurekahedgeHF_Excel_aca_full7[,c("Dividend.Policy","Hurdle.Rate")])

#EurekahedgeHF_Excel_aca_full7[,paste("Dividend.Policy","comments",sep = "_")] <- 
#  gsub("\\(([^()]+)\\)", "\\1", str_extract_all(EurekahedgeHF_Excel_aca_full7[,"Dividend.Policy"], "\\(([^()]+)\\)"))

#EurekahedgeHF_Excel_aca_full7[,paste("Hurdle.Rate","comments",sep = "_")] <- 
#  gsub("\\(([^()]+)\\)", "\\1", str_extract_all(EurekahedgeHF_Excel_aca_full7[,"Hurdle.Rate"], "\\(([^()]+)\\)"))

#EurekahedgeHF_Excel_aca_full7[,paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols[i],"comments",sep = "_")] <- 
#  gsub("\\(([^()]+)\\)", "\\1", str_extract_all(EurekahedgeHF_Excel_aca_full7[,EurekahedgeHF_Excel_aca_full_strip_comments_cols[i]], "\\(([^()]+)\\)"))

#test <- unique(EurekahedgeHF_Excel_aca_full7[,colnames(EurekahedgeHF_Excel_aca_full7)])

#i <- 1
#test <- gsub(" .*$", "", EurekahedgeHF_Excel_aca_full7[,EurekahedgeHF_Excel_aca_full_strip_comments_cols[i]])


EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[rowSums(is.na(EurekahedgeHF_Excel_aca_full7[,1:ncol(EurekahedgeHF_Excel_aca_full7)]))<ncol(EurekahedgeHF_Excel_aca_full7),]


EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[order(EurekahedgeHF_Excel_aca_full7[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full7[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full7[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full7) <- seq(nrow(EurekahedgeHF_Excel_aca_full7))


###############################################################################
cat("CHANGE Y/N TO BINARY", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full_yn_to_bin_cols <- c("Flagship","Closed","Limited","Dead","Invest.In.Private.Placements","Managed.Accounts.Offered","UCITS",
                                                 "Dividend.Policy","Exchange.Name","Fund.Closed","High.Water.Mark","Hurdle.Rate","Listed.on.Exchange")

bin_cols <- paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols,"_bin",sep="")

EurekahedgeHF_Excel_aca_full8 <-  data.frame(EurekahedgeHF_Excel_aca_full7, matrix(NA, ncol=length(bin_cols), nrow=nrow(EurekahedgeHF_Excel_aca_full7), dimnames=list(c(), bin_cols)), stringsAsFactors=FALSE)

#for (i in 1:length(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols))
#{
#  EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")] <- 
#    ifelse(toupper(EurekahedgeHF_Excel_aca_full8[,EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i]])=="YES", 1,EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")])
#  EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")] <- 
#    ifelse(toupper(EurekahedgeHF_Excel_aca_full8[,EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i]])=="NO", 0,EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")])
#  EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")] <- 
#    ifelse(is.na(EurekahedgeHF_Excel_aca_full8[,EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i]]), NA,EurekahedgeHF_Excel_aca_full8[,paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols[i],"_bin",sep = "")])

#} 

EurekahedgeHF_Excel_aca_full8[,bin_cols] <-  EurekahedgeHF_Excel_aca_full8[,EurekahedgeHF_Excel_aca_full_yn_to_bin_cols]

yn_to_binary <- function(x, cols) {
  DT <- data.table(x)
  for (j in cols) {
    
    set(DT, i=grep("Yes", DT[[j]], ignore.case = TRUE, perl=TRUE), j=j, value="1")
    set(DT, i=grep("No", DT[[j]], ignore.case = TRUE, perl=TRUE), j=j, value="0")
    set(DT, i=NULL, j=j, value=as.integer(DT[[j]]))
    
    cat("Loop: ",which(cols==j)," of ",length(cols), "\n")
  }
  return(DT)
}  
EurekahedgeHF_Excel_aca_full8 <- yn_to_binary(EurekahedgeHF_Excel_aca_full8,bin_cols)
EurekahedgeHF_Excel_aca_full8 <- as.data.frame(EurekahedgeHF_Excel_aca_full8,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full8 <- data.table(EurekahedgeHF_Excel_aca_full8)[, (bin_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = bin_cols]
EurekahedgeHF_Excel_aca_full8 <- as.data.frame(EurekahedgeHF_Excel_aca_full8,stringsAsFactors=FALSE)

#test <- unique(EurekahedgeHF_Excel_aca_full8[,colnames(EurekahedgeHF_Excel_aca_full8)])
#test <- unique(EurekahedgeHF_Excel_aca_full8[,c(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols,bin_cols)])
#str(EurekahedgeHF_Excel_aca_full8[,c(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols,bin_cols)])

EurekahedgeHF_Excel_aca_full8 <- EurekahedgeHF_Excel_aca_full8[rowSums(is.na(EurekahedgeHF_Excel_aca_full8[,1:ncol(EurekahedgeHF_Excel_aca_full8)]))<ncol(EurekahedgeHF_Excel_aca_full8),]


EurekahedgeHF_Excel_aca_full8 <- EurekahedgeHF_Excel_aca_full8[order(EurekahedgeHF_Excel_aca_full8[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full8[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full8[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full8) <- seq(nrow(EurekahedgeHF_Excel_aca_full8))


###############################################################################
cat("SPLIT BROKERS", "\n")
###############################################################################

split_cols <- c("Custodian","Principal.Prime.Broker.Broker","Secondary.Prime.Broker.Broker",
                "Synthetic.Prime.Broker","Legal.Advisor.offshore","Legal.Advisor.onshore")

#test <- EurekahedgeHF_Excel_aca_full8[,split_cols]
#test2 <- unique(test[,colnames(test)])

for (i in 1:length(split_cols))
{
  #i <- 2
  
  temp1 <- str_split(EurekahedgeHF_Excel_aca_full8[,split_cols[i]], ",")
  
  temp1_len <- unlist(lapply(temp1, function(x) length(x)))
  temp1_len_max <- as.integer(tail(sort(temp1_len, decreasing = FALSE),1))
  
  temp2 <- lapply(temp1, function(x,max_len){if (length(x) != (max_len+1)) append(x,matrix(NA, ncol=max_len-length(x)+1, nrow=1)) else x}, max_len = temp1_len_max)
  
  temp3 <- do.call(rbind, temp2)
  
  temp4 <- as.data.frame(temp3,stringsAsFactors=FALSE)
  colnames(temp4) <- c(paste(split_cols[i],1:(ncol(temp4)-1),sep=""),paste(split_cols[i],"_count",sep=""))
  
  for (j in 1:ncol(temp4))
  {
    #j <- 1
    
    temp4[,j] <- trim(temp4[,j])
    
  }
  
  temp4[,paste(split_cols[i],"_count",sep="")] <- apply(temp4,1,function(x) sum(!is.na(x[1:(length(x)-1)])))
  
  temp4[,paste(split_cols[i],"_count",sep="")] <- 
    ifelse(toupper(temp4[,paste(split_cols[i],"_count",sep="")])=="0", NA, temp4[,paste(split_cols[i],"_count",sep="")])
  
  
  if (i==1)
  {
    output <- temp4
    
  } else
  {
    output <- cbind(output,temp4)
  }
  
  cat("Loop: ",i," of ",length(split_cols), "\n")
} 


EurekahedgeHF_Excel_aca_full9 <- cbind(EurekahedgeHF_Excel_aca_full8,output)

rm2(temp1,temp2,temp3,temp4,output)


###############################################################################
cat("REORDER COLUMNS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,sort(colnames(EurekahedgeHF_Excel_aca_full9))]

starting_cols <- c(identifier,"Fund_Name","Date.Added","Flagship","Closed","Limited","Dead","Dead.Date","Dead.Reason",
                   "Eurekahedge.ID","ISIN","SEDOL","Valoren","CUSIP","Bloomberg","Reuters","date","yr","month",
                   "AUM",paste("AUM","_lag",seq(1,lag_count),sep=""),
                   "Monthly_Ret",paste("Monthly_Ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "Monthly_Ret2","Yearly_Ret",
                   "nflow",paste("nflow","lag",seq(1,lag_flow_count),sep=""),  
                   "pflow",paste("pflow","lag",seq(1,lag_flow_count),sep=""))

all_cols <- colnames(EurekahedgeHF_Excel_aca_full9)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[rowSums(is.na(EurekahedgeHF_Excel_aca_full9[,1:ncol(EurekahedgeHF_Excel_aca_full9)]))<ncol(EurekahedgeHF_Excel_aca_full9),]





# ###############################################################################
# cat("GET TNA FOR EACH CRSP_FUNDNO", "\n")
# ###############################################################################
# 
# monthly_tna_full <- merge(mflink1, monthly_tna_ret_nav2, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_tna_full <- subset(monthly_tna_full,select=-c(caldt,mnav,mret))
# 
# monthly_tna_full <- monthly_tna_full[c("wficn","crsp_fundno","yr","month","mtna")]
# 
# monthly_tna_full <- monthly_tna_full[order(monthly_tna_full[,"wficn"], 
#                                            monthly_tna_full[,"crsp_fundno"], 
#                                            monthly_tna_full[,"yr"],
#                                            monthly_tna_full[,"month"]),]
# 
# monthly_tna_full <- monthly_tna_full[!(is.na(monthly_tna_full[,"mtna"])),]
# monthly_tna_full <- monthly_tna_full[monthly_tna_full[,"mtna"]>0.1,]
# 
# #TRIM TNA
# TNA_q <- 0.005
# TNA_extrema <- quantile(monthly_tna_full[,"mtna"], c(TNA_q, 1-TNA_q)) 
# #monthly_tna_full[,"mtna"] <- winsorize_top(monthly_tna_full[,"mtna"],q=0.005)
# monthly_tna_full <- monthly_tna_full[monthly_tna_full[,"mtna"]<TNA_extrema[2],]
# 
# #rm2(TNA_q,monthly_tna_full)
# 
# ###############################################################################
# cat("AGGREGATE VALUES OF MONTHLY_TNA_RET_NAV VARIABLES FOR EACH WFICN", "\n")
# ###############################################################################
# 
# monthly_tna_ret_nav2_vars <- c("mret","mnav","sddret","mktadjret")
# for (i in 1:length(monthly_tna_ret_nav2_vars))
# {
#   #i <- 1
#   #i <- 2
#   monthly_tna_ret_nav2[,monthly_tna_ret_nav2_vars[i]] <- winsorize_both(monthly_tna_ret_nav2[,monthly_tna_ret_nav2_vars[i]],q=0.005)
#   
# } 
# 
# for (i in 1:length(monthly_tna_ret_nav2_vars))
# {
#   #i <- 1
#   #i <- 2
#   
#   
#   single_var  <- monthly_tna_ret_nav2[,match(c("crsp_fundno","yr","month",monthly_tna_ret_nav2_vars[i]),names(monthly_tna_ret_nav2))]
#   
#   single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
#   
#   single_var <- single_var[!(is.na(single_var[,monthly_tna_ret_nav2_vars[i]])),]
#   
#   assign(paste("agg_",monthly_tna_ret_nav2_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,monthly_tna_ret_nav2_vars[i]), envir=.GlobalEnv)
#   
#   progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(monthly_tna_ret_nav2_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
#   
#   rm2(single_var)
#   
# } 
# 
# 
# ###############################################################################
# cat("AGGREGATE VALUES OF FUND_FEES_MONTH VARIABLES FOR EACH WFICN", "\n")
# ###############################################################################
# 
# fund_fees_month_vars <- c("actual_12b1", "max_12b1", "exp_ratio", "mgmt_fee", "turn_ratio")
# 
# for (i in 1:length(fund_fees_month_vars))
# {
#   #i <- 1
#   #i <- 2
#   fund_fees_month[,fund_fees_month_vars[i]] <- winsorize_both(fund_fees_month[,fund_fees_month_vars[i]],q=0.005)
#   
# } 
# 
# for (i in 1:length(fund_fees_month_vars))
# {
#   #i <- 1
#   #i <- 2
#   
#   
#   single_var  <- fund_fees_month[,match(c("crsp_fundno","yr","month",fund_fees_month_vars[i]),names(fund_fees_month))]
#   
#   single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
#   
#   single_var <- single_var[!(is.na(single_var[,fund_fees_month_vars[i]])),]
#   
#   assign(paste("agg_",fund_fees_month_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,fund_fees_month_vars[i]), envir=.GlobalEnv)
#   
#   progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(fund_fees_month_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
#   
#   rm2(single_var)
#   
# } 
# 
# ###############################################################################
# cat("AGGREGATE VALUES OF TNA VARIABLES FOR EACH WFICN", "\n")
# ###############################################################################
# 
# monthly_tna_trim <- subset(monthly_tna_full,select=-c(crsp_fundno))
# 
# monthly_tna_trim <- monthly_tna_trim[c("wficn","yr","month","mtna")]
# 
# monthly_tna_trim <- monthly_tna_trim[order(monthly_tna_trim[,"wficn"], 
#                                            monthly_tna_trim[,"yr"], 
#                                            monthly_tna_trim[,"month"]),] 
# 
# agg_mtna <- as.data.frame(data.table(monthly_tna_trim)[, list(summtna=sum(mtna)),by="wficn,yr,month"],stringsAsFactors=FALSE)
# colnames(agg_mtna)[4] <- "mtna_agg"
# 
# #rm2(monthly_tna_trim)
# 
# 
# ###############################################################################
# cat("MERGE AGGREGATE DATA", "\n")
# ###############################################################################
# 
# monthly_agg <- merge(agg_mtna, agg_mret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_mnav, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_mktadjret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_sddret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_actual_12b1, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_max_12b1, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_exp_ratio, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_mgmt_fee, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg <- merge(monthly_agg, agg_turn_ratio, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# #rm2(agg_mret,agg_mnav,agg_mktadjret,agg_sddret,agg_actual_12b1,agg_max_12b1,agg_exp_ratio,agg_mgmt_fee,agg_turn_ratio)
# 
# 


# ###############################################################################
# cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN FUND_NAMES", "\n")
# ###############################################################################
# 
# fund_names_full <- merge(fund_names, mflink1, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_names_full <- subset(fund_names_full,select=-c(cusip8, crsp_portno,crsp_cl_grp,fund_name, nasdaq, ncusip, first_offer_dt, mgmt_name, mgmt_cd, mgr_name, 
#                                                     mgr_dt, adv_name, index_fund_flag, et_flag, end_dt, merge_fundno,delist_cd,header))
# 
# fund_names_full <- fund_names_full[c("wficn","crsp_fundno","chgdt","chgenddt","open_to_inv","retail_fund","inst_fund","m_fund","vau_fund",
#                                      "dead_flag")]
# 
# fund_names_full <- fund_names_full[order(fund_names_full[,"wficn"], fund_names_full[,"crsp_fundno"]),]
# 
# #rm2(fund_names)
# 
# 
# ###############################################################################
# cat("CHANGE Y/N TO BINARY IN FUND_NAMES_FULL", "\n")
# ###############################################################################
# 
# fund_names_full_dv <- data.frame(fund_names_full,retail_fund_dv=NA,open_to_inv_dv=NA,inst_fund_dv=NA, 
#                                  m_fund_dv=NA,vau_fund_dv=NA,dead_flag_dv=NA,stringsAsFactors=FALSE)
# 
# fund_names_full_dv[,"open_to_inv_dv"] <- ifelse(fund_names_full_dv$open_to_inv=="Y", 1, fund_names_full_dv$open_to_inv_dv)
# fund_names_full_dv[,"open_to_inv_dv"] <- ifelse(fund_names_full_dv$open_to_inv=="N", 0, fund_names_full_dv$open_to_inv_dv)
# 
# fund_names_full_dv[,"retail_fund_dv"] <- ifelse(fund_names_full_dv$retail_fund=="Y", 1, fund_names_full_dv$retail_fund_dv)
# fund_names_full_dv[,"retail_fund_dv"] <- ifelse(fund_names_full_dv$retail_fund=="N", 0, fund_names_full_dv$retail_fund_dv)
# 
# fund_names_full_dv[,"inst_fund_dv"] <- ifelse(fund_names_full_dv$inst_fund=="Y", 1, fund_names_full_dv$inst_fund_dv)
# fund_names_full_dv[,"inst_fund_dv"] <- ifelse(fund_names_full_dv$inst_fund=="N", 0, fund_names_full_dv$inst_fund_dv)
# 
# fund_names_full_dv[,"m_fund_dv"] <- ifelse(fund_names_full_dv$m_fund=="Y", 1, fund_names_full_dv$m_fund_dv)
# fund_names_full_dv[,"m_fund_dv"] <- ifelse(fund_names_full_dv$m_fund=="N", 0, fund_names_full_dv$m_fund_dv)
# 
# fund_names_full_dv[,"vau_fund_dv"] <- ifelse(fund_names_full_dv$vau_fund=="Y", 1, fund_names_full_dv$vau_fund_dv)
# fund_names_full_dv[,"vau_fund_dv"] <- ifelse(fund_names_full_dv$vau_fund=="N", 0, fund_names_full_dv$vau_fund_dv)
# 
# fund_names_full_dv[,"dead_flag_dv"] <- ifelse(fund_names_full_dv$dead_flag=="Y", 1, fund_names_full_dv$dead_flag_dv)
# fund_names_full_dv[,"dead_flag_dv"] <- ifelse(fund_names_full_dv$dead_flag=="N", 0, fund_names_full_dv$dead_flag_dv)
# 
# fund_names_full_dv <- subset(fund_names_full_dv,select=-c(retail_fund,open_to_inv,inst_fund,m_fund,vau_fund,dead_flag))
# 
# #rm2(fund_names_full)
# 
# 
# ###############################################################################
# cat("EXPAND FUND NAMES", "\n")
# ###############################################################################
# 
# fund_names_full_dv2 <- transform(fund_names_full_dv, chgdt=as.IDate(chgdt), chgenddt=as.IDate(chgenddt))
# 
# fund_names_full_dv2 <- fund_names_full_dv2[(fund_names_full_dv2[,"chgdt"]>0 & 
#                                               fund_names_full_dv2[,"chgenddt"]>0),]
# fund_names_full_dv2 <- fund_names_full_dv2[!(fund_names_full_dv2[,"chgdt"]>=fund_names_full_dv2[,"chgenddt"]),]
# 
# fund_names_month_temp <- data.table(fund_names_full_dv2)[,{s=seq(chgdt,chgenddt,"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                          by="crsp_fundno,chgdt,chgenddt"]
# 
# #rm2(fund_names_full_dv)
# 
# setkeyv(fund_names_month_temp,c("crsp_fundno","yr","month"))
# fund_names_month_temp <- fund_names_month_temp[unique(fund_names_month_temp[,key(fund_names_month_temp), with = FALSE]), mult = 'first']
# 
# fund_names_month_temp <- as.data.frame(fund_names_month_temp,stringsAsFactors=FALSE) 
# 
# fund_names_month <- merge(fund_names_month_temp, fund_names_full_dv2, by.x=c("crsp_fundno","chgdt","chgenddt"), by.y=c("crsp_fundno","chgdt","chgenddt"), 
#                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# rm2(fund_names_month_temp,fund_names_full_dv2)
# 
# fund_names_month <- transform(fund_names_month, chgdt=as.character(chgdt), chgenddt=as.character(chgenddt))
# 
# fund_names_month <- fund_names_month[c("wficn","crsp_fundno","chgdt","chgenddt","yr","month",
#                                        "retail_fund_dv","open_to_inv_dv","inst_fund_dv","m_fund_dv","vau_fund_dv","dead_flag_dv")]
# 
# fund_names_month <- fund_names_month[order(fund_names_month[,"wficn"],fund_names_month[,"crsp_fundno"],
#                                            fund_names_month[,"yr"],fund_names_month[,"month"]),] 
# 
# 
# ###############################################################################
# cat("AGGREGATE VALUES OF FUND_NAMES_MONTH VARIABLES FOR EACH WFICN", "\n")
# ###############################################################################
# 
# fund_names_month_vars <- c("retail_fund_dv","open_to_inv_dv","inst_fund_dv","m_fund_dv","vau_fund_dv","dead_flag_dv")
# for (i in 1:length(fund_names_month_vars))
# {
#   #i <- 1
#   #i <- 2
#   
#   single_var  <- fund_names_month[,match(c("crsp_fundno","yr","month",fund_names_month_vars[i]),names(fund_names_month))]
#   
#   single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
#   
#   single_var <- single_var[!(is.na(single_var[,fund_names_month_vars[i]])),]
#   
#   assign(paste("agg_",fund_names_month_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,fund_names_month_vars[i]), envir=.GlobalEnv)
#   
#   progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(fund_names_month_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
#   
#   rm2(single_var)
#   
# } 
 
 
# ###############################################################################
# cat("FIND AGE FOR EVERY WFICN", "\n")
# ###############################################################################

EurekahedgeHF_Excel_aca_full9




# fund_age <- data.table(fund_names_full)[,list(chgdt=min(chgdt)),by="wficn,crsp_fundno"]
# fund_age <- data.frame(fund_age,stringsAsFactors=FALSE)
# 
# fund_age_trim <- data.table(fund_age[,-match("crsp_fundno",names(fund_age))])[,list(chgdt=min(chgdt)),by="wficn"]
# 
# #rm2(fund_age)
# 
# fund_age_trim <- as.data.frame(fund_age_trim,stringsAsFactors=FALSE)
# 
# 
# fund_age_trim2 <- transform(fund_age_trim, chgdt=as.IDate(chgdt))
# 
# rm2(fund_age_trim)
# 
# fund_age_month_temp <- data.table(fund_age_trim2)[,{s=seq(chgdt,today(),"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                   by="wficn,chgdt"]
# 
# rm2(fund_age_trim2)
# 
# fund_age_month_temp <- as.data.frame(fund_age_month_temp,stringsAsFactors=FALSE)
# 
# fund_age_month_temp <- unique(fund_age_month_temp, incomparables=FALSE)
# 
# fund_age_month_temp <- transform(fund_age_month_temp, chgdt=as.character(chgdt))
# 
# fund_age_month <- data.table(fund_age_month_temp)[,list(yr,month,chgdt,age_m=(seq(1,.N)-1),age_y=((seq(1,.N)-1)/12)),by="wficn"]
# fund_age_month <- as.data.frame(fund_age_month,stringsAsFactors=FALSE)
# 
# rm2(fund_age_month_temp)
# 
# 
# ###############################################################################
# cat("MERGE AGGREGATE DATA", "\n")
# ###############################################################################
# 
# monthly_agg2 <- merge(agg_mtna, agg_retail_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_open_to_inv_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_inst_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_m_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_vau_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_dead_flag_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, fund_age_month, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# rm2(agg_retail_fund_dv,agg_open_to_inv_dv,agg_inst_fund_dv,agg_m_fund_dv,agg_vau_fund_dv,agg_dead_flag_dv)
# 
# monthly_agg2 <- subset(monthly_agg2,select=-c(mtna_agg))
# 
# 
# ###############################################################################
# cat("MERGE MONTHLY_AGG_LAGS_FULL AND MONTHLY_AGG2", "\n")
# ###############################################################################
# 
# monthly_agg_full <- merge(monthly_agg_lags_full, monthly_agg2, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full <- monthly_agg_full[order(monthly_agg_full[,"wficn"],
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
# fund_style_full_trim <- subset(fund_style_full,select=-c(crsp_fundno,begdt,enddt,lipper_class_name,accrual_fund,sales_restrict))
# 
# fund_style_full_trim <- fund_style_full_trim[c("wficn","yr","month","crsp_obj_cd","lipper_class","lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")]
# 
# fund_style_full_trim <- fund_style_full_trim[order(fund_style_full_trim[,"wficn"], fund_style_full_trim[,"yr"], fund_style_full_trim[,"month"]),]
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
#                                by.x=c("wficn", "yr", "month"), by.y=c("wficn", "yr", "month"), 
#                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_style_full_trim3 <- fund_style_full_trim2[is.na(fund_style_full_trim2[,"Remove"]),]
# fund_style_full_trim3 <- subset(fund_style_full_trim3,select=-c(Remove))
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
#                            by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
#                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full2 <- monthly_agg_full2[order(monthly_agg_full2[,"wficn"],
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
# 
# 
# ###############################################################################
# cat("CREATE LOG ASSETS, SQUARE RETURNS, AND STLYE DUMMIES", "\n")
# ###############################################################################
# 
# monthly_data_all2 <- data.frame(monthly_agg_full2_trim,log_mtna_agg=NA,
#                                 log_mtna_agglag1=NA,log_mtna_agglag2=NA,log_mtna_agglag3=NA,log_mtna_agglag4=NA,
#                                 mktadjret_agg_sq=NA,
#                                 mktadjret_agglag1_sq=NA,mktadjret_agglag2_sq=NA,mktadjret_agglag3_sq=NA,mktadjret_agglag4_sq=NA,
#                                 avg_mkt_exp=NA,
#                                 avg_mkt_explag1=NA,avg_mkt_explag2=NA,avg_mkt_explag3=NA,avg_mkt_explag4=NA,
#                                 lipper_asset_cd_eq_dv=NA, lipper_asset_cd_tx_dv=NA, lipper_asset_cd_mb_dv=NA, 
#                                 lipper_tax_cd_t_dv = NA, lipper_tax_cd_te_dv = NA)
# 
# monthly_data_all2[,"log_mtna_agg"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agg"]))
# monthly_data_all2[,"log_mtna_agglag1"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag1"]))
# monthly_data_all2[,"log_mtna_agglag2"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag2"]))
# monthly_data_all2[,"log_mtna_agglag3"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag3"]))
# monthly_data_all2[,"log_mtna_agglag4"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag4"]))
# 
# monthly_data_all2[,"mktadjret_agg_sq"] <- (monthly_data_all2[,"mktadjret_agg"])^2
# monthly_data_all2[,"mktadjret_agglag1_sq"] <- (monthly_data_all2[,"mktadjret_agglag1"])^2
# monthly_data_all2[,"mktadjret_agglag2_sq"] <- (monthly_data_all2[,"mktadjret_agglag2"])^2
# monthly_data_all2[,"mktadjret_agglag3_sq"] <- (monthly_data_all2[,"mktadjret_agglag3"])^2
# monthly_data_all2[,"mktadjret_agglag4_sq"] <- (monthly_data_all2[,"mktadjret_agglag4"])^2
# 
# monthly_data_all2[,"avg_mkt_exp"] <- (monthly_data_all2[,"actual_12b1_agg"]/monthly_data_all2[,"exp_ratio_agg"])
# monthly_data_all2[,"avg_mkt_explag1"] <- (monthly_data_all2[,"actual_12b1_agglag1"]/monthly_data_all2[,"exp_ratio_agglag1"])
# monthly_data_all2[,"avg_mkt_explag2"] <- (monthly_data_all2[,"actual_12b1_agglag2"]/monthly_data_all2[,"exp_ratio_agglag2"])
# monthly_data_all2[,"avg_mkt_explag3"] <- (monthly_data_all2[,"actual_12b1_agglag3"]/monthly_data_all2[,"exp_ratio_agglag3"])
# monthly_data_all2[,"avg_mkt_explag4"] <- (monthly_data_all2[,"actual_12b1_agglag4"]/monthly_data_all2[,"exp_ratio_agglag4"])
# 
# #lipper_class_u <- unique(monthly_data_all2[,"lipper_class"],incomparables=FALSE)
# #lipper_obj_cd_u <- unique(monthly_data_all2[,"lipper_obj_cd"],incomparables=FALSE)
# #lipper_asset_cd_u <- unique(monthly_data_all2[,"lipper_asset_cd"],incomparables=FALSE)
# #lipper_tax_cd_u <- unique(monthly_data_all2[,"lipper_tax_cd"],incomparables=FALSE)
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
# 
# 
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
# fund_names_msd2 <- fund_names_msd2[c("wficn","crsp_fundno","Broad_Cat_Group","Global_Cat","MS_Cat","MS_Inst_Cat",
#                                      "MS_Rating_Overall", "US_Broad_Asset_Class","Equity_Style_Box_Long","MS_Anal_Rating",
#                                      "Firm_Name","Branding_Name","Prospectus_Objective")]
# 
# fund_names_msd2 <- fund_names_msd2[order(fund_names_msd2[,"wficn"], fund_names_msd2[,"crsp_fundno"]),]
# fund_names_msd2 <- unique(fund_names_msd2,comparables=FALSE)
# 
# fund_names_msd3 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Firm_Name,Branding_Name,Global_Cat,
#                                                     MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# 
# #fund_names_msd3[fund_names_msd3[,"wficn"]==103259,"MS_Anal_Rating"] <- "Silver" 
# #fund_names_msd3[fund_names_msd3[,"wficn"]==103380,"MS_Anal_Rating"] <- "Bronze" 
# #fund_names_msd3[fund_names_msd3[,"wficn"]==400001,"MS_Anal_Rating"] <- "Bronze" 
# 
# fund_names_msd3 <- unique(fund_names_msd3,comparables=FALSE)
# 
# fund_names_msd3_row_count <- as.data.frame(data.table(fund_names_msd3)[, list(count=.N),by="wficn"],stringsAsFactors=FALSE)
# fund_names_msd3_bad <- fund_names_msd3_row_count[fund_names_msd3_row_count[,"count"]>1,]
# 
# fund_names_msd4 <- fund_names_msd3[!(fund_names_msd3[,"wficn"] %in% fund_names_msd3_bad[,"wficn"]),]
# 
# #rm2(fund_names_msd,fund_names_msd2,fund_names_msd3,fund_names_msd3_row_count,fund_names_msd3_bad)
# 
# fund_names_msd5 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Broad_Cat_Group,Equity_Style_Box_Long,Global_Cat,
#                                                     MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# fund_names_msd5 <- subset(fund_names_msd5,select=-c(Firm_Name))
# fund_names_msd5 <- unique(fund_names_msd5,comparables=FALSE)
# 
# fund_names_msd5_row_count <- as.data.frame(data.table(fund_names_msd5)[, list(count=.N),by="wficn"],stringsAsFactors=FALSE)
# fund_names_msd5_bad <- fund_names_msd5_row_count[fund_names_msd5_row_count[,"count"]>1,]
# 
# fund_names_msd6 <- fund_names_msd5[!(fund_names_msd5[,"wficn"] %in% fund_names_msd5_bad[,"wficn"]),]
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
#                            by.x=c("wficn"), by.y=c("wficn"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_data_all4 <- data.frame(monthly_data_all3,FundRetMktNeg=NA,stringsAsFactors=FALSE)
# 
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(monthly_data_all4$mktadjret_agg<0, monthly_data_all4$mktadjret_agg, 0)
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(is.na(monthly_data_all4$mktadjret_agg), NA, monthly_data_all4$FundRetMktNeg)
# 
# monthly_data_all4 <- monthly_data_all4[order(monthly_data_all4[,"wficn"],
#                                              monthly_data_all4[,"yr"],
#                                              monthly_data_all4[,"month"]),]
# 
# #rm2(fund_names_msd_dv)
# 
# 
# ###############################################################################
# cat("COMPUTE AVERAGE GRADE-LEVEL", "\n")
# ###############################################################################

read_stats_ios_f  <- read_stats_ios_f[((!is.na(read_stats_ios_f[,"ARI_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"Coleman_Liau_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"Flesch_Kincaid_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"FOG_ios"])) &   
                                         (!is.na(read_stats_ios_f[,"SMOG_ios"]))),]

avg_grade_level_ios <- as.data.frame(rowMeans(read_stats_ios_f[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios")], na.rm=TRUE),stringsAsFactors=FALSE)
colnames(avg_grade_level_ios) <- "avg_grade_level_ios"

read_stats_ios_f <- cbind(read_stats_ios_f,avg_grade_level_ios)

rm2(avg_grade_level_ios)


# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY BROAD_CAT_GROUP AND YEAR", "\n")
# ###############################################################################
# 
# #similarity_db_tables <- ListTables(similarity_db)
# #similarity_db_fields <- ListFields(similarity_db)
# 
# group_column <- "Broad_Cat_Group"
# 
# sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd4[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
#                            by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
# text_group_vars <- toupper(text_group_vars)
# text_group_vars <- trim(text_group_vars)
# text_group_vars <- text_group_vars[!is.na(text_group_vars)]
# text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]
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
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
#     
#     if (text_variables[j]=="iois")
#     {
#       year_sim_iois_broad_cat_group_stacked <- rbind(year_sim_iois_broad_cat_group_stacked,temp_sim_stacked)
#       
#     } else if (text_variables[j]=="pr")
#     {
#       year_sim_pr_broad_cat_group_stacked <- rbind(year_sim_pr_broad_cat_group_stacked,temp_sim_stacked)
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
# colnames(year_sim_iois_broad_cat_group_stacked) <- paste(group_column,colnames(year_sim_iois_broad_cat_group_stacked),sep="_")
# colnames(year_sim_iois_broad_cat_group_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_iois_broad_cat_group_stacked <- year_sim_iois_broad_cat_group_stacked[order(year_sim_iois_broad_cat_group_stacked[,"wficn"], 
#                                                                                      year_sim_iois_broad_cat_group_stacked[,"yr"]),]
# 
# colnames(year_sim_pr_broad_cat_group_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_broad_cat_group_stacked),sep="_")
# colnames(year_sim_pr_broad_cat_group_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_pr_broad_cat_group_stacked <- year_sim_pr_broad_cat_group_stacked[order(year_sim_pr_broad_cat_group_stacked[,"wficn"], 
#                                                                                  year_sim_pr_broad_cat_group_stacked[,"yr"]),]
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
# 
# 
# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY EQUITY STYLE LONG BOX AND YEAR", "\n")
# ###############################################################################
# 
# group_column <- "Equity_Style_Box_Long"
# 
# sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd4[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
#                            by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
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
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
#     
#     if (text_variables[j]=="iois")
#     {
#       year_sim_iois_equity_style_box_long_stacked <- rbind(year_sim_iois_equity_style_box_long_stacked,temp_sim_stacked)
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
# colnames(year_sim_iois_equity_style_box_long_stacked) <- paste(group_column,colnames(year_sim_iois_equity_style_box_long_stacked),sep="_")
# colnames(year_sim_iois_equity_style_box_long_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_iois_equity_style_box_long_stacked <- year_sim_iois_equity_style_box_long_stacked[order(year_sim_iois_equity_style_box_long_stacked[,"wficn"], 
#                                                                                                  year_sim_iois_equity_style_box_long_stacked[,"yr"]),]
# 
# colnames(year_sim_pr_equity_style_box_long_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_equity_style_box_long_stacked),sep="_")
# colnames(year_sim_pr_equity_style_box_long_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_pr_equity_style_box_long_stacked <- year_sim_pr_equity_style_box_long_stacked[order(year_sim_pr_equity_style_box_long_stacked[,"wficn"], 
#                                                                                              year_sim_pr_equity_style_box_long_stacked[,"yr"]),]
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
# sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd6[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
#                            by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
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
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
#     
#     if (text_variables[j]=="iois")
#     {
#       year_sim_iois_branding_name_stacked <- rbind(year_sim_iois_branding_name_stacked,temp_sim_stacked)
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
# colnames(year_sim_iois_branding_name_stacked) <- paste(group_column,colnames(year_sim_iois_branding_name_stacked),sep="_")
# colnames(year_sim_iois_branding_name_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_iois_branding_name_stacked <- year_sim_iois_branding_name_stacked[order(year_sim_iois_branding_name_stacked[,"wficn"], 
#                                                                                  year_sim_iois_branding_name_stacked[,"yr"]),]
# 
# colnames(year_sim_pr_branding_name_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_branding_name_stacked),sep="_")
# colnames(year_sim_pr_branding_name_stacked)[1:3] <- c("wficn","yr",group_column)
# 
# year_sim_pr_branding_name_stacked <- year_sim_pr_branding_name_stacked[order(year_sim_pr_branding_name_stacked[,"wficn"], 
#                                                                              year_sim_pr_branding_name_stacked[,"yr"]),]
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
# 
# 
# ###############################################################################
# cat("BACKUP SIMIALRITY DATA", "\n")
# ###############################################################################
# 
# descriptive_stats_tables <- ListTables(descriptive_stats_db)
# descriptive_stats_fields <- ListFields(descriptive_stats_db)
# 
# ExportTable(descriptive_stats_db,"year_sim_iois_all_stacked",year_sim_iois_all_stacked)
# ExportTable(descriptive_stats_db,"year_sim_pr_all_stacked",year_sim_pr_all_stacked)
# ExportTable(descriptive_stats_db,"year_sim_iois_broad_cat_group_stacked",year_sim_iois_broad_cat_group_stacked)
# ExportTable(descriptive_stats_db,"year_sim_pr_broad_cat_group_stacked",year_sim_pr_broad_cat_group_stacked)
# ExportTable(descriptive_stats_db,"year_sim_iois_equity_style_box_long_stacked",year_sim_iois_equity_style_box_long_stacked)
# ExportTable(descriptive_stats_db,"year_sim_pr_equity_style_box_long_stacked",year_sim_pr_equity_style_box_long_stacked)
# ExportTable(descriptive_stats_db,"year_sim_iois_branding_name_stacked",year_sim_iois_branding_name_stacked)
# ExportTable(descriptive_stats_db,"year_sim_pr_branding_name_stacked",year_sim_pr_branding_name_stacked)
# 
# #year_sim_iois_all_stacked                   <- runsql("SELECT * FROM year_sim_iois_all_stacked",descriptive_stats_db)
# #year_sim_pr_all_stacked                     <- runsql("SELECT * FROM year_sim_pr_all_stacked",descriptive_stats_db)
# #year_sim_iois_broad_cat_group_stacked       <- runsql("SELECT * FROM year_sim_iois_broad_cat_group_stacked",descriptive_stats_db)
# #year_sim_pr_broad_cat_group_stacked         <- runsql("SELECT * FROM year_sim_pr_broad_cat_group_stacked",descriptive_stats_db)
# #year_sim_iois_equity_style_box_long_stacked <- runsql("SELECT * FROM year_sim_iois_equity_style_box_long_stacked",descriptive_stats_db)
# #year_sim_pr_equity_style_box_long_stacked   <- runsql("SELECT * FROM year_sim_pr_equity_style_box_long_stacked",descriptive_stats_db)
# #year_sim_iois_branding_name_stacked         <- runsql("SELECT * FROM year_sim_iois_branding_name_stacked",descriptive_stats_db)
# #year_sim_pr_branding_name_stacked           <- runsql("SELECT * FROM year_sim_pr_branding_name_stacked",descriptive_stats_db)
# 
# 
# ###############################################################################
# cat("MERGE IOIS TEXT DATA", "\n")
# ###############################################################################
# 
# read_stats_iois <- merge(monthly_data_all4[,c("wficn","yr","month")], read_stats_iois_f, 
#                          by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# read_stats_iois <- read_stats_iois[order(read_stats_iois[,"wficn"],read_stats_iois[,"yr"],read_stats_iois[,"month"]),]
# 
# sim_stats_iois <- merge(monthly_data_all4[,c("wficn","yr","month")], year_sim_iois_all_stacked,
#                         by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_broad_cat_group_stacked, 
#                         by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_equity_style_box_long_stacked, 
#                         by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_branding_name_stacked, 
#                         by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_iois <- sim_stats_iois[order(sim_stats_iois[,"wficn"],sim_stats_iois[,"yr"],sim_stats_iois[,"month"]),]
# 
# text_stats_iois <- merge(read_stats_iois, sim_stats_iois, by.x=c("wficn","yr","month"), 
#                          by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# #rm2(read_stats_iois_f)
# #rm2(year_sim_iois_all_stacked,year_sim_iois_broad_cat_group_stacked)
# #rm2(year_sim_iois_equity_style_box_long_stacked,year_sim_iois_branding_name_stacked)
# rm2(read_stats_iois,sim_stats_iois)
# 
# 
# ###############################################################################
# cat("MERGE PR TEXT DATA", "\n")
# ###############################################################################
# 
# read_stats_pr <- merge(monthly_data_all4[,c("wficn","yr","month")], read_stats_pr_f, 
#                        by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# read_stats_pr <- read_stats_pr[order(read_stats_pr[,"wficn"],read_stats_pr[,"yr"],read_stats_pr[,"month"]),]
# 
# sim_stats_pr <- merge(monthly_data_all4[,c("wficn","yr","month")], year_sim_pr_all_stacked,
#                       by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_broad_cat_group_stacked, 
#                       by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_equity_style_box_long_stacked, 
#                       by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_branding_name_stacked, 
#                       by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# sim_stats_pr <- sim_stats_pr[order(sim_stats_pr[,"wficn"],sim_stats_pr[,"yr"],sim_stats_pr[,"month"]),]
# 
# text_stats_pr <- merge(read_stats_pr, sim_stats_pr, by.x=c("wficn","yr","month"), 
#                        by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# #rm2(read_stats_pr_f)
# #rm2(year_sim_pr_all_stacked,year_sim_pr_broad_cat_group_stacked)
# #rm2(year_sim_pr_equity_style_box_long_stacked,year_sim_pr_branding_name_stacked)
# rm2(read_stats_pr,sim_stats_pr)
# 
# 
# ###############################################################################
# cat("BACKUP DATA", "\n")
# ###############################################################################
# 
# descriptive_stats_tables <- ListTables(descriptive_stats_db)
# descriptive_stats_fields <- ListFields(descriptive_stats_db)
# 
# ExportTable(descriptive_stats_db,"monthly_data_all4",monthly_data_all4)
# ExportTable(descriptive_stats_db,"text_stats_iois",text_stats_iois)
# ExportTable(descriptive_stats_db,"text_stats_pr",text_stats_pr)
