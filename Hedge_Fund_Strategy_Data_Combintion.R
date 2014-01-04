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
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp2/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 5) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/R/", winslash = "\\", mustWork = TRUE)
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

create_lags <- function(data,variable){
  
  #data <- monthly_agg
  #variable <- "mnav_agg"
  
  text0 <- paste0(variable, ",")
  text1 <- paste0(variable, "lag1=shift(",variable,",-1),")
  text2 <- paste0(variable, "lag2=shift(",variable,",-2),")
  text3 <- paste0(variable, "lag3=shift(",variable,",-3),")
  text4 <- paste0(variable, "lag4=shift(",variable,",-4)")
  str <-  paste0("list(",text0,text1,text2,text3,text4,")")
  
  #expr <- parse(text="list(mnav_agg,mnav_agglag1=shift(mnav_agg,-1),mnav_agglag2=shift(mnav_agg,-2),mnav_agglag3=shift(mnav_agg,-3),mnav_agglag4=shift(mnav_agg,-4))")
  expr <- parse(text=str)
  
  data_lags <- as.data.frame(data.table(data)[,eval(expr),by=identifier], stringsAsFactors=FALSE)
  
  return(data.frame(data[,-match(c(variable),names(data))],data_lags[,-match(c(identifier),names(data_lags))],stringsAsFactors=FALSE))
  
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
          
          rm(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
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
          
          rm(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
        } else
        {
          rm(temp_data_full_cat_temp)
          
        }
        
        rm(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm(temp_data_full_no_na,temp_data_full_no_na_ids,temp_data_full_no_na_trim)
        
      } else
      {
        rm(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm(temp_data_full_no_na)
        
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
external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
                       "pander","pbapply","plm","plyr","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
                       "sandwich","sqldf","stargazer","stringr","SWordInstaller","texreg","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")




###############################################################################
cat("IMPORT READABILITY TEXT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

#Import .CSV files

read_stats_ios_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
for(i in which(sapply(read_stats_ios_f,class)=="character"))
{
  read_stats_ios_f[[i]] = trim(read_stats_ios_f[[i]])
}
for (i in 1:ncol(read_stats_ios_f))
{
  read_stats_ios_f[,i] <- unknownToNA(read_stats_ios_f[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  read_stats_ios_f[,i] <- ifelse(is.na(read_stats_ios_f[,i]),NA, read_stats_ios_f[,i])
}

sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 


read_stats_ios_f <- subset(read_stats_ios_f,select=-c(foreign_ios))
sample_data_all <- subset(sample_data_all,select=-c(Strategy))


###############################################################################
cat("IMPORT SIMILARITY TEXT DATA", "\n")
###############################################################################

text_variables <- c("ios")
#text_percentages <- c("050pct","100pct","250pct","500pct","750pct","900pct")
text_percentages <- c("900pct")

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
    
    for(k in which(sapply(year_sim,class)=="character"))
    {
      year_sim[[k]] = trim(year_sim[[k]])
    }
    for (k in 1:ncol(year_sim))
    {
      year_sim[,k] <- unknownToNA(year_sim[,k], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
      year_sim[,k] <- ifelse(is.na(year_sim[,k]),NA, year_sim[,k])
    }
    
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



###############################################################################
cat("IMPORT MFLINKS DATA", "\n")
###############################################################################

mflinks_tables <- ListTables(mflinks_db)
mflinks_fields <- ListFields(mflinks_db)

mflink1_fields <- mflinks_fields[mflinks_fields[,1]=="mflink1",]
mflink1 <- runsql("SELECT * FROM mflink1",mflinks_db)

for(i in which(sapply(mflink1,class)=="character"))
{
  mflink1[[i]] = trim(mflink1[[i]])
}

for (i in 1:ncol(mflink1))
{
  mflink1[,i] <- unknownToNA(mflink1[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  mflink1[,i] <- ifelse(is.na(mflink1[,i]),NA, mflink1[,i])
} 

mflink1 <- mflink1[!(is.na(mflink1[,"crsp_fundno"])),]




###############################################################################
cat("IMPORT MDMF DATA", "\n")
###############################################################################

mdmf_tables <- ListTables(msd_db)
mdmf_fields <- ListFields(msd_db)

mdmf_fields <- mdmf_fields[mdmf_fields[,1]=="Mdmf_data_raw",]
Mdmf_data_raw <- runsql("SELECT * FROM Mdmf_data_raw",msd_db)

for(i in which(sapply(Mdmf_data_raw,class)=="character"))
{
  Mdmf_data_raw[[i]] = trim(Mdmf_data_raw[[i]])
}

for (i in 1:ncol(Mdmf_data_raw))
{
  Mdmf_data_raw[,i] <- unknownToNA(Mdmf_data_raw[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  Mdmf_data_raw[,i] <- ifelse(is.na(Mdmf_data_raw[,i]),NA, Mdmf_data_raw[,i])
} 

Mdmf_data_trim <- subset(Mdmf_data_raw,select=c(CUSIP, Broad_Cat_Group, Global_Cat, MS_Cat, MS_Inst_Cat, 
                                                MS_Rating_Overall, US_Broad_Asset_Class, Equity_Style_Box_Long, MS_Anal_Rating, 
                                                Firm_Name, Branding_Name, Prospectus_Objective))
Mdmf_data_trim <- Mdmf_data_trim[order(Mdmf_data_trim[,"CUSIP"]),]











