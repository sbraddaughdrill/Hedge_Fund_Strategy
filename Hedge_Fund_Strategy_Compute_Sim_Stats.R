# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Compute_Sim_Stats.R
# Version: 1.0
# Date:    01.05.2015
# Purpose: Compute Sim Stats
#
###############################################################################

###############################################################################
# INITIAL SETUP;
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
Location <- 4

if (Location == 1) {
  
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  input_directory <- normalizePath("F:/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp2/",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp2",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  #input_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp2/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp2/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 6) {
  
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
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
#"funprog","Snowball","SnowballC"
external_packages <- c("colbycol","compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
                       "Hmisc","koRpus","limma","mitools","pbapply","plyr","R.oo","reshape2","rJava","RWeka","RWekajars",
                       "splitstackshape","sqldf","stringi","stringr","tcltk","tm")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
# PREALLOCATE DATA;
cat("SECTION: PREALLOCATE DATA", "\n")
###############################################################################

#Create base column table
#temp_data_cols <- as.data.frame(matrix(NA, ncol=7, nrow=200),stringsAsFactors=FALSE)
#colnames(temp_data_cols) <- c("order","isnum","ischar","isdate","isfactor","colnames","desc")

temp_data_cols <- data.frame(matrix(NA, ncol=7, nrow=200, 
                                    dimnames=list(c(), c("order","isnum","ischar","isdate","isfactor","colnames","desc"))), 
                             stringsAsFactors=FALSE)

temp_data_cols[,1] <- as.numeric(temp_data_cols[,1])
temp_data_cols[,2] <- as.numeric(temp_data_cols[,2])
temp_data_cols[,3] <- as.numeric(temp_data_cols[,3])
temp_data_cols[,4] <- as.numeric(temp_data_cols[,4])
temp_data_cols[,5] <- as.numeric(temp_data_cols[,5])
temp_data_cols[,6] <- as.character(temp_data_cols[,6])
temp_data_cols[,7] <- as.character(temp_data_cols[,7])

# #Files table
# #file_list <- c("EurekahedgeHF_Excel_aca.csv","EurekahedgeHF_Excel_aca_NAV_AUM.csv","EurekahedgeHF_Excel_aca_Instruments_Traded.csv")
# file_list <- c("EurekahedgeHF_Profile_Strategy_part3.csv")
# files_cols_count <- 2
# files_cols <- temp_data_cols[1:files_cols_count,]
# files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors=FALSE)
# files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors=FALSE)
# files <- as.data.frame(matrix(NA, ncol=files_cols_count, nrow=length(file_list)),stringsAsFactors=FALSE)
# colnames(files) <- files_cols[,6]
# files <- format_function(files,files_cols)


#Populate Percentiles table

#Note: If Confidence_Level is 0.900, 
#          word_grand  -  this means that if the word is the 10% most common occuring or higher overall word, it is removed (90% removed/10% kept)
#          word_unique -  this means that if the word is the 10% most common occuring or higher unique word, it is removed (90% removed/10% kept)
#          id_unique -  this means that if a word is in 10% of the ids or higher, it is removed  (90% removed/10% kept)
#Note: If Confidence_Level is 0.050, 
#          word_grand  -  this means that if the word is the 95% most common occuring or higher overall word, it is removed (5% removed/95% kept)
#          word_unique -  this means that if the word is the 95% most common occuring or higher unique word, it is removed (5% removed/95% kept)
#          id_unique -  this means that if a word is in 95% of the ids or higher, it is removed  (5% removed/95% kept)
# As confidence level increases, the words in the dictionary should decrease.
# A smaller dicitonary means that that smilarity will be less because there are fewer possible words that could be common between two texts 
# Thus, as confidence level increases, the smilarity scores will decrease

#Percentiles table
percentiles_cols_count <- 8
percentiles_cols <- temp_data_cols[1:percentiles_cols_count,]
percentiles_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Level",stringsAsFactors=FALSE)
percentiles_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Pct",stringsAsFactors=FALSE)
percentiles_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Confidence_lbl",stringsAsFactors=FALSE)
percentiles_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Level",stringsAsFactors=FALSE)
percentiles_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Pct",stringsAsFactors=FALSE)
percentiles_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Significance_lbl",stringsAsFactors=FALSE)
percentiles_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_lbl",stringsAsFactors=FALSE)
percentiles_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_DV",stringsAsFactors=FALSE)

#percentile_vals <- c(0.990,0.950,0.900)
percentile_vals <- c(0.900,0.750,0.500,0.250,0.100,0.050)
#percentile_vals <- c(0.900)

percentiles <- as.data.frame(matrix(NA, ncol=percentiles_cols_count, nrow=length(percentile_vals)),stringsAsFactors=FALSE)
colnames(percentiles) <- percentiles_cols[,6]

percentiles[,"Confidence_Level"] <- format(as.double(percentile_vals), digits=3)
percentiles[,"Confidence_Pct"] <- as.double(percentiles[,"Confidence_Level"])*100
percentiles[,"Confidence_lbl"] <- formatC(percentiles[,"Confidence_Pct"],format="f", digits=1,width=4,  flag="0")
percentiles[,"Significance_Level"] <- 1-as.double(percentiles[,"Confidence_Level"])
percentiles[,"Significance_Pct"] <- as.double(percentiles[,"Significance_Level"])*100
percentiles[,"Significance_lbl"] <- formatC(percentiles[,"Significance_Pct"], format="f",digits=1, width=5,  flag="0")
percentiles[,"Confidence_lbl"] <-  paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Confidence_lbl"]),"pct",sep="")
percentiles[,"Significance_lbl"] <- paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Significance_lbl"]),"pct",sep="")
percentiles[,"Column_lbl"] <- paste("Word_Cutoff_",percentiles[,"Confidence_lbl"],sep="")
percentiles[,"Column_DV"] <- paste("Word_DV_",percentiles[,"Confidence_lbl"],sep="")
percentiles <- format_function(percentiles,percentiles_cols)
percentiles <- percentiles[order(percentiles[,"Confidence_Level"]),]

#Readability columns table
readbl_vars_cols_count <- 4
readbl_vars_cols <- temp_data_cols[1:readbl_vars_cols_count,]
readbl_vars_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="column",stringsAsFactors=FALSE)
readbl_vars_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="suffix",stringsAsFactors=FALSE)
readbl_vars_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="readabilitystats_table",stringsAsFactors=FALSE)
readbl_vars_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token_table",stringsAsFactors=FALSE)
readbl_vars <- as.data.frame(matrix(NA, ncol=readbl_vars_cols_count, nrow=1),stringsAsFactors=FALSE)
colnames(readbl_vars) <- readbl_vars_cols[,6]

# #Readability statistics table
# readbl_all_df_cols_count <- 5
# readbl_all_df_cols <- temp_data_cols[1:readbl_all_df_cols_count,]
# readbl_all_df_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index",stringsAsFactors=FALSE)
# readbl_all_df_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="flavour",stringsAsFactors=FALSE)
# readbl_all_df_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="raw",stringsAsFactors=FALSE)
# readbl_all_df_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="grade",stringsAsFactors=FALSE)
# readbl_all_df_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="age",stringsAsFactors=FALSE)
# readbl_all_df <- as.data.frame(matrix(NA, ncol=readbl_all_df_cols_count, nrow=44),stringsAsFactors=FALSE)
# colnames(readbl_all_df) <- readbl_all_df_cols[,6]
# readbl_all_df <- format_function(readbl_all_df,readbl_all_df_cols)

# #Tokens table
# tokens_all_cols_count <- 5
# tokens_all_cols <- temp_data_cols[1:tokens_all_cols_count,]
# tokens_all_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",stringsAsFactors=FALSE)
# tokens_all_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",stringsAsFactors=FALSE)
# tokens_all_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token",stringsAsFactors=FALSE)
# tokens_all_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="desc",stringsAsFactors=FALSE)
# tokens_all_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Remove",stringsAsFactors=FALSE)


###############################################################################
cat("SECTION: POPULATE DATA", "\n")
###############################################################################

#List variables to compute readability statistics and the suffixes used for each in the sample_data_statistics_cols table
readbl_vars[,1] <- c("Strategy")
readbl_vars[,2] <- c("_ios")
readbl_vars[,3] <- c("read_stats_ios_f")
readbl_vars[,4] <- c("tokens_all_ios_f")

# remove stopwords
myStopwords <- c(stopwords('english'),stopwords('SMART'),"available", "via")
myStopwords_no_punct <- gsub(pattern="[^[[:alnum:][:space:]]", replacement="", x=myStopwords)
myStopwords_all <- c(myStopwords,myStopwords_no_punct)
myStopwords_all <- sort(myStopwords_all)
myStopwords_all <- unique(myStopwords_all, incomparables=FALSE)
myStopwords_all <- toupper(myStopwords_all)

rm(myStopwords,myStopwords_no_punct)

#idx <- which(myStopwords_all %in% c("R",keep_one_letter_tokens,keep_two_letter_tokens))
idx <- which(myStopwords_all %in% c("R"))
myStopwords_all <- myStopwords_all[-idx]

rm(idx)

#measures <- c("word_grand","word_unique","id_unique")
measures <- c("id_unique")

keep_one_letter_words <- c("I")
keep_one_letter_ratings <- c("A","B","C","D","P")
keep_one_letter_tokens <- sort(c(keep_one_letter_words,keep_one_letter_ratings))

rm2(keep_one_letter_words,keep_one_letter_ratings)

keep_two_letter_words <- c("AM","AN","AS","AT","BE","BY","DO","EG","EX","HA","ID","IE","IF","IN","IS",
                           "IT","MY","NO","OF","ON","OR","QA","RD","SO","SP","TM","TO","TV","UM","UN",
                           "UP","US","VP","WE")
keep_state_abbreviations <- c("AK","AL","AR","AZ","CA","CF","CL","CO","CT","DC","DE","DL","FL","GA","HA",
                              "HI","IA","ID","IL","IN","KA","KS","KY","LA","MA","MC","MD","ME","MI","MN",
                              "MO","MS","MT","NB","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR",
                              "PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WN","WS","WV","WY")
keep_two_letter_ratings <- c("AA","BA","BB","CA","CC")
keep_two_letter_tokens <- sort(c(keep_two_letter_words,keep_state_abbreviations,keep_two_letter_ratings))

rm2(keep_two_letter_words,keep_state_abbreviations,keep_two_letter_ratings)


###############################################################################
cat("SECTION: COMPUTE SIMILARITY STATISTICS", "\n")
###############################################################################

# sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# 
# for(i in which(sapply(sample_data_all,class)=="character"))
# {
#   sample_data_all[[i]] <- trim(sample_data_all[[i]])
# }
# for (i in 1:ncol(sample_data_all))
# {
#   sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=unknowns_strings,force=TRUE)
#   sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
# } 


for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  cat("Token table: ",readbl_vars[m,4], "\n")
  if (m==1)
  {
    
    #input_row_count <- nrow(read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE))
    #tokens_all_temp <- as.data.frame(matrix(NA, ncol=tokens_all_cols_count, nrow=input_row_count))
    #colnames(tokens_all_temp) <- tokens_all_cols[,6]
    
    #tokens_all_temp[,"ID"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("ID")]
    #tokens_all_temp[,"token"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("token")]
    #tokens_all_temp[,"desc"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("desc")]
    
    #tokens_all_temp <- read.csv(file=paste(output_directory,readbl_vars[m,4],"_full",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
    
    tokens_all_col_names_all <- as.vector(t(read.csv(file=paste(output_directory,readbl_vars[m,4],"_full",".csv",sep=""),header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1)))
    tokens_all_col_names_keep <- which(!(tokens_all_col_names_all %in% c("Strat_ID","Strategy")))
    #tokens_all_temp <- cbc.read.table2(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),just.read=tokens_all_col_names_keep,header=TRUE,sep=",")
    #tokens_all_temp <- as.data.frame(tokens_all_temp,stringsAsFactors=FALSE)

    tokens_all_temp <- read.columns(file=paste(output_directory,readbl_vars[m,4],"_full",".csv",sep=""),required.col=tokens_all_col_names_all[tokens_all_col_names_keep],sep=",",na.strings="NA",stringsAsFactors=FALSE)
    tokens_all_temp <- as.data.frame(tokens_all_temp,stringsAsFactors=FALSE)
    
    rm2(tokens_all_col_names_all,tokens_all_col_names_keep)
    
  } else if (m==2)
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  }
  
  for(i in which(sapply(tokens_all_temp,class)=="character"))
  {
    #tokens_all_temp[[i]] <- trim(tokens_all_temp[[i]])
    tokens_all_temp[[i]] <- gsub("^\\s+|\\s+$", "", tokens_all_temp[[i]], perl=TRUE)
  }
  rm(i)
  for (i in 1:ncol(tokens_all_temp))
  {
    tokens_all_temp[,i] <- unknownToNA(tokens_all_temp[,i], unknown=unknowns_strings,force=TRUE)
    tokens_all_temp[,i] <- ifelse(is.na(tokens_all_temp[,i]),NA, tokens_all_temp[,i])
  } 
  rm(i)
  
  tokens_all_temp <- tokens_all_temp[!(rowSums(is.na(tokens_all_temp[,1:ncol(tokens_all_temp)]))==ncol(tokens_all_temp)),]
  
  tokens_all_temp <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"]),] 
  #row.names(tokens_all_temp) <- seq(nrow(tokens_all_temp))
  
  #Trim strings
  #tokens_all_temp[,"token"] <- trim(tokens_all_temp[,"token"])
  tokens_all_temp[,"token"] <- gsub("^\\s+|\\s+$", "", tokens_all_temp[,"token"], perl=TRUE)
  
  #Upcase strings
  tokens_all_temp[,"token"] <- toupper(tokens_all_temp[,"token"])
  
  #Remove everything except letters, space, apostrophe, hyphen, and ampersand
  tokens_all_temp[,"token"] <- as.data.frame(gsub(pattern="[^[[:alnum:][:space:]'&-]", replacement=" ", x=tokens_all_temp[,"token"]))
  
  #Remove multiple spaces (run a couple times)
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"] <- gsub(pattern=" {2,}", replacement=" ", x=tokens_all_temp[,"token"])
  }
  rm(a)
  
  #Remove numbers
  tokens_all_temp[,"token"]  <- gsub(pattern="\\d", replacement="", x=tokens_all_temp[,"token"])
  
  saved_symbols <- c("&","-","'")
  
  #Remove double apostrophe, hyphen, and ampersand
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"]  <- remove_duplicate_symbols(tokens_all_temp[,"token"], saved_symbols)
    
  }
  rm(a)
  
  #Remove single, leading, and trailing symbols
  tokens_all_temp[,"token"]  <-  remove_single_symbols(tokens_all_temp[,"token"], saved_symbols)
  tokens_all_temp[,"token"]  <-  remove_leading_symbols(tokens_all_temp[,"token"], saved_symbols)
  tokens_all_temp[,"token"]  <-  remove_trailing_symbols(tokens_all_temp[,"token"], saved_symbols)
  
  rm(saved_symbols)
  
  #Remove single spaces (run a couple times)
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"]  <- gsub(pattern=" ", replacement="", x=tokens_all_temp[,"token"])
  }
  rm(a)
  
  #Default Remove to NA
  tokens_all_temp[,"Remove"] <- as.numeric(rep(NA, nrow(tokens_all_temp)))
  
  #Find which rows to remove
  remove_descriptions <- c("Cardinal number","Comma","Sentence ending punctuation","Symbol",
                           "Opening bracket","Closing bracket","Quote","End quote")
  remove_punct <- c("%","&","\t","-","--","---","'",""," ")
  remove_phone <- c("1-800-XXX-XXXX","XXX-XXX-XXXX","XXX-XXXX","-XXX-XXX-XXXX")
  remove_tokens <- c(remove_punct,remove_phone)
  
  #keep_three_letter_ratings <- c("AAA","BAA","BBB","CAA","CCC","DDD")
  
  tokens_all_temp[,"Remove"] <- ifelse(!(tokens_all_temp$desc %in% remove_descriptions) & !(tokens_all_temp$token %in% remove_tokens), 0, 1)
  tokens_all_temp[,"Remove"] <- ifelse(((nchar(tokens_all_temp$token)==1) & !(tokens_all_temp$token %in% keep_one_letter_tokens)), 1, tokens_all_temp$Remove)
  tokens_all_temp[,"Remove"] <- ifelse(((nchar(tokens_all_temp$token)==2) & !(tokens_all_temp$token %in% keep_two_letter_tokens)), 1, tokens_all_temp$Remove)
  
  #Remove stop words
  #tokens_all_temp[,"Remove"] <- ifelse(tokens_all_temp$token %in% myStopwords_all, 1, tokens_all_temp$Remove)
  
  #==============================================================================;
  #Stem Words;
  #==============================================================================;
  
  tokens_all_temp <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"]),] 
  #row.names(tokens_all_temp) <- seq(nrow(tokens_all_temp))
  
  tokens_all_temp_dt <- data.table(tokens_all_temp[(tokens_all_temp[,"Remove"]==0),], key = c("Fund_ID","yr"))
  tokens_all_temp1 <- tokens_all_temp_dt[,list(word=stem_words(token,myStopwords_all)),by="Fund_ID,yr"]
  tokens_all_temp1 <- as.data.frame(tokens_all_temp1,stringsAsFactors=FALSE)
  tokens_all_temp  <- data.frame(Fund_ID=tokens_all_temp1[,"Fund_ID"],
                                 yr=tokens_all_temp1[,"yr"],
                                 token=tokens_all_temp1[,"word"],
                                 Remove=0,stringsAsFactors=FALSE)
  
  rm2(tokens_all_temp_dt,tokens_all_temp1)
  
  #==============================================================================;
  #Find Unique Words for Each Fund_ID;
  #==============================================================================;
  
  colnames(tokens_all_temp)[match("Fund.ID",names(tokens_all_temp))] <- "Fund_ID"
  
  #Sort tokens_all_temp
  tokens_all_temp  <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"],tokens_all_temp[,"token"]),]
  #row.names(tokens_all_temp) <- seq(nrow(tokens_all_temp))
  
  query_tokens_all_temp2 <- ""
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"select distinct  Fund_ID, yr, Upper(token) token, Count(token) Count, Remove  ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"from             tokens_all_temp                                              ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"where            Remove=0                                                     ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"group by         Fund_ID, yr, Upper(token)                                    ", sep=" ")
  query_tokens_all_temp2 <- trim(gsub(" {2,}", " ", query_tokens_all_temp2))
  
  #tokens_all_temp2 <- cbind(sqldf(query_tokens_all_temp2), NA, NA, NA)
  #colnames(tokens_all_temp2) <- c("ID","yr","token","Count","Remove","uTotal","gTotal","Total_Percentage")
  
  
  tokens_all_temp2 <- data.frame(sqldf(query_tokens_all_temp2),uTotal=NA,gTotal=NA,Total_Percentage=NA,stringsAsFactors=FALSE)
  
  tokens_all_temp2[,c("uTotal","gTotal","Total_Percentage")] <- subset(data.table(tokens_all_temp2)[, list(uTotal=length(token), gTotal=sum(Count), Total_Percentage=(Count)/sum(Count)),by="yr,Fund_ID"],
                                                                       select=c("uTotal","gTotal","Total_Percentage"))
  
  rm(query_tokens_all_temp2)
  
  #==============================================================================;
  cat("SECTION: GLOBAL DICTIONARY (AGGREGATE)", "\n")
  #
  # global_agg_word_grand_temp: total words across ids 
  # global_agg_word_unique_temp: total unique words across ids 
  #
  #==============================================================================;
  
  identifier <- "Fund_ID"
  
  #==============================================================================;
  #Create global aggregate grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_agg_word_grand_temp
  global_agg_word_grand_temp <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_word_grand_temp <- global_agg_word_grand_temp[!(rowSums(is.na(global_agg_word_grand_temp[,1:ncol(global_agg_word_grand_temp)]))==ncol(global_agg_word_grand_temp)),]
  global_agg_word_grand_temp[,"yr"] <- 9999
  global_agg_word_grand_temp <- global_agg_word_grand_temp[order(global_agg_word_grand_temp[,"yr"], global_agg_word_grand_temp[,identifier],global_agg_word_grand_temp[,"token"]),] 
  #row.names(global_agg_word_grand_temp) <- seq(nrow(global_agg_word_grand_temp))
  
  global_agg_word_grand_temp3 <- create_global_dictionary_word(global_agg_word_grand_temp,"word_grand",percentiles)
  
  rm2(global_agg_word_grand_temp)
  
  
  #==============================================================================;
  #Create global aggregate unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_agg_word_unique_temp
  global_agg_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_word_unique_temp <- global_agg_word_unique_temp[!(rowSums(is.na(global_agg_word_unique_temp[,1:ncol(global_agg_word_unique_temp)]))==ncol(global_agg_word_unique_temp)),]
  global_agg_word_unique_temp[,"yr"] <- 9999
  global_agg_word_unique_temp <- unique(global_agg_word_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_word_unique_temp <- global_agg_word_unique_temp[order(global_agg_word_unique_temp[,"yr"], global_agg_word_unique_temp[,identifier],global_agg_word_unique_temp[,"token"]),] 
  #row.names(global_agg_word_unique_temp) <- seq(nrow(global_agg_word_unique_temp))
  
  global_agg_word_unique_temp3 <- create_global_dictionary_word(global_agg_word_unique_temp,"word_unique",percentiles)
  
  rm2(global_agg_word_unique_temp)
  
  #==============================================================================;
  #Create global aggregate unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_agg_id_unique_temp
  global_agg_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_id_unique_temp <- global_agg_id_unique_temp[!(rowSums(is.na(global_agg_id_unique_temp[,1:ncol(global_agg_id_unique_temp)]))==ncol(global_agg_id_unique_temp)),]
  global_agg_id_unique_temp[,"yr"] <- 9999
  global_agg_id_unique_temp <- unique(global_agg_id_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_id_unique_temp <- global_agg_id_unique_temp[order(global_agg_id_unique_temp[,"yr"], global_agg_id_unique_temp[,identifier],global_agg_id_unique_temp[,"token"]),] 
  #row.names(global_agg_id_unique_temp) <- seq(nrow(global_agg_id_unique_temp))
  
  #Copy tokens_all_temp to unique_ids_agg
  unique_ids_agg <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  unique_ids_agg <- unique_ids_agg[!(rowSums(is.na(unique_ids_agg[,1:ncol(unique_ids_agg)]))==ncol(unique_ids_agg)),]
  unique_ids_agg[,"yr"] <- 9999
  unique_ids_agg <- unique_ids_agg[order(unique_ids_agg[,"yr"], unique_ids_agg[,identifier]),] 
  #row.names(unique_ids_agg) <- seq(nrow(unique_ids_agg))
  
  #Get list of all unique words
  unique_ids_agg <- ddply(unique_ids_agg, "yr", function(x) as.data.frame(unique(x[,c("yr",identifier)], incomparables=FALSE)))
  unique_ids_agg <- ddply(unique_ids_agg, "yr", function(x) data.frame(x,nrow=as.numeric(nrow(x))))
  unique_ids_agg <- unique_ids_agg[order(unique_ids_agg[,"yr"], unique_ids_agg[,identifier]),] 
  #row.names(unique_ids_agg) <- seq(nrow(unique_ids_agg))
  write.csv(unique_ids_agg, file=paste(output_directory,"unique_ids_agg",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_agg_id_unique_temp3 <- create_global_dictionary_id(global_agg_id_unique_temp,"id_unique",percentiles,unique_ids_agg)
  
  #write.csv(global_agg_id_unique_temp3, file=paste(output_directory,"global_agg_id_unique_temp3",".csv",sep=""))
  
  rm2(global_agg_id_unique_temp)
  
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (aggregate);
  #==============================================================================;
  
  global_agg_comb1 <- merge(global_agg_word_grand_temp3, global_agg_word_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  global_agg_comb2 <- merge(global_agg_comb1, global_agg_id_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token") , all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  
  global_agg_count_vector <- grep(pattern='*Count_*', colnames(global_agg_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_agg_dv_vector <- grep(pattern='*_DV_*', colnames(global_agg_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_agg_col_vector <- append(global_agg_count_vector,global_agg_dv_vector)
  global_agg_col_vector <- append("token",global_agg_col_vector)
  global_agg_col_vector <- append("yr",global_agg_col_vector)
  global_agg_comb  <- global_agg_comb2[,global_agg_col_vector]
  write.csv(global_agg_comb, file=paste(output_directory,"global_agg_comb",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  rm2(global_agg_comb1,global_agg_comb2,global_agg_word_grand_temp3,global_agg_word_unique_temp3,global_agg_id_unique_temp3,global_agg_col_vector,global_agg_count_vector)
  
  
  #==============================================================================;
  #Create global aggregate tables based on the percentiles;
  #==============================================================================;
  
  global_agg_dv_vector_used <- as.data.frame(cbind(global_agg_dv_vector,rep(NA,length(global_agg_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_agg_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Remove trimmed words and save individual dictionaries
  for (b in 1:length(measures))
  {
    #b <- 1
    
    global_agg_dv_vector_used <- trim_global_dictionary(global_agg_comb,measures[b],percentiles,global_agg_dv_vector_used,readbl_vars[m,2])
    
    progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  rm(b)
  
  assign(paste("global_agg_dv_vector_used",readbl_vars[m,2],sep=""), global_agg_dv_vector_used, envir=.GlobalEnv)
  
  rm2(global_agg_dv_vector_used,global_agg_dv_vector)
  
  
  #==============================================================================;
  cat("SECTION: GLOBAL DICTIONARY (YEARLY)", "\n")
  #==============================================================================;
  
  #==============================================================================;
  #Create global yearly grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_year_word_grand_temp
  global_year_word_grand_temp <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_word_grand_temp <- global_year_word_grand_temp[!(rowSums(is.na(global_year_word_grand_temp[,1:ncol(global_year_word_grand_temp)]))==ncol(global_year_word_grand_temp)),]
  global_year_word_grand_temp <- global_year_word_grand_temp[order(global_year_word_grand_temp[,"yr"], global_year_word_grand_temp[,identifier],global_year_word_grand_temp[,"token"]),] 
  #row.names(global_year_word_grand_temp) <- seq(nrow(global_year_word_grand_temp))
  
  global_year_word_grand_temp3 <- create_global_dictionary_word(global_year_word_grand_temp,"word_grand",percentiles)
  
  rm2(global_year_word_grand_temp)
  
  
  #==============================================================================;
  #Create global year unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_year_word_unique_temp
  global_year_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_word_unique_temp <- global_year_word_unique_temp[!(rowSums(is.na(global_year_word_unique_temp[,1:ncol(global_year_word_unique_temp)]))==ncol(global_year_word_unique_temp)),]
  global_year_word_unique_temp <- global_year_word_unique_temp[order(global_year_word_unique_temp[,"yr"], global_year_word_unique_temp[,identifier],global_year_word_unique_temp[,"token"]),] 
  global_year_word_unique_temp <- unique(global_year_word_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  #row.names(global_year_word_unique_temp) <- seq(nrow(global_year_word_unique_temp))
  
  global_year_word_unique_temp3 <- create_global_dictionary_word(global_year_word_unique_temp,"word_unique",percentiles)
  
  rm2(global_year_word_unique_temp)
  
  
  #==============================================================================;
  #Create global year unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_year_id_unique_temp
  global_year_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_id_unique_temp <- global_year_id_unique_temp[!(rowSums(is.na(global_year_id_unique_temp[,1:ncol(global_year_id_unique_temp)]))==ncol(global_year_id_unique_temp)),]
  global_year_id_unique_temp <- global_year_id_unique_temp[order(global_year_id_unique_temp[,"yr"], global_year_id_unique_temp[,identifier],global_year_id_unique_temp[,"token"]),] 
  global_year_id_unique_temp <- unique(global_year_id_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  #row.names(global_year_id_unique_temp) <- seq(nrow(global_year_id_unique_temp))
  
  #Copy tokens_all_temp to unique_ids_year
  unique_ids_year <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  unique_ids_year <- unique_ids_year[!(rowSums(is.na(unique_ids_year[,1:ncol(unique_ids_year)]))==ncol(unique_ids_year)),]
  unique_ids_year <- unique_ids_year[order(unique_ids_year[,"yr"], unique_ids_year[,identifier]),] 
  #row.names(unique_ids_year) <- seq(nrow(unique_ids_year))
  
  #Get list of all unique words
  unique_ids_year <- ddply(unique_ids_year, "yr", function(x) as.data.frame(unique(x[,c("yr",identifier)], incomparables=FALSE)))
  unique_ids_year <- ddply(unique_ids_year, "yr", function(x) data.frame(x,nrow=as.numeric(nrow(x))))
  unique_ids_year <- unique_ids_year[order(unique_ids_year[,"yr"], unique_ids_year[,identifier]),] 
  #row.names(unique_ids_year) <- seq(nrow(unique_ids_year))
  write.csv(unique_ids_year, file=paste(output_directory,"unique_ids_year",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_year_id_unique_temp3 <- create_global_dictionary_id(global_year_id_unique_temp,"id_unique",percentiles,unique_ids_year)
  
  #write.csv(global_year_id_unique_temp3, file=paste(output_directory,"global_year_id_unique_temp3",".csv",sep=""))
  
  rm2(global_year_id_unique_temp)
  
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (year);
  #==============================================================================;
  
  global_year_comb1 <- merge(global_year_word_grand_temp3, global_year_word_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  global_year_comb2 <- merge(global_year_comb1, global_year_id_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token") , all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  
  global_year_count_vector <- grep(pattern='*Count_*', colnames(global_year_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_year_dv_vector <- grep(pattern='*_DV_*', colnames(global_year_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_year_col_vector <- append(global_year_count_vector,global_year_dv_vector)
  global_year_col_vector <- append("token",global_year_col_vector)
  global_year_col_vector <- append("yr",global_year_col_vector)
  global_year_comb  <- global_year_comb2[,global_year_col_vector]
  write.csv(global_year_comb, file=paste(output_directory,"global_year_comb",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  
  #global_year_comb_diff <- global_year_comb[global_year_comb[,11]!=global_year_comb[,14],]
  
  rm2(global_year_comb1,global_year_comb2,global_year_word_grand_temp3,global_year_word_unique_temp3,global_year_id_unique_temp3,global_year_col_vector,global_year_count_vector)
  
  
  #==============================================================================;
  #Create global year tables based on the percentiles;
  #==============================================================================;
  
  global_year_dv_vector_used <- as.data.frame(cbind(global_year_dv_vector,rep(NA,length(global_year_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_year_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Remove trimmed words and save individual dictionaries
  for (b in 1:length(measures))
  {
    #b <- 1
    
    global_year_dv_vector_used <- trim_global_dictionary(global_year_comb,measures[b],percentiles,global_year_dv_vector_used,readbl_vars[m,2])
    
    progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  rm(b)
  
  assign(paste("global_year_dv_vector_used",readbl_vars[m,2],sep=""), global_year_dv_vector_used, envir=.GlobalEnv)
  
  rm2(global_year_dv_vector_used,global_year_dv_vector)
  rm2(tokens_all_temp)
  
  #OUPUT INDIVIDUAL TOKEN DICTIONARIES
  
  global_agg_tokens <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_tokens <- global_agg_tokens[!(rowSums(is.na(global_agg_tokens[,1:ncol(global_agg_tokens)]))==ncol(global_agg_tokens)),]
  global_agg_tokens[,"yr"] <- 9999
  global_agg_tokens <- unique(global_agg_tokens[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_tokens <- global_agg_tokens[order(global_agg_tokens[,"yr"], global_agg_tokens[,identifier],global_agg_tokens[,"token"]),] 
  #row.names(global_agg_tokens) <- seq(nrow(global_agg_tokens))
  write.csv(global_agg_tokens, file=paste(output_directory,"global_agg_tokens",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_year_tokens <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_tokens <- global_year_tokens[!(rowSums(is.na(global_year_tokens[,1:ncol(global_year_tokens)]))==ncol(global_year_tokens)),]
  global_year_tokens <- unique(global_year_tokens[,c("yr",identifier,"token")], incomparables=FALSE)
  global_year_tokens <- global_year_tokens[order(global_year_tokens[,"yr"], global_year_tokens[,identifier],global_year_tokens[,"token"]),] 
  #row.names(global_year_tokens) <- seq(nrow(global_year_tokens))
  write.csv(global_year_tokens, file=paste(output_directory,"global_year_tokens",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  rm2(tokens_all_temp2)
  
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  rm2(identifier)
  rm2(global_agg_comb,unique_ids_agg,global_agg_tokens)
  rm2(global_year_comb,unique_ids_year,global_year_tokens)
  
  progress_function(outer_loop_count=m, outer_loop_start_val=1, outer_loop_end_val=nrow(readbl_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  #END OF M FOR LOOP
  
}

rm2(m)
rm2(remove_descriptions,remove_punct,remove_phone,remove_tokens)
rm2(keep_one_letter_tokens,keep_two_letter_tokens)
rm2(myStopwords_all)

#==============================================================================;
cat("SECTION: INDIVIDUAL DICTIONARY - AGGREGATE", "\n")
#==============================================================================;

identifier <- "Fund_ID"

output_db <- paste(output_directory,"Text_Analysis.s3db",sep="")

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  if (m==1)
  {
    file_type_str <- readbl_vars[m,2]
  } else if (m==2)
  {
    #file_type_str <- readbl_vars[m,2]
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
  }
  
  #AGGREGATE
  combined_data <- as.data.frame(fread(paste(output_directory,"global_agg_comb",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  tokens_data <- as.data.frame(fread(paste(output_directory,"global_agg_tokens",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  id_data <- as.data.frame(fread(paste(output_directory,"unique_ids_agg",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  
  for (a in 1:length(measures))
  {
    for (b in 1:nrow(percentiles))
    {
      #a <- 1
      #b <- 1
      
      create_individual_dictionary(combined_data,"agg",file_type_str,tokens_data,id_data,measures[a],percentiles[b,],output_db,output_directory,identifier)
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
    }
    rm2(b)
  }
  rm2(a,combined_data,tokens_data,id_data,file_type_str)
}
rm2(m)
rm2(output_db,identifier)

# SQLite Test
# 
# Text_Analysis_tables <- ListTables("Text_Analysis.s3db")
# 
# Text_Analysis_fields <- ListFields("Text_Analysis.s3db")
# write.csv(Text_Analysis_fields, file=paste(output_directory,"Text_Analysis_fields",".csv",sep=""), row.names=FALSE)
# 
# sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
# aaa <- runsql(sql_q,"Text_Analysis.s3db")
# rm2(aaa)
# 
# 
# agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
# ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
# runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
# rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)
# 
# 
# Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")


#==============================================================================;
cat("SECTION: INDIVIDUAL DICTIONARY - YEARLY", "\n")
#==============================================================================;

identifier <- "Fund_ID"

output_db <- paste(output_directory,"Text_Analysis.s3db",sep="")

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  if (m==1)
  {
    file_type_str <- readbl_vars[m,2]
  } else if (m==2)
  {
    #file_type_str <- readbl_vars[m,2]
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
  }
  
  #YEARLY
  combined_data <- as.data.frame(fread(paste(output_directory,"global_year_comb",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  tokens_data <- as.data.frame(fread(paste(output_directory,"global_year_tokens",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  id_data <- as.data.frame(fread(paste(output_directory,"unique_ids_year",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  
  for (a in 1:length(measures))
  {
    for (b in 1:nrow(percentiles))
    {
      #a <- 1
      #a <- 3
      #b <- 1
      
      create_individual_dictionary(combined_data,"year",file_type_str,tokens_data,id_data,measures[a],percentiles[b,],output_db,output_directory,identifier)
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
    }
    rm2(b)
  }
  rm2(a,combined_data,tokens_data,id_data,file_type_str)
  
}
rm2(m)
rm2(output_db,identifier)

# SQLite Test
# 
# Text_Analysis_tables <- ListTables("Text_Analysis.s3db")
# 
# Text_Analysis_fields <- ListFields("Text_Analysis.s3db")
# write.csv(Text_Analysis_fields, file=paste(output_directory,"Text_Analysis_fields",".csv",sep=""), row.names=FALSE)
# 
# sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
# aaa <- runsql(sql_q,"Text_Analysis.s3db")
# rm2(aaa)
# 
# 
# agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
# ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
# runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
# rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)
# 
# 
#Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")


###############################################################################
cat("SECTION: IMPORT STRATEGIES", "\n")
###############################################################################

sample_data_all_id_cols <- c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name")

sample_data_all <- read.csv(file=paste(output_directory,"text_clean_trim.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  #sample_data_all[[i]] <- trim(sample_data_all[[i]])
  sample_data_all[[i]] <- gsub("^\\s+|\\s+$", "", sample_data_all[[i]], perl=TRUE)
}
rm2(i)

for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=unknowns_strings,force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 
rm2(i)

sample_data_all <- sample_data_all[order(sample_data_all[,"Fund_ID"],sample_data_all[,"yr"]),]
row.names(sample_data_all) <- seq(nrow(sample_data_all))


###############################################################################
cat("SECTION: CREATE STRATEGY ID", "\n")
###############################################################################

sample_data_all_strategy_id0 <- data.frame(Strat_ID=NA,Strategy=unique(sample_data_all[,"Strategy"]),stringsAsFactors=FALSE)
sample_data_all_strategy_id <- sample_data_all_strategy_id0[!is.na(sample_data_all_strategy_id0[,"Strategy"]),]

sample_data_all_strategy_id[,"Strat_ID"] <- seq(1,nrow(sample_data_all_strategy_id))
row.names(sample_data_all_strategy_id) <- seq(nrow(sample_data_all_strategy_id))

rm(sample_data_all_strategy_id0)

sample_data_all_with_ids <- merge(sample_data_all,sample_data_all_strategy_id,
                                  by.x=c("Strategy"), by.y=c("Strategy"), 
                                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

rm2(sample_data_all_strategy_id)
#rm2(sample_data_all)

sample_data_all_with_ids <- sample_data_all_with_ids[,c("Strat_ID",
                                                        colnames(sample_data_all_with_ids)[!(colnames(sample_data_all_with_ids) %in% c("Strat_ID"))])]

sample_data_all_with_ids <- sample_data_all_with_ids[,c(sample_data_all_id_cols,
                                                        colnames(sample_data_all_with_ids)[!(colnames(sample_data_all_with_ids) %in% c(sample_data_all_id_cols))])]

sample_data_all_with_ids <- sample_data_all_with_ids[order(sample_data_all_with_ids[,"Fund_ID"],
                                                           sample_data_all_with_ids[,"yr"]),] 
row.names(sample_data_all_with_ids) <- seq(nrow(sample_data_all_with_ids))

rm2(sample_data_all)


###############################################################################
cat("COMPUTE SIMILARITY MEASURES", "\n")
###############################################################################

identifier <- "Fund_ID"

input_db <- paste(output_directory,"Text_Analysis.s3db",sep="")
output_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")

#input_db_tables <- ListTables(input_db)
#input_db_fields <- ListFields(input_db)

#temp_fields <- input_db_fields[input_db_fields[,1]=="year_word_grand_expand_norm_990pct_iois",]

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  if (m==1)
  {
    file_type_str <- readbl_vars[m,2]
  } else if (m==2)
  {
    file_type_str <- readbl_vars[m,2]
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
  }
  
  #AGGREGATE
  for (a in 1:length(measures))
  {
   for (b in 1:nrow(percentiles))
   {
     calculate_cosine_similarity("agg",file_type_str,measures[a],percentiles[b,],input_db,output_db,output_directory,"cosine_normalized",sample_data_all_with_ids,identifier)
     
     progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
  
   }
   rm(b)
  }
  rm(a)
  capture.output(gc(),file='NUL')
  
  #YEARLY
  for (a in 1:length(measures))
  {
    for (b in 1:nrow(percentiles))
    {
      #a <- 1
      #b <- 3
      
      calculate_cosine_similarity("year",file_type_str,measures[a],percentiles[b,],input_db,output_db,output_directory,"cosine_normalized",sample_data_all_with_ids,identifier)
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
      
    }
    rm(b)
  }
  rm(a)
  capture.output(gc(),file='NUL')
  
}
rm2(m)
rm2(input_db,output_db,identifier)

#Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")

#Similarity_Analysis_fields <- ListFields("Similarity_Analysis.s3db")
#write.csv(Similarity_Analysis_fields, file=paste(output_directory,"Similarity_Analysis_fields",".csv",sep=""), row.names=FALSE)

#sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
#aaa <- runsql(sql_q,"Text_Analysis.s3db")
#rm2(aaa)

#agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
#runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)


###############################################################################
cat("DONE", "\n")
###############################################################################

rm2(sample_data_all_with_ids,sample_data_all_id_cols)

rm2(temp_data_cols)
#rm2(files,file_list,files_cols,files_cols_count)
rm2(percentiles,percentile_vals,percentiles_cols,percentiles_cols_count)
rm2(readbl_vars,readbl_vars_cols,readbl_vars_cols_count)
#rm2(readbl_all_df,readbl_all_df_cols,readbl_all_df_cols_count)
#rm2(tokens_all_cols,tokens_all_cols_count)

rm2(measures)
