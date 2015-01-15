# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Compute_Read_Stats.R
# Version: 1.0
# Date:    01.05.2015
# Purpose: Compute Text Stats
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
Location <- 1

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
external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
                       "Hmisc","koRpus","mitools","pbapply","plyr","R.oo","reshape2","rJava","RWeka","RWekajars",
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

# 
# #Populate Percentiles table
# 
# #Note: If Confidence_Level is 0.900, 
# #          word_grand  -  this means that if the word is the 10% most common occuring or higher overall word, it is removed (90% removed/10% kept)
# #          word_unique -  this means that if the word is the 10% most common occuring or higher unique word, it is removed (90% removed/10% kept)
# #          id_unique -  this means that if a word is in 10% of the ids or higher, it is removed  (90% removed/10% kept)
# #Note: If Confidence_Level is 0.050, 
# #          word_grand  -  this means that if the word is the 95% most common occuring or higher overall word, it is removed (5% removed/95% kept)
# #          word_unique -  this means that if the word is the 95% most common occuring or higher unique word, it is removed (5% removed/95% kept)
# #          id_unique -  this means that if a word is in 95% of the ids or higher, it is removed  (5% removed/95% kept)
# # As confidence level increases, the words in the dictionary should decrease.
# # A smaller dicitonary means that that smilarity will be less because there are fewer possible words that could be common between two texts 
# # Thus, as confidence level increases, the smilarity scores will decrease
# 
# #Percentiles table
# percentiles_cols_count <- 8
# percentiles_cols <- temp_data_cols[1:percentiles_cols_count,]
# percentiles_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Level",stringsAsFactors=FALSE)
# percentiles_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Pct",stringsAsFactors=FALSE)
# percentiles_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Confidence_lbl",stringsAsFactors=FALSE)
# percentiles_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Level",stringsAsFactors=FALSE)
# percentiles_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Pct",stringsAsFactors=FALSE)
# percentiles_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Significance_lbl",stringsAsFactors=FALSE)
# percentiles_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_lbl",stringsAsFactors=FALSE)
# percentiles_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_DV",stringsAsFactors=FALSE)
# 
# #percentile_vals <- c(0.990,0.950,0.900)
# percentile_vals <- c(0.900,0.750,0.500,0.250,0.100,0.050)
# #percentile_vals <- c(0.900)
# 
# percentiles <- as.data.frame(matrix(NA, ncol=percentiles_cols_count, nrow=length(percentile_vals)),stringsAsFactors=FALSE)
# colnames(percentiles) <- percentiles_cols[,6]
# 
# percentiles[,"Confidence_Level"] <- format(as.double(percentile_vals), digits=3)
# percentiles[,"Confidence_Pct"] <- as.double(percentiles[,"Confidence_Level"])*100
# percentiles[,"Confidence_lbl"] <- formatC(percentiles[,"Confidence_Pct"],format="f", digits=1,width=4,  flag="0")
# percentiles[,"Significance_Level"] <- 1-as.double(percentiles[,"Confidence_Level"])
# percentiles[,"Significance_Pct"] <- as.double(percentiles[,"Significance_Level"])*100
# percentiles[,"Significance_lbl"] <- formatC(percentiles[,"Significance_Pct"], format="f",digits=1, width=5,  flag="0")
# percentiles[,"Confidence_lbl"] <-  paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Confidence_lbl"]),"pct",sep="")
# percentiles[,"Significance_lbl"] <- paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Significance_lbl"]),"pct",sep="")
# percentiles[,"Column_lbl"] <- paste("Word_Cutoff_",percentiles[,"Confidence_lbl"],sep="")
# percentiles[,"Column_DV"] <- paste("Word_DV_",percentiles[,"Confidence_lbl"],sep="")
# percentiles <- format_function(percentiles,percentiles_cols)
# percentiles <- percentiles[order(percentiles[,"Confidence_Level"]),]

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

# #measures <- c("word_grand","word_unique","id_unique")
# measures <- c("id_unique")
# 
# keep_one_letter_words <- c("I")
# keep_one_letter_ratings <- c("A","B","C","D","P")
# keep_one_letter_tokens <- sort(c(keep_one_letter_words,keep_one_letter_ratings))
# 
# rm2(keep_one_letter_words,keep_one_letter_ratings)
# 
# keep_two_letter_words <- c("AM","AN","AS","AT","BE","BY","DO","EG","EX","HA","ID","IE","IF","IN","IS",
#                            "IT","MY","NO","OF","ON","OR","QA","RD","SO","SP","TM","TO","TV","UM","UN",
#                            "UP","US","VP","WE")
# keep_state_abbreviations <- c("AK","AL","AR","AZ","CA","CF","CL","CO","CT","DC","DE","DL","FL","GA","HA",
#                               "HI","IA","ID","IL","IN","KA","KS","KY","LA","MA","MC","MD","ME","MI","MN",
#                               "MO","MS","MT","NB","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR",
#                               "PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WN","WS","WV","WY")
# keep_two_letter_ratings <- c("AA","BA","BB","CA","CC")
# keep_two_letter_tokens <- sort(c(keep_two_letter_words,keep_state_abbreviations,keep_two_letter_ratings))
# 
# rm2(keep_two_letter_words,keep_state_abbreviations,keep_two_letter_ratings)


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

# sample_data_all <- ddply(.data=sample_data_all, .variables=c("Fund_ID"), .fun = function(x){
#   
#   return(data.frame(Overall_ID=NA,Local_ID=seq(1,nrow(x)),x,stringsAsFactors=FALSE))
#   
# }, .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

#sample_data_all <- data.frame(Overall_ID=seq(1,nrow(sample_data_all)),sample_data_all,stringsAsFactors=FALSE)
#sample_data_all[,"Overall_ID"] <- paste("", formatC(sample_data_all[,"Overall_ID"], width=6, format="d", flag="0"), sep="")
#sample_data_all[,"Overall_ID"] <- paste("", formatC(seq(1,nrow(sample_data_all)), width=6, format="d", flag="0"), sep="")

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
cat("SECTION: COMPUTE READABILITY STATISTICS", "\n")
###############################################################################

Dale.Chall_word_list <- read.csv(file=paste(input_directory,"DaleChall_word_list.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(Dale.Chall_word_list,class)=="character"))
{
  Dale.Chall_word_list[[i]] <- trim(Dale.Chall_word_list[[i]])
}
rm2(i)

for (i in 1:ncol(Dale.Chall_word_list))
{
  Dale.Chall_word_list[,i] <- unknownToNA(Dale.Chall_word_list[,i], unknown=unknowns_strings,force=TRUE)
  Dale.Chall_word_list[,i] <- ifelse(is.na(Dale.Chall_word_list[,i]),NA, Dale.Chall_word_list[,i])
} 
rm2(i)

tagged_text_desc_stats <- c("lines","chars.no.space","letters.only","digits","normalized.space")
hyph_text_en_desc_stats <- c("num.syll")
readability_stats <- c("ARI", "Coleman.Liau","Flesch.Kincaid", "FOG","SMOG")
readability_desc_stats <- c("sentences","words","all.chars","punct","conjunctions","prepositions","pronouns",
                            "foreign","FOG.hard.words","TTR","sntc.per.word","avg.sentc.length","avg.word.length",
                            "avg.syll.word","sntc.per100","syll.per100","lett.per100")
readability_all_stats <- c(tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats)

#token_stats <- c("token","desc","stop","stem")
token_stats <- c("token","desc")

#sample_data_all_backup <- sample_data_all
#sample_data_all <- sample_data_all_backup
#sample_data_all <- sample_data_all[1:50,]

#sample_data_all_with_ids_trim <- unique(sample_data_all_with_ids[1:500,c("Strat_ID","Strategy")])
sample_data_all_with_ids_trim <- unique(sample_data_all_with_ids[,c("Strat_ID","Strategy")])

for (l in 1:nrow(readbl_vars))
{
  #l <- 1
  
  readability_all_stats_temp <- paste(readability_all_stats, readbl_vars[l,2], sep="")
  readability_all_stats_temp <- gsub(pattern="\\.", replacement="_", x=readability_all_stats_temp)
  
  #sample_data_all <- as.data.table(sample_data_all)
  #temp4 <- data.frame(stri_split_fixed(sample_data_all[[which(colnames(sample_data_all) %in% readbl_vars[l,1])]], "\n", simplify = TRUE),stringsAsFactors=FALSE)
  #tagged_text <- adply(.data=temp4, .margins=1, .fun = function(x){  
  #  return(treetag(x[,1], treetagger="manual",lang="en", TT.options=list(path="C:/TreeTagger", preset="en"),debug=FALSE,format="obj"))
  #},.progress = "text", .expand = TRUE)
  
  #sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)
  #sample_cell_temp <- sample_data_all[1,readbl_vars[l,1]]
  #temp_text <- unlist(strsplit(sample_cell_temp, "\n"))
  #tagged_text <- treetag(temp_text, treetagger="manual",lang="en", TT.options=list(path="C:/TreeTagger", preset="en"),debug=FALSE,format="obj")
  
  #tagged_text <- treetag(temp4[,1], treetagger="manual",lang="en", TT.options=list(path="C:/TreeTagger", preset="en"),debug=FALSE,format="obj")
  
  
  sample_results <- pbsapply(sample_data_all_with_ids_trim[,readbl_vars[l,1]],compute_readability_stats,
                             tagged_text_desc_measures=tagged_text_desc_stats,
                             hyph_text_en_desc_measures=hyph_text_en_desc_stats,
                             readability_measures=readability_stats,
                             readability_desc_measures=readability_desc_stats,
                             token_measures=token_stats,
                             dc_word_list=Dale.Chall_word_list,
                             stop_words=myStopwords_all,
                             #treetag_dir="C:/TreeTagger",
                             #treetag_dir="\\\\tsclient\\C\\TreeTagger",
                             #treetag_dir="\\tsclient\C\TreeTagger",
                             treetag_dir=treetag_directory,
                             debug=FALSE,
                             simplify=FALSE, USE.NAMES=FALSE)
  
  sample_read_stats <- pblapply(sample_results, "[[","readstats")
  sample_read_stats <- pblapply(seq_along(sample_read_stats), function(x) data.frame(ID=x,sample_read_stats[x],stringsAsFactors=FALSE))
  sample_read_stats_df <- rbindlist(l=sample_read_stats, use.names=TRUE, fill=FALSE)
  sample_read_stats_df <- as.data.frame(sample_read_stats_df,stringsAsFactors=FALSE) 

  colnames(sample_read_stats_df) <- c("Strat_ID",readability_all_stats_temp)
  
  rm2(sample_read_stats,readability_all_stats_temp)
  
  #sample_data_all_temp <- cbind(subset(sample_data_all,select=c("ID","Fund_ID","yr")),subset(sample_data_all,select=c(readbl_vars[l,1])))
  #sample_data_all_temp <- subset(sample_data_all,select=c("ID","yr","crsp_fundno"))
  
  sample_read_stats_df_merge <- merge(sample_data_all_with_ids_trim, sample_read_stats_df, 
                                      by.x=c("Strat_ID"), by.y=c("Strat_ID"), 
                                      all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  
  rm2(sample_read_stats_df)

  #sample_read_stats_df_merge <- subset(sample_read_stats_df_merge,select=-c(ID))
  #sample_read_stats_df_merge <- sample_read_stats_df_merge[,!(colnames(sample_read_stats_df_merge) %in% c("Overall_ID","Local_ID"))]
  
  write.csv(sample_read_stats_df_merge, file=paste(output_directory,readbl_vars[l,3],".csv",sep=""),row.names=FALSE)
  
  rm2(sample_read_stats_df_merge)
  
  sample_tokens <- pblapply(sample_results, "[[","tokens") 
  sample_tokens <- pblapply(seq_along(sample_tokens), function(x) data.frame(ID=x,sample_tokens[x],stringsAsFactors=FALSE))
  sample_tokens_df <- rbindlist(l=sample_tokens, use.names=TRUE, fill=FALSE)
  sample_tokens_df <- as.data.frame(sample_tokens_df,stringsAsFactors=FALSE) 
  
  colnames(sample_tokens_df) <- c("Strat_ID",token_stats)
  
  rm2(sample_tokens)
  
  sample_tokens_df_merge <- merge(sample_data_all_with_ids_trim, sample_tokens_df, 
                                  by.x=c("Strat_ID") , by.y=c("Strat_ID"), 
                                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
  
  rm2(sample_tokens_df)
  
  #sample_tokens_df_merge <- subset(sample_tokens_df_merge,select=-c(ID))
  #sample_tokens_df_merge <- sample_tokens_df_merge[,!(colnames(sample_tokens_df_merge) %in% c("Overall_ID","Local_ID"))]
  
  write.csv(sample_tokens_df_merge, file=paste(output_directory,readbl_vars[l,4],".csv",sep=""),row.names=FALSE)
  
  rm2(sample_tokens_df_merge)
  
  rm2(sample_results)
  
}
rm2(l)

rm2(sample_data_all_with_ids_trim)
rm2(tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats,readability_all_stats,token_stats)
rm2(Dale.Chall_word_list)

###############################################################################
cat("SECTION: TEMP - EXPAND YEARS", "\n")
###############################################################################

l <- 1

# sample_data_all <- read.csv(file=paste(output_directory,"text_clean_trim.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# for(i in which(sapply(sample_data_all,class)=="character"))
# {
#   sample_data_all[[i]] <- trim(sample_data_all[[i]])
# }
# rm2(i)
# 
# for (i in 1:ncol(sample_data_all))
# {
#   sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=unknowns_strings,force=TRUE)
#   sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
# } 
# rm2(i)

sample_data_all_with_ids_trim <- unique(sample_data_all_with_ids[sample_data_all_with_ids[,"yr"]>=2007,c("Fund_ID","yr","Strat_ID")])

sample_data_all_with_ids_trim <- sample_data_all_with_ids_trim[order(sample_data_all_with_ids_trim[,"Fund_ID"],
                                                                     sample_data_all_with_ids_trim[,"yr"]),]
row.names(sample_data_all_with_ids_trim) <- seq(nrow(sample_data_all_with_ids_trim))

rm2(sample_data_all_with_ids)

sample_read_stats_df_merge <- read.csv(file=paste(output_directory,readbl_vars[l,3],".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_read_stats_df_merge,class)=="character"))
{
  sample_read_stats_df_merge[[i]] <- trim(sample_read_stats_df_merge[[i]])
}
rm2(i)

for (i in 1:ncol(sample_read_stats_df_merge))
{
  sample_read_stats_df_merge[,i] <- unknownToNA(sample_read_stats_df_merge[,i], unknown=unknowns_strings,force=TRUE)
  sample_read_stats_df_merge[,i] <- ifelse(is.na(sample_read_stats_df_merge[,i]),NA, sample_read_stats_df_merge[,i])
} 
rm2(i)

sample_tokens_df_merge <- read.csv(file=paste(output_directory,readbl_vars[l,4],".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_tokens_df_merge,class)=="character"))
{
  sample_tokens_df_merge[[i]] <- trim(sample_tokens_df_merge[[i]])
}
rm2(i)

for (i in 1:ncol(sample_tokens_df_merge))
{
  sample_tokens_df_merge[,i] <- unknownToNA(sample_tokens_df_merge[,i], unknown=unknowns_strings,force=TRUE)
  sample_tokens_df_merge[,i] <- ifelse(is.na(sample_tokens_df_merge[,i]),NA, sample_tokens_df_merge[,i])
} 
rm2(i)

# sample_read_stats_df_merge_full <- merge(subset(sample_data_all,select=-c(Fund_Name,Strategy)),
#                                          sample_read_stats_df_merge, 
#                                          by.x=c("Fund_ID") , by.y=c("Fund_ID"), 
#                                          all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_read_stats_df_merge_full <- merge(sample_data_all_with_ids_trim[,c("Fund_ID","yr","Strat_ID")],
                                         sample_read_stats_df_merge, 
                                         by.x=c("Strat_ID") , by.y=c("Strat_ID"), 
                                         all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

write.csv(sample_read_stats_df_merge_full, file=paste(output_directory,readbl_vars[l,3],"_full",".csv",sep=""),row.names=FALSE)

rm2(sample_read_stats_df_merge_full,sample_read_stats_df_merge)


# sample_tokens_df_merge_full <- merge(subset(sample_data_all,select=-c(Fund_Name,Strategy)), 
#                                      sample_tokens_df_merge, 
#                                      by.x=c("Fund_ID") , by.y=c("Fund_ID"), 
#                                      all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_tokens_df_merge_full <- merge(sample_data_all_with_ids_trim[,c("Fund_ID","yr","Strat_ID")],
                                     sample_tokens_df_merge, 
                                     by.x=c("Strat_ID") , by.y=c("Strat_ID"), 
                                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

write.csv(sample_tokens_df_merge_full, file=paste(output_directory,readbl_vars[l,4],"_full",".csv",sep=""),row.names=FALSE)

rm2(sample_tokens_df_merge_full,sample_tokens_df_merge)

rm2(sample_data_all_with_ids_trim,sample_data_all_id_cols,l)
rm2(myStopwords_all)

rm2(temp_data_cols)
#rm2(files,file_list,files_cols,files_cols_count)
#rm2(percentiles,percentile_vals,percentiles_cols,percentiles_cols_count)
rm2(readbl_vars,readbl_vars_cols,readbl_vars_cols_count)
#rm2(readbl_all_df,readbl_all_df_cols,readbl_all_df_cols_count)
#rm2(tokens_all_cols,tokens_all_cols_count)
