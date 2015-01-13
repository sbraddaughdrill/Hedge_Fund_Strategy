# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Clean_Funds.R
# Version: 1.0
# Date:    01.05.2015
# Purpose: Find Bad/Good Fumds
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

external_packages <- c("colbycol","compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
                       "Hmisc","koRpus","limma","LaF","mitools","pbapply","plyr","R.oo","reshape","reshape2","rJava","RWeka","RWekajars",
                       "splitstackshape","sqldf","stringi","stringr","tcltk","tm")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
cat("SECTION: IMPORT STRATEGIES", "\n")
###############################################################################

sample_data_all <- read.csv(file=paste(output_directory,"text_clean.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
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

sample_data_all_id_cols <- c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name")


###############################################################################
cat("SECTION: IMPORT FUND DETAILS", "\n")
###############################################################################

#Check to see if final folder exists.  If not, create it.
final_folder_expand3_path <- paste(normalizePath("F:/Import_Data/Data/Eurekahedge",winslash="\\", mustWork=TRUE), "Final_Expand3", sep = "//", collapse = "//")  

fund_details_id_cols <- c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name")
fund_details_location_cols <- c("Minimum_Investment_Currency_combcol","Subsequent_Investment_Currency_combcol","Geography_combcol","Domicile","Currency_combcol")

#fund_details <- read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Fund_Details_part3.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[,c(fund_details_id_cols,fund_details_location_cols)]

#fund_details <- laf_open_csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Fund_Details_part3.csv",sep="\\"),just.read=c(1,2),header=TRUE,sep=",",stringsAsFactors=FALSE)

#fund_details_col_names_all <- t(read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Fund_Details_part3.csv",sep="\\"),header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1))
#fund_details_col_names_pos <- which(fund_details_col_names_all[,1] %in% c(fund_details_id_cols,fund_details_location_cols))
#fund_details <- cbc.read.table2(file=paste(final_folder_expand3_path,"EurekahedgeHF_Fund_Details_part3.csv",sep="\\"),just.read=fund_details_col_names_pos[1:4],header=TRUE,sep=",")
#fund_details <- as.data.frame(fund_details,stringsAsFactors=FALSE)
#rm2(fund_details_col_names_all,fund_details_col_names_pos)

fund_details <- read.columns(file=paste(final_folder_expand3_path,"EurekahedgeHF_Fund_Details_part3.csv",sep="\\"),required.col=c(fund_details_id_cols,fund_details_location_cols),sep=",",na.strings="NA",stringsAsFactors=FALSE)

for(i in which(sapply(fund_details,class)=="character"))
{
  #fund_details[[i]] <- trim(fund_details[[i]])
  fund_details[[i]] <- gsub("^\\s+|\\s+$", "", fund_details[[i]], perl=TRUE)
}
rm2(i)

for (i in 1:ncol(fund_details))
{
  fund_details[,i] <- unknownToNA(fund_details[,i], unknown=unknowns_strings,force=TRUE)
  fund_details[,i] <- ifelse(is.na(fund_details[,i]),NA, fund_details[,i])
} 
rm2(i)

fund_details_trim <- unique(fund_details[,(colnames(fund_details) %in% c(fund_details_id_cols,fund_details_location_cols))])

rm2(fund_details)


###############################################################################
cat("SECTION: IMPORT RETURNS", "\n")
###############################################################################

#Check to see if final folder exists.  If not, create it.
final_folder_expand3_path <- paste(normalizePath("F:/Import_Data/Data/Eurekahedge",winslash="\\", mustWork=TRUE), "Final_Expand3", sep = "//", collapse = "//")  

returns_id_cols <- c("Fund_ID","yr","month","pull_trim")
returns_data_cols <- c("Monthly_Ret","AUM")

#returns <- read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

#returns <- laf_open_csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),just.read=c(1,2),header=TRUE,sep=",",stringsAsFactors=FALSE)

#returns_col_names_all <- t(read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1))
#returns_col_names_pos <- which(returns_col_names_all[,1] %in% c(returns_id_cols,returns_data_cols))
#returns <- cbc.read.table2(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),just.read=returns_col_names_pos[1:4],header=TRUE,sep=",")
#returns <- as.data.frame(returns,stringsAsFactors=FALSE)
#rm2(returns_col_names_all,returns_col_names_pos)

returns <- read.columns(file=paste(final_folder_expand3_path,"EurekahedgeHF_NAV_AUM_Ret.csv",sep="\\"),required.col=c(returns_id_cols,returns_data_cols),sep=",",na.strings="NA",stringsAsFactors=FALSE)

for(i in which(sapply(returns,class)=="character"))
{
  #returns[[i]] <- trim(returns[[i]])
  returns[[i]] <- gsub("^\\s+|\\s+$", "", returns[[i]], perl=TRUE)
}
rm2(i)

for (i in 1:ncol(returns))
{
  returns[,i] <- unknownToNA(returns[,i], unknown=unknowns_strings,force=TRUE)
  returns[,i] <- ifelse(is.na(returns[,i]),NA, returns[,i])
} 
rm2(i)

returns_trim <- unique(returns[,(colnames(returns) %in% c(returns_id_cols,returns_data_cols))])

rm2(returns)


###############################################################################
cat("SECTION: IMPORT MANAGER DETAILS", "\n")
###############################################################################

#Check to see if final folder exists.  If not, create it.
final_folder_expand3_path <- paste(normalizePath("F:/Import_Data/Data/Eurekahedge",winslash="\\", mustWork=TRUE), "Final_Expand3", sep = "//", collapse = "//")  

other_details_id_cols <- c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name")
other_details_manager_cols <- c("Advisory_Company","Management_Company","Country_combcol",
                                "Principal_Prime_Broker_combcol","Secondary_Prime_Broker","Synthetic_Prime_Broker",
                                "Legal_Advisor_Offshore","Legal_Advisor_Onshore","Legal_Advisor")

#other_details <- read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Other_part3.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

#other_details <- laf_open_csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Other_part3.csv",sep="\\"),just.read=c(1,2),header=TRUE,sep=",",stringsAsFactors=FALSE)

#other_details_col_names_all <- t(read.csv(file=paste(final_folder_expand3_path,"EurekahedgeHF_Other_part3.csv",sep="\\"),header=FALSE,na.strings="NA",stringsAsFactors=FALSE,nrows=1))
#other_details_col_names_pos <- which(other_details_col_names_all[,1] %in% c(other_details_id_cols,other_details_manager_cols))
#other_details <- cbc.read.table2(file=paste(final_folder_expand3_path,"EurekahedgeHF_Other_part3.csv",sep="\\"),just.read=other_details_col_names_pos[1:4],header=TRUE,sep=",")
#other_details <- as.data.frame(other_details,stringsAsFactors=FALSE)
#rm2(other_details_col_names_all,other_details_col_names_pos)

other_details <- read.columns(file=paste(final_folder_expand3_path,"EurekahedgeHF_Other_part3.csv",sep="\\"),required.col=c(other_details_id_cols,other_details_manager_cols),sep=",",na.strings="NA",stringsAsFactors=FALSE)

for(i in which(sapply(other_details,class)=="character"))
{
  #other_details[[i]] <- trim(other_details[[i]])
  other_details[[i]] <- gsub("^\\s+|\\s+$", "", other_details[[i]], perl=TRUE)
}
rm2(i)

for (i in 1:ncol(other_details))
{
  other_details[,i] <- unknownToNA(other_details[,i], unknown=unknowns_strings,force=TRUE)
  other_details[,i] <- ifelse(is.na(other_details[,i]),NA, other_details[,i])
} 
rm2(i)

other_details_trim <- unique(other_details[,(colnames(other_details) %in% c(other_details_id_cols,other_details_manager_cols))])

rm2(other_details)


###############################################################################
cat("SECTION: CREATE CURRENCY HASH TABLE", "\n")
###############################################################################

currency_NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT APPILCABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                         "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","UPON REQUEST",
                         "SUBJECT TO MANAGER'S DISCRETION")

currency_Bad <- c("ANY FREE FLOATING CURRENCY","UNIT")

currency0 <- unique(c(fund_details_trim[,"Minimum_Investment_Currency_combcol"],fund_details_trim[,"Subsequent_Investment_Currency_combcol"],fund_details_trim[,"Currency_combcol"]))

currency1 <- currency0[!is.na(currency0)]

currency2 <- currency1[!(toupper(currency1) %in% currency_NA_Phrases)]

currency3 <- currency2[!(toupper(currency2) %in% currency_Bad)]

currency4 <- sort(currency3)

currency_cols <- data.frame(currency=currency4,nchar=NA,currency_alnum=NA,col_name=NA,USD_flag=NA,stringsAsFactors=FALSE)

currency_cols[,"nchar"] <- nchar(currency_cols[,"currency"])
currency_cols <- currency_cols[order(-currency_cols[,"nchar"],currency_cols[,"currency"]),] 
row.names(currency_cols) <- seq(nrow(currency_cols))

currency_cols[,"currency_alnum"] <- currency_cols[,"currency"]
currency_cols[,"currency_alnum"] <- gsub(" {2,}", " ", currency_cols[,"currency_alnum"], perl=TRUE)
currency_cols[,"currency_alnum"] <- gsub("[^[:alnum:] ]", " ", currency_cols[,"currency_alnum"], perl=TRUE)
currency_cols[,"currency_alnum"] <- gsub(" {2,}", " ", currency_cols[,"currency_alnum"], perl=TRUE)
currency_cols[,"currency_alnum"] <- gsub("^\\s+|\\s+$", "", currency_cols[,"currency_alnum"], perl=TRUE)

currency_cols[,"col_name"] <- currency_cols[,"currency_alnum"]
currency_cols[,"col_name"] <- gsub(" ", "_", currency_cols[,"col_name"], perl=TRUE)
currency_cols[,"col_name"] <- paste("flag",currency_cols[,"col_name"],sep="_")

currency_cols[,"USD_flag"] <- ifelse(grepl("USD",currency_cols[,"currency"]),1,0)

#currency_cols_USD <- currency_cols[currency_cols[,"USD_flag"]==1,]
#currency_cols_nonUSD <- currency_cols[currency_cols[,"USD_flag"]==0,]

rm2(currency_NA_Phrases,currency_Bad)
rm2(currency0,currency1,currency2,currency3,currency4)


###############################################################################
cat("SECTION: CREATE SHARE CLASS HASH TABLE", "\n")
###############################################################################

Fund_Names0 <- data.frame(unique(fund_details_trim[,c("Fund_ID","Fund_Name")]),stringsAsFactors=FALSE)

Fund_Names1 <- Fund_Names0[!(is.na(Fund_Names0[,"Fund_ID"]) | is.na(Fund_Names0[,"Fund_Name"])),]

Fund_Names1 <- Fund_Names1[order(Fund_Names1[,"Fund_ID"],Fund_Names1[,"Fund_Name"]),] 
row.names(Fund_Names1) <- seq(nrow(Fund_Names1))

# Find Series, Classes, Shares, etc.

class_Phrases <- c("Series","Class","Share","Institutional")

Fund_Names_classes <- data.frame(Fund_Names1,
                                 matrix(NA, ncol=length(class_Phrases), nrow=1, dimnames=list(c(), paste(class_Phrases,"Phrase",sep="_"))),
                                 stringsAsFactors=FALSE)

Fund_Names_classes[,c("Fund_Name")] <- paste(Fund_Names_classes[,c("Fund_Name")]," ",sep="")

for (k in 1:length(class_Phrases)) {
  temp_matches <- regmatches(Fund_Names_classes[,"Fund_Name"], gregexpr(paste(class_Phrases[k]," .*$",sep=""), Fund_Names_classes[,"Fund_Name"], perl = TRUE), invert = FALSE)
  Fund_Names_classes[,paste(class_Phrases[k],"Phrase",sep="_")] <- ldply(.data=temp_matches, .fun = function(x){if (length(x)==0) {return(NA)} else {return(x)}})
  rm(temp_matches)
}
rm(k)

classes0 <- unique(stack(Fund_Names_classes[,paste(class_Phrases,"Phrase",sep="_")]))
classes1 <- sort(unique(classes0[,"values"]))

#classes_cols <- data.frame(classes=classes1,nchar=NA,classes_alnum=NA,col_name=NA,USD_flag=NA,stringsAsFactors=FALSE)
classes_cols <- data.frame(classes=classes1,nchar=NA,classes_alnum=NA,classes_alnum_no_currency=NA,col_name=NA,stringsAsFactors=FALSE)

classes_cols[,"nchar"] <- nchar(classes_cols[,"classes"])
classes_cols <- classes_cols[order(-classes_cols[,"nchar"],classes_cols[,"classes"]),] 
row.names(classes_cols) <- seq(nrow(classes_cols))

classes_cols[,"classes_alnum"] <- classes_cols[,"classes"]
classes_cols[,"classes_alnum"] <- gsub(" {2,}", " ", classes_cols[,"classes_alnum"], perl=TRUE)
classes_cols[,"classes_alnum"] <- gsub("[^[:alnum:] ]", " ", classes_cols[,"classes_alnum"], perl=TRUE)
classes_cols[,"classes_alnum"] <- gsub(" {2,}", " ", classes_cols[,"classes_alnum"], perl=TRUE)
classes_cols[,"classes_alnum"] <- gsub("^\\s+|\\s+$", "", classes_cols[,"classes_alnum"], perl=TRUE)

classes_cols[,"classes_alnum_no_currency"] <- classes_cols[,"classes_alnum"]

classes_cols <- data.table(classes_cols)
setkeyv(classes_cols,NULL)

# Add space to end
set(classes_cols, i=NULL, j=which(colnames(classes_cols)==c("classes_alnum_no_currency")), value=paste(" ",classes_cols[[which(colnames(classes_cols)==c("classes_alnum_no_currency"))]]," ",sep=""))

# Remove currency
for(i in 1:nrow(currency_cols))
{
  set(classes_cols, i=NULL, j=which(colnames(classes_cols)==c("classes_alnum_no_currency")), value=gsub(paste(" ",currency_cols[i,"currency_alnum"]," ",sep=""), " ", classes_cols[[which(colnames(classes_cols)==c("classes_alnum_no_currency"))]], perl=TRUE))
  
}
rm2(i)

for (a in 1:5)
{
  #a <- 1
  set(classes_cols, i=NULL, j=which(colnames(classes_cols)==c("classes_alnum_no_currency")), value=gsub(" {2,}", " ", classes_cols[[which(colnames(classes_cols)==c("classes_alnum_no_currency"))]], perl=TRUE))
}
rm2(a)

classes_cols <- as.data.frame(classes_cols,stringsAsFactors=FALSE)

for(i in which(sapply(classes_cols,class)=="character"))
{
  #classes_cols[[i]] <- trim(classes_cols0[[i]])
  classes_cols[[i]] <- gsub("^\\s+|\\s+$", "", classes_cols[[i]], perl=TRUE)
}
rm2(i)

classes_cols_trim <- unique(classes_cols[,c("classes_alnum_no_currency","col_name")])

classes_cols_trim[,"col_name"] <- classes_cols_trim[,"classes_alnum_no_currency"]
classes_cols_trim[,"col_name"] <- gsub(" ", "_", classes_cols_trim[,"col_name"], perl=TRUE)
classes_cols_trim[,"col_name"] <- paste("flag",classes_cols_trim[,"col_name"],sep="_")

#classes_cols_trim[,"USD_flag"] <- ifelse(grepl("USD",classes_cols_trim[,"classes"]),1,0)

rm2(Fund_Names0,Fund_Names1)
rm2(class_Phrases,Fund_Names_classes)
rm2(classes0,classes1)

# Compare Words

# Fund_Names2 <- alply(.data=Fund_Names1, .margins=1, .fun = function(x){
#   
#   # x <- Fund_Names1[1,]
# 
#   x_out <- data.frame(Fund_ID=x[,"Fund_ID"],Fund_Name=x[,"Fund_Name"],t(unlist(strsplit(x[,"Fund_Name"], " ", fixed=FALSE))),stringsAsFactors=FALSE)
#   colnames(x_out) <- c("Fund_ID","Fund_Name",paste("W",seq(1,ncol(x_out)-2),sep=""))
#   return(x_out)
#   
# }, .expand = FALSE, .progress = "text")
# 
# Fund_Names3 <- rbindlist(Fund_Names2, use.names=TRUE, fill=TRUE)
# Fund_Names3 <- as.data.frame(Fund_Names3,stringsAsFactors=FALSE)
# 
# rm2(Fund_Names2)
# 
# #Fund_Name_word_test_count <- 3
# #Fund_Name_word_test_count <- 4
# #Fund_Name_word_test_count <- 5
# Fund_Name_word_test_count <- 6
# 
# Fund_Names_word_test0 <- Fund_Names3[,c("Fund_ID",paste("W",seq(1,Fund_Name_word_test_count),sep=""))]
# row.names(Fund_Names_word_test0) <- seq(nrow(Fund_Names_word_test0))
# 
# Fund_Names_word_test <- ddply(.data=Fund_Names_word_test0, .variables=c(paste("W",seq(1,Fund_Name_word_test_count),sep="")), .fun = function(x){return(data.frame(x,freq=nrow(x),stringsAsFactors=FALSE))},.progress="none")
# 
# Fund_Names_word_test_multiple_ids <- unique(Fund_Names_word_test[Fund_Names_word_test[,"freq"]>1,c("Fund_ID")])
# 
# Fund_Names_word_test_multiple <- unique(Fund_Names3[Fund_Names3[,"Fund_ID"] %in% Fund_Names_word_test_multiple_ids,c("Fund_ID","Fund_Name")])
# row.names(Fund_Names_word_test_multiple) <- seq(nrow(Fund_Names_word_test_multiple))
# 
# rm2(Fund_Name_word_test_count,Fund_Names_word_test0,Fund_Names_word_test,Fund_Names_word_test_multiple_ids,Fund_Names_word_test_multiple)
# rm2(Fund_Names3)


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


###############################################################################
cat("SECTION: REMOVE FUNDS WITH MISSING STRATEGIES", "\n")
###############################################################################

sample_data_all_strategy_nonNA_drop_ids <- unique(sample_data_all_with_ids[is.na(sample_data_all_with_ids[,"Strategy"]),c("Fund_ID")])
sample_data_all_strategy_nonNA_drop <- sample_data_all_with_ids[sample_data_all_with_ids[,"Fund_ID"] %in% sample_data_all_strategy_nonNA_drop_ids,]
row.names(sample_data_all_strategy_nonNA_drop) <- seq(nrow(sample_data_all_strategy_nonNA_drop))

sample_data_all_strategy_nonNA_keep_ids <- unique(sample_data_all_with_ids[!(sample_data_all_with_ids[,"Fund_ID"] %in% sample_data_all_strategy_nonNA_drop_ids),c("Fund_ID")])
sample_data_all_strategy_nonNA_keep <- sample_data_all_with_ids[sample_data_all_with_ids[,"Fund_ID"] %in% sample_data_all_strategy_nonNA_keep_ids,]
row.names(sample_data_all_strategy_nonNA_keep) <- seq(nrow(sample_data_all_strategy_nonNA_keep))

rm2(sample_data_all_strategy_nonNA_keep_ids,sample_data_all_strategy_nonNA_drop_ids)
rm2(sample_data_all_with_ids)


###############################################################################
cat("SECTION: ADD FUND DETAILS", "\n")
###############################################################################

sample_data_all_details_merge1 <- merge(sample_data_all_strategy_nonNA_keep,fund_details_trim,
                                        by.x=c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name"), 
                                        by.y=c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name"), 
                                        all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

rm2(sample_data_all_strategy_nonNA_keep,sample_data_all_strategy_nonNA_drop)
#rm2(fund_details_trim)

sample_data_all_details_merge1 <- sample_data_all_details_merge1[order(sample_data_all_details_merge1[,"Fund_ID"],
                                                                       sample_data_all_details_merge1[,"yr"]),] 
row.names(sample_data_all_details_merge1) <- seq(nrow(sample_data_all_details_merge1))


###############################################################################
cat("SECTION: REMOVE NON-USD FUNDS (FROM CURRENCY LISTED)", "\n")
###############################################################################

sample_data_all_remove_nonUS_flags <- data.frame(sample_data_all_details_merge1,nonUSD_flag=NA,stringsAsFactors=FALSE)
#sample_data_all_remove_nonUS_flags[,"nonUSD_flag"] <- ifelse(grepl("USD",sample_data_all_remove_nonUS_flags[,"Currency_combcol"]),0,1)
#sample_data_all_remove_nonUS_flags[,"nonUSD_flag"] <- ifelse(sample_data_all_remove_nonUS_flags[,"Currency_combcol"] %in% currency[grepl("USD",currency)],0,1)
sample_data_all_remove_nonUS_flags[,"nonUSD_flag"] <- ifelse(sample_data_all_remove_nonUS_flags[,"Currency_combcol"] %in% currency_cols[currency_cols[,"USD_flag"]==1,"currency"],0,1)

sample_data_all_remove_nonUS_flags[,"nonUSD_flag"] <- ifelse(is.na(sample_data_all_remove_nonUS_flags[,"Currency_combcol"]),NA,sample_data_all_remove_nonUS_flags[,"nonUSD_flag"])

rm2(sample_data_all_details_merge1)

# IF Fund_ID is ever USD currency
#sample_data_all_remove_nonUS_flags_keep_ids <- unique(sample_data_all_remove_nonUS_flags[sample_data_all_remove_nonUS_flags[,"nonUSD_flag"]==0,c("Fund_ID")])

# IF Fund_ID is always USD currency
sample_data_all_remove_nonUS_flags_keep_ids <- ddply(.data=sample_data_all_remove_nonUS_flags, .variables=c("Fund_ID"), .fun = function(x){
  if (length(which(x[,"nonUSD_flag"]!=0 | is.na(x[,"nonUSD_flag"])))==0) {return(data.frame(keep_flag=1,stringsAsFactors=FALSE))} else {return(data.frame(keep_flag=0,stringsAsFactors=FALSE))}
})
sample_data_all_remove_nonUS_flags_keep_ids <- sample_data_all_remove_nonUS_flags_keep_ids[sample_data_all_remove_nonUS_flags_keep_ids[,"keep_flag"]==1,!(colnames(sample_data_all_remove_nonUS_flags_keep_ids) %in% c("keep_flag"))]

sample_data_all_remove_nonUS_flags_keep <- sample_data_all_remove_nonUS_flags[sample_data_all_remove_nonUS_flags[,"Fund_ID"] %in% sample_data_all_remove_nonUS_flags_keep_ids,
                                                                              !(colnames(sample_data_all_remove_nonUS_flags) %in% c("nonUSD_flag"))]

sample_data_all_remove_nonUS_flags_drop_ids <- unique(sample_data_all_remove_nonUS_flags[!(sample_data_all_remove_nonUS_flags[,"Fund_ID"] %in% sample_data_all_remove_nonUS_flags_keep_ids),c("Fund_ID")])

sample_data_all_remove_nonUS_flags_drop <- sample_data_all_remove_nonUS_flags[sample_data_all_remove_nonUS_flags[,"Fund_ID"] %in% sample_data_all_remove_nonUS_flags_drop_ids,
                                                                              !(colnames(sample_data_all_remove_nonUS_flags) %in% c("nonUSD_flag"))]

rm2(sample_data_all_remove_nonUS_flags_keep_ids,sample_data_all_remove_nonUS_flags_drop_ids)
rm2(sample_data_all_remove_nonUS_flags)


###############################################################################
cat("SECTION: FIND FUNDS WITH MULTIPLE CURRENCIES", "\n")
###############################################################################

sample_data_all_strategy_currency <- unique(sample_data_all_remove_nonUS_flags_keep[,c("Fund_ID","Fund_Name")])
sample_data_all_strategy_currency <- sample_data_all_strategy_currency[order(sample_data_all_strategy_currency[,"Fund_ID"],
                                                                             sample_data_all_strategy_currency[,"Fund_Name"]),] 
row.names(sample_data_all_strategy_currency) <- seq(nrow(sample_data_all_strategy_currency))


sample_data_all_strategy_currency <- data.frame(sample_data_all_strategy_currency,Fund_Name_temp=NA,
                                                matrix(NA, ncol=nrow(currency_cols[currency_cols[,"USD_flag"]==1,]), nrow=nrow(sample_data_all_strategy_currency), dimnames=list(c(),currency_cols[currency_cols[,"USD_flag"]==1,"col_name"])),
                                                matrix(NA, ncol=nrow(currency_cols[currency_cols[,"USD_flag"]==0,]), nrow=nrow(sample_data_all_strategy_currency), dimnames=list(c(),currency_cols[currency_cols[,"USD_flag"]==0,"col_name"])),
                                                cum_flag_USD=NA,cum_flag_nonUSD=NA,cum_flag_overall=NA,stringsAsFactors=FALSE)

sample_data_all_strategy_currency[,"Fund_Name_temp"] <- sample_data_all_strategy_currency[,"Fund_Name"]

sample_data_all_strategy_currency <- data.table(sample_data_all_strategy_currency)
setkeyv(sample_data_all_strategy_currency,NULL)

#sample_data_all_strategy_currency <- sample_data_all_strategy_currency[, Fund_Name_temp:=Fund_Name,by=NULL]
#sample_data_all_strategy_currency <- sample_data_all_strategy_currency[, USD_flag:=NA_integer_,by=NULL]

for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

for (a in 1:5)
{
  #a <- 1
  #set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub("[^[:alpha:] ]", " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub("[^[:alnum:] ]", " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
  
} 
rm2(a) 
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

sample_data_all_strategy_currency <- as.data.frame(sample_data_all_strategy_currency,stringsAsFactors=FALSE)

for(i in which(sapply(sample_data_all_strategy_currency,class)=="character"))
{
  #sample_data_all_strategy_currency[[i]] <- trim(sample_data_all_strategy_currency[[i]])
  sample_data_all_strategy_currency[[i]] <- gsub("^\\s+|\\s+$", "", sample_data_all_strategy_currency[[i]], perl=TRUE)
}
rm2(i)

# for (i in 1:ncol(sample_data_all_strategy_currency))
# {
#   sample_data_all_strategy_currency[,i] <- unknownToNA(sample_data_all_strategy_currency[,i], unknown=unknowns_strings,force=TRUE)
#   sample_data_all_strategy_currency[,i] <- ifelse(is.na(sample_data_all_strategy_currency[,i]),NA, sample_data_all_strategy_currency[,i])
# } 
# rm2(i)

# Add space to end
sample_data_all_strategy_currency[,c("Fund_Name_temp")] <- paste(sample_data_all_strategy_currency[,c("Fund_Name_temp")]," ",sep="")


# Populate flags

sample_data_all_strategy_currency <- data.table(sample_data_all_strategy_currency)
setkeyv(sample_data_all_strategy_currency,NULL)

for(i in 1:nrow(currency_cols))
{
  # i <- 3
  #set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==currency_cols[i,"col_name"]), 
  #    value=ifelse(grepl(paste(" ",currency_cols[i,"currency"]," ",sep=""),sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]]),1,sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==currency_cols[i,"col_name"])]]))
  
  #set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==currency_cols[i,"col_name"]), 
  #    value=ifelse(grepl(paste(" ",currency_cols[i,"currency"]," ",sep=""),sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]]),1,0))
  
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==currency_cols[i,"col_name"]), 
      value=ifelse(grepl(paste(" ",currency_cols[i,"currency_alnum"]," ",sep=""),sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]]),1,0)) 
}
rm2(i)

# Remove Currencies from temp name
for(i in 1:nrow(currency_cols))
{
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub(paste(" ",currency_cols[i,"currency_alnum"]," ",sep=""), " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
  
}
rm2(i)

for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_currency, i=NULL, j=which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_currency[[which(colnames(sample_data_all_strategy_currency)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

sample_data_all_strategy_currency <- as.data.frame(sample_data_all_strategy_currency,stringsAsFactors=FALSE)

for(i in which(sapply(sample_data_all_strategy_currency,class)=="character"))
{
  #sample_data_all_strategy_currency[[i]] <- trim(sample_data_all_strategy_currency[[i]])
  sample_data_all_strategy_currency[[i]] <- gsub("^\\s+|\\s+$", "", sample_data_all_strategy_currency[[i]], perl=TRUE)
}
rm2(i)


# Get flag sums
sample_data_all_strategy_currency[,"cum_flag_USD"] <- rowSums(sample_data_all_strategy_currency[,currency_cols[currency_cols[,"USD_flag"]==1,"col_name"]], na.rm = TRUE)
sample_data_all_strategy_currency[,"cum_flag_nonUSD"] <- rowSums(sample_data_all_strategy_currency[,currency_cols[currency_cols[,"USD_flag"]==0,"col_name"]], na.rm = TRUE)
sample_data_all_strategy_currency[,"cum_flag_overall"] <- rowSums(sample_data_all_strategy_currency[,c("cum_flag_USD","cum_flag_nonUSD")], na.rm = TRUE)

# Get counts
sample_data_all_strategy_currency <- sample_data_all_strategy_currency[order(sample_data_all_strategy_currency[,"Fund_Name_temp"],
                                                                             sample_data_all_strategy_currency[,"Fund_Name"],
                                                                             sample_data_all_strategy_currency[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_currency) <- seq(nrow(sample_data_all_strategy_currency))

#sample_data_all_strategy_currency_counts <- ddply(.data=sample_data_all_strategy_currency, .variables=c("Fund_Name_temp"), .fun = function(x){return(data.frame(x,freq=nrow(x),stringsAsFactors=FALSE))}, .progress = "none")
sample_data_all_strategy_currency_counts <- ddply(.data=sample_data_all_strategy_currency[,!(colnames(sample_data_all_strategy_currency) %in% currency_cols[,"col_name"])], .variables=c("Fund_Name_temp"), .fun = function(x){return(data.frame(x,freq=nrow(x),stringsAsFactors=FALSE))}, .progress = "none")

rm2(sample_data_all_strategy_currency)


###############################################################################
cat("SECTION: REMOVE NON-USD FUNDS (FROM FUNDS WITH SAME NAMES LESS CURRENCIES)", "\n")
###############################################################################

#sample_data_all_strategy_currency_counts_single <- sample_data_all_strategy_currency_counts[sample_data_all_strategy_currency_counts[,"freq"]==1,]

#sample_data_all_strategy_currency_counts_multiple0 <- sample_data_all_strategy_currency_counts[sample_data_all_strategy_currency_counts[,"freq"]>1,]
#sample_data_all_strategy_currency_counts_multiple1 <- sample_data_all_strategy_currency_counts_multiple0[sample_data_all_strategy_currency_counts_multiple0[,"cum_flag_overall"]>=1,]
#sample_data_all_strategy_currency_counts_multiple2 <- sample_data_all_strategy_currency_counts_multiple1[sample_data_all_strategy_currency_counts_multiple1[,"cum_flag_nonUSD"]>=1,]

sample_data_all_strategy_currency_drop_ids <- unique(sample_data_all_strategy_currency_counts[sample_data_all_strategy_currency_counts[,"cum_flag_nonUSD"]>=1,"Fund_ID"])

sample_data_all_strategy_currency_drop <- sample_data_all_remove_nonUS_flags_keep[sample_data_all_remove_nonUS_flags_keep[,"Fund_ID"] %in% sample_data_all_strategy_currency_drop_ids,]

sample_data_all_strategy_currency_keep_ids <- unique(sample_data_all_remove_nonUS_flags_keep[!(sample_data_all_remove_nonUS_flags_keep[,"Fund_ID"] %in% sample_data_all_strategy_currency_drop_ids),c("Fund_ID")])

sample_data_all_strategy_currency_keep <- sample_data_all_remove_nonUS_flags_keep[sample_data_all_remove_nonUS_flags_keep[,"Fund_ID"] %in% sample_data_all_strategy_currency_keep_ids,]

rm2(sample_data_all_strategy_currency_drop_ids,sample_data_all_strategy_currency_keep_ids)
rm2(sample_data_all_remove_nonUS_flags_keep,sample_data_all_remove_nonUS_flags_drop)
rm2(sample_data_all_strategy_currency_counts)
#rm2(currency_cols)


###############################################################################
cat("SECTION: ADD RETURN & AUM DETAILS", "\n")
###############################################################################

sample_data_all_details_merge2 <- merge(sample_data_all_strategy_currency_keep,returns_trim, 
                                        by.x=c("Fund_ID","yr","pull_trim"), by.y=c("Fund_ID","yr","pull_trim"), 
                                        all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

rm2(sample_data_all_strategy_currency_keep,sample_data_all_strategy_currency_drop)
#rm2(returns_trim)

sample_data_all_details_merge2 <- sample_data_all_details_merge2[order(sample_data_all_details_merge2[,"Fund_ID"],
                                                                       sample_data_all_details_merge2[,"yr"],
                                                                       sample_data_all_details_merge2[,"month"]),] 
row.names(sample_data_all_details_merge2) <- seq(nrow(sample_data_all_details_merge2))

sample_data_all_details_merge2 <- sample_data_all_details_merge2[,c("Fund_ID","yr","month",
                                                                    colnames(sample_data_all_details_merge2)[!(colnames(sample_data_all_details_merge2) %in% c("Fund_ID","yr","month"))])]


###############################################################################
cat("SECTION: REMOVE FUNDS WITH NO RETURNS AND AUM", "\n")
###############################################################################

sample_data_all_details_merge2_no_ret0 <- ddply(.data=sample_data_all_details_merge2[,c("Fund_ID","Strat_ID",returns_data_cols)], .variables=c("Fund_ID","Strat_ID"), .fun = function(x){
  
  # x <- sample_data_all_details_merge2[(sample_data_all_details_merge2[,"Strat_ID"]==185 & sample_data_all_details_merge2[,"Fund_ID"]==5241),]
  
  x_out <- data.frame(Monthly_Ret_all_NA=NA,AUM_all_NA=NA,avg_Ret=NA,avg_AUM=NA,avg_NA=NA,stringsAsFactors=FALSE)
  x_out[,"Monthly_Ret_all_NA"] <- ifelse(sum(is.na(x[,"Monthly_Ret"]))==nrow(x),1,0)
  x_out[,"AUM_all_NA"] <- ifelse(sum(is.na(x[,"AUM"]))==nrow(x),1,0)
  return(x_out)
})
sample_data_all_details_merge2_no_ret1 <- sample_data_all_details_merge2_no_ret0[(sample_data_all_details_merge2_no_ret0[,"Monthly_Ret_all_NA"]==1 | sample_data_all_details_merge2_no_ret0[,"AUM_all_NA"]==1),]

sample_data_all_details_merge2_no_ret_all_ids <- unique(sample_data_all_details_merge2[,c("Fund_ID")])
sample_data_all_details_merge2_no_ret_drop_ids <- unique(sample_data_all_details_merge2_no_ret1[,c("Fund_ID")])
sample_data_all_details_merge2_no_ret_keep_ids <- sample_data_all_details_merge2_no_ret_all_ids[!(sample_data_all_details_merge2_no_ret_all_ids %in% sample_data_all_details_merge2_no_ret_drop_ids)]

#sample_data_all_details_merge2_no_ret_trim <- sample_data_all_details_merge2[sample_data_all_details_merge2[,"Fund_ID"] %in% sample_data_all_details_merge2_no_ret_keep_ids,c("Fund_ID","yr","month","Strat_ID",fund_details_location_cols,returns_data_cols)]
sample_data_all_details_merge2_no_ret_trim <- sample_data_all_details_merge2[sample_data_all_details_merge2[,"Fund_ID"] %in% sample_data_all_details_merge2_no_ret_keep_ids,]

rm2(sample_data_all_details_merge2)
rm2(sample_data_all_details_merge2_no_ret0,sample_data_all_details_merge2_no_ret1)
rm2(sample_data_all_details_merge2_no_ret_all_ids,sample_data_all_details_merge2_no_ret_drop_ids,sample_data_all_details_merge2_no_ret_keep_ids)


###############################################################################
cat("SECTION: FIND FUNDS WITH MULTIPLE CLASSES", "\n")
###############################################################################

sample_data_all_strategy_classes <- unique(sample_data_all_details_merge2_no_ret_trim[,c("Fund_ID","Fund_Name")])
sample_data_all_strategy_classes <- sample_data_all_strategy_classes[order(sample_data_all_strategy_classes[,"Fund_ID"],
                                                                           sample_data_all_strategy_classes[,"Fund_Name"]),] 
row.names(sample_data_all_strategy_classes) <- seq(nrow(sample_data_all_strategy_classes))

sample_data_all_strategy_classes <- data.frame(sample_data_all_strategy_classes,Fund_Name_temp=NA,
                                               matrix(NA, ncol=nrow(classes_cols_trim), nrow=nrow(sample_data_all_strategy_classes), dimnames=list(c(),classes_cols_trim[,"col_name"])),
                                               cum_flag_overall=NA,stringsAsFactors=FALSE)

sample_data_all_strategy_classes[,"Fund_Name_temp"] <- sample_data_all_strategy_classes[,"Fund_Name"]

sample_data_all_strategy_classes <- data.table(sample_data_all_strategy_classes)
setkeyv(sample_data_all_strategy_classes,NULL)

#sample_data_all_strategy_classes <- sample_data_all_strategy_classes[, Fund_Name_temp:=Fund_Name,by=NULL]
#sample_data_all_strategy_classes <- sample_data_all_strategy_classes[, USD_flag:=NA_integer_,by=NULL]

for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

for (a in 1:5)
{
  #a <- 1
  #set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub("[^[:alpha:] ]", " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub("[^[:alnum:] ]", " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
  
} 
rm2(a) 
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

sample_data_all_strategy_classes <- as.data.frame(sample_data_all_strategy_classes,stringsAsFactors=FALSE)

for(i in which(sapply(sample_data_all_strategy_classes,class)=="character"))
{
  #sample_data_all_strategy_classes[[i]] <- trim(sample_data_all_strategy_classes[[i]])
  sample_data_all_strategy_classes[[i]] <- gsub("^\\s+|\\s+$", "", sample_data_all_strategy_classes[[i]], perl=TRUE)
}
rm2(i)

# for (i in 1:ncol(sample_data_all_strategy_classes))
# {
#   sample_data_all_strategy_classes[,i] <- unknownToNA(sample_data_all_strategy_classes[,i], unknown=unknowns_strings,force=TRUE)
#   sample_data_all_strategy_classes[,i] <- ifelse(is.na(sample_data_all_strategy_classes[,i]),NA, sample_data_all_strategy_classes[,i])
# } 
# rm2(i)

# Add space to end
sample_data_all_strategy_classes[,c("Fund_Name_temp")] <- paste(sample_data_all_strategy_classes[,c("Fund_Name_temp")]," ",sep="")


# Populate flags

sample_data_all_strategy_classes <- data.table(sample_data_all_strategy_classes)
setkeyv(sample_data_all_strategy_classes,NULL)

for(i in 1:nrow(classes_cols_trim))
{
  # i <- 3
  #set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==classes_cols_trim[i,"col_name"]), 
  #    value=ifelse(grepl(paste(" ",classes_cols_trim[i,"classes"]," ",sep=""),sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]]),1,sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==classes_cols_trim[i,"col_name"])]]))
  
  #set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==classes_cols_trim[i,"col_name"]), 
  #    value=ifelse(grepl(paste(" ",classes_cols_trim[i,"classes"]," ",sep=""),sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]]),1,0))
  
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==classes_cols_trim[i,"col_name"]), 
      value=ifelse(grepl(paste(" ",classes_cols_trim[i,"classes_alnum_no_currency"]," ",sep=""),sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]]),1,0)) 
}
rm2(i)

# Remove Classes from temp name
for(i in 1:nrow(classes_cols_trim))
{
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub(paste(" ",classes_cols_trim[i,"classes_alnum_no_currency"]," ",sep=""), " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
  
}
rm2(i)

for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_strategy_classes, i=NULL, j=which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp")), value=gsub(" {2,}", " ", sample_data_all_strategy_classes[[which(colnames(sample_data_all_strategy_classes)==c("Fund_Name_temp"))]], perl=TRUE))
}
rm2(a)

sample_data_all_strategy_classes <- as.data.frame(sample_data_all_strategy_classes,stringsAsFactors=FALSE)

for(i in which(sapply(sample_data_all_strategy_classes,class)=="character"))
{
  #sample_data_all_strategy_classes[[i]] <- trim(sample_data_all_strategy_classes[[i]])
  sample_data_all_strategy_classes[[i]] <- gsub("^\\s+|\\s+$", "", sample_data_all_strategy_classes[[i]], perl=TRUE)
}
rm2(i)


# Get flag sums
sample_data_all_strategy_classes[,"cum_flag_overall"] <- rowSums(sample_data_all_strategy_classes[,classes_cols_trim[,"col_name"]], na.rm = TRUE)

# Get counts
sample_data_all_strategy_classes <- sample_data_all_strategy_classes[order(sample_data_all_strategy_classes[,"Fund_Name_temp"],
                                                                           sample_data_all_strategy_classes[,"Fund_Name"],
                                                                           sample_data_all_strategy_classes[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_classes) <- seq(nrow(sample_data_all_strategy_classes))

sample_data_all_strategy_classes_counts <- ddply(.data=sample_data_all_strategy_classes[,!(colnames(sample_data_all_strategy_classes) %in% classes_cols_trim[,"col_name"])], .variables=c("Fund_Name_temp"), .fun = function(x){
  return(data.frame(x,freq=nrow(x),id_u_count=length(unique(x[,"Fund_ID"])),stringsAsFactors=FALSE))
}, .progress = "none")

rm2(sample_data_all_strategy_classes)

sample_data_all_strategy_classes_counts <- sample_data_all_strategy_classes_counts[order(sample_data_all_strategy_classes_counts[,"Fund_Name_temp"],
                                                                                         sample_data_all_strategy_classes_counts[,"Fund_Name"],
                                                                                         sample_data_all_strategy_classes_counts[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_classes_counts) <- seq(nrow(sample_data_all_strategy_classes_counts))


###############################################################################
cat("SECTION: REMOVE NON-FLAGSHIP CLASSES", "\n")
###############################################################################

#cc <- unique(sample_data_all_strategy_classes_counts[,c("Fund_ID","freq")])
#cc <- cc[order(cc[,"Fund_ID"]),] 
#row.names(cc) <- seq(nrow(cc))
#cc2 <- count(data.frame(Fund_ID=cc[,1],stringsAsFactors=FALSE),"Fund_ID")

#cc3 <- sample_data_all_strategy_classes_counts[sample_data_all_strategy_classes_counts[,"Fund_ID"] %in% unique(cc2[cc2[,"freq"]>1,"Fund_ID"]),]
#cc3 <- cc3[order(cc3[,"Fund_ID"]),] 
#row.names(cc3) <- seq(nrow(cc3))


sample_data_all_strategy_classes_counts_all_ids <- sort(unique(sample_data_all_strategy_classes_counts[,"Fund_ID"]))

#sample_data_all_strategy_classes_counts_multiple_ids <- sort(unique(sample_data_all_strategy_classes_counts[(sample_data_all_strategy_classes_counts[,"freq"]>1),"Fund_ID"]))

#sample_data_all_strategy_classes_counts_multiple_ids <- sort(unique(sample_data_all_strategy_classes_counts[(sample_data_all_strategy_classes_counts[,"freq"]>1 & 
#                                                                                                                sample_data_all_strategy_classes_counts[,"cum_flag_overall"]>=1),"Fund_ID"]))

sample_data_all_strategy_classes_counts_multiple_ids <- sort(unique(sample_data_all_strategy_classes_counts[(sample_data_all_strategy_classes_counts[,"freq"]>1 & 
                                                                                                               sample_data_all_strategy_classes_counts[,"cum_flag_overall"]>=1  &
                                                                                                               sample_data_all_strategy_classes_counts[,"id_u_count"]>1),"Fund_ID"]))

#sample_data_all_strategy_classes_counts_single_ids <- sort(unique(sample_data_all_strategy_classes_counts[sample_data_all_strategy_classes_counts[,"freq"]==1,"Fund_ID"]))
sample_data_all_strategy_classes_counts_single_ids <- sample_data_all_strategy_classes_counts_all_ids[!(sample_data_all_strategy_classes_counts_all_ids %in% c(sample_data_all_strategy_classes_counts_multiple_ids))]

sample_data_all_strategy_classes_counts_multiple <- sample_data_all_strategy_classes_counts[sample_data_all_strategy_classes_counts[,"Fund_ID"] %in% sample_data_all_strategy_classes_counts_multiple_ids,]

rm2(sample_data_all_strategy_classes_counts)

#sample_data_all_strategy_classes_counts_multiple_details <- sample_data_all_details_merge2_no_ret_trim[sample_data_all_details_merge2_no_ret_trim[,"Fund_ID"] %in% sample_data_all_strategy_classes_counts_multiple[,"Fund_ID"],]

sample_data_all_strategy_classes_counts_multiple_details <- merge(sample_data_all_strategy_classes_counts_multiple,
                                                                  unique(sample_data_all_details_merge2_no_ret_trim[,!(colnames(sample_data_all_details_merge2_no_ret_trim) %in% c("month","Monthly_Ret","AUM"))]), 
                                                                  by.x=c("Fund_ID","Fund_Name"), by.y=c("Fund_ID","Fund_Name"), 
                                                                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

rm2(sample_data_all_strategy_classes_counts_multiple)

sample_data_all_strategy_classes_counts_multiple_details <- sample_data_all_strategy_classes_counts_multiple_details[order(sample_data_all_strategy_classes_counts_multiple_details[,"Fund_Name_temp"],
                                                                                                                           #sample_data_all_strategy_classes_counts_multiple_details[,"Fund_Name"],
                                                                                                                           sample_data_all_strategy_classes_counts_multiple_details[,"Fund_ID"],
                                                                                                                           sample_data_all_strategy_classes_counts_multiple_details[,"yr"]),] 
row.names(sample_data_all_strategy_classes_counts_multiple_details) <- seq(nrow(sample_data_all_strategy_classes_counts_multiple_details))


# IF Fund_ID is ever flagship
sample_data_all_strategy_classes_counts_multiple_keep_ids <- sort(unique(sample_data_all_strategy_classes_counts_multiple_details[sample_data_all_strategy_classes_counts_multiple_details[,"Flagship_bin"]==1,c("Fund_ID")]))

# IF Fund_ID is always flagship
#sample_data_all_strategy_classes_counts_multiple_keep_ids <- ddply(.data=sample_data_all_strategy_classes_counts_multiple_details, .variables=c("Fund_ID"), .fun = function(x){
#  if (length(which(x[,"Flagship_bin"]!=1 | is.na(x[,"Flagship_bin"])))==0) {return(data.frame(keep_flag=1,stringsAsFactors=FALSE))} else {return(data.frame(keep_flag=0,stringsAsFactors=FALSE))}
#})
#sample_data_all_strategy_classes_counts_multiple_keep_ids <- sample_data_all_strategy_classes_counts_multiple_keep_ids[sample_data_all_strategy_classes_counts_multiple_keep_ids[,"keep_flag"]==1,!(colnames(sample_data_all_strategy_classes_counts_multiple_keep_ids) %in% c("keep_flag"))]

sample_data_all_strategy_classes_counts_multiple_drop_ids <- sort(unique(sample_data_all_strategy_classes_counts_multiple_details[!(sample_data_all_strategy_classes_counts_multiple_details[,"Fund_ID"] %in% sample_data_all_strategy_classes_counts_multiple_keep_ids),c("Fund_ID")]))

sample_data_all_strategy_classes_all_ids <- sort(unique(sample_data_all_details_merge2_no_ret_trim[,"Fund_ID"]))

sample_data_all_strategy_classes_keep_ids <- sample_data_all_strategy_classes_all_ids[sample_data_all_strategy_classes_all_ids %in% c(sample_data_all_strategy_classes_counts_single_ids,sample_data_all_strategy_classes_counts_multiple_keep_ids)]
sample_data_all_strategy_classes_keep <- sample_data_all_details_merge2_no_ret_trim[sample_data_all_details_merge2_no_ret_trim[,"Fund_ID"] %in% sample_data_all_strategy_classes_keep_ids,]

#sample_data_all_strategy_classes_drop_ids <- sample_data_all_strategy_classes_all_ids[sample_data_all_strategy_classes_all_ids %in% c(sample_data_all_strategy_classes_counts_multiple_drop_ids)]
sample_data_all_strategy_classes_drop_ids <- sample_data_all_strategy_classes_all_ids[!(sample_data_all_strategy_classes_all_ids %in% c(sample_data_all_strategy_classes_counts_single_ids,sample_data_all_strategy_classes_counts_multiple_keep_ids))]
sample_data_all_strategy_classes_drop <- sample_data_all_details_merge2_no_ret_trim[sample_data_all_details_merge2_no_ret_trim[,"Fund_ID"] %in% sample_data_all_strategy_classes_drop_ids,]


###########
#aa <- sort(unique(c(sample_data_all_strategy_classes_drop_ids,sample_data_all_strategy_classes_counts_multiple_drop_ids)))
#bb <- sample_data_all_strategy_classes_counts_multiple_details[sample_data_all_strategy_classes_counts_multiple_details[,"Fund_ID"]==5252,]
##########

rm2(sample_data_all_strategy_classes_counts_all_ids,sample_data_all_strategy_classes_counts_multiple_ids,sample_data_all_strategy_classes_counts_single_ids)
rm2(sample_data_all_strategy_classes_counts_multiple_keep_ids,sample_data_all_strategy_classes_counts_multiple_drop_ids)
rm2(sample_data_all_strategy_classes_all_ids,sample_data_all_strategy_classes_keep_ids,sample_data_all_strategy_classes_drop_ids)
rm2(sample_data_all_strategy_classes_counts_multiple_details)
rm2(sample_data_all_details_merge2_no_ret_trim)
#rm2(classes_cols,classes_cols_trim)


###############################################################################
cat("SECTION: ADD MANAGER DETAILS", "\n")
###############################################################################

sample_data_all_details_merge3 <- merge(sample_data_all_strategy_classes_keep,other_details_trim, 
                                        by.x=c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name"), 
                                        by.y=c("Fund_ID","yr","pull_trim","pull_trim2","Fund_Name"), 
                                        all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

rm2(sample_data_all_strategy_classes_keep,sample_data_all_strategy_classes_drop)
#rm2(other_details_trim)

sample_data_all_details_merge3 <- sample_data_all_details_merge3[order(sample_data_all_details_merge3[,"Fund_ID"],
                                                                       sample_data_all_details_merge3[,"yr"],
                                                                       sample_data_all_details_merge3[,"month"]),] 
row.names(sample_data_all_details_merge3) <- seq(nrow(sample_data_all_details_merge3))


###############################################################################
cat("SECTION: FIND STRATEGY COUNTS BEFORE FIRST DUPLICATE STRATEGY REMOVAL", "\n")
###############################################################################

sample_data_all_strategy_count1 <- unique(sample_data_all_details_merge3[,c("Fund_ID","Strat_ID","Strategy")])

sample_data_all_strategy_count1_count <- count(sample_data_all_strategy_count1,c("Strat_ID"))
colnames(sample_data_all_strategy_count1_count)[match("freq",names(sample_data_all_strategy_count1_count))] <- "freq_strategy1"

sample_data_all_strategy_count1_count <- sample_data_all_strategy_count1_count[order(sample_data_all_strategy_count1_count[,"Strat_ID"]),] 
row.names(sample_data_all_strategy_count1_count) <- seq(nrow(sample_data_all_strategy_count1_count))

sample_data_all_strategy_count1_merge <- merge(sample_data_all_strategy_count1,
                                               sample_data_all_strategy_count1_count, 
                                               by.x=c("Strat_ID"), by.y=c("Strat_ID"), 
                                               all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_data_all_strategy_count1_merge <- sample_data_all_strategy_count1_merge[order(sample_data_all_strategy_count1_merge[,"Strat_ID"],
                                                                                     sample_data_all_strategy_count1_merge[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_count1_merge) <- seq(nrow(sample_data_all_strategy_count1_merge))

sample_data_all_strategy_count1_merge <- sample_data_all_strategy_count1_merge[,c("Fund_ID","Strat_ID","freq_strategy1",
                                                                                  colnames(sample_data_all_strategy_count1_merge)[!(colnames(sample_data_all_strategy_count1_merge) %in% c("Fund_ID","Strat_ID","freq_strategy1"))])]


sample_data_all_strategy_count1_merge_full <- merge(sample_data_all_details_merge3,sample_data_all_strategy_count1_merge, 
                                                    by.x=c("Fund_ID","Strat_ID","Strategy"), by.y=c("Fund_ID","Strat_ID","Strategy"), 
                                                    all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_data_all_strategy_count1_merge_full <- sample_data_all_strategy_count1_merge_full[order(sample_data_all_strategy_count1_merge_full[,"Strat_ID"],
                                                                                               sample_data_all_strategy_count1_merge_full[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_count1_merge_full) <- seq(nrow(sample_data_all_strategy_count1_merge_full))

sample_data_all_strategy_count1_merge_full <- sample_data_all_strategy_count1_merge_full[,c(sample_data_all_id_cols,"Strat_ID","freq_strategy1",
                                                                                            colnames(sample_data_all_strategy_count1_merge_full)[!(colnames(sample_data_all_strategy_count1_merge_full) %in% c(sample_data_all_id_cols,"Strat_ID","freq_strategy1"))])]

sample_data_all_strategy_count1_merge_full <- sample_data_all_strategy_count1_merge_full[,c("Fund_ID","yr","month",
                                                                                            colnames(sample_data_all_strategy_count1_merge_full)[!(colnames(sample_data_all_strategy_count1_merge_full) %in% c("Fund_ID","yr","month"))])]

rm2(sample_data_all_strategy_count1,sample_data_all_strategy_count1_count)
rm2(sample_data_all_details_merge3,sample_data_all_strategy_count1_merge)

# sample_data_all_strategy_count1 <- unique(sample_data_all_details_merge3[,c("Fund_ID","Strategy")])
# 
# sample_data_all_strategy_count1_count0 <- count(sample_data_all_strategy_count1,"Strategy")
# colnames(sample_data_all_strategy_count1_count0)[match("freq",names(sample_data_all_strategy_count1_count0))] <- "freq_strategy1"
# 
# sample_data_all_strategy_count1_u <- unique(data.frame(Strat_ID=NA,Strategy=sample_data_all_strategy_count1[,"Strategy"],stringsAsFactors=FALSE))
# sample_data_all_strategy_count1_u[,"Strat_ID"] <- seq(1,nrow(sample_data_all_strategy_count1_u))
# row.names(sample_data_all_strategy_count1_u) <- seq(nrow(sample_data_all_strategy_count1_u))
# 
# sample_data_all_strategy_count1_count <- merge(sample_data_all_strategy_count1_count0, sample_data_all_strategy_count1_u, 
#                                                by.x=c("Strategy"), by.y=c("Strategy"), 
#                                                all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_strategy_count1_count0,sample_data_all_strategy_count1_u)
# 
# 
# sample_data_all_strategy_count1_count <- sample_data_all_strategy_count1_count[,c("Strat_ID","freq_strategy1",
#                                                                                   colnames(sample_data_all_strategy_count1_count)[!(colnames(sample_data_all_strategy_count1_count) %in% c("Strat_ID","freq_strategy1"))])]
# 
# sample_data_all_strategy_count1_count <- sample_data_all_strategy_count1_count[order(sample_data_all_strategy_count1_count[,"Strat_ID"]),] 
# row.names(sample_data_all_strategy_count1_count) <- seq(nrow(sample_data_all_strategy_count1_count))
# 
# sample_data_all_strategy_count1_merge <- merge(sample_data_all_strategy_count1, sample_data_all_strategy_count1_count, 
#                                                by.x=c("Strategy"), by.y=c("Strategy"), 
#                                                all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_strategy_count1,sample_data_all_strategy_count1_count)
# 
# sample_data_all_strategy_count1_merge <- sample_data_all_strategy_count1_merge[order(sample_data_all_strategy_count1_merge[,"Strat_ID"],
#                                                                                      sample_data_all_strategy_count1_merge[,"Fund_ID"]),] 
# row.names(sample_data_all_strategy_count1_merge) <- seq(nrow(sample_data_all_strategy_count1_merge))
# 
# sample_data_all_strategy_count1_merge <- sample_data_all_strategy_count1_merge[,c("Fund_ID","Strat_ID","freq_strategy1",
#                                                                                   colnames(sample_data_all_strategy_count1_merge)[!(colnames(sample_data_all_strategy_count1_merge) %in% c("Fund_ID","Strat_ID","freq_strategy1"))])]
# 
# 
# sample_data_all_strategy_count1_merge_full <- merge(sample_data_all_details_merge2,sample_data_all_strategy_count1_merge, 
#                                                     by.x=c("Fund_ID","Strategy"), by.y=c("Fund_ID","Strategy"), 
#                                                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_details_merge2,sample_data_all_strategy_count1_merge)
# 
# sample_data_all_strategy_count1_merge_full <- sample_data_all_strategy_count1_merge_full[order(sample_data_all_strategy_count1_merge_full[,"Strat_ID"],
#                                                                                                sample_data_all_strategy_count1_merge_full[,"Fund_ID"]),] 
# row.names(sample_data_all_strategy_count1_merge_full) <- seq(nrow(sample_data_all_strategy_count1_merge_full))
# 
# sample_data_all_strategy_count1_merge_full <- sample_data_all_strategy_count1_merge_full[,c(other_details_id_cols,"Strat_ID","freq_strategy1",
#                                                                                             colnames(sample_data_all_strategy_count1_merge_full)[!(colnames(sample_data_all_strategy_count1_merge_full) %in% c(other_details_id_cols,"Strat_ID","freq_strategy1"))])]


###############################################################################
cat("SECTION: REMOVE NON-FLAGSHIPS WITH DUPLICATE STRATEGIES", "\n")
###############################################################################

sample_data_all_remove_duplicates1_all_ids <- sort(unique(sample_data_all_strategy_count1_merge_full[,"Fund_ID"]))

sample_data_all_remove_duplicates1_multiple_ids <- sort(unique(sample_data_all_strategy_count1_merge_full[(sample_data_all_strategy_count1_merge_full[,"freq_strategy1"]>1),"Fund_ID"]))

sample_data_all_remove_duplicates1_single_ids <- sample_data_all_remove_duplicates1_all_ids[!(sample_data_all_remove_duplicates1_all_ids %in% c(sample_data_all_remove_duplicates1_multiple_ids))]


sample_data_all_remove_duplicates1_multiple <- sample_data_all_strategy_count1_merge_full[sample_data_all_strategy_count1_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_ids,]


# IF Fund_ID is ever flagship for a given year
# sample_data_all_remove_duplicates1_multiple_keep <- ddply(.data=sample_data_all_remove_duplicates1_multiple, .variables=c("Strat_ID","yr"), .fun = function(x){
#   
#   # x <- sample_data_all_remove_duplicates1_multiple[(sample_data_all_remove_duplicates1_multiple[,"Strat_ID"]==3 & sample_data_all_remove_duplicates1_multiple[,"yr"]==1987),]
#   # x <- sample_data_all_remove_duplicates1_multiple[(sample_data_all_remove_duplicates1_multiple[,"Strat_ID"]==658 & sample_data_all_remove_duplicates1_multiple[,"yr"]==1986),]
#   
#   if (nrow(x)==1) {
#     x_out <- x
#   } else if (length(which(x[,"Flagship_bin"]!=1 | is.na(x[,"Flagship_bin"])))==0) {
#     x_out <- x
#   } else {
#     x_out <- x[x[,"Flagship_bin"]==1,]
#   }
#   return(x_out)
# 
# }, .progress = "none")

# IF Fund_ID is ever flagship
#sample_data_all_remove_duplicates1_multiple_keep_ids <- unique(sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Flagship_bin"]==1,c("Fund_ID")])

# IF Fund_ID is always flagship
sample_data_all_remove_duplicates1_multiple_keep_ids <- ddply(.data=sample_data_all_remove_duplicates1_multiple, .variables=c("Fund_ID"), .fun = function(x){
  if (length(which(x[,"Flagship_bin"]!=1 | is.na(x[,"Flagship_bin"])))==0) {return(data.frame(keep_flag=1,stringsAsFactors=FALSE))} else {return(data.frame(keep_flag=0,stringsAsFactors=FALSE))}
})
sample_data_all_remove_duplicates1_multiple_keep_ids <- sample_data_all_remove_duplicates1_multiple_keep_ids[sample_data_all_remove_duplicates1_multiple_keep_ids[,"keep_flag"]==1,!(colnames(sample_data_all_remove_duplicates1_multiple_keep_ids) %in% c("keep_flag"))]
#sample_data_all_remove_duplicates1_multiple_keep <- sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_keep_ids,]

sample_data_all_remove_duplicates1_multiple_drop_ids <- unique(sample_data_all_remove_duplicates1_multiple[!(sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_keep_ids),c("Fund_ID")])
#sample_data_all_remove_duplicates1_multiple_drop <- sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_drop_ids,]

sample_data_all_remove_duplicates1_keep_ids <- sample_data_all_remove_duplicates1_all_ids[sample_data_all_remove_duplicates1_all_ids %in% c(sample_data_all_remove_duplicates1_single_ids,sample_data_all_remove_duplicates1_multiple_keep_ids)]
sample_data_all_remove_duplicates1_keep <- sample_data_all_strategy_count1_merge_full[sample_data_all_strategy_count1_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_keep_ids,]


sample_data_all_remove_duplicates1_drop_ids <- sample_data_all_remove_duplicates1_all_ids[!(sample_data_all_remove_duplicates1_all_ids %in% c(sample_data_all_remove_duplicates1_single_ids,sample_data_all_remove_duplicates1_multiple_keep_ids))]
sample_data_all_remove_duplicates1_drop <- sample_data_all_strategy_count1_merge_full[sample_data_all_strategy_count1_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_drop_ids,]


sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_keep

sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[order(sample_data_all_remove_duplicates1_comb[,"Strat_ID"],
                                                                                         sample_data_all_remove_duplicates1_comb[,"Fund_ID"]),] 
row.names(sample_data_all_remove_duplicates1_comb) <- seq(nrow(sample_data_all_remove_duplicates1_comb))

#sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[,!(colnames(sample_data_all_remove_duplicates1_comb) %in% c("Strat_ID","freq_strategy1"))]
sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[,!(colnames(sample_data_all_remove_duplicates1_comb) %in% c("freq_strategy1"))]


rm2(sample_data_all_remove_duplicates1_all_ids,sample_data_all_remove_duplicates1_multiple_ids,sample_data_all_remove_duplicates1_single_ids)
rm2(sample_data_all_remove_duplicates1_keep_ids,sample_data_all_remove_duplicates1_drop_ids)
rm2(sample_data_all_remove_duplicates1_multiple_keep_ids,sample_data_all_remove_duplicates1_multiple_drop_ids)
#rm2(sample_data_all_remove_duplicates1_multiple_keep,sample_data_all_remove_duplicates1_multiple_drop)
rm2(sample_data_all_remove_duplicates1_keep,sample_data_all_remove_duplicates1_drop)
rm2(sample_data_all_remove_duplicates1_multiple)
rm2(sample_data_all_strategy_count1_merge_full)


# #sample_data_all_strategy_count1_merge_full_old <- sample_data_all_strategy_count1_merge_full
# 
# #sample_data_all_strategy_count1_merge_full <- unique(sample_data_all_strategy_count1_merge_full[,!(colnames(sample_data_all_strategy_count1_merge_full) %in% c("month"))])
# #sample_data_all_strategy_count1_merge_full <- unique(sample_data_all_strategy_count1_merge_full[,!(colnames(sample_data_all_strategy_count1_merge_full) %in% c("month"))])
# 
# sample_data_all_remove_duplicates1_single <- sample_data_all_strategy_count1_merge_full[sample_data_all_strategy_count1_merge_full[,"freq_strategy1"]==1,]
# #sample_data_all_remove_duplicates1_single <- sample_data_all_remove_duplicates1_single[,!(colnames(sample_data_all_remove_duplicates1_single) %in% c("nonUSD_flag"))]
# 
# sample_data_all_remove_duplicates1_multiple <- sample_data_all_strategy_count1_merge_full[sample_data_all_strategy_count1_merge_full[,"freq_strategy1"]>1,]
# #sample_data_all_remove_duplicates1_multiple <- sample_data_all_remove_duplicates1_multiple[,!(colnames(sample_data_all_remove_duplicates1_multiple) %in% c("nonUSD_flag"))]
# 
# 
# # IF Fund_ID is ever flagship for a given year
# # sample_data_all_remove_duplicates1_multiple_keep <- ddply(.data=sample_data_all_remove_duplicates1_multiple, .variables=c("Strat_ID","yr"), .fun = function(x){
# #   
# #   # x <- sample_data_all_remove_duplicates1_multiple[(sample_data_all_remove_duplicates1_multiple[,"Strat_ID"]==3 & sample_data_all_remove_duplicates1_multiple[,"yr"]==1987),]
# #   # x <- sample_data_all_remove_duplicates1_multiple[(sample_data_all_remove_duplicates1_multiple[,"Strat_ID"]==658 & sample_data_all_remove_duplicates1_multiple[,"yr"]==1986),]
# #   
# #   if (nrow(x)==1) {
# #     x_out <- x
# #   } else if (length(which(x[,"Flagship_bin"]!=1 | is.na(x[,"Flagship_bin"])))==0) {
# #     x_out <- x
# #   } else {
# #     x_out <- x[x[,"Flagship_bin"]==1,]
# #   }
# #   return(x_out)
# # 
# # }, .progress = "none")
# 
# # IF Fund_ID is ever flagship
# #sample_data_all_remove_duplicates1_multiple_keep_ids <- unique(sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Flagship_bin"]==1,c("Fund_ID")])
# 
# # IF Fund_ID is always flagship
# sample_data_all_remove_duplicates1_multiple_keep_ids <- ddply(.data=sample_data_all_remove_duplicates1_multiple, .variables=c("Fund_ID"), .fun = function(x){
#   if (length(which(x[,"Flagship_bin"]!=1 | is.na(x[,"Flagship_bin"])))==0) {return(data.frame(keep_flag=1,stringsAsFactors=FALSE))} else {return(data.frame(keep_flag=0,stringsAsFactors=FALSE))}
# })
# sample_data_all_remove_duplicates1_multiple_keep_ids <- sample_data_all_remove_duplicates1_multiple_keep_ids[sample_data_all_remove_duplicates1_multiple_keep_ids[,"keep_flag"]==1,!(colnames(sample_data_all_remove_duplicates1_multiple_keep_ids) %in% c("keep_flag"))]
# 
# sample_data_all_remove_duplicates1_multiple_keep <- sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_keep_ids,]
# 
# sample_data_all_remove_duplicates1_multiple_drop_ids <- unique(sample_data_all_remove_duplicates1_multiple[!(sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_keep_ids),c("Fund_ID")])
# 
# sample_data_all_remove_duplicates1_multiple_drop <- sample_data_all_remove_duplicates1_multiple[sample_data_all_remove_duplicates1_multiple[,"Fund_ID"] %in% sample_data_all_remove_duplicates1_multiple_drop_ids,]
# 
# rm2(sample_data_all_remove_duplicates1_multiple_keep_ids,sample_data_all_remove_duplicates1_multiple_drop_ids)
# rm2(sample_data_all_remove_duplicates1_multiple)
# 
# sample_data_all_remove_duplicates1_comb <- rbind(sample_data_all_remove_duplicates1_single,sample_data_all_remove_duplicates1_multiple_keep)
# 
# 
# sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[order(sample_data_all_remove_duplicates1_comb[,"Strat_ID"],
#                                                                                          sample_data_all_remove_duplicates1_comb[,"Fund_ID"]),] 
# row.names(sample_data_all_remove_duplicates1_comb) <- seq(nrow(sample_data_all_remove_duplicates1_comb))
# 
# #sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[,!(colnames(sample_data_all_remove_duplicates1_comb) %in% c("Strat_ID","freq_strategy1"))]
# sample_data_all_remove_duplicates1_comb <- sample_data_all_remove_duplicates1_comb[,!(colnames(sample_data_all_remove_duplicates1_comb) %in% c("freq_strategy1"))]
# 
# rm2(sample_data_all_remove_duplicates1_single,sample_data_all_remove_duplicates1_multiple_keep,sample_data_all_remove_duplicates1_multiple_drop)
# rm2(sample_data_all_strategy_count1_merge_full)


###############################################################################
cat("SECTION: FIND STRATEGY COUNTS BEFORE SECOND DUPLICATE STRATEGY REMOVAL", "\n")
###############################################################################

sample_data_all_strategy_count2 <- unique(sample_data_all_remove_duplicates1_comb[,c("Fund_ID","Strat_ID","Strategy")])

sample_data_all_strategy_count2_count <- count(sample_data_all_strategy_count2,c("Strat_ID"))
colnames(sample_data_all_strategy_count2_count)[match("freq",names(sample_data_all_strategy_count2_count))] <- "freq_strategy2"

sample_data_all_strategy_count2_count <- sample_data_all_strategy_count2_count[order(sample_data_all_strategy_count2_count[,"Strat_ID"]),] 
row.names(sample_data_all_strategy_count2_count) <- seq(nrow(sample_data_all_strategy_count2_count))

sample_data_all_strategy_count2_merge <- merge(sample_data_all_strategy_count2,
                                               sample_data_all_strategy_count2_count, 
                                               by.x=c("Strat_ID"), by.y=c("Strat_ID"), 
                                               all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_data_all_strategy_count2_merge <- sample_data_all_strategy_count2_merge[order(sample_data_all_strategy_count2_merge[,"Strat_ID"],
                                                                                     sample_data_all_strategy_count2_merge[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_count2_merge) <- seq(nrow(sample_data_all_strategy_count2_merge))

sample_data_all_strategy_count2_merge <- sample_data_all_strategy_count2_merge[,c("Fund_ID","Strat_ID","freq_strategy2",
                                                                                  colnames(sample_data_all_strategy_count2_merge)[!(colnames(sample_data_all_strategy_count2_merge) %in% c("Fund_ID","Strat_ID","freq_strategy2"))])]


sample_data_all_strategy_count2_merge_full <- merge(sample_data_all_remove_duplicates1_comb,sample_data_all_strategy_count2_merge, 
                                                    by.x=c("Fund_ID","Strat_ID","Strategy"), by.y=c("Fund_ID","Strat_ID","Strategy"), 
                                                    all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

sample_data_all_strategy_count2_merge_full <- sample_data_all_strategy_count2_merge_full[order(sample_data_all_strategy_count2_merge_full[,"Strat_ID"],
                                                                                               sample_data_all_strategy_count2_merge_full[,"Fund_ID"]),] 
row.names(sample_data_all_strategy_count2_merge_full) <- seq(nrow(sample_data_all_strategy_count2_merge_full))

sample_data_all_strategy_count2_merge_full <- sample_data_all_strategy_count2_merge_full[,c(sample_data_all_id_cols,"Strat_ID","freq_strategy2",
                                                                                            colnames(sample_data_all_strategy_count2_merge_full)[!(colnames(sample_data_all_strategy_count2_merge_full) %in% c(sample_data_all_id_cols,"Strat_ID","freq_strategy2"))])]

sample_data_all_strategy_count2_merge_full <- sample_data_all_strategy_count2_merge_full[,c("Fund_ID","yr","month",
                                                                                            colnames(sample_data_all_strategy_count2_merge_full)[!(colnames(sample_data_all_strategy_count2_merge_full) %in% c("Fund_ID","yr","month"))])]

rm2(sample_data_all_strategy_count2,sample_data_all_strategy_count2_count)
rm2(sample_data_all_remove_duplicates1_comb,sample_data_all_strategy_count2_merge)

# sample_data_all_strategy_count2 <- unique(sample_data_all_remove_duplicates1_comb[,c("Fund_ID","Strategy")])
# 
# sample_data_all_strategy_count2_count0 <- count(sample_data_all_strategy_count2,"Strategy")
# colnames(sample_data_all_strategy_count2_count0)[match("freq",names(sample_data_all_strategy_count2_count0))] <- "freq_strategy2"
# 
# sample_data_all_strategy_count2_u <- unique(data.frame(Strat_ID=NA,Strategy=sample_data_all_strategy_count2[,"Strategy"],stringsAsFactors=FALSE))
# sample_data_all_strategy_count2_u[,"Strat_ID"] <- seq(1,nrow(sample_data_all_strategy_count2_u))
# row.names(sample_data_all_strategy_count2_u) <- seq(nrow(sample_data_all_strategy_count2_u))
# 
# sample_data_all_strategy_count2_count <- merge(sample_data_all_strategy_count2_count0, sample_data_all_strategy_count2_u, 
#                                                by.x=c("Strategy"), by.y=c("Strategy"), 
#                                                all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_strategy_count2_count0, sample_data_all_strategy_count2_u)
# 
# sample_data_all_strategy_count2_count <- sample_data_all_strategy_count2_count[,c("Strat_ID","freq_strategy2",
#                                                                                   colnames(sample_data_all_strategy_count2_count)[!(colnames(sample_data_all_strategy_count2_count) %in% c("Strat_ID","freq_strategy2"))])]
# 
# sample_data_all_strategy_count2_count <- sample_data_all_strategy_count2_count[order(sample_data_all_strategy_count2_count[,"Strat_ID"]),] 
# row.names(sample_data_all_strategy_count2_count) <- seq(nrow(sample_data_all_strategy_count2_count))
# 
# sample_data_all_strategy_count2_merge <- merge(sample_data_all_strategy_count2, sample_data_all_strategy_count2_count, 
#                                                by.x=c("Strategy"), by.y=c("Strategy"), 
#                                                all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_strategy_count2,sample_data_all_strategy_count2_count)
# 
# sample_data_all_strategy_count2_merge <- sample_data_all_strategy_count2_merge[order(sample_data_all_strategy_count2_merge[,"Strat_ID"],
#                                                                                      sample_data_all_strategy_count2_merge[,"Fund_ID"]),] 
# row.names(sample_data_all_strategy_count2_merge) <- seq(nrow(sample_data_all_strategy_count2_merge))
# 
# sample_data_all_strategy_count2_merge <- sample_data_all_strategy_count2_merge[,c("Fund_ID","Strat_ID","freq_strategy2",
#                                                                                   colnames(sample_data_all_strategy_count2_merge)[!(colnames(sample_data_all_strategy_count2_merge) %in% c("Fund_ID","Strat_ID","freq_strategy2"))])]
# 
# 
# sample_data_all_strategy_count2_merge_full <- merge(sample_data_all_remove_duplicates1_comb,sample_data_all_strategy_count2_merge, 
#                                                     by.x=c("Fund_ID","Strategy"), by.y=c("Fund_ID","Strategy"), 
#                                                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
# 
# rm2(sample_data_all_remove_duplicates1_comb,sample_data_all_strategy_count2_merge)
# 
# sample_data_all_strategy_count2_merge_full <- sample_data_all_strategy_count2_merge_full[order(sample_data_all_strategy_count2_merge_full[,"Strat_ID"],
#                                                                                                sample_data_all_strategy_count2_merge_full[,"Fund_ID"]),] 
# row.names(sample_data_all_strategy_count2_merge_full) <- seq(nrow(sample_data_all_strategy_count2_merge_full))
# 
# sample_data_all_strategy_count2_merge_full <- sample_data_all_strategy_count2_merge_full[,c(other_details_id_cols,"Strat_ID","freq_strategy2",
#                                                                                             colnames(sample_data_all_strategy_count2_merge_full)[!(colnames(sample_data_all_strategy_count2_merge_full) %in% c(other_details_id_cols,"Strat_ID","freq_strategy2"))])]


###############################################################################
cat("SECTION: REMOVE FUND WITH DUPLICATE STRATEGIES THAT HAS THE LEAST INFORMATION", "\n")
###############################################################################

sample_data_all_remove_duplicates2_all_ids <- sort(unique(sample_data_all_strategy_count2_merge_full[,"Fund_ID"]))

#sample_data_all_remove_duplicates2_single_ids <- sort(unique(sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"freq_strategy2"]==1,"Fund_ID"]))
#sample_data_all_remove_duplicates2_multiple_ids <- sample_data_all_remove_duplicates2_all_ids[!(sample_data_all_remove_duplicates2_all_ids %in% c(sample_data_all_remove_duplicates2_single_ids))]

sample_data_all_remove_duplicates2_multiple_ids <- sort(unique(sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"freq_strategy2"]>1,"Fund_ID"]))
sample_data_all_remove_duplicates2_single_ids <- sample_data_all_remove_duplicates2_all_ids[!(sample_data_all_remove_duplicates2_all_ids %in% c(sample_data_all_remove_duplicates2_multiple_ids))]

sample_data_all_remove_duplicates2_multiple <- sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates2_multiple_ids,]

#sample_data_all_remove_duplicates2_single <- sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"freq_strategy2"]==1,]
#sample_data_all_remove_duplicates2_single <- sample_data_all_remove_duplicates2_single[,!(colnames(sample_data_all_remove_duplicates2_single) %in% c("nonUSD_flag"))]

#sample_data_all_remove_duplicates2_multiple <- sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"freq_strategy2"]>1,]
#sample_data_all_remove_duplicates2_multiple <- sample_data_all_remove_duplicates2_multiple[,!(colnames(sample_data_all_remove_duplicates2_multiple) %in% c("nonUSD_flag"))]

#rm2(sample_data_all_strategy_count2_merge_full)

#aa <- count(sample_data_all_remove_duplicates2_single,"Strat_ID")
#bb <- count(sample_data_all_remove_duplicates2_multiple,"Strat_ID")

#zz <- sample_data_all_remove_duplicates2_single[sample_data_all_remove_duplicates2_single[,"Fund_ID"]==36148,]


### FOR EACH STRAT ID THAT IS A MULTIPLE
### GET COUNT OF MONTHS OF RETURNS
### CHECK CORRELATION OF RETURNS FOR MONTHS THAT ARE BOTH PRESENT
###
### FIND COLUMNS THAT ARE SIMILAR FOR EACH YEAR
### FIND TOTAL NUMBER OF NA COLUMNS
### 
### MAYBE LOOK AT THE MANAGER INFO
### 
### I PLAN FOR THIS TO BE THE LAST REMOVAL SO END UP WITH NO DUPLICATE STRATEGIES

sample_data_all_remove_duplicates2_multiple_cols <- c("Fund_ID","Strat_ID","yr","month",fund_details_location_cols,returns_data_cols,other_details_manager_cols)

sample_data_all_remove_duplicates2_multiple_all_ids <- ddply(.data=sample_data_all_remove_duplicates2_multiple[,sample_data_all_remove_duplicates2_multiple_cols], .variables=c("Strat_ID"), .fun = function(x){
  
  # x <- sample_data_all_remove_duplicates2_multiple[(sample_data_all_remove_duplicates2_multiple[,"Strat_ID"]==338),sample_data_all_remove_duplicates2_multiple_cols]
  # x <- sample_data_all_remove_duplicates2_multiple[(sample_data_all_remove_duplicates2_multiple[,"Strat_ID"]==374),sample_data_all_remove_duplicates2_multiple_cols]
  # x <- sample_data_all_remove_duplicates2_multiple[(sample_data_all_remove_duplicates2_multiple[,"Strat_ID"]==504),sample_data_all_remove_duplicates2_multiple_cols]
  # x <- sample_data_all_remove_duplicates2_multiple[(sample_data_all_remove_duplicates2_multiple[,"Strat_ID"]==518),sample_data_all_remove_duplicates2_multiple_cols]
  
  # ww <- count(unique(sample_data_all_remove_duplicates2_multiple[,c("Strat_ID","Fund_ID")]),"Strat_ID")
  # yy <- sample_data_all_remove_duplicates2_multiple[(sample_data_all_remove_duplicates2_multiple[,"Fund_ID"]==5439),sample_data_all_remove_duplicates2_multiple_cols]
  
  require(reshape)
  
  #cat("Strat_ID:",unique(x[,"Strat_ID"]),"\n")
  
  if (length(unique(x[,"Fund_ID"])) == 1) {
    
    ids_out <- data.frame(Fund_ID=unique(x[,"Fund_ID"]),keep_flag=1,stringsAsFactors=FALSE)
    
  } else {
    
    #### Expand dates ####
    
    x_expand <- as.data.frame(expand.grid(yr=sort(unique(x[,c("yr")])), month=seq(1,12,by=1)),stringsAsFactors=FALSE)
    x_expand[sapply(x_expand,is.factor)] <- lapply(x_expand[sapply(x_expand,is.factor)], as.character)
    x_expand[,"yr"] <- as.integer(x_expand[,"yr"])
    x_expand[,"month"] <- as.integer(x_expand[,"month"])
    
    x_expand <- x_expand[order(x_expand[,"yr"],x_expand[,"month"]),] 
    row.names(x_expand) <- seq(nrow(x_expand))
    
    x_expand2_temp <- merge(x_expand,x[,!(colnames(x) %in% c("Strat_ID"))], 
                            by.x=c("yr","month"), by.y=c("yr","month"), 
                            all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
    
    x_expand2 <- x_expand2_temp[!is.na(x_expand2_temp[,"Fund_ID"]),]
    
    rm(x_expand2_temp)
    
    x_expand2 <- x_expand2[order(x_expand2[,"Fund_ID"],x_expand2[,"yr"],x_expand2[,"month"]),] 
    row.names(x_expand2) <- seq(nrow(x_expand2))
    
    x_expand2_cast_id_cols <- c("Fund_ID","yr","month")
    x_expand2_cast_other_char_cols0 <- colnames(x_expand2)[which(sapply(x_expand2,class)=="character")]
    x_expand2_cast_other_char_cols <- x_expand2_cast_other_char_cols0[!(x_expand2_cast_other_char_cols0 %in% x_expand2_cast_id_cols)]
    x_expand2_cast_other_num_cols0 <- colnames(x_expand2)[!(colnames(x_expand2) %in% c(x_expand2_cast_id_cols,x_expand2_cast_other_char_cols))]
    x_expand2_cast_other_num_cols <- x_expand2_cast_other_num_cols0
    
    rm(x_expand2_cast_other_char_cols0,x_expand2_cast_other_num_cols0)
    
    x_expand2_cast0 <- reshape(x_expand2, direction="wide",idvar=c("yr","month"),timevar=c("Fund_ID"))
    x_expand2_cast0 <- x_expand2_cast0[,c("yr","month",sort(colnames(x_expand2_cast0)[!(colnames(x_expand2_cast0) %in% c("yr","month"))]))]
    
    x_expand2_cast0 <- x_expand2_cast0[order(x_expand2_cast0[,"yr"],
                                             x_expand2_cast0[,"month"]),] 
    row.names(x_expand2_cast0) <- seq(nrow(x_expand2_cast0))
    
    x_expand2_cast <- merge(x_expand,x_expand2_cast0, 
                            by.x=c("yr","month"), by.y=c("yr","month"), 
                            all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
    
    x_expand2_cast <- x_expand2_cast[order(x_expand2_cast[,"yr"],x_expand2_cast[,"month"]),] 
    row.names(x_expand2_cast) <- seq(nrow(x_expand2_cast))
    
    rm(x_expand,x_expand2_cast0)
    
    
    #### Get na counts and averages ####
    
    x_expand2_na_count <- data.table(x_expand2)  
    x_expand2_na_count <- x_expand2_na_count[,na_count_monthly_h := rowSums(is.na(.SD)),.SDcols=colnames(x_expand2_na_count)]
    x_expand2_na_count <- as.data.frame(x_expand2_na_count,stringsAsFactors=FALSE)
    
    x_expand2_cast_compare_stats0 <- ddply(.data=x_expand2_na_count[,c("Fund_ID",returns_data_cols,"na_count_monthly_h")], .variables=c("Fund_ID"), .fun = function(x){
      
      # x <- x_expand2_na_count[(x_expand2_na_count[,"Fund_ID"]==5401),c("Fund_ID",returns_data_cols,"na_count_monthly_h")]
      
      x_out0 <- alply(.data=x[,!(colnames(x) %in% c("Fund_ID"))], .margins=2, .fun = function(w){
        return(data.frame(mean(unlist(w),na.rm=TRUE),stringsAsFactors=FALSE))
      }, .expand = FALSE)
      
      x_out <- do.call(cbind,x_out0)
      colnames(x_out) <- paste("mean",colnames(x)[!(colnames(x) %in% c("Fund_ID"))],sep="_")
      
      return(x_out)
    })
    
    x_expand2_cast_compare_stats1a <- ddply(.data=x_expand2_na_count, .variables=c("Fund_ID"), .fun = function(x){
      
      # x <- x_expand2_na_count[(x_expand2_na_count[,"Fund_ID"]==5401),]
      
      x_out0 <- alply(.data=x[,!(colnames(x) %in% c("Fund_ID","yr","month"))], .margins=2, .fun = function(w){
        return(data.frame(sum(is.na(unlist(w)),na.rm=FALSE)/nrow(w),stringsAsFactors=FALSE))
      }, .expand = FALSE)
      
      x_out <- do.call(cbind,x_out0)
      colnames(x_out) <- paste("percent_missing",colnames(x)[!(colnames(x) %in% c("Fund_ID","yr","month"))],sep="_")
      
      return(x_out)
    })
    x_expand2_cast_compare_stats1b <- ddply(.data=x_expand2_cast_compare_stats1a, .variables=c("Fund_ID"), .fun = function(x){
      
      # x <- x_expand2_cast_compare_stats1a[(x_expand2_cast_compare_stats1a[,"Fund_ID"]==5401),]
      
      return(data.frame(mean_percent_missing=rowMeans(x[,!(colnames(x) %in% c("Fund_ID"))], na.rm = TRUE),stringsAsFactors=FALSE))
    })
    
    x_expand2_cast_compare_stats_cast <- merge(x_expand2_cast_compare_stats0,x_expand2_cast_compare_stats1b, 
                                               by.x=c("Fund_ID"), by.y=c("Fund_ID"), 
                                               all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
    
    rm(x_expand2_cast_compare_stats0,x_expand2_cast_compare_stats1a,x_expand2_cast_compare_stats1b)
    
    
    #### Get correlation of numeric variables ####
    
    x_expand2_cast_compare_num0 <- llply(.data=x_expand2_cast_other_num_cols, .fun = function(y,data){
      
      # y <- x_expand2_cast_other_num_cols[1]
      # y <- x_expand2_cast_other_num_cols[2]
      # data <- x_expand2_cast
      
      #cat(y, "\n")
      
      #data_temp <- data[,c(colnames(data)[grep(y,colnames(data))])]
      #data_temp <- data[,c(colnames(data)[!grepl(paste("^((?!",y,").*)$",sep=""), colnames(data),perl=TRUE)])]
      data_temp <- data[,colnames(data)[which(gsub("([[:digit:]]|\\.)","",colnames(data))==y)]]
      
      #data_trim_counts <- adply(data_temp, 1, function(x){length(which(!is.na(unique(as.vector(unlist(x))))))})
      #colnames(data_trim_counts) <- c(colnames(data_temp),paste("unique_count",y,sep="_"))
      
      #corr_input <- as.data.frame(expand.grid(method=c("pearson","spearman","kendall"),use=c("complete.obs","pairwise.complete.obs")),stringsAsFactors=FALSE)
      corr_input <- as.data.frame(expand.grid(method=c("pearson","spearman","kendall"),use=c("pairwise.complete.obs")),stringsAsFactors=FALSE)
      corr_input[sapply(corr_input,is.factor)] <- lapply(corr_input[sapply(corr_input,is.factor)], as.character)
      
      corr_out <- ddply(.data=corr_input, .variables=c("method","use"), .fun = function(z,data_temp){
        suppressWarnings(corr_temp <- cor(data_temp, use=z[,"use"], method=z[,"method"]))
        diag(corr_temp) <- NA
        return(data.frame(type=row.names(corr_temp),corr_temp,stringsAsFactors=FALSE))
      }, data_temp=data_temp)
      
      rm(corr_input,data_temp)
      
      corr_out_summary <- adply(.data=corr_out[,!(colnames(corr_out) %in% colnames(corr_out)[which(sapply(corr_out,class)=="character")])], .margins=2, .fun = function(w){
        suppressWarnings(mean_temp <- mean(unlist(w),na.rm=TRUE))
        suppressWarnings(median_temp <- median(unlist(w),na.rm=TRUE))
        suppressWarnings(max_temp <- max(unlist(w),na.rm=TRUE))
        suppressWarnings(min_temp <- min(unlist(w),na.rm=TRUE))
        return(data.frame(mean_corr=mean_temp,median_corr=median_temp,max_corr=max_temp,min_corr=min_temp,stringsAsFactors=FALSE))
      }, .expand = TRUE)
      
      colnames(corr_out_summary) <- c("Fund_ID","mean_corr","median_corr","max_corr","min_corr")
      
      rm(corr_out)
      
      for (i in 1:ncol(corr_out_summary))
      {
        corr_out_summary[,i] <- unknownToNA(corr_out_summary[,i], unknown=unknowns_strings,force=TRUE)
        corr_out_summary[,i] <- ifelse(is.na(corr_out_summary[,i]),NA, corr_out_summary[,i])
        corr_out_summary[,i] <- ifelse(is.nan(corr_out_summary[,i]),NA, corr_out_summary[,i])
        corr_out_summary[,i] <- ifelse(is.infinite(corr_out_summary[,i]),NA, corr_out_summary[,i])
      } 
      rm(i)
      
      corr_out_summary[,"Fund_ID"] <- gsub("[^[:digit:]]","",corr_out_summary[,"Fund_ID"])
      corr_out_summary[,"Fund_ID"] <- as.integer(corr_out_summary[,"Fund_ID"])
      
      corr_out_summary2 <- data.frame(type=y,corr_out_summary,stringsAsFactors=FALSE)
      
      rm(corr_out_summary)
      
      corr_out_summary2 <- corr_out_summary2[order(corr_out_summary2[,"Fund_ID"]),] 
      row.names(corr_out_summary2) <- seq(nrow(corr_out_summary2))
      
      return(corr_out_summary2)
      
    }, data=x_expand2_cast, .progress = "none")
    
    x_expand2_cast_compare_num <- do.call(rbind,x_expand2_cast_compare_num0)
    
    x_expand2_cast_compare_num_cast <- reshape(x_expand2_cast_compare_num, direction="wide",idvar=c("Fund_ID"),timevar=c("type"))
    #x_expand2_cast_compare_num_cast_final <- data.frame(x_expand2_cast_compare_num_cast,avg_corr.overall=NA,stringsAsFactors=FALSE)
    #x_expand2_cast_compare_num_cast_final[,"avg_corr.overall"] <- rowMeans(x_expand2_cast_compare_num_cast[,c(colnames(x_expand2_cast_compare_num_cast)[grep("avg_corr",colnames(x_expand2_cast_compare_num_cast))])], na.rm = TRUE)
    
    rm(x_expand2_cast_compare_num0,x_expand2_cast_compare_num)
    
    
    #### Get 'correlation' of character variables ####
    
    x_expand2_cast_compare_char0 <- llply(.data=x_expand2_cast_other_char_cols, .fun = function(y,data){
      
      # y <- x_expand2_cast_other_char_cols[1]
      # y <- x_expand2_cast_other_char_cols[5]
      # y <- x_expand2_cast_other_char_cols[14]
      # data <- x_expand2_cast
      
      #data_temp <- data[,colnames(data)[grep(y,colnames(data))]]
      #data_temp <- data[,colnames(data)[!grepl(paste("^((?!",y,").*)$",sep=""), colnames(data),perl=TRUE)]]
      data_temp <- data[,colnames(data)[which(gsub("([[:digit:]]|\\.)","",colnames(data))==y)]]
      
      data_trim_comp <- ldply(colnames(data_temp), function(j,data_temp){
        
        # j <- colnames(data_temp)[1]
        
        data_trim_single <- data_temp[,colnames(data_temp)[(colnames(data_temp) %in% j)]]
        data_trim_single <- data.frame(temp=data_trim_single,stringsAsFactors=FALSE)
        colnames(data_trim_single)[match("temp",names(data_trim_single))] <- j
        
        data_trim_other <- data_temp[,colnames(data_temp)[!(colnames(data_temp) %in% j)]]
        data_trim_other <- as.data.frame(data_trim_other,stringsAsFactors=FALSE)
        colnames(data_trim_other) <- colnames(data_temp)[!(colnames(data_temp) %in% j)]
        
        data_trim_comp_temp <- adply(data_trim_other, 2, function(k,data_trim_single){
          
          # k <- data_trim_other[,1]
          
          return(unique(mean(ifelse(k==data_trim_single[,1],1,0),na.rm=TRUE)))
          
        },data_trim_single=data_trim_single,.expand=TRUE)
        
        return(data_trim_comp_temp_out <- data.frame(type=j,data_trim_comp_temp,stringsAsFactors=FALSE))
        
      },data_temp=data_temp)
      
      rm(data_temp)
      
      comp_out_summary <- reshape(data_trim_comp, direction="wide",idvar=c("X1"),timevar=c("type"))
      
      rm(data_trim_comp)
      
      colnames(comp_out_summary)[1] <- c("Fund_ID")
      
      for (i in 1:ncol(comp_out_summary))
      {
        comp_out_summary[,i] <- unknownToNA(comp_out_summary[,i], unknown=unknowns_strings,force=TRUE)
        comp_out_summary[,i] <- ifelse(is.na(comp_out_summary[,i]),NA, comp_out_summary[,i])
        comp_out_summary[,i] <- ifelse(is.nan(comp_out_summary[,i]),NA, comp_out_summary[,i])
        comp_out_summary[,i] <- ifelse(is.infinite(comp_out_summary[,i]),NA, comp_out_summary[,i])
      } 
      rm(i)
      
      comp_out_summary[,"Fund_ID"] <- gsub("[^[:digit:]]","",comp_out_summary[,"Fund_ID"])
      comp_out_summary[,"Fund_ID"] <- as.integer(comp_out_summary[,"Fund_ID"])
      
      comp_out_summary2 <- data.frame(type=y,comp_out_summary,stringsAsFactors=FALSE)
      
      rm(comp_out_summary)
      
      comp_out_summary2 <- comp_out_summary2[order(comp_out_summary2[,"Fund_ID"]),] 
      row.names(comp_out_summary2) <- seq(nrow(comp_out_summary2))
      
      colnames(comp_out_summary2) <- gsub("V1.","",colnames(comp_out_summary2))
      
      comp_out_summary2 <- comp_out_summary2[,c("type","Fund_ID",
                                                sort(colnames(comp_out_summary2)[!(colnames(comp_out_summary2) %in% c("type","Fund_ID"))]))]
      
      comp_out_summary2 <- comp_out_summary2[,c("type","Fund_ID",paste(comp_out_summary2[,"type"],comp_out_summary2[,"Fund_ID"],sep="."))]
      
      comp_out_summary3 <- ddply(.data=comp_out_summary2,.variables=c("type","Fund_ID"), .fun = function(n){data.frame(mean_comp=rowMeans(n[,!(colnames(n) %in% c("type","Fund_ID"))], na.rm = TRUE),stringsAsFactors=FALSE)})
      
      rm(comp_out_summary2)
      
      return(comp_out_summary3)
      
    }, data=x_expand2_cast, .progress = "none")
    
    x_expand2_cast_compare_char <- do.call(rbind,x_expand2_cast_compare_char0)
    
    x_expand2_cast_compare_char_cast <- reshape(x_expand2_cast_compare_char, direction="wide",idvar=c("Fund_ID"),timevar=c("type"))
    
    rm(x_expand2_cast_compare_char0,x_expand2_cast_compare_char)
    
    rm(x_expand2_na_count,x_expand2,x_expand2_cast)
    rm(x_expand2_cast_id_cols,x_expand2_cast_other_char_cols,x_expand2_cast_other_num_cols)
    
    
    #### Determine keep and drop IDs ####
    ### only using x_expand2_cast_compare_stats_cast -- keep fund with most data
    
    ids_out0 <- data.frame(x_expand2_cast_compare_stats_cast,keep_flag=NA,stringsAsFactors=FALSE)
    
    ids_out0 <- ids_out0[order(ids_out0[,"mean_percent_missing"],
                               ids_out0[,"mean_na_count_monthly_h"],
                               ids_out0[,"mean_AUM"],
                               ids_out0[,"mean_Monthly_Ret"]),] 
    row.names(ids_out0) <- seq(nrow(ids_out0))
    
    ids_out0[,"keep_flag"] <- ifelse(as.numeric(rownames(ids_out0))==1,1,0)
    
    # ids_out0[,"keep_flag"] <- ifelse(ids_out0[,"mean_percent_missing"]==min(ids_out0[,"mean_percent_missing"]),1,0)
    # if (sum(ids_out0[,"keep_flag"])>1) {ids_out0[,"keep_flag"] <- ifelse(ids_out0[,"mean_AUM"]==max(ids_out0[,"mean_AUM"]),1,0)}
    
    ids_out <- ids_out0[,c("Fund_ID","keep_flag")]
    
    rm(x_expand2_cast_compare_stats_cast,x_expand2_cast_compare_num_cast,x_expand2_cast_compare_char_cast)
    rm(ids_out0)
    
  }
  return(ids_out)
  
}, .progress = "text")


sample_data_all_remove_duplicates2_multiple_drop_ids <- sort(unique(sample_data_all_remove_duplicates2_multiple_all_ids[sample_data_all_remove_duplicates2_multiple_all_ids[,"keep_flag"]==0,"Fund_ID"]))

sample_data_all_remove_duplicates2_multiple_keep_ids <- sort(unique(sample_data_all_remove_duplicates2_multiple_all_ids[!(sample_data_all_remove_duplicates2_multiple_all_ids[,"Fund_ID"] %in% sample_data_all_remove_duplicates2_multiple_drop_ids),c("Fund_ID")]))

sample_data_all_remove_duplicates2_keep_ids <- sample_data_all_remove_duplicates2_all_ids[sample_data_all_remove_duplicates2_all_ids %in% c(sample_data_all_remove_duplicates2_single_ids,sample_data_all_remove_duplicates2_multiple_keep_ids)]
sample_data_all_remove_duplicates2_keep <- sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates2_keep_ids,]

sample_data_all_remove_duplicates2_drop_ids <- sample_data_all_remove_duplicates2_all_ids[!(sample_data_all_remove_duplicates2_all_ids %in% c(sample_data_all_remove_duplicates2_single_ids,sample_data_all_remove_duplicates2_multiple_keep_ids))]
sample_data_all_remove_duplicates2_drop <- sample_data_all_strategy_count2_merge_full[sample_data_all_strategy_count2_merge_full[,"Fund_ID"] %in% sample_data_all_remove_duplicates2_drop_ids,]

sample_data_all_remove_duplicates2_comb <- sample_data_all_remove_duplicates2_keep

sample_data_all_remove_duplicates2_comb <- sample_data_all_remove_duplicates2_comb[order(sample_data_all_remove_duplicates2_comb[,"Strat_ID"],
                                                                                         sample_data_all_remove_duplicates2_comb[,"Fund_ID"]),] 
row.names(sample_data_all_remove_duplicates2_comb) <- seq(nrow(sample_data_all_remove_duplicates2_comb))

sample_data_all_remove_duplicates2_comb <- sample_data_all_remove_duplicates2_comb[,!(colnames(sample_data_all_remove_duplicates2_comb) %in% c("Strat_ID","freq_strategy2"))]

rm2(sample_data_all_remove_duplicates2_all_ids,sample_data_all_remove_duplicates2_multiple_ids,sample_data_all_remove_duplicates2_single_ids)
rm2(sample_data_all_remove_duplicates2_multiple_all_ids,sample_data_all_remove_duplicates2_multiple_keep_ids,sample_data_all_remove_duplicates2_multiple_drop_ids)
rm2(sample_data_all_remove_duplicates2_keep_ids,sample_data_all_remove_duplicates2_drop_ids)
rm2(sample_data_all_remove_duplicates2_keep,sample_data_all_remove_duplicates2_drop)
rm2(sample_data_all_remove_duplicates2_multiple)
#rm2(sample_data_all_strategy_count2_merge_full)


###############################################################################
cat("SECTION: REMOVE STRATEGIES WITH LESS THAN 100 WORDS", "\n")
###############################################################################

sample_data_all_remove_less_than_100_words <- sample_data_all_remove_duplicates2_comb

sample_data_all_remove_less_than_100_words <- data.table(sample_data_all_remove_less_than_100_words)
setkeyv(sample_data_all_remove_less_than_100_words,NULL)

for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("Strategy")), value=gsub(" {2,}", " ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("Strategy"))]], perl=TRUE))
}
rm2(a)

sample_data_all_remove_less_than_100_words[,c("temp_text_no_space","temp_text_no_punct","temp_text_no_punct_no_space","temp_count_no_space","temp_count_no_punct_no_space"):=list(Strategy,Strategy,Strategy,NA_integer_,NA_integer_)]

#Remove spaces in column temp_text_no_space
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_space")), value=gsub(" ", "", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_space"))]], perl=TRUE))
}
rm2(a)

#Remove punct in column temp_text_no_punct
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("[[:digit:]]+", " ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
} 
rm2(a)  
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("-+", "", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("'+", "", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
}
rm2(a)
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("\\.+", ".", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("\\?+", "?", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("!+", "!", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub(":+", ":", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub(";+", ";", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
}
rm2(a)
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("\\.", ". ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("\\?", "? ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("!", "! ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub(":", ": ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub(";", "; ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
}
rm2(a)
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub("[^[:alpha:] ]", " ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
} 
rm2(a) 
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct")), value=gsub(" {2,}", " ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]], perl=TRUE))
}
rm2(a)

#Remove punct and space in column temp_text_no_punct_no_space
for (a in 1:5)
{
  #a <- 1
  set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct_no_space")), value=gsub("[^[:alpha:]]+", "", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct_no_space"))]], perl=TRUE))
}
rm2(a)
# for (a in 1:5)
# {
#   #a <- 1
#   set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct_no_space")), value=gsub(" {2,}", " ", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct_no_space"))]], perl=TRUE))
# }
# rm2(a)
# 
# for (a in 1:5)
# {
#   #a <- 1
#   set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_count_no_punct_no_space")), value=gsub(" ", "", sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_count_no_punct_no_space"))]], perl=TRUE))
# }
# rm2(a)

# Get counts
set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_count_no_space")), value=nchar(sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("Strategy"))]])-nchar(sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_space"))]]))
set(sample_data_all_remove_less_than_100_words, i=NULL, j=which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_count_no_punct_no_space")), value=nchar(sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct"))]])-nchar(sample_data_all_remove_less_than_100_words[[which(colnames(sample_data_all_remove_less_than_100_words)==c("temp_text_no_punct_no_space"))]]))


# aa <- sample_data_all_remove_less_than_100_words[,c("temp_count_no_space"),with=FALSE]
# setorderv(aa, c("temp_count_no_space"),c(1))
# aa_count <- count(as.data.frame(aa),"temp_count_no_space")
# aa_count <- data.frame(aa_count,cum_sum=cumsum(aa_count[,"freq"]),cum_percent=NA)
# aa_count[,"cum_percent"] <- aa_count[,"cum_sum"]/max(aa_count[,"cum_sum"])

# bb <- sample_data_all_remove_less_than_100_words[,c("temp_count_no_punct_no_space"),with=FALSE]
# setorderv(bb, c("temp_count_no_punct_no_space"),c(1)) 
# bb_count <- count(as.data.frame(bb),"temp_count_no_punct_no_space")
# bb_count <- data.frame(bb_count,cum_sum=cumsum(bb_count[,"freq"]),cum_percent=NA)
# bb_count[,"cum_percent"] <- bb_count[,"cum_sum"]/max(bb_count[,"cum_sum"])


#sample_data_all_remove_less_than_100_words_trim_length <- sample_data_all_remove_less_than_100_words[,][temp_count_no_space>100][,c("temp_text","temp_count_no_space") := NULL]

setorderv(sample_data_all_remove_less_than_100_words, c("Fund_ID","yr"),c(1,1))

sample_data_all_remove_less_than_100_words <- as.data.frame(sample_data_all_remove_less_than_100_words,stringsAsFactors=FALSE)

sample_data_all_trim_length_bad0 <- sample_data_all_remove_less_than_100_words[(sample_data_all_remove_less_than_100_words[,"temp_count_no_space"]<100 | 
                                                                                  sample_data_all_remove_less_than_100_words[,"temp_count_no_punct_no_space"]<100),]

sample_data_all_trim_length_bad <- sample_data_all_trim_length_bad0[,!(colnames(sample_data_all_trim_length_bad0) %in% c("temp_text_no_space","temp_text_no_punct","temp_text_no_punct_no_space","temp_count_no_space","temp_count_no_punct_no_space"))]

#sample_data_all_trim_length <- sample_data_all_remove_less_than_100_words
#sample_data_all_trim_length <- sample_data_all_trim_length[,][temp_count_no_space>100][temp_count_no_punct_no_space>100]
#sample_data_all_trim_length <- sample_data_all_trim_length[,][,c("temp_text_no_space","temp_text_no_punct","temp_text_no_punct_no_space","temp_count_no_space","temp_count_no_punct_no_space"):=NULL]

rm2(sample_data_all_remove_duplicates2_comb)
rm2(sample_data_all_remove_less_than_100_words)

invisible(gc(verbose = FALSE, reset = TRUE))


###############################################################################
cat("SECTION: OUTPUT", "\n")
###############################################################################

data_out_all_ids <- unique(sample_data_all_remove_less_than_100_words[,"Fund_ID"])
data_out_drop_ids <- data_out_all_ids[data_out_all_ids %in% unique(sample_data_all_trim_length_bad[,"Fund_ID"])]
data_out_keep_ids <- data_out_all_ids[!(data_out_all_ids %in% data_out_drop_ids)]

#data_out <- unique(sample_data_all_trim_length_bad[,colnames(sample_data_all)])
data_out <- sample_data_all[sample_data_all[,"Fund_ID"] %in% data_out_keep_ids,]

write.csv(data_out,file=paste(output_directory,"text_clean_trim.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(data_out)
invisible(gc(verbose = FALSE, reset = TRUE))


#length(sort(unique(sample_data_all[,"Fund_ID"])))
#length(sort(unique(sample_data_all_trim_length_bad0[,"Fund_ID"])))
#length(sort(unique(sample_data_all_trim_length[,"Fund_ID"])))
#length(sort(unique(data_out[,"Fund_ID"])))

#aa <- as.data.frame(sample_data_all_remove_less_than_100_words,stringsAsFactors=FALSE)
#aa2 <- aa[aa[,"Fund_ID"] %in% c(5056,5058,5119,5135,5144),]
#bb <- data_out[data_out[,"Fund_ID"] %in% c(5056,5058,5119,5135,5144),]


