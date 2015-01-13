# TODO: Add comment
# 
# Author:  Brad
# File:    Hedge_Fund_Strategy_Clean_Text.R
# Version: 1.0
# Date:    04.28.2013
# Purpose: Clean Hedge fund data
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
                       "sqldf","stringr","tcltk","tm")
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

#Files table
#file_list <- c("EurekahedgeHF_Excel_aca.csv","EurekahedgeHF_Excel_aca_NAV_AUM.csv","EurekahedgeHF_Excel_aca_Instruments_Traded.csv")
file_list <- c("EurekahedgeHF_Profile_Strategy_part3.csv")
files_cols_count <- 2
files_cols <- temp_data_cols[1:files_cols_count,]
files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors=FALSE)
files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors=FALSE)
files <- as.data.frame(matrix(NA, ncol=files_cols_count, nrow=length(file_list)),stringsAsFactors=FALSE)
colnames(files) <- files_cols[,6]
files <- format_function(files,files_cols)


###############################################################################
cat("SECTION: IMPORT DATA", "\n")
###############################################################################

#Check to see if final folder exists.  If not, create it.
final_folder_expand3_path <- paste(normalizePath("F:/Import_Data/Data/Eurekahedge",winslash="\\", mustWork=TRUE), "Final_Expand3", sep = "//", collapse = "//")  
create_directory(final_folder_expand3_path,remove=1)

#files[,"filename"] <-  gsub(".csv","_org.csv",file_list)
files[,"filename"] <-  file_list
files[,"filepath"] <-  unlist(mapply(merge_cols_function,col_one=paste(final_folder_expand3_path,"//",sep=""),col_two=files[,"filename"],separator="", SIMPLIFY=FALSE,USE.NAMES=FALSE))


###############################################################################
cat("SECTION: FORMAT TEXT", "\n")
###############################################################################

sample_data_all <- data.table(read.csv(file=files[1,"filepath"],header=TRUE,na.strings="NA",stringsAsFactors=FALSE))
setkeyv(sample_data_all,NULL)

setorderv(sample_data_all, c("Fund_ID","pull_trim","pull_trim2","yr","month"),c(1,1,1,1,1))

sample_data_all_trim1 <- sample_data_all

rm2(sample_data_all)
invisible(gc(verbose = FALSE, reset = TRUE))

#set(sample_data_all_trim1, j=which(colnames(sample_data_all_trim1) %in% "date"), value=NULL)
#set(sample_data_all_trim1, j=which(colnames(sample_data_all_trim1) %in% "month"), value=NULL)
sample_data_all_trim1 <- sample_data_all_trim1[ ,c("date","month") := NULL]
sample_data_all_trim1 <- unique(sample_data_all_trim1)

setorderv(sample_data_all_trim1, c("Fund_ID","pull_trim","pull_trim2","yr"),c(1,1,1,1))


sample_data_all_trim2 <- sample_data_all_trim1

rm2(sample_data_all_trim1)
invisible(gc(verbose = FALSE, reset = TRUE))

sample_data_all_trim2[,droprow := is.na(Fund_Name)]
sample_data_all_trim2 <- sample_data_all_trim2[!(droprow)][,droprow:=NULL][]


sample_data_all_trim3 <- sample_data_all_trim2

rm2(sample_data_all_trim2)
invisible(gc(verbose = FALSE, reset = TRUE))

sample_data_all_trim3[,droprow := bad_max==1]
sample_data_all_trim3 <- sample_data_all_trim3[!(droprow)][,droprow:=NULL][]

sample_data_all_trim4 <- sample_data_all_trim3

rm2(sample_data_all_trim3)
invisible(gc(verbose = FALSE, reset = TRUE))

#set(sample_data_all_trim4, j=which(colnames(sample_data_all_trim4) %in% "bad_min"), value=NULL)
#set(sample_data_all_trim4, j=which(colnames(sample_data_all_trim4) %in% "bad_max"), value=NULL)
#set(sample_data_all_trim4, j=which(colnames(sample_data_all_trim4) %in% "Manager_Profile"), value=NULL)
sample_data_all_trim4 <- sample_data_all_trim4[ ,c("bad_min","bad_max","Manager_Profile") := NULL]

setkeyv(sample_data_all_trim4, c("Fund_ID","yr"))
sample_data_all_trim4 <- sample_data_all_trim4[, tail(.SD, 1), by=c("Fund_ID","yr")]

for (k in which(sapply(sample_data_all_trim4,class)=="character")) 
{
  set(sample_data_all_trim4, i=NULL, j=k, value=gsub("^\\s+|\\s+$", "", sample_data_all_trim4[[k]], perl=TRUE))
}
rm(k)
for (k in colnames(sample_data_all_trim4)) 
{
  #k <- 1
  set(sample_data_all_trim4, i=NULL, j=k, value=unknownToNA(sample_data_all_trim4[[k]], unknown=unknowns_strings,force=TRUE))
  set(sample_data_all_trim4, i=NULL, j=k, value=ifelse(is.na(sample_data_all_trim4[[k]]),NA,sample_data_all_trim4[[k]]))
}
rm(k)


#Convert to ASCII encoding
for (k in which(sapply(sample_data_all_trim4,class)=="character")) 
{
  set(sample_data_all_trim4, i=NULL, j=k, value=iconv(sample_data_all_trim4[[k]], "latin1", "ASCII", sub=" "))
}
rm(k)
#set(sample_data_all_trim4, i=NULL, j=which(colnames(sample_data_all_trim4)==c("Strategy")), value=iconv(sample_data_all_trim4[[which(colnames(sample_data_all_trim4)==c("Strategy"))]], "latin1", "ASCII", sub=" "))

#Remove multiple spaces (run a couple times)
for (a in 1:5)
{
  #a <- 1
  for (k in which(sapply(sample_data_all_trim4,class)=="character")) 
  {
    set(sample_data_all_trim4, i=NULL, j=k, value=gsub(" {2,}", " ", sample_data_all_trim4[[k]], perl=TRUE))
  }
  rm(k)
  #set(sample_data_all_trim4, i=NULL, j=which(colnames(sample_data_all_trim4)==c("Strategy")), value=gsub(" {2,}", " ", sample_data_all_trim4[[which(colnames(sample_data_all_trim4)==c("Strategy"))]], perl=TRUE))
  
}

#Remove double hyphens
for (a in 1:5)
{
  #a <- 1
  for (k in which(sapply(sample_data_all_trim4,class)=="character")) 
  {
    set(sample_data_all_trim4, i=NULL, j=k, value=gsub("--", "-", sample_data_all_trim4[[k]], perl=TRUE))
  }
  rm(k)
  #set(sample_data_all_trim4, i=NULL, j=which(colnames(sample_data_all_trim4)==c("Strategy")), value=gsub("--", "-", sample_data_all_trim4[[which(colnames(sample_data_all_trim4)==c("Strategy"))]], perl=TRUE))
}

write.csv(sample_data_all_trim4,file=paste(output_directory,"text_clean.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(sample_data_all_trim4)
invisible(gc(verbose = FALSE, reset = TRUE))
