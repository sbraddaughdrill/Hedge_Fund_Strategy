input_directory <- normalizePath("F:/Research_temp2/",winslash="\\", mustWork=TRUE)


###############################################################################
cat("CONTINUOUS", "\n")
###############################################################################

files_continuous1 <- list(path=rep(paste(input_directory,"univariate_continuous\\",sep=""), each=3),
                          file_names=c("quantiles_year_pflow_1994_2011_continuous_4",
                                       "quantiles_year_exret_1994_2011_continuous_4",
                                       "quantiles_year_mktadjret_1994_2011_continuous_4"),
                          note=c("pflow","exret","mktadjret"),
                          type=rep("quantiles_year_1994_2011_continuous", each=3))
files_continuous2 <- list(path=rep(paste(input_directory,"univariate_continuous\\",sep=""), each=3),
                          file_names=c("quantiles_year_int_ffm_nonloading_24_1994_2011_continuous_4",
                                       "quantiles_year_int_hf7_nonloading_24_1994_2011_continuous_4",
                                       "quantiles_year_int_hf8_nonloading_24_1994_2011_continuous_4"),
                          note=c("ffm_nonloading","hf7_nonloading","hf8_nonloading"),
                          type=rep("quantiles_year_1994_2011_continuous", each=3))
files_continuous3 <- list(path=rep(paste(input_directory,"univariate_continuous\\",sep=""), each=3),
                          file_names=c("quantiles_year_int_ffm_loading_24_1994_2011_continuous_4",
                                       "quantiles_year_int_hf7_loading_24_1994_2011_continuous_4",
                                       "quantiles_year_int_hf8_loading_24_1994_2011_continuous_4"),
                          note=c("ffm_loading","hf7_loading","hf8_loading"),
                          type=rep("quantiles_year_1994_2011_continuous", each=3))

files_continuous <- list(files_continuous1,files_continuous2,files_continuous3)

rm(files_continuous1,files_continuous2,files_continuous3)

# files_continuous_note <- sapply(files_continuous, "[[", "note")
# files_continuous_names <- sapply(files_continuous, "[[", "file_names")
# 
# files_continuous_names_all <- unlist(files_continuous_names)
# files_continuous_names_all <- data.frame(id=NA,
#                               files_continuous_names_all, 
#                               stringsAsFactors=FALSE)
# files_continuous_names_all[,c("id")] <- seq(nrow(files_continuous_names_all))
# colnames(files_continuous_names_all) <- c("id",paste("file_names",seq(1,ncol(files_continuous_names_all)-1),sep=""))
# 
# files_continuous_names_all <- files_continuous_names_all[order(files_continuous_names_all[,"id"]),]
# row.names(files_continuous_names_all) <- seq(nrow(files_continuous_names_all))

for (l in 1:length(files_continuous))
{
  #l <- 1
  #l <- 2
  
  files_continuous_path_temp <- sapply(files_continuous[l], "[[", "path")
  files_continuous_names_temp <- sapply(files_continuous[l], "[[", "file_names")
  files_continuous_note_temp <- sapply(files_continuous[l], "[[", "note")
  files_continuous_type_temp <- sapply(files_continuous[l], "[[", "type")
  
  files_continuous_comb <- data.frame(files_continuous_path_temp,
                                      files_continuous_names_temp,
                                      files_continuous_note_temp, 
                                      files_continuous_type_temp, 
                                      stringsAsFactors=FALSE)
  colnames(files_continuous_comb) <- c("path","names","note","type")
  
  for (j in 1:nrow(files_continuous_comb))
  {
    #j <- 1
    
    df <- read.csv(file=paste(files_continuous_comb[j,"path"],files_continuous_comb[j,"names"],".csv",sep=""),header=TRUE,na.strings="",stringsAsFactors=FALSE)
    #df1 <- df[,c("cut_var", "X1", "X4", "t_minus_b", "t_p_val")]
    df1 <- data.frame(df[,c("cut_var", "X1", "X4", "t_minus_b")],
                      space="",
                      df[,c("t_p_val")],
                      stringsAsFactors=FALSE)
    
    colnames(df1) <- paste(files_continuous_comb[j,"note"],c("cut_var","X1", "X4", "t_minus_b","space","t_p_val"),sep="_")
    
    if (j==1)
    {
      output <- df1
      
    } else
    {
      output <- cbind(output, df1[,!(colnames(df1) %in% paste(files_continuous_comb[j,"note"],c("cut_var"),sep="_"))])
      
    }
    rm(df,df1)
    
  } 
  write.csv(output,file=paste(unique(files_continuous_comb[,"path"]),"merged\\",unique(files_continuous_comb[,"type"]),"_",paste(files_continuous_comb[,"note"],collapse="_"),".csv",sep=""),
            na="",quote=TRUE,row.names=FALSE)
  
  rm(files_continuous_path_temp,files_continuous_names_temp,files_continuous_note_temp,files_continuous_type_temp)
  rm(files_continuous_comb,output,j)
}
rm(files_continuous,l)


###############################################################################
cat("BINARY", "\n")
###############################################################################

files_binary1 <- list(path=rep(paste(input_directory,"univariate_binary\\",sep=""), each=3),
                      file_names=c("quantiles_year_pflow_1994_2011_binary_2",
                                   "quantiles_year_exret_1994_2011_binary_2",
                                   "quantiles_year_mktadjret_1994_2011_binary_2"),
                      note=c("pflow","exret","mktadjret"),
                      type=rep("quantiles_year_1994_2011_binary", each=3))
files_binary2 <- list(path=rep(paste(input_directory,"univariate_binary\\",sep=""), each=3),
                      file_names=c("quantiles_year_int_ffm_nonloading_24_1994_2011_binary_2",
                                   "quantiles_year_int_hf7_nonloading_24_1994_2011_binary_2",
                                   "quantiles_year_int_hf8_nonloading_24_1994_2011_binary_2"),
                      note=c("ffm_nonloading","hf7_nonloading","hf8_nonloading"),
                      type=rep("quantiles_year_1994_2011_binary", each=3))
files_binary3 <- list(path=rep(paste(input_directory,"univariate_binary\\",sep=""), each=3),
                      file_names=c("quantiles_year_int_ffm_loading_24_1994_2011_binary_2",
                                   "quantiles_year_int_hf7_loading_24_1994_2011_binary_2",
                                   "quantiles_year_int_hf8_loading_24_1994_2011_binary_2"),
                      note=c("ffm_loading","hf7_loading","hf8_loading"),
                      type=rep("quantiles_year_1994_2011_binary", each=3))

files_binary <- list(files_binary1,files_binary2,files_binary3)

rm(files_binary1,files_binary2,files_binary3)

for (l in 1:length(files_binary))
{
  #l <- 1
  #l <- 2
  
  files_binary_path_temp <- sapply(files_binary[l], "[[", "path")
  files_binary_names_temp <- sapply(files_binary[l], "[[", "file_names")
  files_binary_note_temp <- sapply(files_binary[l], "[[", "note")
  files_binary_type_temp <- sapply(files_binary[l], "[[", "type")
  
  files_binary_comb <- data.frame(files_binary_path_temp,
                                  files_binary_names_temp,
                                  files_binary_note_temp, 
                                  files_binary_type_temp, 
                                  stringsAsFactors=FALSE)
  colnames(files_binary_comb) <- c("path","names","note","type")
  
  for (j in 1:nrow(files_binary_comb))
  {
    #j <- 1
    
    df <- read.csv(file=paste(files_binary_comb[j,"path"],files_binary_comb[j,"names"],".csv",sep=""),header=TRUE,na.strings="",stringsAsFactors=FALSE)
    #df1 <- df[,c("cut_var", "X0", "X1", "t_minus_b", "t_p_val")]
    df1 <- data.frame(df[,c("cut_var", "X0", "X1", "t_minus_b")],
                      space="",
                      df[,c("t_p_val")],
                      stringsAsFactors=FALSE)
    
    colnames(df1) <- paste(files_binary_comb[j,"note"],c("cut_var","X0", "X1", "t_minus_b","space","t_p_val"),sep="_")
    
    if (j==1)
    {
      output <- df1
      
    } else
    {
      output <- cbind(output, df1[,!(colnames(df1) %in% paste(files_binary_comb[j,"note"],c("cut_var"),sep="_"))])
      
    }
    rm(df,df1)
    
  } 
  write.csv(output,file=paste(unique(files_binary_comb[,"path"]),"merged\\",unique(files_binary_comb[,"type"]),"_",paste(files_binary_comb[,"note"],collapse="_"),".csv",sep=""),
            na="",quote=TRUE,row.names=FALSE)
  
  rm(files_binary_path_temp,files_binary_names_temp,files_binary_note_temp,files_binary_type_temp)
  rm(files_binary_comb,output,j)
}
rm(files_binary,l)





