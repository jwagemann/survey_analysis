# Creates a dummy table of 0 and 1s
getDummies <- function(df,colNo){
  df_cols <- unique(unlist(strsplit(as.character(df[,colNo]), ";", fixed = TRUE)))
  dummies <- sapply(df_cols, function(co)grepl(co,df[,colNo], fixed=TRUE))
  return(dummies)
}

# Creates the crosstable of two data frames
getCrossTabMelt <- function(df_1, df_2, colNamesVec){
  ncol_1 <- ncol(df_1)
  df_crosstab <- data.frame(df_1,df_2)
  ncol_2 <- ncol(df_crosstab)
  
  df_2_freq <- colSums(df_2)
  if(typeof(df_2)=='logical' | typeof(df_1)=='logical'){
    df_1 <- df_1 *1
    df_2 <- df_2*1
  }
  crosstab_final <- df_2_freq
  for (i in 1:ncol_1){
    print(i)
    if(typeof(df_1)=='list'){
      df_subset <- subset(df_crosstab[,c(i,(ncol_1+1):ncol_2)],(!is.na(df_crosstab[,i])))     
    } else {
      df_subset <- subset(df_crosstab[,c(i,(ncol_1+1):ncol_2)],(df_crosstab[,i]==1))      
    }
    crosstab_final <- rbind(crosstab_final, colSums(df_subset[,2:ncol(df_subset)]))
    assign(paste('df_subset',i,sep='_'),df_subset)
  }
  crosstab_final <- as.data.frame(crosstab_final)
  rownames(crosstab_final) <- c('all',colNamesVec)
  print(crosstab_final)
  crosstab_final$cat <- rownames(crosstab_final)
  crosstab_final_melt <- melt(crosstab_final,id='cat')
  return(crosstab_final_melt)
}

# Splits all the entries of one column into multiple rows and return the summary table of the columns entries
splitInRows <- function(df_subset, col, noOfRespondents){
  df_split <- separate_rows(df_subset,col, sep=';')
  df_freq <- count(df_split)
  df_freq$perc <- df_freq$freq / noOfRespondents * 100
  return(df_freq)
}
