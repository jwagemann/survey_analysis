# Creates a dummy table of 0 and 1s
getDummies <- function(df,colNo){
  df_cols <- unique(unlist(strsplit(as.character(df[,colNo]), ";", fixed = TRUE)))
  dummies <- sapply(df_cols, function(co)grepl(co,df[,colNo], fixed=TRUE))
  return(dummies)
}

# Create crosstable 
createCrossTab <- function(df_1,df_2,rowNamesVec){
  tmp <- data.frame(df_1,df_2)
  print(length(tmp))
  
  tmp2 <- colSums(df_2)
  print(tmp2)
  count1 <- length(df_1)
  print(count1)
  for(i in 1:count1){
    print(i)
    df_subset <- subset(tmp[,c(i,(count1+1):length(tmp))], (!is.na(tmp[,i])))
    tmp2 <- rbind(tmp2,colSums(df_subset[,2:length(df_subset)]))
    print(tmp2)
  }
  print(tmp2)
  rownames(tmp2) <- rowNamesVec
  return(tmp2)
}


# Creates the crosstable of two data frames
getCrossTabMelt <- function(df_1, df_2, colNamesVec){
  # Get Number of columns
  ncol_1 <- ncol(df_1)
  # Bring both dfs into one data frame
  df_crosstab <- data.frame(df_1,df_2)
  # Get number of columns of merged df
  ncol_2 <- ncol(df_crosstab)
  
  # get the sums of all responses for each column
  crosstab_final <- colSums(df_2)

  
  # Initiate vector for total number of responses for each category
  sum_vec <- c(nrow(df_2))
  
  # for each column create a subset and add the sums as row to the crosstab_final data frame
  for (i in 1:ncol_1){
    print(i)
    print(typeof(df_1))
    if(typeof(df_1)=='list'){
      df_subset <- subset(df_crosstab[,c(i,(ncol_1+1):ncol_2)],(!is.na(df_crosstab[,i])))     
    } else {
      df_subset <- subset(df_crosstab[,c(i,(ncol_1+1):ncol_2)],(df_crosstab[,i]==1))
      print(df_subset)
    }

    crosstab_final <- rbind(crosstab_final, colSums(df_subset[,2:ncol(df_subset)], na.rm=TRUE))
    assign(paste('df_subset',i,sep='_'),df_subset)
    # Build up a vector with the number of responses for each subset along the way
    sum_vec <- c(sum_vec, nrow(df_subset))
    
  }
  crosstab_final <- cbind(crosstab_final,sum_vec)
  print(sum_vec)
  # convert it to a data frame
  crosstab_final <- as.data.frame(crosstab_final)
  # set row names
  rownames(crosstab_final) <- c('all',colNamesVec)
  
  # # Get column names
  colNames = colnames(crosstab_final)
  # # For each column, build the equivalent percent based on the sum vector
  for(col in 1:ncol(crosstab_final)) {
    colname = paste(colNames[col], 'perc', sep='_')
    print(crosstab_final$sum_vec)
    crosstab_final[,colname] <- crosstab_final[,col] / crosstab_final$sum_vec 
  }

  print(crosstab_final)
  return(crosstab_final)
}

df_melt <- function(df){
  # Add row names as category column
  df$cat <- rownames(df)
  # Melt the data frame into one columns based on the category column
  df_melt <- reshape2::melt(df,id='cat')
  return(df_melt)  
}

# # Add row names as category column
# crosstab_final$cat <- rownames(crosstab_final)
# # Melt the data frame into one columns based on the category column
# crosstab_final_melt <- reshape2::melt(crosstab_final,id='cat')
# return(crosstab_final_melt) 

# Splits all the entries of one column into multiple rows and return the summary table of the columns entries
splitInRows <- function(df_subset, col, noOfRespondents){
  df_split <- separate_rows(df_subset, col, sep=';')
  df_freq <- plyr::count(df_split)
  df_freq$perc <- df_freq$freq / noOfRespondents * 100
  return(df_freq)
}
