df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

df_38 <- df_new[, c('X3.8')]

count(df_38)
nrow(na.omit(df_38))     
