df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

# What hindered you to work with the data you would like to use in the future so far?
df_33 <- df_new[,'X3.3']
df_33 <- data.frame(df_33)

df_33_other <- df_new[,'X3.3.1']

df_33<- data.frame(df_33)
df_33_sum <- df_33 %>% count(df_33)
df_33_sum['perc'] <- df_33_sum$n / no_of_respondents
