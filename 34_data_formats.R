df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

# What data formats do you use or you want to use in your data analysis tasks?
df_34 <- df_new[,'X3.4']
df_34 <- data.frame(df_34)

df_34_other <- df_new[,'X3.4.1']

test_formats_cols <- unique(unlist(strsplit(df_formats$df_formats, ";", fixed = TRUE)))

dummies_formats <- sapply(test_formats_cols, function(co)grepl(co,df_formats$df_formats))

df_sum_1 <- test %>% count(format1)
df_sum_2 <- test %>% count(format2)
df_sum_3 <- test %>% count(format3)
df_sum_4 <- test %>% count(format4)
df_sum_5 <- test %>% count(format5)
df_sum_6 <- test %>% count(format6)
df_sum_7 <- test %>% count(format7)
df_sum_8 <- test %>% count(format8)
df_sum_9 <- test %>% count(format9)

colnames(df_sum_1)<- c('format', 'n')
colnames(df_sum_2)<- c('format', 'n')
colnames(df_sum_3)<- c('format', 'n')
colnames(df_sum_4)<- c('format', 'n')
colnames(df_sum_5)<- c('format', 'n')
colnames(df_sum_6)<- c('format', 'n')
colnames(df_sum_7)<- c('format', 'n')
colnames(df_sum_8)<- c('format', 'n')
colnames(df_sum_9)<- c('format', 'n')

df_sum_final <- Reduce(function(x,y) merge(x,y, all=TRUE), list(df_sum_1, df_sum_2, df_sum_3, df_sum_4, df_sum_5, df_sum_6, df_sum_7, df_sum_8, df_sum_9))
df_final <- dplyr::group_by(df_sum_final, format) %>% dplyr::summarise_all(sum)
df_final <- na.omit(df_final)
df_final <- df_final[-10,]


df_final['perc'] <- df_final$n / no_of_respondents


test
