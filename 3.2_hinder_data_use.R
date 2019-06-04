library('tidyr')
library('dplyr')
df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

# What hindered you to work with the data you would like to use in the future so far?
df_32 <- df_new[,'X3.2']
df_32 <- data.frame(df_32)

df_32_other <- df_new[,'X3.2.1']

df_32_stacked <- stack(setNames(strsplit(as.character(df_32),';'), df_32))

test <- df_32 %>% separate(df_32,c('reason_1', 'reason_2', 'reason_3', 'reason_4','reason_5', 'reason_6'), ';')

df_sum_1 <- test %>% count(reason_1)
df_sum_2 <- test %>% count(reason_2)
df_sum_3 <- test %>% count(reason_3)
df_sum_4 <- test %>% count(reason_4)
df_sum_5 <- test %>% count(reason_5)
df_sum_6 <- test %>% count(reason_6)

colnames(df_sum_1) <- c('reason', 'n')
colnames(df_sum_2) <- c('reason', 'n')
colnames(df_sum_3) <- c('reason', 'n')
colnames(df_sum_4) <- c('reason', 'n')
colnames(df_sum_5) <- c('reason', 'n')
colnames(df_sum_6) <- c('reason', 'n')


df_sum <- Reduce(function(y,x) merge(y,x, all=TRUE), list(df_sum_1, df_sum_2, df_sum_3, df_sum_4, df_sum_5, df_sum_6))

df_final <- dplyr::group_by(df_sum, reason) %>% dplyr::summarise_all(sum)
df_final <- na.omit(df_final)

sort(df_final,decreasing)
df_final <- df_final[-5,]
bp_21 <- ggplot(data=df_final,aes(x=reorder(reason,n), y=n, fill=reason)) +
  geom_bar(stat='identity') +
  coord_flip()+
  labs(x="Constraint", y="Number of users") +
  scale_x_discrete(labels=wrap_format(15), position='right')+
  scale_fill_uchicago(palette='light') +
  theme_light() +
  theme(legend.position='none', axis.text.x=element_text(size=14),axis.text.y=element_text(size=12))



