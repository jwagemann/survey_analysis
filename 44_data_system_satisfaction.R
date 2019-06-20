source('./44_data_system_satisfaction_prepare.R')

col <- brewer.pal(n=5,'BrBG')

likert_perc <- ggplot(data=df_44_merged_melt, aes(x=variable, y=value, fill=satisfaction)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() 

likert_abs <- ggplot(data=df_44_merged_abs_melt, aes(x=variable, y=value, fill=satisfaction)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  theme_light() 