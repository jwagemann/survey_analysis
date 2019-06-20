source('51_data_challenges_prepare.R')

col <- brewer.pal(n=5,'BrBG')

likert_perc <- ggplot(data=df_51_perc_melt, aes(x=variable, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() 

likert_abs <- ggplot(data=df_51_freq_melt, aes(x=variable, y=reorder(-value), fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=-col) +
  theme_light() 
