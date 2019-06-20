source('./46_important_statements_prepare.R')

col <- brewer.pal(n=5,'BrBG')

likert_perc <- ggplot(data=df_46_perc_melt, aes(x=variable, y=value, fill=Importance)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() 

likert_abs <- ggplot(data=df_46_freq_melt, aes(x=variable, y=value, fill=Importance)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  theme_light() 
