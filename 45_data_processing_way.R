source('dummies_prepare.R')

df_45_melt <- melt(df_45_freqs[-4,],id.vars = 'Frequency')

col_45 <- brewer.pal(n=3,'Blues')

likert_perc <- ggplot(data=df_45_melt, aes(x=variable, y=value, fill=Frequency)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col_45) +
  coord_flip() +
  theme_light() 
