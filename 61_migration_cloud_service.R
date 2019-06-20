source('dummies_prepare.R')

df_61_freq$motivation <- c('Motivation')
df_61_perc$motivation <- c('Motivation')

likert_perc <- ggplot(data=df_61_perc, aes(x=motivation,y=freq, fill=Interest)) +
  geom_bar(stat='identity', width=0.5) +
  scale_fill_manual(values=col) +
  coord_flip() +
  theme_light() +
  theme(aspect.ratio = 1/3)
