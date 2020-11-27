df_32_freq <- df_32_freq[c(-1,-2),]

# Barplot of reasons why to not use some data formats
bp_32 <- ggplot(data=df_32_freq,aes(x=data.use.constraint, y=freq, fill=data.use.constraint)) +
  geom_bar(stat='identity', width=0.7) +
  coord_flip()+
  labs(x="Constraint", y="Number of users") +
  scale_x_discrete(labels=wrap_format(20), position='right')+
  scale_fill_uchicago(palette='light') +
  theme_light() +
  theme(legend.position='none', axis.text.x=element_text(size=14),axis.text.y=element_text(size=14))



