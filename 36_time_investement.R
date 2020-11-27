
# Barplot of 'motivation to invest time' to use an unfamiliar data format
ggplot(df_36_freq[-1,], aes(y=freq, x=x,fill=x, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Motivation to invest time", y="Percent") +
  scale_fill_brewer(palette='Blues',direction = -1) +
  theme_light() +
  ylim(0,180) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = wrap_format(5))
