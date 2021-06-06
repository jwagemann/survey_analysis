
# What data formats do you use or you want to use in your data analysis tasks?
df_34_freq <- df_34_freq[c(-10,-12),]
df_34_order <- df_34_freq %>%
  arrange(perc) %>% 
  mutate(data.format = factor(data.format, unique(data.format)))

# Barplot for frequencies in data formats, in a decreasing order
ggplot(df_34_order, aes(y=freq, x=reorder(data.format, -freq),fill=data.format, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="\n Data format", y="n\n") +
  scale_fill_brewer(palette='Spectral') + 
  ylim(0,180)+
  theme_light() +
  geom_label(y=170, aes(label=round(perc,1)),vjust=0, size=6) +  
  geom_text(aes(y=freq, label=freq), color='black', vjust=1.5, size=5)+
  theme(legend.position="none",
        aspect.ratio=1/3,
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=14),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black")) +
  scale_x_discrete(labels = wrap_format(8))

