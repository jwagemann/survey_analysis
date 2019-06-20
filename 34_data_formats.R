source('dummies_prepare.R')

# What data formats do you use or you want to use in your data analysis tasks?
df_34_freq <- df_34_freq[c(-10,-12),]
df_34_order <- df_34_freq %>%
  arrange(-perc) %>% 
  mutate(data.format = factor(data.format, unique(data.format)))

myPalette <- brewer.pal(9,'Blues')
test = colorRampPalette(myPalette)(11)

ggplot(df_34_order, aes(y=perc, x=data.format,fill=data.format, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Data format", y="Percent") +
  scale_fill_brewer(palette='Spectral') + 
  ylim(0,80) +
  theme_light() +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))

