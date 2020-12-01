# Rephrase the statements for plotting
statements <- c('The geographic location of the cloud server is very important',
                'I would like to work privately on a cloud and the security is very important',
                'I would like to work on the cloud in a collaborative way',
                'I need to transfer intermediary or final results out of the cloud',
                'I need to upload own datasets and combine them with datasets on the cloud')

# Change the statements and reorder the factors based on the frequency
df_64_freq_ord$Statement <- statements
df_64_freq_ord <- df_64_freq_ord %>%
  arrange(freq) %>% 
  mutate(Statement = factor(Statement, unique(Statement)))

# Horizontal bar plot
barplot_64_freq <- ggplot(df_64_freq_ord, aes(fill=Statement,y=freq, x=Statement)) + 
  geom_bar(stat="identity",width=0.5) +
  scale_fill_uchicago(palette='light', alpha=0.7) +
  coord_flip() +
  labs(x="Statement", y="n") +
  scale_x_discrete(labels = wrap_format(25)) +
  ylim(0,200) +
  geom_text(aes(y=freq, label=freq), size=5, hjust=1.5, color='black') +
  geom_label(y=198, aes(label=round(perc,1)), size=5) +
  theme_light() +
  theme(legend.title=element_blank(),
        legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=1)
