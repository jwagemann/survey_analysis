source('./dummies_prepare.R')

df_37_relfreq_melt <- melt(df_37_relfreq,id='cat')
labels <- c(freq='Combination of different data sources', production_value.added.products = 'I produce value-added products based on open datasets', 
            consumption.open.data='I simply consume open data',
            sharing.results = 'I need to share my results with third parties',
            importance.task.parallelistation = 'Parellelisation of my processing tasks is important')
df_37_relfreq_melt$variable2 <- plyr::revalue(df_37_relfreq_melt$variable, labels)



facet_plot <- ggplot(data=df_relfreq_melt, aes(x=cat, y=value)) +
  geom_bar(stat='identity', aes(fill=cat), width=0.7) +
  scale_fill_brewer(palette='Blues') +
  facet_wrap(~ variable2, ncol=2, labeller=labeller(variable2=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))





