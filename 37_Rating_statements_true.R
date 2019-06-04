df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

df_likert_37 <- df_new[, c('X3.7.Combination.data.sources',
                        'X3.7.Procuction.value.added.products',
                        'X3.7.Consumption.open.data',
                        'X3.7.Sharing.results',
                        'X3.7.Importantance.task.parallelisation')]

df_separated <- separate(df_likert_37,X3.7.Combination.data.sources, sep='/',into='X3.7.Combination.data.sources')
df_separated <- separate(df_separated,X3.7.Procuction.value.added.products, sep='/',into='X3.7.Production.value.added.products')
df_separated <- separate(df_separated,X3.7.Consumption.open.data, sep='/',into='X3.7.Consumption.open.data')
df_separated <- separate(df_separated,X3.7.Sharing.results, sep='/',into='X3.7.Sharing.results')
df_separated <- separate(df_separated,X3.7.Importantance.task.parallelisation, sep='/',into='X3.7.Importance.task.parallelisation')

count(df_separated$X3.7.Combination.data.sources)
count(df_separated$X3.7.Production.value.added.products)[-6,]
count(df_separated$X3.7.Consumption.open.data)
count(df_separated$X3.7.Sharing.results)
count(df_separated$X3.7.Importance.task.parallelisation)


df_freq <- count(df_separated$X3.7.Combination.data.sources)
df_freq$production_value.added.products <- count(df_separated$X3.7.Production.value.added.products)[-6,2]
df_freq$consumption.open.data <- count(df_separated$X3.7.Consumption.open.data)[-6,2]
df_freq$sharing.results <- count(df_separated$X3.7.Sharing.results)[-6,2]
df_freq$importance.task.parallelistation <- count(df_separated$X3.7.Importance.task.parallelisation)[-6,2]

df_relfreq <- df_freq[,c(2:6)] / no_of_respondents * 100
df_relfreq$cat <- df_freq$x

df_relfreq_melt <- melt(df_relfreq,id='cat')
labels <- c(freq='Combination of different data sources', production_value.added.products = 'I produced value-added products based on open datasets', 
            consumption.open.data='I simply consume open data',
            sharing.results = 'I need to share my results with third parties',
            importance.task.parallelistation = 'Parellelisation of my processing tasks is important')
df_relfreq_melt$variable2 <- plyr::revalue(df_relfreq_melt$variable, labels)



facet_plot <- ggplot(data=df_relfreq_melt, aes(x=cat, y=value)) +
  geom_bar(stat='identity', aes(fill=cat), width=0.7) +
  scale_fill_brewer(palette='Blues') +
  facet_wrap(~ variable2, ncol=2, labeller=labeller(variable2=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))





