source('31_data_use_prepare.R') # Load dataUse_freq
source('data_survey_functions.R') # Load dataUse_freq
source('dummies_prepare.R')

dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]

#Load desktop software
df_421 <- as.data.frame(df_new[,'X4.2.1'])
colnames(df_421) <- 'software'
df_421_split <- separate_rows(df_421,software, sep=';')
df_421_freq <- count(df_21_split)
df_421_freq$perc <- df_21_freq$freq / no_of_respondents * 100


#Load work sector
df_21 <- as.data.frame(df_new[,'X2.1'])
colnames(df_21) <- 'work.sector'
df_21_split <- separate_rows(df_21,work.sector, sep=';')
df_21_freq <- count(df_21_split)
df_21_freq$perc <- df_21_freq$freq / no_of_respondents * 100



dummies_ds <- getDummies(df_421,1)
dummies_ds <- dummies_ds[,c(-1,-5)]

dummies_ws <- getDummies(df_21,1)
dummies_ws <- dummies_ws[,c(-7)]

dummies_cor <- getDummies(df_11,2)
dummies_cor <- dummies_cor[,c(-2)]

colNames_vector_du <- c('Climate reanalysis','Daily meteorological data', 'Seasonal forecast data', 'Environmental forecast data', 'Earth Observations','Other Geospatial data','Value-added products')

crosstab_du_ds <- getCrossTabMelt(dataUse_freq, dummies_ds,colNames_vector_du)
crosstab_cor_ds <- getCrossTabMelt(dummies_cor, dummies_ds, colNames_vector_region)

facet_plot <- ggplot(data=crosstab_cor_ds, aes(x=variable, y=value)) +
  geom_bar(stat='identity', aes(fill=variable), width=0.7) +
  scale_fill_brewer(palette='Spectral') +
  facet_wrap(~ cat, ncol=2, labeller=labeller(data=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))
