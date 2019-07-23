source('31_data_use_prepare.R') # Load dataUse_freq
source('data_survey_functions.R')
source('dummies_prepare.R')

dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]

# Desktop-based software dummies
dummies_ds
dummies_ds_type


# Dummy table data service
dummies_ds

# Dummy table work sector
dummies_ws

# Dummy table country of residence
dummies_cor

# Dummy table data user type
dummies_du

crosstab_du_ds <- getCrossTabMelt(dataUse_freq, dummies_ds_type,colNames_vector_du)
crosstab_cor_ds <- getCrossTabMelt(dummies_cor, dummies_ds_type, colNames_vector_cor)
crosstab_corf_ds <- getCrossTabMelt(dummies_cor_filter, dummies_ds_type, colNames_vector_region)
crosstab_ws_ds <- getCrossTabMelt(dummies_ws, dummies_ds_type, colNames_vector_ws)
crosstab_du_ds <- getCrossTabMelt(dummies_du, dummies_ds_type, colNames_vector_dut)
crosstab_app_ds <- getCrossTabMelt(dummies_app, dummies_ds_type, colNames_vector_app)


facet_plot <- ggplot(data=crosstab_app_ds, aes(x=variable, y=value)) +
  geom_bar(stat='identity', aes(fill=variable), width=0.7) +
  scale_fill_brewer(palette='Spectral') +
  facet_wrap(~ cat, ncol=2, labeller=labeller(data=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))
