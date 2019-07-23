source('dummies_prepare.R')
source('data_survey_functions.R')

# Dummies work sector
dummies_ws

# Dummies country of residence region
dummies_cor_filter

# Dummies data user type
dummies_du

# Dummies application area
dummies_app



# Filter data types for data that are currently used
dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]
# Set column names to data frame
colnames(dataUse_freq) <- colNames_vector_du

# Cross table work sector and data type
crosstab_du_ws <- as.data.frame(createCrossTab(dataUse_freq, dummies_ws, c('all',colNames_vector_du)))

crosstab_du_ws$cat <- rownames(crosstab_du_ws)
crosstab_du_ws_melt <- melt(crosstab_du_ws, id='cat')

# Cross table region of country of residence and data type
crosstab_du_cor <- getCrossTabMelt(dataUse_freq,dummies_cor_filter, colNames_vector_du)

# Cross table data user type and data type
crosstab_du_dut <- getCrossTabMelt(dataUse_freq, dummies_du, colNames_vector_du)

# Cross table data type and application area
crosstab_du_app <- getCrossTabMelt(dataUse_freq, dummies_app, colNames_vector_du)

facet_plot <- ggplot(data=crosstab_du_app, aes(x=cat, y=value)) +
  geom_bar(stat='identity', aes(fill=cat), width=0.7) +
  scale_fill_brewer(palette='Spectral') +
  facet_wrap(~ variable, ncol=2, labeller=labeller(data=label_wrap_gen(10))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))
