source('31_data_use_prepare.R')
source('data_survey_functions.R')
source('dummies_prepare.R')


####################################################################
# Crosstab - Data formats and Data type
####################################################################
# Dummies data formats
dummies_df

# Filter data types for data that are currently used
dataUse_freq <- dataUse_freq[,c('A1', 'B1','C1','D1','E1','F1','G1')]

# Set column names to data frame
colnames(dataUse_freq) <- colNames_vector_du

# Dummies work sector
dummies_ws

# Dummies data user type
dummies_du

# Crosstab between worksectors and specific data formats
crosstab_ws_df <- getCrossTabMelt(dummies_ws, dummies_df, colNames_vector_ws)

# Crossstab between data formats and data types
crosstab_du_df <- getCrossTabMelt(dataUse_freq, dummies_df, colNames_vector_du)

# Crosstab between data formats and data user type
crosstab_dut_df <- getCrossTabMelt(dummies_du, dummies_df, colNames_vector_dut)


facet_plot <- ggplot(data=crosstab_dut_df, aes(x=variable, y=value)) +
  geom_bar(stat='identity', aes(fill=variable), width=0.7) +
  scale_fill_brewer(palette='Spectral') +
  facet_wrap(~ cat, ncol=2, labeller=labeller(data=label_wrap_gen(45))) +
  labs(x='Rating from 1 (not true) to 5 (very true)', y='Percent') +
  theme_light() +
  theme(legend.position = 'none', strip.text.x= element_text(size=12))
