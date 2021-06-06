
source('31_data_use_prepare.R') # load dataUse_freq for the data frame that prepares the data use responses

dataUse_other <- surveyData[,22]

# Get frequencies for each option
freqs <- lapply(dataUse_freq, function(x) as.data.frame(table(x)))

# Build a vector for current use
currentUse <- c(freqs$A1[,2],freqs$B1[,2],freqs$C1[,2],freqs$D1[,2],freqs$E1[,2],
                freqs$F1[,2],freqs$G1[,2])

# Build a vector for future use
futureUse <- c(freqs$A2[,2],freqs$B2[,2],freqs$C2[,2],freqs$D2[,2],freqs$E2[,2],
               freqs$F2[,2],freqs$G2[,2])

# Build percentanges based on no of respondents
currentUse_perc <- currentUse/no_of_respondents * 100

# Future use basis is the no of respondents minus the ones, that are already using the data, 
# as we expect that data that is currently used will also be used in the future
futureUse_basis <- no_of_respondents - currentUse
futureUse_perc <- futureUse/futureUse_basis * 100

# Build vector for data types
dataType_2 <- c("Climate reanalysis", "Meteorological forecasts", "Seasonal forecasts", "Environmental forecasts", "Earth Observations","Other Geospatial Data", "Value-added products")

# Build data frame for current and future use with data type, absolute and percentage values
df_current <- data.frame(dataType_2,currentUse,currentUse_perc)
df_future <- data.frame(dataType_2, futureUse,futureUse_perc, futureUse_basis)

# Sort the data frame decreasing based on the frequencies
df_current_sort <- df_current[order(-df_current$currentUse),]
df_future_sort <- df_future[order(-df_current$currentUse),]

# Adjust the factors for current and future data types
df_future_sort$dataType_2 <-factor(df_future_sort$dataType_2, levels(df_future_sort$dataType_2)[c(5,6,7,1,2,4,3)])
df_current_sort$dataType_2 <-factor(df_current_sort$dataType_2, levels(df_current_sort$dataType_2)[c(5,6,7,1,2,4,3)])

df_current_sort['use'] <- 'current'
df_future_sort$use <- 'future'
colnames(df_future_sort) <- c('dataType_2', 'abs', 'rel', 'use')
colnames(df_current_sort) <- c('dataType_2', 'abs', 'rel', 'use')


# May plot with current use of data types
plot_current <- ggplot(df_current_sort, aes(fill=dataType_2, y=abs, x=reorder(dataType_2, abs))) +
  geom_bar(stat='identity', width=0.6) +
  scale_y_reverse(
    name='n - Currently used\n',
    lim=c(200,0)) +
  labs(x="", Colour="Data use") +
  scale_fill_uchicago(palette='dark') +
  scale_x_discrete(labels=wrap_format(15)) +
  geom_text(aes(y=abs, label=abs),position=position_dodge(width=0.9), vjust=-0.8, color='white', size=5) +
  geom_label(y=-200, aes(label=round(rel,1)),vjust=0, size=6, colour='white') +
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black"))

# Define positions, which are the data types
positions <- c('Seasonal forecasts','Environmental forecasts', 'Meteorological forecasts','Climate reanalysis','Value-added products','Other Geospatial Data', 'Earth Observations')

# Plot future use of data type
plot_future <- ggplot(df_future_sort, aes(fill=dataType_2, y=abs, x=dataType_2)) +
  geom_bar(stat='identity', width=0.6) +
  labs(x="", Colour="Data use") +
  scale_y_continuous(
    name='n - Would like to use in future\n',
    lim=c(0,150)) +
  scale_x_discrete(labels=wrap_format(15), limits=positions, position='top') +
  scale_fill_uchicago(palette='light',alpha=0.5) +
  geom_text(aes(label=abs), position=position_dodge(width=0.9), vjust=2, color='black', size=5) +
  theme_light() + 
  geom_label(y=143, aes(label=round(rel,1)),vjust=0, size=6) +
  theme(legend.position='none',
        plot.title=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black"))

# Bring both plots together on one grid
grid.draw(rbind(ggplotGrob(plot_future), ggplotGrob(plot_current), size='first'))

  