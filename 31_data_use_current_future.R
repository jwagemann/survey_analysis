library('tidyr')
library('dplyr')
library('ggplot2')
require(gridExtra)
library('gtable')
library('grid')
library('ggsci')

source('31_data_use_prepare.R') # load dataUse_freq for the data frame that prepares the data use responses

dataUse_other <- surveyData[,22]


freqs <- lapply(dataUse_freq, function(x) as.data.frame(table(x)))

sums <- c(freqs$A1[,2],freqs$A2[,2],freqs$B1[,2],freqs$B2[,2],freqs$C1[,2],freqs$C2[,2],freqs$D1[,2],freqs$D2[,2],freqs$E1[,2],freqs$E2[,2],
          freqs$F1[,2],freqs$F2[,2],freqs$G1[,2],freqs$G2[,2])

currentUse <- c(freqs$A1[,2],freqs$B1[,2],freqs$C1[,2],freqs$D1[,2],freqs$E1[,2],
                freqs$F1[,2],freqs$G1[,2])

futureUse <- c(freqs$A2[,2],freqs$B2[,2],freqs$C2[,2],freqs$D2[,2],freqs$E2[,2],
               freqs$F2[,2],freqs$G2[,2])

currentUse_perc <- currentUse/no_of_respondents * 100
futureUse_basis <- no_of_respondents - currentUse
futureUse_perc <- futureUse/futureUse_basis * 100

dataType_2 <- c("Climate reanalysis", "Meteorological forecasts", "Seasonal forecasts", "Environmental forecasts", "Earth Observations","Other Geospatial Data", "Value-added products")

df_current <- data.frame(dataType_2,currentUse,currentUse_perc)
df_future <- data.frame(dataType_2, futureUse,futureUse_perc)

df_current_sort <- df_current[order(-df_current$currentUse),]
df_future_sort <- df_future[order(df_current$currentUse),]

df_future_sort$dataType_2 <-factor(df_future_sort$dataType_2, levels(df_future_sort$dataType_2)[c(5,6,7,1,2,4,3)])


df_current_sort['use'] <- 'current'
df_future_sort['use'] <- 'future'
colnames(df_future_sort) <- c('dataType_2', 'abs', 'rel', 'use')
colnames(df_current_sort) <- c('dataType_2', 'abs', 'rel', 'use')

# Grouped
plot_current <- ggplot(df_current_sort, aes(fill=dataType_2, y=abs, x=reorder(dataType_2, abs))) +
  geom_bar(stat='identity', width=0.6) +
#  coord_flip() +
  labs(x="", y="Number of users", Colour="Data use") +
#  scale_fill_brewer(palette='BrBG') +
  scale_fill_uchicago(palette='dark') +
  scale_y_reverse()+
  scale_x_discrete(labels=wrap_format(15)) + 
#  ggtitle('Current') + 
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
#        axis.text.y=element_text(angle=90, vjust=0.5, hjust=0.5),
        axis.text.y=element_text(size=12))
#        axis.title.y = element_blank())

positions <- c('Seasonal forecasts','Environmental forecasts', 'Meteorological forecasts','Climate reanalysis','Value-added products','Other Geospatial Data', 'Earth Observations')
plot_future <- ggplot(df_future_sort, aes(fill=dataType_2, y=abs, x=dataType_2)) +
  geom_bar(stat='identity', width=0.6) +
#  coord_flip() +
  labs(x="", y="Number of users", Colour="Data use") +
  scale_x_discrete(labels=wrap_format(15), limits=positions, position='top') +
  #ggtitle('Future')+
  ylim(0,150)+
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
#        axis.title.y = element_blank())


grid.draw(rbind(ggplotGrob(plot_future), ggplotGrob(plot_current), size='first'))


  