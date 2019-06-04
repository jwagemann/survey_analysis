library('tidyr')
library('dplyr')
library('ggplot2')
require(gridExtra)
library('gtable')
library('grid')
library('ggsci')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')
source('31_data_use_prepare.R') # load dataUse_freq for the data frame that prepares the data use responses



surveyData <- read.csv('./data/20190131_final_results.csv', header = TRUE, na.strings="")
no_of_respondents <- nrow(surveyData)

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
df_future_sort <- df_future[order(-df_future$futureUse),]


df_current_sort['use'] <- 'current'
df_future_sort['use'] <- 'future'
colnames(df_future_sort) <- c('dataType_2', 'abs', 'rel', 'use')
colnames(df_current_sort) <- c('dataType_2', 'abs', 'rel', 'use')

# Grouped
plot_current <- ggplot(df_current_sort, aes(fill=dataType_2, y=abs, x=reorder(dataType_2, abs))) +
  geom_bar(stat='identity', width=0.6) +
  coord_flip() +
  labs(x="Use", y="Number of users", Colour="Data use") +
#  scale_fill_brewer(palette='BrBG') +
  scale_fill_uchicago(palette='dark') +
  scale_y_reverse()+
  scale_x_discrete(labels=wrap_format(15), position='right')+
  ggtitle('Current') + 
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text=element_text(size=12),
        axis.text.y=element_text(angle=90, vjust=0.5, hjust=0.5),
        axis.title.y = element_blank())

positions <- c('Seasonal forecasts','Environmental forecasts', 'Meteorological forecasts','Climate reanalysis','Value-added products','Other Geospatial Data', 'Earth Observations')
plot_future <- ggplot(df_future_sort, aes(fill=dataType_2, y=abs, x=dataType_2)) +
  geom_bar(stat='identity', width=0.6) +
  coord_flip() +
  labs(x="Use", y="Number of users", Colour="Data use") +
  scale_x_discrete(labels=wrap_format(15), limits=positions) +
  ggtitle('Future')+
  ylim(0,150)+
  scale_fill_uchicago(palette='light') +
  theme_light() + 
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y=element_blank(),
        axis.title.y = element_blank())


grid.draw(cbind(ggplotGrob(plot_current), ggplotGrob(plot_future), size='first'))


  