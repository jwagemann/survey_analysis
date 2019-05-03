library('dplyr')
library('ggplot2')
library('tidyr')
install.packages('stringr')
load('tidyr')
library('stringr')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

surveyData <- read.csv('./20190131_final_results.csv', header = TRUE)

dataFormat <- as.data.frame(surveyData[,27])

reason <- c("Data formats are too cumbersome to use",'Data quantity and volume is too large',"Downloading data is uncomfortable","I don't know where to find data","Time Constraints", "Tools for analysis/processing are lacking or too complex")

perc <- c(23.0,37.6,32.9,24.4,40.8,32.9)
data_use_sums <- data.frame(reason, perc)

ggplot(data_use_sums, aes(y=perc, x=reason,fill=reason, ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  coord_flip() +
  labs(x="Obstacle", y="Percent") +
  scale_fill_brewer(palette='Spectral') +
  ylim(0,60) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

