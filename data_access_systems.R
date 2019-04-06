library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')


dataService <- c(rep("Download Service", 3), 
                 rep('Cloud computing infrastructure',3), 
                 rep("OGC web services",3), 
                 rep("Custom API / OpenDAP",3), 
                 rep("Virtual Research Environment",3), 
                 rep("Data Cube technology",3), 
                 rep("Spatial or Array database",3))
use <- rep(c("Current", "Future/Continue","No Interest"),7)
sums <- c(70.9,21.6,42.0,38.0,50,8.5,34.3,34.4,19.7,26.8,30.5,22.1,4.7,27.2,42.3,7.0,43.2,28.2,15.5,36.6,27.7)


data_use_sums <- data.frame(dataService, use, sums)

# Grouped
ggplot(data_use_sums, aes(fill=use, y=sums, x=use)) + 
  geom_bar(stat="identity",width=0.7) +
  facet_wrap(dataService~., ncol=1) +
  coord_flip() +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  labs(x="Use", y="Counts", title="Data access systems") +
  theme(legend.title=element_blank())
