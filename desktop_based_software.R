library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

formats <- c("ArcGIS",
             "ENVI",
             "Excel",
             "GRASS GIS",
             "IDL",
             "Matlab",
             "QGIS"
)

perc <- c(31.5,12.2,24.9,17.4,7.0,15.5,44.1)
data_use_sums <- data.frame(as.factor(formats), perc)

data_use_sums$as.factor.formats. <- factor(data_use_sums$as.factor.formats., levels=data_use_sums$as.factor.formats.[order(data_use_sums$perc)])

colourCount = length(unique(data_use_sums$perc))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(data_use_sums, aes(y=perc, x=as.factor.formats.,fill=as.factor.formats., ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Desktop-based software", y="Percent") +
  scale_fill_brewer(palette="Spectral",direction=-1) +
  ylim(0,80) +
  coord_flip()+
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
