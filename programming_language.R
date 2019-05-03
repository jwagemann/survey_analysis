library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

formats <- c("C",
             "C++",
             "Fortran",
             "Java",
             "Javascript",
             "Julia",
             "None",
             "PHP",
             "Python",
             "R",
             "Scala", 
             "gdal"
)

perc <- c(8.5,12.2,16.0,14.6,26.8,1.9,4.2,6.6,77.0,43.0,1.4,36.2)
data_use_sums <- data.frame(as.factor(formats), perc)

data_use_sums$as.factor.formats. <- factor(data_use_sums$as.factor.formats., levels=data_use_sums$as.factor.formats.[order(data_use_sums$perc)])

colourCount = length(unique(data_use_sums$perc))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(data_use_sums, aes(y=perc, x=as.factor.formats.,fill=getPalette(colourCount), ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Programming language", y="Percent") +
  scale_fill_manual(values=getPalette(colourCount)) +
  ylim(0,80) +
  coord_flip()+
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=2/1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
