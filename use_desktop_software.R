library('stringr')
library(ggplot2)
library(RColorBrewer)

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

formats <- c("Yes",
             "No"
)

perc <- c(63.0,35.7)
data_use_sums <- data.frame(as.factor(formats), perc)

data_use_sums$as.factor.formats. <- factor(data_use_sums$as.factor.formats., levels=data_use_sums$as.factor.formats.[order(data_use_sums$perc)])

colourCount = length(unique(data_use_sums$perc))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(data_use_sums, aes(y=perc, x=as.factor.formats.,fill=as.factor.formats., ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="", y="Percent") +
  scale_fill_manual(values=getPalette(colourCount)) +
  ylim(0,80) +
  theme(legend.position="none", aspect.ratio = 3/1,
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
