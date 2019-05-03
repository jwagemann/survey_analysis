library('stringr')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

formats <- c("ASCII Grid",
             "CSV",
             "Cloud-optimised zarr",
             "GRIB",
             "GeoTIFF", 
             "JPEG/PNG",
             "JPEG2000",
             "JSON / GeoJSON",
             'NetCDF',
             "Sentinel-SAFE"
)

perc <- c(24.4,62.4,6.5,27.7,69.0,29.1,19.7,44.1,66.2,13.6)
data_use_sums <- data.frame(as.factor(formats), perc)

data_use_sums$as.factor.formats. <- factor(data_use_sums$as.factor.formats., levels=data_use_sums$as.factor.formats.[order(data_use_sums$perc)])

ggplot(data_use_sums, aes(y=perc, x=as.factor.formats.,fill=as.factor.formats., ymin=0)) + 
  geom_bar(stat="identity",width=0.6) +
  labs(x="Data format", y="Percent") +
  scale_fill_brewer(palette='Spectral') +
  ylim(0,80) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
