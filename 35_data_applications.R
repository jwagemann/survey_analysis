
# Likert scale visualizations of data application
data_applications <- plot(likertObj_35, centered=FALSE, include.center=TRUE, text.size=4, panel.arrange='h', digits=1, wrap=15, include.histogram=FALSE)
data_applications + theme(legend.text=element_text(size=14),legend.position='none',
                          axis.text = element_text(size=14))
