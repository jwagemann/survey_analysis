source('./46_important_statements_prepare.R')

# Build percentage sum for 'important' and 'very important' response
important <- colSums(df_46_perc_ord[4:5,-1])
# Build percentage sum for 'not important' and 'not at all important' response
not_important <- colSums(df_46_perc_ord[1:2,-1])

# Melt the two data frames
important_melt <- reshape2::melt(important)
not_important_melt <- reshape2::melt(not_important)

# Create a data frame with important and not important sums
vals_combined <- data.frame(important_melt, not_important_melt, rownames(important_melt))
# Add column names to data frame
colnames(vals_combined) <- c('important', 'not_important', 'variable')

# Add 'important' and 'not important' percentages to the melted data frame
df_46_perc_melt_2 <- merge(x=df_46_perc_melt, y=vals_combined, by='variable', all.x=TRUE )

# Change naming of statement with revalue
df_46_perc_melt_2$variable <- revalue(factor(df_46_perc_melt_2$variable),c("standard.data.access"="Data access with standard protocol, e.g. WMS or WCS",
                                                                       "web.applications.visualization"="On-demand data access for e.g. web applications",
                                                                       "cloud.processing"="Server-/Cloud-based processing",
                                                                       "download.large.volumes"="Download of large data volumes",
                                                                       "parallel.computing"="Parallel computing",
                                                                       "data.discovery"="Easier data discovery",
                                                                       "time.series.retrieval"="Time-series retrieval",
                                                                       "interoperability"="Interoperability of data and data systems"))

col <- brewer.pal(n=5,'BrBG')

# Horizontal stacked bar plot on the importance of pre-defined statement
likert_perc <- ggplot(data=df_46_perc_melt_2, aes(x=variable, y=value, fill=Importance)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=brewer.pal(n=5,'BrBG')) +
  labs(x="Data task\n", y="%") +
  coord_flip() +
  theme_light()+
  ylim(-4,104)+
  geom_label(y=-4, aes(label=round(important,1)), fill=col[5],col='white', size=6 )+
  geom_label(y=104, aes(label=round(not_important,1)), fill=col[1],col='white', size=6 )+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position='bottom',
        legend.title=element_blank(),
        plot.title=element_text(size=16),
        axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.title = element_text(size=16),
        aspect.ratio = 1/2) +
  scale_x_discrete(labels = wrap_format(26)) 
