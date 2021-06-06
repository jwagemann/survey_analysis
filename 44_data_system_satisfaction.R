source('./44_data_system_satisfaction_prepare.R')

# Order the Satisfaction levels in reversed order
df_44_merged_melt$Satisfaction <- factor(df_44_merged_melt$Satisfaction, levels=rev(levels(df_44_merged_melt$Satisfaction)))

# Replace entries with 0 responses with NA
df_44_merged_abs_melt$value_NA <- ifelse(df_44_merged_abs_melt$value ==0, NA, df_44_merged_abs_melt$value)


# Build percentage sum for 'very satisfied' and 'satisfied' response
satisfied <- colSums(df_44_merged_ord[4:5,-1])
# Build percentage sum for 'dissatisfied' and 'very dissatisfied' response
not_satisfied <- colSums(df_44_merged_ord[1:2,-1])

# Melt the two data frames
satisfied_melt <- reshape2::melt(satisfied)
not_satisfied_melt <- reshape2::melt(not_satisfied)

# Create a data frame with important and not important sums
vals_combined <- data.frame(satisfied_melt, not_satisfied_melt, rownames(satisfied_melt))
# Add column names to data frame
colnames(vals_combined) <- c('satisfied', 'not_satisfied', 'variable')

# Add 'important' and 'not important' percentages to the melted data frame
df_44_perc_melt_2 <- merge(x=df_44_merged_abs_melt, y=vals_combined, by='variable', all.x=TRUE )

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
# Stacked bar plot with absolut number about satisfaction levels of the three most used data access systems
likert_abs <- ggplot(data=df_44_perc_melt_2, aes(x=variable, y=value_NA, fill=Satisfaction)) +
  geom_bar(stat='identity', width=.85) +
  scale_fill_manual(values=brewer.pal(n=5,'BrBG')) +
  labs(x="Data system\n", y="n", Colour="") +
  ylim(-6,165)+
  coord_flip() +
  theme_light() +
  theme(legend.position='top',
        legend.title=element_blank(), 
    plot.title=element_text(size=14),
    axis.text=element_text(size=14),
  legend.text = element_text(size=14),
  strip.text.x=element_text(size=14),
  axis.title = element_text(size=14),
  aspect.ratio=1/2)+
  guides(fill=guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = wrap_format(20))+
  geom_label(y=-6, aes(label=round(satisfied,1)), fill=col[5],col='white', size=6) +
  geom_label(y=165, aes(label=round(not_satisfied,1)), fill=col[1],col='white', size=6)

