source('51_data_challenges_prepare.R')

# Create column sums for 'obstacle' and 'no obstacle' responses in order to add lables to the plot
obstacle <- colSums(df_51_perc[4:5,-1])
no_obstacle <- colSums(df_51_perc[1:2,-1])

# Melt the data frames and combine them
obstacle_melt <- reshape2::melt(obstacle)
no_obstacle_melt <- reshape2::melt(no_obstacle)

vals_combined <- data.frame(obstacle_melt, no_obstacle_melt, rownames(obstacle_melt))
colnames(vals_combined) <- c('obstacle', 'no_obstacle', 'variable')

# Merge the data frames above with the prepared data frame
df_51_perc_melt_2 <- merge(x=df_51_perc_melt, y=vals_combined, by='variable', all.x=TRUE )

# Horizontal stacked barplot to show Big Earth Data challenges
likert_perc <- ggplot(data=df_51_perc_melt_2, aes(x=variable, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=brewer.pal(n=5,'BrBG')) +
  labs(x="Challenge", y="rel. Frequency") +
  coord_flip() +
  theme_light()+
  ylim(-4,104)+
  scale_x_discrete(labels = wrap_format(20)) +
  guides(fill=guide_legend(reverse=TRUE))+
  geom_label(y=-4, aes(label=round(obstacle,1)), fill=col[5],col='white', size=6 )+
  geom_label(y=104, aes(label=round(no_obstacle,1)), fill=col[1],col='white', size=6 )+
  theme(legend.position='bottom',
        legend.title=element_blank(),
    axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))
