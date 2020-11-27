source('./44_data_system_satisfaction_prepare.R')

# Order the Satisfaction levels in reversed order
df_44_merged_melt$Satisfaction <- factor(df_44_merged_melt$Satisfaction, levels=rev(levels(df_44_merged_melt$Satisfaction)))

# Replace entries with 0 responses with NA
df_44_merged_abs_melt$value_NA <- ifelse(df_44_merged_abs_melt$value ==0, NA, df_44_merged_abs_melt$value)

# Stacked bar plot with absolut number about satisfaction levels of the three most used data access systems
likert_abs <- ggplot(data=df_44_merged_abs_melt, aes(x=variable, y=value_NA, fill=Satisfaction)) +
  geom_bar(stat='identity', width=.6) +
  scale_fill_manual(values=brewer.pal(n=5,'BrBG')) +
  labs(x="Data system", y="Number of users", Colour="") +
  coord_flip() +
  theme_light() +
  theme(legend.position='top',
    plot.title=element_text(size=14),
    axis.text=element_text(size=12),
  legend.text = element_text(size=12),
  strip.text.x=element_text(size=12),
  axis.title = element_text(size=14),
  aspect.ratio=1/3)+
  guides(fill=guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = wrap_format(5)) +
  geom_text(aes(label = value_NA), color='black', size = 5, position=position_stack(vjust=0.5) )

