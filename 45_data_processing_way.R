# Question: how do you process large volumes of Big Earth data?

# Remove NA entries
df_45_freqs_2 <- df_45_freqs[-4,]

# Reorder the levels from Never to Always
levels_45 <- c('Never', 'Sometimes', 'Always')
df_45_freqs_2$Frequency <- factor(df_45_freqs_2$Frequency,levels=levels_45)
df_45_ord <- df_45_freqs_2[order(factor(df_45_freqs_2$Frequency,levels=levels_45)),]

# Reorder columns in an increasing order
df_45_ord_col <- df_45_ord[,c(1,4,5,3,2)]

# Melt data into a data frame with one column
df_45_melt <- reshape2::melt(df_45_ord_col)

# Calculate frequencies
df_45_perc <- df_45_melt$value/no_of_respondents*100

# Add percent values as column to melted data frame
df_45_melt_2 <-cbind(df_45_melt, df_45_perc)

# Stacked bar plot
likert_perc <- ggplot(data=df_45_melt_2, aes(x=reorder(variable, desc(variable)), y=df_45_perc, fill=Frequency)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=brewer.pal(n=3,'BrBG')) +
  labs(x="Modality - data processing", y="rel. Frequency", Colour="") +
  coord_flip() +
  theme_light() +
  theme(legend.position='top',
        plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        legend.title = element_blank(),
        axis.title = element_text(size=14),
        aspect.ratio = 1/3,
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0))) +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label = round(df_45_perc,1)), color='black', size = 5, position=position_stack(vjust=0.5) )
  scale_x_discrete(labels = label_wrap(20))
