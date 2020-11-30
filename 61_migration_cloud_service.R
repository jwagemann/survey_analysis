# Question 6.1 How much are you interested in migrating your processing tasks to a cloud service in the future?

# Add general groups to data frame
df_61_freq_ord$group <- c('Interest', 'Interest', 'Neutral', 'No interest', 'No interest') 

# Calculate summarized percentage for interest / no interest classes
interest_sum <- sum(df_61_freq_ord[c(1,2),3])
no_interest_sum <- sum(df_61_freq_ord[c(4,5),3])

# Make vector with percentage sums to add as labels
percSums <- c(interest_sum,interest_sum, df_61_freq_ord[3,3], no_interest_sum, no_interest_sum)

# Bar plot with combining Interest and no interest classes
likert_perc <- ggplot(data=df_61_freq_ord, aes(x=group, y=perc, fill=Interest)) +
  geom_bar(stat='identity', position=position_dodge(width=0.55),width=0.5,aes(y=perc, fill=Interest))+
  scale_fill_manual(values=rev(brewer.pal(n=5,'BrBG'))) +
  labs(x="", y="rel. Frequency") +
  theme_light()+
  ylim(-1,45)+
  geom_text(aes(y=0,label=round(perc,1)), position=position_dodge(width=0.55), vjust=-0.4, color='black', size=4) +
  geom_label(y= 42, aes(x=group,label=round(percSums,1)), size=5, show.legend=FALSE) +
  guides(fill=guide_legend())+
  theme(legend.position='bottom',
        legend.title=element_blank(),
        plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio = 1/4) +
  scale_x_discrete(labels = wrap_format(20)) 