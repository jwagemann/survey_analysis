source('43_data_access_systems_prepare.R')

# Add relative frequencies to absolute counts
dataAccess_order$ds_current_perc <- dataAccess_order$ds_current/no_of_respondents*100
dataAccess_order$ds_future_perc <- dataAccess_order$ds_futureUse/no_of_respondents*100
dataAccess_order$ds_no_interest_perc <- dataAccess_order$ds_no_interest/no_of_respondents*100

# Coefficients to arrange the various plot items
x_nudge_pos = .2
x_nudge_neg = -.2

# Bar plot of data systems currently used
plot_current <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_current, x=reorder(dataSystems,-ds_current))) +
  geom_bar(stat='identity', width=0.6) +
  scale_y_reverse(
    name='n',
    lim=c(220,0)) +
  labs(x="", y="n\n", Colour="Data system") +
  scale_fill_uchicago(palette='dark') +
  scale_x_discrete(labels=wrap_format(15)) + 
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title = element_text(size=16)) +
  geom_text(aes(y=0, label=ds_current), size=5, vjust=0, nudge_y =-11, color='white') +
  geom_label(aes(y=200, label=round(ds_current_perc,1)), size=6, position=position_dodge(width=0.9), color='white')+
  annotate('text', label='Current use', size=6, fontface='bold',  x=6.8, y=215)


# Calculate the ratio future use vs. no interest at all
ratio = dataAccess_order$ds_futureUse / dataAccess_order$ds_no_interest

# x-Axis positions
positions <- dataAccess_order$dataSystems

# Barplot of intended future use and not interest
plot_future <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_futureUse, x=dataSystems)) +
  geom_bar(stat='identity', width=0.4,aes(y=ds_no_interest), position=position_nudge(x=x_nudge_pos),alpha=0.3, group=1)+
  geom_bar(stat='identity', width=0.4, aes(y=ds_futureUse), position=position_nudge(x=x_nudge_neg)) +
  scale_y_continuous(
    name='n',
    lim=c(-20,220)) +
  geom_label(y = -12, aes(label=round(ratio,1)), size=6) +
  labs(x="", y="n\n", Colour="Data system") +
  scale_x_discrete(labels=wrap_format(15), limits=positions, position='top') +
  scale_fill_uchicago(palette='light', alpha=0.7) +
  theme_light() + 
  theme(legend.position='none',
        plot.title=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title = element_text(size=16)) +
  geom_label(aes(y=180, label=round(ds_no_interest_perc,1)), alpha=0.3,size=6, position=position_nudge(x=x_nudge_pos), color='black') +
  geom_label(aes(y=180, label=round(ds_future_perc,1)), size=6, position=position_nudge(x=x_nudge_neg), color='black') +
  geom_text(aes(y=0, label=ds_futureUse), size=5, nudge_x=x_nudge_neg, nudge_y=5, vjust=0,  color='black') +
  geom_text(aes(y=0, label=ds_no_interest), size=5, vjust=0, nudge_x=x_nudge_pos, nudge_y=5, color='black') +
  annotate('text', label='Future use (left) vs. no interest (right)', size=6, fontface='bold',  x=1.8, y=215)

# Combine both plots
grid.draw(rbind(ggplotGrob(plot_future), ggplotGrob(plot_current), size='first'))


