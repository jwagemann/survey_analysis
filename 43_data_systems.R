source('43_data_access_systems_prepare.R')

# Add relative frequencies to absolute counts
dataAccess_order$ds_current_perc <- dataAccess_order$ds_current/no_of_respondents*100
dataAccess_order$ds_future_perc <- dataAccess_order$ds_futureUse/no_of_respondents*100
dataAccess_order$ds_no_interest_perc <- dataAccess_order$ds_no_interest/no_of_respondents*100

# Coefficients to arrange the various plot items
coeff <- 3
x_nudge = .2

# Bar plot of data systems currently used
plot_current <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_current, x=reorder(dataSystems,-ds_current))) +
  geom_bar(stat='identity', width=0.6) +
  scale_y_reverse(
    name='n',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./coeff, name="rel. Frequency"),
    lim=c(220,0)) +
  geom_point(aes(y=ds_current_perc*coeff), color='black')+
  geom_line(aes(y=ds_current_perc*coeff), linetype='twodash', color='black', group=1)+
  labs(x="", y="n - Currently used", Colour="Data system") +
  scale_fill_uchicago(palette='dark') +
  scale_x_discrete(labels=wrap_format(15)) + 
  theme_light()+
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=14),
        axis.title = element_text(size=13)) +
  geom_text(aes(y=0, label=ds_current), size=4, vjust=0, nudge_y =-9, color='white') +
  geom_text(aes(y=ds_current_perc*coeff, label=round(ds_current_perc,1)), size=4, position=position_dodge(width=0.9), vjust=2.2, color='black')+
  annotate('text', label='Current use', size=5, fontface='bold',  x=7, y=215)


# Calculate the ratio future use vs. no interest at all
ratio = dataAccess_order$ds_futureUse / dataAccess_order$ds_no_interest

# x-Axis positions
positions <- dataAccess_order$dataSystems

# Barplot of intended future use and not interest
plot_future <- ggplot(dataAccess_order, aes(fill=dataSystems, y=ds_futureUse, x=dataSystems)) +
  geom_bar(stat='identity', width=0.4,aes(y=ds_no_interest), position=position_nudge(x=x_nudge),alpha=0.3, group=1)+
  geom_bar(stat='identity', width=0.4, position=position_nudge(x=x_nudge)) +
  scale_y_continuous(
    name='n',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./coeff, name="rel. Frequency"),
    lim=c(-20,220)) +
  geom_label(y = -10, aes(label=round(ratio,1)), size=5) +
  geom_point(aes(y=ds_future_perc*coeff), position=position_nudge(x=x_nudge), color='black')+
  geom_line(aes(y=ds_future_perc*coeff), position=position_nudge(x=x_nudge), linetype='twodash', color='black', group=1)+
  geom_line(aes(y=ds_no_interest_perc*coeff), position=position_nudge(x=x_nudge), linetype='dotted', color='black', group=1)+
  geom_point(aes(y=ds_no_interest_perc*coeff), position=position_nudge(x=x_nudge), color='black')+
  labs(x="", y="n", Colour="Data system") +
  scale_x_discrete(labels=wrap_format(15), limits=positions, position='top') +
  scale_fill_uchicago(palette='light', alpha=0.7) +
  theme_light() + 
  theme(legend.position='none',
        plot.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=14),
        axis.title = element_text(size=13)) +
  geom_text(aes(y=ds_no_interest_perc*coeff, label=round(ds_no_interest_perc,1)), size=4, position=position_nudge(x=x_nudge), vjust=-1, color='black') +
  geom_text(aes(y=ds_future_perc*coeff, label=round(ds_future_perc,1)), size=4, position=position_nudge(x=x_nudge), vjust=-1, color='black') +
  geom_text(aes(y=0, label=ds_futureUse), size=4, nudge_x=x_nudge, nudge_y=5, vjust=0,  color='black') +
  geom_text(aes(y=ds_no_interest - ds_no_interest, label=ds_no_interest), size=4, vjust=-.7, nudge_x=x_nudge, color='black') +
  annotate('text', label='Future use (left) vs. no interest (right)', size=5, fontface='bold',  x=1.5, y=215)

# Combine both plots
grid.draw(rbind(ggplotGrob(plot_future), ggplotGrob(plot_current), size='first'))


