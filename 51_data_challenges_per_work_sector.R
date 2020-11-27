# Load responses to Big Earth Data challenges
df_51 <- df_new[,c('X5.1.growing.data.volume',
                   'X5.1.limited.processing.capacity',
                   'X5.1.complex.data.formats',
                   'X5.1.non.standard.data',
                   'X5.1.data.discovery',
                   'X5.1.data.access.systems',
                   'X5.1.data.complexity',
                   'X5.1.data.combination',
                   'X5.1.too.many.data.platforms',
                   'X5.1.restricted.data.services',
                   'X5.1.lack.of.tools',
                   'X5.1.data.services.cost')]

# Remove strange entries
df_51 <- df_51[-67,]

#df_subset <- cbind(dataSystems_freq, df_51)
# Combine challenges with work sector responses
df_subset <- cbind(df_21, df_51)
df_subset <- df_subset[-67,]
# Seperate rows for multiple work sector entries
df_subset <- separate_rows(df_subset,1, sep=';')

df_subset_A <- subset(df_subset[,c(2:ncol(df_subset))], (!is.na(df_subset[,19])))
df_subset_A <- subset(df_subset[,c(2:ncol(df_subset))], df_subset[,1]=='Established company' | df_subset[,1]=='Start-up')
df_subset_A <- subset(df_subset[,c(2:ncol(df_subset))], df_subset[,1]=='Government')


# Remove all NA entries
df_subset_A <- df_subset_A[!(rowSums(is.na(df_subset_A))==12),]

# Translate the number 1 - 5 to obtacle classes
df_subset_A <- data.frame(list(ifelse(df_subset_A==1,'No obstacle at all', 
                                ifelse(df_subset_A==2, 'No obstacle', 
                                       ifelse(df_subset_A==3, 'Neither no obstacle nor an obstacle', 
                                              ifelse(df_subset_A==4, 'An obstacle', 
                                                     ifelse(df_subset_A==5, 'A great obstacle', 'test')))))))

# Define the obstacle classes
levels_51 <- c('No obstacle at all','No obstacle','Neither no obstacle nor an obstacle','An obstacle','A great obstacle')

# Convert NA answers to 'Neither no obstacle nor an obstacle'
df_subset_A[is.na(df_subset_A)] <- "Neither no obstacle nor an obstacle"
# Get number of rows
nrow_51 <- nrow(df_subset_A)

# For each challenge, count the number of rows
a_51 <- plyr::count(df_subset_A$X5.1.growing.data.volume)
b_51 <- plyr::count(df_subset_A$X5.1.limited.processing.capacity)
c_51 <- plyr::count(df_subset_A$X5.1.complex.data.formats)
d_51 <- plyr::count(df_subset_A$X5.1.non.standard.data)
e_51 <- plyr::count(df_subset_A$X5.1.data.discovery)
f_51 <- plyr::count(df_subset_A$X5.1.data.access.systems)
g_51 <- plyr::count(df_subset_A$X5.1.data.complexity)
h_51 <- plyr::count(df_subset_A$X5.1.data.combination)
i_51 <- plyr::count(df_subset_A$X5.1.too.many.data.platforms)
k_51 <- plyr::count(df_subset_A$X5.1.restricted.data.services)
l_51 <- plyr::count(df_subset_A$X5.1.lack.of.tools)
m_51 <- plyr::count(df_subset_A$X5.1.data.services.cost)

a_51 <- rbind(a_51, list('No obstacle at all', 0))
a_51$x <- factor(a_51$x, unique(c(levels(a_51$x), list('No obstacle at all',0))))

c_51 <- rbind(c_51, list('No obstacle at all', 0))
c_51$x <- factor(c_51$x, unique(c(levels(c_51$x), list('No obstacle at all',0))))

k_51 <- rbind(k_51, list('No obstacle at all', 0))
k_51$x <- factor(k_51$x, unique(c(levels(k_51$x), list('No obstacle at all',0))))

# Bring frequencies together into one dataframe
df_subset_A_freq <- a_51 %>% left_join(b_51, by='x') %>% left_join(c_51, by='x') %>% left_join(d_51, by='x') %>% 
  left_join(e_51, by='x') %>% left_join(f_51, by='x') %>% left_join(g_51, by='x') %>% left_join(h_51, by='x') %>% 
  left_join(i_51, by='x') %>% left_join(k_51, by='x') %>% left_join(l_51, by='x') %>% left_join(m_51, by='x')

# Define colnames
colnames(df_subset_A_freq) <- c('Scale',
                          'Growing data volume',
                          'Limited processing capacity',
                          'Complex data formats',
                          'Data are dissemintated in a non-standardised way',
                          'Data discovery',
                          'Data access systems',
                          'Data complexity',
                          'Combining different kind of geospatial data',
                          'Too many data platforms and portals',
                          'Data services are too restricted',
                          'Lacking easy-to-use tools ',
                          'Cost of data services'
)


df_subset_A_freq$Scale <- factor(df_subset_A_freq$Scale, levels=levels_51)

df_subset_A_perc <- df_subset_A_freq[,-1] / nrow_51 * 100
df_subset_A_perc <- cbind(df_subset_A_freq[1], df_subset_A_perc)

df_subset_A_freq <- df_subset_A_freq[order(factor(df_subset_A_freq$Scale, levels=levels_51)),]
df_subset_A_perc <- df_subset_A_perc[order(factor(df_subset_A_perc$Scale,levels=levels_51)),]

df_subset_A_main <- df_subset_A_freq[c(4,5),]
df_subset_A_main_sum <- as.data.frame(colSums(df_subset_A_main[,-1]))
df_subset_A_main_sum$items <- rownames(df_subset_A_main_sum)
df_subset_A_main_ord <- df_subset_A_main_sum[order(df_subset_A_main_sum$`colSums(df_subset_A_main[, -1])`),]

df_subset_A_freq_melt <- reshape2::melt(df_subset_A_freq)
df_subset_A_perc_melt <- reshape2::melt(df_subset_A_perc)

df_subset_A_freq_melt$variable <- factor(df_subset_A_freq_melt$variable, levels=df_subset_A_main_ord$items)
df_subset_A_perc_melt$variable <- factor(df_subset_A_perc_melt$variable, levels=df_subset_A_main_ord$items)

likert_perc <- ggplot(data=df_subset_A_perc_melt, aes(x=variable, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x="Challenge", y="Percent") +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(20)) +
  guides(fill=guide_legend(reverse=TRUE))+
  #  scale_x_continuous(name="Percent") +
  
  theme(legend.position='bottom',
        legend.title=element_blank(),
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14))

df_university <- df_subset_A_perc_melt
df_government <- df_subset_A_perc_melt
df_company <- df_subset_A_perc_melt
df_io_non_profit <- df_subset_A_perc_melt

# Growing data volume
growing_volume_1 <- subset(df_university, df_university[,2]=='Growing data volume')
growing_volume_1$work_sector <- 'University'

growing_volume_2 <- subset(df_government, df_government[,2]=='Growing data volume')
growing_volume_2$work_sector <- 'Government'

growing_volume_3 <- subset(df_company, df_company[,2]=='Growing data volume')
growing_volume_3$work_sector <- 'Established company / Start-up'

growing_volume_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Growing data volume')
growing_volume_4$work_sector <- 'Intergov. Org. / Non-profit'

a_final <- rbind(growing_volume_1,growing_volume_2, growing_volume_3, growing_volume_4)

# Limite processing capacity

b_1 <- subset(df_university, df_university[,2]=='Limited processing capacity')
b_1$work_sector <- 'University'

b_2 <- subset(df_government, df_government[,2]=='Limited processing capacity')
b_2$work_sector <- 'Government'

b_3 <- subset(df_company, df_company[,2]=='Limited processing capacity')
b_3$work_sector <- 'Established company / Start-up'

b_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Limited processing capacity')
b_4$work_sector <- 'Intergov. Org. / Non-profit'

b_final <- rbind(b_1,b_2, b_3, b_4)

#Data are disseminated in a non-standardised way
c_1 <- subset(df_university, df_university[,2]=='Data are dissemintated in a non-standardised way')
c_1$work_sector <- 'University'

c_2 <- subset(df_government, df_government[,2]=='Data are dissemintated in a non-standardised way')
c_2$work_sector <- 'Government'

c_3 <- subset(df_company, df_company[,2]=='Data are dissemintated in a non-standardised way')
c_3$work_sector <- 'Established company / Start-up'

c_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Data are dissemintated in a non-standardised way')
c_4$work_sector <- 'Intergov. Org. / Non-profit'

c_final <- rbind(c_1,c_2, c_3, c_4)

# Too many data platforms

d_1 <- subset(df_university, df_university[,2]=='Too many data platforms and portals')
d_1$work_sector <- 'University'

d_2 <- subset(df_government, df_government[,2]=='Too many data platforms and portals')
d_2$work_sector <- 'Government'

d_3 <- subset(df_company, df_company[,2]=='Too many data platforms and portals')
d_3$work_sector <- 'Established company / Start-up'

d_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Too many data platforms and portals')
d_4$work_sector <- 'Intergov. Org. / Non-profit'

d_final <- rbind(d_1, d_2, d_3, d_4)

# data discovery
e_1 <- subset(df_university, df_university[,2]=='Data discovery')
e_1$work_sector <- 'University'

e_2 <- subset(df_government, df_government[,2]=='Data discovery')
e_2$work_sector <- 'Government'

e_3 <- subset(df_company, df_company[,2]=='Data discovery')
e_3$work_sector <- 'Established company / Start-up'

e_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Data discovery')
e_4$work_sector <- 'Intergov. Org. / Non-profit'

e_final <- rbind(e_1, e_2, e_3, e_4)

# Data service are too restricted
f_1 <- subset(df_university, df_university[,2]=='Data services are too restricted')
f_1$work_sector <- 'University'

f_2 <- subset(df_government, df_government[,2]=='Data services are too restricted')
f_2$work_sector <- 'Government'

f_3 <- subset(df_company, df_company[,2]=='Data services are too restricted')
f_3$work_sector <- 'Established company / Start-up'

f_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Data services are too restricted')
f_4$work_sector <- 'Intergov. Org. / Non-profit'

f_final <- rbind(f_1, f_2, f_3, f_4)

# Complex data formats
g_1 <- subset(df_university, df_university[,2]=='Complex data formats')
g_1$work_sector <- 'University'

g_2 <- subset(df_government, df_government[,2]=='Complex data formats')
g_2$work_sector <- 'Government'

g_3 <- subset(df_company, df_company[,2]=='Complex data formats')
g_3$work_sector <- 'Established company / Start-up'

g_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Complex data formats')
g_4$work_sector <- 'Intergov. Org. / Non-profit'

g_final <- rbind(g_1, g_2, g_3, g_4)


# Lacking easy-to-use tools
h_1 <- subset(df_university, df_university[,2]=='Lacking easy-to-use tools ')
h_1$work_sector <- 'University'

h_2 <- subset(df_government, df_government[,2]=='Lacking easy-to-use tools ')
h_2$work_sector <- 'Government'

h_3 <- subset(df_company, df_company[,2]=='Lacking easy-to-use tools ')
h_3$work_sector <- 'Established company / Start-up'

h_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Lacking easy-to-use tools ')
h_4$work_sector <- 'Intergov. Org. / Non-profit'

h_final <- rbind(h_1, h_2, h_3, h_4)

# Data access systems
i_1 <- subset(df_university, df_university[,2]=='Data access systems')
i_1$work_sector <- 'University'

i_2 <- subset(df_government, df_government[,2]=='Data access systems')
i_2$work_sector <- 'Government'

i_3 <- subset(df_company, df_company[,2]=='Data access systems')
i_3$work_sector <- 'Established company / Start-up'

i_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Data access systems')
i_4$work_sector <- 'Intergov. Org. / Non-profit'

i_final <- rbind(i_1, i_2, i_3, i_4)

# Cost of data services
k_1 <- subset(df_university, df_university[,2]=='Cost of data services')
k_1$work_sector <- 'University'

k_2 <- subset(df_government, df_government[,2]=='Cost of data services')
k_2$work_sector <- 'Government'

k_3 <- subset(df_company, df_company[,2]=='Cost of data services')
k_3$work_sector <- 'Established company / Start-up'

k_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Cost of data services')
k_4$work_sector <- 'Intergov. Org. / Non-profit'

k_final <- rbind(k_1, k_2, k_3, k_4)

# Combining different kind of geospatial data
l_1 <- subset(df_university, df_university[,2]=='Combining different kind of geospatial data')
l_1$work_sector <- 'University'

l_2 <- subset(df_government, df_government[,2]=='Combining different kind of geospatial data')
l_2$work_sector <- 'Government'

l_3 <- subset(df_company, df_company[,2]=='Combining different kind of geospatial data')
l_3$work_sector <- 'Established company / Start-up'

l_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Combining different kind of geospatial data')
l_4$work_sector <- 'Intergov. Org. / Non-profit'

l_final <- rbind(l_1, l_2, l_3, l_4)

# Data complexity
m_1 <- subset(df_university, df_university[,2]=='Data complexity')
m_1$work_sector <- 'University'

m_2 <- subset(df_government, df_government[,2]=='Data complexity')
m_2$work_sector <- 'Government'

m_3 <- subset(df_company, df_company[,2]=='Data complexity')
m_3$work_sector <- 'Established company / Start-up'

m_4 <- subset(df_io_non_profit, df_io_non_profit[,2]=='Data complexity')
m_4$work_sector <- 'Intergov. Org. / Non-profit'

m_final <- rbind(m_1, m_2, m_3, m_4)

likert_perc <- ggplot(data=m_final, aes(x=work_sector, y=value, fill=Scale)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=col) +
  labs(x="Challenge", y="Percent") +
  coord_flip() +
  theme_light()+
  scale_x_discrete(labels = wrap_format(20)) +
  guides(fill=guide_legend(reverse=TRUE))+
  #  scale_x_continuous(name="Percent") +
  
  theme(legend.position='bottom',
        legend.title=element_blank(),
        axis.text=element_text(size=14),
        legend.text = element_text(size=12),
        strip.text.x=element_text(size=12),
        axis.title = element_text(size=14),
        aspect.ratio=1/3)
