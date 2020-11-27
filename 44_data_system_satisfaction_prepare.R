
# How satisfied are you with the current data access service you use?
df_44 <- as.data.frame(df_new[, c('X4.4.download.service',
                           'X4.4.cloud.computing.infrastructure',
                           'X4.4.ogc.service',
                           'X4.4.custom.api.opendap',
                           'X4.4.virtual.research.infrastructure',
                           'X4.4.data.cube.technology',
                           'X4.4.spatial.array.database')])
# Remove responses that do not make sense
df_44 <- df_44[-67,]

# Combine data systems that are currently used with satisfaction
a <- cbind.data.frame(dataSystems_freq$A1,df_44[,1])
b <- cbind.data.frame(dataSystems_freq$B1,df_44[,2])
c <- cbind.data.frame(dataSystems_freq$C1,df_44[,3])
d <- cbind.data.frame(dataSystems_freq$D1,df_44[,4])
e <- cbind.data.frame(dataSystems_freq$E1,df_44[,5])
f <- cbind.data.frame(dataSystems_freq$F1,df_44[,6])
g <- cbind.data.frame(dataSystems_freq$G1,df_44[,7])

# Filter all entries who are currently used and satisfaction is not NA or "Not applicable"
a_filter <- plyr::count(a[!is.na(a[,1]) & !a[,2]=='Not applicable' & !is.na(a[,2]),])
b_filter <- plyr::count(b[!is.na(b[,1]) & !b[,2]=='Not applicable' & !is.na(b[,2]),])
c_filter <- plyr::count(c[!is.na(c[,1]) & !c[,2]=='Not applicable' & !is.na(c[,2]),])
d_filter <- plyr::count(d[!is.na(d[,1]) & !d[,2]=='Not applicable' & !is.na(d[,2]),])
e_filter <- plyr::count(e[!is.na(e[,1]) & !e[,2]=='Not applicable' & !is.na(e[,2]),])
f_filter <- plyr::count(f[!is.na(f[,1]) & !f[,2]=='Not applicable' & !is.na(f[,2]),])
g_filter <- plyr::count(g[!is.na(g[,1]) & !g[,2]=='Not applicable' & !is.na(g[,2]),])

# Get number of rows for filtered data frames, in order to calculate percentages
a_nrow <- nrow(a[!is.na(a[,1]) & !a[,2]=='Not applicable' & !is.na(a[,2]),])
b_nrow <- nrow(b[!is.na(b[,1]) & !b[,2]=='Not applicable' & !is.na(b[,2]),])
c_nrow <- nrow(c[!is.na(c[,1]) & !c[,2]=='Not applicable' & !is.na(c[,2]),])
d_nrow <- nrow(d[!is.na(d[,1]) & !d[,2]=='Not applicable' & !is.na(d[,2]),])
e_nrow <- nrow(e[!is.na(e[,1]) & !e[,2]=='Not applicable' & !is.na(e[,2]),])
f_nrow <- nrow(f[!is.na(f[,1]) & !f[,2]=='Not applicable' & !is.na(f[,2]),])
g_nrow <- nrow(g[!is.na(g[,1]) & !g[,2]=='Not applicable' & !is.na(g[,2]),])

# Calculate relative frequencies
a_filter$perc <- a_filter$freq / a_nrow * 100
b_filter$perc <- b_filter$freq / b_nrow * 100
c_filter$perc <- c_filter$freq / c_nrow * 100
d_filter$perc <- d_filter$freq / d_nrow * 100
e_filter$perc <- e_filter$freq / e_nrow * 100
f_filter$perc <- f_filter$freq / f_nrow * 100
g_filter$perc <- g_filter$freq / g_nrow * 100

# set colnames
colnames(a_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(b_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(c_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(d_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(e_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(f_filter) <- c('current.use', 'satisfaction', 'freq','perc')
colnames(g_filter) <- c('current.use', 'satisfaction', 'freq','perc')

# Bring filtered data frames together - Absolute numbers
df_44_merged_abs <- a_filter[,c(2,3)] %>% left_join(b_filter[,c(2,3)], by='satisfaction') %>% 
  left_join(c_filter[,c(2,3)], by='satisfaction') %>% 
  left_join(d_filter[,c(2,3)], by='satisfaction') %>% 
  left_join(e_filter[,c(2,3)], by='satisfaction') %>% 
  left_join(f_filter[,c(2,3)], by='satisfaction') %>% 
  left_join(g_filter[,c(2,3)], by='satisfaction')

# Set colnames
colnames(df_44_merged_abs) <- c('Satisfaction','Download Service','Cloud computing', 'OGC web service', 'Custom API', 'Virtual research infrastructure', 'Data Cube Technology','Array Database')

# Bring filtered data frame together - Relative frequencies
df_44_merged_rel <- a_filter[,c(2,4)] %>% left_join(b_filter[,c(2,4)], by='satisfaction') %>% 
  left_join(c_filter[,c(2,4)], by='satisfaction') %>% 
  left_join(d_filter[,c(2,4)], by='satisfaction') %>% 
  left_join(e_filter[,c(2,4)], by='satisfaction') %>% 
  left_join(f_filter[,c(2,4)], by='satisfaction') %>% 
  left_join(g_filter[,c(2,4)], by='satisfaction')

# Set colnames
colnames(df_44_merged_rel) <- c('Satisfaction','Download service','Cloud-computing infrastructure', 'OGC web service', 'Custom API / OpenDAP', 'Virtual research infrastructure', 'Data cube technology','Array / Spatial database')

# Set NAs to zero responses
df_44_merged_rel[is.na(df_44_merged_rel)] <- 0
df_44_merged_abs[is.na(df_44_merged_abs)] <- 0

# Order of satisfaction levels
levels_44 <- c('Very dissatisfied', 'Dissatisfied', 'Neither satisfied nor dissatisfied', 'Satisfied', 'Very satisfied')

df_44_merged_rel$Satisfaction <- factor(df_44_merged_rel$Satisfaction,levels=levels_44)
df_44_merged_abs$Satisfaction <- factor(df_44_merged_abs$Satisfaction,levels=levels_44)

# Order data based on satisfaction levels
df_44_merged_ord <- df_44_merged_rel[order(factor(df_44_merged_rel$Satisfaction,levels=levels_44)),]
df_44_merged_ord_abs <- df_44_merged_abs[order(factor(df_44_merged_abs$Satisfaction,levels=levels_44)),]

# Extract entries for 'satisfied' and 'very satisfied' in percent
df_44_satisfaction_subset <-df_44_merged_ord[c(4,5),]
df_44_satisfaction_sum <- as.data.frame(colSums(df_44_satisfaction_subset[,-1]))
df_44_satisfaction_sum$items <- rownames(df_44_satisfaction_sum)
df_44_satisfaction_sum$data.systems <- rownames(df_44_satisfaction_sum)
# Order the entries in an increasing order 
df_44_satisfaction_ord <- df_44_satisfaction_sum[order(df_44_satisfaction_sum$`colSums(df_44_satisfaction_subset[, -1])`),]

# Extract entries for 'satisfied' and 'very satisfied' in absolute numbers
df_44_satisfaction_subset_abs <-df_44_merged_ord_abs[c(4,5),]
df_44_satisfaction_sum_abs <- as.data.frame(colSums(df_44_satisfaction_subset_abs[,-1]))
df_44_satisfaction_sum_abs$data.systems <- rownames(df_44_satisfaction_sum_abs)
# Order the entries in an increasing order
df_44_satisfaction_ord_abs <- df_44_satisfaction_sum_abs[order(df_44_satisfaction_sum_abs$`colSums(df_44_satisfaction_subset_abs[, -1])`),]


# Melt data frame for stacked bar and reorder data.systems based on satisfaction levels - Frequencies
df_44_merged_melt <- reshape2::melt(df_44_merged_ord,id.vars='Satisfaction')
df_44_merged_melt$variable <- factor(df_44_merged_melt$variable, levels=df_44_satisfaction_ord$data.systems)

# Melt data frame for stacked bar and reorder data.systems based on satisfaction levels - Absolute numbers
df_44_merged_abs_melt <- reshape2::melt(df_44_merged_ord_abs[,1:4], id.vars='Satisfaction')
df_44_merged_abs_melt$variable <- factor(df_44_merged_abs_melt$variable, levels=df_44_satisfaction_ord_abs$data.systems)
