# Question 6.5 How strong do you consider the following security aspects as a risk of cloud services?
df_65 <- df_new[, c('X6.5.data.integrity',
                    'X6.5.data.breaches',
                    'X6.5.data.loss',
                    'X6.5.service.unavailability',
                    'X6.5.data.security')]

levels_65 <- c('Major risk', 'Risk','No risk at all','Might be a risk, but not important for me')
wr <- df_11_filter_all$region

# Bring security together with regions Europe and US / Canada
security <- cbind(wr, df_65)

# nrow=112
security_eur <- as.data.frame(subset(security, security[,1]=='Europe'))
security_eur_clean <- security_eur[!(rowSums(is.na(security_eur))==5),]
nrow(security_eur_clean)
# --> nrow=111

# nrow=47
security_us_ca <- as.data.frame(subset(security, security[,1]=='United States of America & Canada'))
security_us_ca_clean <- security_us_ca[!(rowSums(is.na(security_us_ca))==5),]
nrow(security_us_ca_clean)

# For each response in the US subset, calculate the frequencies
a_us <- plyr::count(security_us_ca$X6.5.data.integrity)
b_us <- plyr::count(security_us_ca$X6.5.data.breaches)
c_us <- plyr::count(security_us_ca$X6.5.data.loss)
d_us <- plyr::count(security_us_ca$X6.5.service.unavailability)
e_us <- plyr::count(security_us_ca$X6.5.data.security)

# For each response in the EUR subset, calculate the frequencies
a_eur <- plyr::count(security_eur_clean$X6.5.data.integrity)
b_eur <- plyr::count(security_eur_clean$X6.5.data.breaches)
c_eur <- plyr::count(security_eur_clean$X6.5.data.loss)
d_eur <- plyr::count(security_eur_clean$X6.5.service.unavailability)
e_eur <- plyr::count(security_eur_clean$X6.5.data.security)

# Bring frequencies together into a data frame
df_eur_freq <- a_eur %>% left_join(b_eur, by='x') %>% left_join(c_eur, by='x') %>% left_join(d_eur, by='x') %>% 
  left_join(e_eur, by='x')

# Bring frequencies together into a data frame
df_us_freq <- a_us %>% left_join(b_us, by='x') %>% left_join(c_us, by='x') %>% left_join(d_us, by='x') %>% 
  left_join(e_us, by='x')

# Add column names
colnames(df_eur_freq) <- c('risk','data.integrity','data.breaches','data.loss','service.unavailability','data.security')
colnames(df_us_freq) <- c('risk','data.integrity','data.breaches','data.loss','service.unavailability','data.security')

# Order risk levels
df_eur_freq$risk <- factor(df_eur_freq$risk, levels=levels_65)
df_eur_freq <- df_eur_freq[-5,]

df_us_freq$risk <- factor(df_us_freq$risk, levels=levels_65)

# Calculate percentages
df_eur_perc <- df_eur_freq[,2:6] / nrow(security_eur_clean) * 100
df_eur_perc <- cbind(df_eur_freq[1], df_eur_perc)
df_eur_freq_ord <- df_eur_freq[order(factor(df_eur_freq$risk, levels=levels_65)),]
df_eur_perc_ord <- df_eur_perc[order(factor(df_eur_perc$risk,levels=levels_65)),]

df_us_perc <- df_us_freq[,2:6] / nrow(security_us_ca) * 100
df_us_perc <- cbind(df_us_freq[1], df_us_perc)
df_us_freq_ord <- df_us_freq[order(factor(df_us_freq$risk, levels=levels_65)),]
df_us_perc_ord <- df_us_perc[order(factor(df_us_perc$risk,levels=levels_65)),]

# Reorder horizontal items based on frequencies of two most important mentions
df_eur_main <-df_eur_freq_ord[c(1,2),]
df_eur_main_sum <- as.data.frame(colSums(df_eur_main[,-1]))
df_eur_main_sum$items <- rownames(df_eur_main_sum)
df_eur_main_ord <- df_eur_main_sum[order(df_eur_main_sum$`colSums(df_eur_main[, -1])`),]

df_us_main <-df_us_freq_ord[c(1,2),]
df_us_main_sum <- as.data.frame(colSums(df_us_main[,-1]))
df_us_main_sum$items <- rownames(df_us_main_sum)
df_us_main_ord <- df_us_main_sum[order(df_us_main_sum$`colSums(df_us_main[, -1])`),]

# Melt data frame
df_eur_perc_melt <- reshape2::melt(df_eur_perc_ord)
df_eur_freq_melt <- reshape2::melt(df_eur_freq_ord)

df_us_perc_melt <- reshape2::melt(df_us_perc_ord)
df_us_freq_melt <- reshape2::melt(df_us_freq_ord)

# Set factor to reorder items
df_eur_perc_melt$variable <- factor(df_eur_perc_melt$variable, levels=df_eur_main_ord$items)
df_eur_freq_melt$variable <- factor(df_eur_freq_melt$variable, levels=df_eur_main_ord$items)

df_us_perc_melt$variable <- factor(df_us_perc_melt$variable, levels=df_us_main_ord$items)
df_us_freq_melt$variable <- factor(df_us_freq_melt$variable, levels=df_us_main_ord$items)

