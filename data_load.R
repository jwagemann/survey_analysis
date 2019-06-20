source('data_survey_functions.R') # Load dataUse_freq
library('data.table')

# Set working directory
wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

# Load data set
df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

# Number of respondents 
no_of_respondents <- nrow(df_new)

#####################################################################
# 1 Personal information
#####################################################################

#Load country of residence
df_11 <- as.data.frame(df_new['X1.1'])
colnames(df_11) <- c('cor')
df_11_freq <- as.data.frame(count(df_11))
df_11_freq <- df_11_freq[-38,]

# Filter all countries with more than 10 responses
df_11_filter <- df_11_freq[df_11_freq$freq >=10,]

df_11_filter_all <- df_11
is.na(df_11_filter_all) <- df_11_filter_all != c('Austria') & df_11_filter_all !=c('Germany') & df_11_filter_all !=c('Italy') & df_11_filter_all !=c('Spain') & df_11_filter_all !=c('United Kingdom') & df_11_filter_all !=c('United States of America') & df_11_filter_all !=c('Canada')

df_11_filter_all$region <- ifelse(df_11_filter_all$cor == "United States of America" | df_11_filter_all$cor == 'Canada','United States of America & Canada', 'Europe')

# Sort work sectors based on percent values
df_11_freq_ord <- df_11_freq %>%
  arrange(freq) %>%               # sort your dataframe
  mutate(cor = factor(cor, unique(cor)))

# Load age groups
df_14 <- as.data.frame(df_new[,'X1.4'])
colnames(df_14) <- c('age_group')

df_14_freq <- count(na.omit(df_14))
colnames(df_14_freq) <- c('age.group','freq')
levels_14 = c('< 20 years', '20 - 30 years', '30 - 40 years', as.character(df_14_freq[5,1]), '50 - 60 years', '> 60 years')
df_14_freq$age.group <- factor(df_14_freq$age.group, levels=levels_14)



#####################################################################
# 2 Information about work
#####################################################################

# Work sector - What sector do you work in?
df_2 <- df_new[,'X2.1']

# Differentiation public / private research institute
# If you work in University, please specify if you work in a public or private research institute
df_211 <- df_new[,'X2.1.1']

# Responses for Other - Other work sector
df_213 <- df_new[,'X2.1.3']

# Data user / Data provider - Who do you most identify with?
df_22 <- df_new[,c('X2.2','X2.2.4')]
df_22_1 <- df_new[,c('X2.2')]
df_22_freq <- count(df_22_1)
no_data_providers <- df_22_freq[1,2]
no_data_users <- df_22_freq[2,2]
# Data user - Please specify
df_221 <- as.data.frame(df_new[,'X2.2.1'])
# Data provider - Please specify
df_222 <- df_new[,'X2.2.2']
# Data user - Other
df_223 <- df_new[,'X2.2.3']
# Data provider - Other
df_224 <- df_new[,'X2.2.4']

colnames(df_221) <- c('data_user_type')
df_221$data_user_type <- as.character(df_221$data_user_type)

# Integrate responses "Other", when users responded with both
df_22$X2.2 <- ifelse(df_22$X2.2=="Other" & (grepl("Both",df_22$X2.2.4) | grepl("user and",df_22$X2.2.4) | grepl(";",df_22$X2.2.4) | grepl("user/",df_22$X2.2.4)),"Data user;Data provider",as.character(df_22$X2.2))
df_22$X2.2 <- ifelse(df_22$X2.2=="Other" & grepl("Data user for",df_22$X2.2.4),"Data user",as.character(df_22$X2.2))

# Count multiple work sectors into different listings
df_2_stacked <- stack(setNames(strsplit(as.character(df_2),';'), df_2))
df_2_summary <- as.data.frame(table(df_2_stacked$values))
df_2_summary$per <- df_2_summary$Freq / no_of_respondents * 100

# Summarize research institute responses and add percents
df_211_summary <- as.data.frame(table(df_211))
df_211_summary$per <- df_211_summary$Freq / df_2_summary[7,'Freq'] * 100

# Sort work sectors based on percent values
df_2_summary <- df_2_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(Var1 = factor(Var1, unique(Var1)))

df_211_summary <- df_211_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(df_211 = factor(df_211, unique(df_211)))

# Summarize data user / data provider 
df_22_stacked <- stack(setNames(strsplit(as.character(df_22$X2.2),';'),df_22))
df_22_summary <- as.data.frame(table(df_22_stacked$values))
df_22_summary$per <- df_22_summary$Freq / no_of_respondents * 100

# Summarize data user roles
no_of_data_users <- df_22_summary[2,2]
df_221_summary <- as.data.frame(table(df_221))
df_221_summary$per <- df_221_summary$Freq / no_data_users * 100

# Sort data user roles based on percent values
df_221_summary <- df_221_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(df_221 = factor(df_221, unique(df_221)))

# Summarize data provider roles
no_of_data_providers <- df_22_summary[1,2]
df_222_summary <- as.data.frame(table(df_222))
df_222_summary$per <- df_222_summary$Freq / no_data_providers * 100

# Sort data provider roles based on percent values
df_222_summary <- df_222_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(df_222 = factor(df_222, unique(df_222)))

#####################################################################
# 3 Data usage
#####################################################################

# 3.1 What kind of data do you currently use or would like to use in the future?
# --> see 31_data_use_prepare.R - dataUse_freq

# What hindered you to work with the data you would like to use in the future so far?
df_32 <- as.data.frame(df_new[,'X3.2'])
colnames(df_32) <- c('data.use.constraint')

df_32_other <- df_new[,'X3.2.1']

df_32_freq <- splitInRows(df_32,1,no_of_respondents)


# Application area = What do you use the data for?
df_33 <- as.data.frame(df_new[,'X3.3'])

df_33_other <- df_new[,'X3.3.1']

df_33_sum <- count(df_33)
colnames(df_33_sum) <- c('application.area', 'freq')
df_33_sum$per <- df_33_sum$freq / no_of_respondents


# Load data formats
df_34 <- as.data.frame(df_new[,'X3.4'])
df_34_other <- df_new[,'X3.4.1']
colnames(df_34) <- c('data.format')

df_34_freq <- splitInRows(df_34,1,no_of_respondents)


# What applications are you interested in doing with Big Earth Data?
df_likert_35 <- df_new[, c('X3.5.machine.learning',
                           'X3.5.web.applications',
                           'X3.5.data.visualizations',
                           'X3.5.global.regional.analyses',
                           'X3.5.Analysis.over.long.time.spans',
                           'X3.5.time.series.analysis')]

levels_35 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very much interested')

df_likert_35_ord <- lapply(df_likert_35, function(x) ordered(x, levels = levels_35))
df_likert_35_res <- do.call(data.frame, df_likert_35_ord)

names(df_likert_35_res) <- c(
  X3.5.machine.learning="Machine-learning / Deep learning",
  X3.5.web.applications="Interactive web applications",
  X3.5.data.visualizations="Data visualizations",
  X3.5.global.regional.analyses="Global / regional data analysis",
  X3.5.Analysis.over.long.time="Analyses over long time spans",
  X3.5.time.series.analysis="Time-series analysis")

likertObj_35 <- likert(df_likert_35_res)

# Would you invest time and resources to work with data in a complex or non familiar format, if you are really interested in using the data?
df_36 <- df_new[,'X3.6']
df_36_reason <- df_new[,'X3.6.1']

df_36_freq <- count(df_36)
df_36_freq$perc <- df_36_freq$freq/ no_of_respondents * 100

# Sort reasons based on freq values
df_36_freq <- df_36_freq %>%
  arrange(freq) %>%               # sort your dataframe
  mutate(x = factor(x, unique(x)))

# Please rate how much of the following statements are true for you?
df_37 <- df_new[, c('X3.7.Combination.data.sources',
                    'X3.7.Procuction.value.added.products',
                    'X3.7.Consumption.open.data',
                    'X3.7.Sharing.results',
                    'X3.7.Importantance.task.parallelisation')]

df_37_split <- separate(df_37,X3.7.Combination.data.sources, sep='/',into='X3.7.Combination.data.sources')
df_37_split <- separate(df_37_split,X3.7.Procuction.value.added.products, sep='/',into='X3.7.Production.value.added.products')
df_37_split <- separate(df_37_split,X3.7.Consumption.open.data, sep='/',into='X3.7.Consumption.open.data')
df_37_split <- separate(df_37_split,X3.7.Sharing.results, sep='/',into='X3.7.Sharing.results')
df_37_split <- separate(df_37_split,X3.7.Importantance.task.parallelisation, sep='/',into='X3.7.Importance.task.parallelisation')

df_37_freq <- count(df_37_split$X3.7.Combination.data.sources)
df_37_freq$production_value.added.products <- count(df_37_split$X3.7.Production.value.added.products)[-6,2]
df_37_freq$consumption.open.data <- count(df_37_split$X3.7.Consumption.open.data)[-6,2]
df_37_freq$sharing.results <- count(df_37_split$X3.7.Sharing.results)[-6,2]
df_37_freq$importance.task.parallelistation <- count(df_37_split$X3.7.Importance.task.parallelisation)[-6,2]

df_37_relfreq <- df_37_freq[,c(2:6)] / no_of_respondents * 100
df_37_relfreq$cat <- df_37_freq$x

# Example of a data processing chain
df_38 <- df_new[, c('X3.8')]

count(df_38)
nrow(na.omit(df_38))     


#####################################################################
# 4 Data handling
#####################################################################
# Load programming languages
df_41 <- as.data.frame(df_new[,'X4.1'])
colnames(df_41) <- c('programming.language')

df_411 <- df_new[,'X4.1.1']

df_41_freq <- splitInRows(df_41,1,no_of_respondents)
df_41_freq <- df_41_freq[c(-8,-9,-14),]
df_41_order <- df_41_freq %>%
  arrange(perc) %>% 
  mutate(programming.language = factor(programming.language, unique(programming.language)))


#Load desktop software - Yes / No
df_42 <- as.data.frame(df_new[,'X4.2'])
colnames(df_42) <- c('yes.no')
df_42_freq <- count(df_42)
df_42_freq$perc <- df_42_freq$freq / no_of_respondents * 100
df_42_freq <- df_42_freq[-3,]

# Load what kind of desktop software
df_421 <- as.data.frame(df_new[,'X4.2.1'])
colnames(df_421) <- 'software'
df_422 <- as.data.frame(df_new[,'X4.2.2'])

df_421_freq <- splitInRows(df_421,1,no_of_respondents)
df_421_freq <- df_421_freq[c(-9),]

df_421_order <- df_421_freq %>%
  arrange(perc) %>% 
  mutate(software = factor(software, unique(software)))


# 4.3 How do you currently or how would you like in the future to access large volumes of Big Earth Data
# --> load 43_data_access_systems_prepare.R

df_431 <- df_new[,'X4.3.1']

# 4.4 How satisfied are you with the current data access service you use?
# --> load 44_system_satisfaction_prepare.R

df_441 <- df_new[,'X4.4.other']

# 4.5 How do you process and analyse data?

df_45 <- df_new[,c('X4.5.cloud.code.editor', 'X4.5.code.routines.access.cloud.services', 'X4.5.code.routines.python.r','X4.5.geospatial.software', 'X4.5.Other')]
df_451 <- df_new[,c('X4.5.1')]


df_45_freqs <- count(df_45$X4.5.cloud.code.editor)  %>% left_join(count(df_45$X4.5.code.routines.access.cloud.services),by='x') %>%
  left_join(count(df_45$X4.5.code.routines.python.r),by='x') %>%
  left_join(count(df_45$X4.5.geospatial.software) ,by='x')

colnames(df_45_freqs) <- c('Frequency','cloud.code.editor', 'code.routines.access.cloud.services', 'code.routines.python.r', 'geospatial.software')

df_45_colsums <- as.data.frame(colSums(x=df_45_freqs[-4,-1]))

df_45_perc <- df_45_freqs %>% mutate(cloud.code.editor = cloud.code.editor / df_45_colsums[1,1] *100) %>%
  mutate(code.routines.access.cloud.services = code.routines.access.cloud.services / df_45_colsums[2,1] * 100) %>%
  mutate(code.routines.python.r = code.routines.python.r / df_45_colsums[3,1] * 100) %>%
  mutate(geospatial.software = geospatial.software / df_45_colsums[4,1] * 100)

tmp <- as.data.frame(df_45_perc[1,])
levels_45_freqs <- colnames(df_45_perc[,-1])
df_45_transpose <- transpose(tmp[,2:5])
df_45_transpose$processing.type <- levels_45_freqs
df_45_transpose_ord <- df_45_transpose[order(df_45_transpose$V1),]


levels_45 <- c('Never', 'Sometimes', 'Always')
df_45_melt <- melt(df_45_freqs[-4,],id.vars='Frequency')
df_45_melt$variable <- factor(df_45_melt$variable, levels=df_45_transpose_ord$processing.type)
df_45_freqs$Frequency <- factor(df_45_freqs$Frequency,levels=levels_45)


# 4.6. Rate how important the following tasks are for you?
# --> see 46_important_statements_prepare.R

#####################################################################
# 5 Data challenges
#####################################################################
# --> see 51_data_challenges_prepare.R


#####################################################################
# 6 Future data services
#####################################################################

df_61 <- df_new[, c('X6.1')]

levels_61 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very interested')

df_61 <- as.data.frame(df_61[complete.cases(df_61)])
nrow_61 <- nrow(df_61)
df_61_count <- as.data.frame(count(df_61))
colnames(df_61_count) <- c('Interest', 'freq')

df_61_perc <- df_61_count$freq / nrow_61 * 100
df_61_perc <- cbind.data.frame(df_61_count[,1], df_61_perc)
colnames(df_61_perc) <- c('Interest', 'freq')

df_61_freq <- df_61_count %>%
  arrange(freq) %>% 
  mutate(Interest = factor(Interest, levels=levels_61))

df_61_perc <- df_61_perc %>%
  arrange(freq) %>% 
  mutate(Interest = factor(Interest, levels=levels_61))

df_611 <- df_new[,c('X6.1.1')]

# 6.2 Concerning the legal policy of the cloud service, what would you prefer?

df_62 <- df_new[,'X6.2']
df_62 <- as.data.frame(df_62[complete.cases(df_62)])
nrow_62 <- nrow(df_62)
df_62_freq <- count(df_62)
df_62_perc <- df_62_freq$freq / nrow_62 * 100
df_62_perc <- cbind.data.frame(df_62_freq[,1], df_62_perc)
colnames(df_62_perc) <- c('policy', 'freq')
colnames(df_62_freq) <- c('policy', 'freq')

levels_62 <- c("Commercial cloud vendor, such as AWS or GCP",
               "Publicly-funded cloud, e.g. EOSC", 
               'Publicly-funded specialised cloud, e.g.WekEO',
               "I do not mind",
               "None of the above")

df_62_freq$policy <- levels_62
df_62_perc$policy <- levels_62

df_62_freq_ord <- df_62_freq %>%
  arrange(freq) %>% 
  mutate(policy = factor(policy, levels=levels_62))

df_62_perc_ord <- df_62_perc %>%
  arrange(freq) %>% 
  mutate(policy = factor(policy, levels=levels_62))

# 6.3 Use of cloud services
df_63 <- df_new[,'X6.3']
df_631 <- df_new[,'X6.3.1']

df_632 <- df_new[,'X6.3.2']

df_63 <- as.data.frame(df_63[complete.cases(df_63)])
nrow_63 <- nrow(df_63)

df_63_freq <- splitInRows(df_63,1,nrow_63)
colnames(df_63_freq) <- c('Use', 'freq','perc')

df_631 <- as.data.frame(df_631[complete.cases(df_631)])
nrow_631 <- nrow(df_631)
df_631_freq <- splitInRows(df_631,1,nrow_631)
colnames(df_631_freq) <- c('Response', 'freq','perc')

levels_631 <- c("Yes", "No", "I don't know")
df_631_freq_ord <- df_631_freq %>%
  arrange(freq) %>% 
  mutate(Response = factor(Response, levels=levels_631))

# 6.4 Working with cloud services, which statements would be true?

df_64 <- df_new[,'X6.4']
df_64 <- as.data.frame(df_64[complete.cases(df_64)])
nrow_64 <- nrow(df_64)
df_64_freq <- splitInRows(df_64,1,nrow_64)
colnames(df_64_freq) <- c('Statement', 'freq', 'perc')

df_64_freq_ord <- df_64_freq %>%
  arrange(freq) %>% 
  mutate(Statement = factor(Statement, unique(Statement)))

# 6.5 How strong do you consider the following security aspects as a risk of cloud services?

# --> see 65_cloud_security_aspects_prepare.R

df_651 <- df_new[,'X6.5.1']

# 6.6 Please provide an example of a data workflow or data processing task you would like to do in the cloud

df_66 <- df_new[,'X6.6']

# 6.7 Would you be ablet to estimate the technical requirements you would need for your data storage and/or processing tasks in the cloud?

df_67 <- df_new[,'X6.7']
df_67_freq <- count(df_67)

df_671 <- df_new['X6.7.1']

# 6.8 Would you be willing to pay for processing services

df_68 <- df_new[,'X6.8']
df_68_freq <- count(df_68)
df_681 <- as.data.frame(df_new[,'X6.8.1'])
nrow_681 <- nrow(na.omit(df_681))

df_681_freq <- splitInRows(df_681,1,nrow_681)
colnames(df_681_freq) <- c('cloud.service', 'freq','perc')

df_68_freq_ord <- df_68_freq %>%
  arrange(freq) %>% 
  mutate(x = factor(x, unique(x)))

df_68_freq_ord <- df_68_freq_ord[-1,]

df_681_freq_ord <- df_681_freq %>%
  arrange(freq) %>% 
  mutate(cloud.service = factor(cloud.service, unique(cloud.service)))

df_681_freq_ord <- df_681_freq_ord[c(-1,-5),]

df_6811 <- df_new[,'X6.8.1.1']

df_682 <- as.data.frame(df_new[,'X6.8.2'])
df_682_freq <- splitInRows(df_682,1,nrow_681)
colnames(df_682_freq) <- c('amount', 'freq','perc')

levels_682 <- c('up to 100 Euro / 100 US Dollars', 
                'up to 500 Euro / 500 US Dollars', 
                'up to 1,000 Euros / 1,000 US Dollars',
                'up to 10,000 Euros / 10,000 US Dollars',
                'up to 50,000 Euros / 50,000 US Dollars',
                'up to 100,000 Euros / 100,000 US Dollars',
                'more than 100,000 Euros / 100,000 US Dollars',
                'I prefer a costing model based on a monthly/annual subscription fee')

df_682_freq <- df_682_freq[-9,]
df_682_freq_ord <- df_682_freq %>%
  arrange(freq) %>% 
  mutate(amount = factor(amount, levels=levels_682))

#####################################################################
# 7 Final comment
#####################################################################

df_71 <- df_new[,'X7.1']
df_72 <- df_new[,'X7.2']
df_73 <- df_new[,'X7.3']

