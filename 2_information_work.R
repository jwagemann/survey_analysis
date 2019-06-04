setwd('/Users/julia_wagemann/Documents/github//survey_analysis/')

library(ggplot2)
library(dplyr)
library(scales)

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")

no_of_respondents <- nrow(df_new)

# Work sector - What sector do you work in?
df_2 <- df_new[,'X2.1']

# Differentiation public / private research institute
# If you work in University, please specify if you work in a public or private research institute
df_211 <- df_new[,'X2.1.1']

# Responses for Other - Other work sector
df_213 <- df_new[,'X2.1.3']

# Data user / Data provider - Who do you most identify with?
df_22 <- df_new[,c('X2.2','X2.2.4')]
# Data user - Please specify
df_221 <- df_new[,'X2.2.1']
# Data provider - Please specify
df_222 <- df_new[,'X2.2.2']
# Data user - Other
df_223 <- df_new[,'X2.2.3']
# Data provider - Other
df_224 <- df_new[,'X2.2.4']

df_22$X2.2 <- ifelse(df_22$X2.2=="Other" & (grepl("Both",df_22$X2.2.4) | grepl("user and",df_22$X2.2.4) | grepl(";",df_22$X2.2.4) | grepl("user/",df_22$X2.2.4)),"Data user;Data provider",as.character(df_22$X2.2))
df_22$X2.2 <- ifelse(df_22$X2.2=="Other" & grepl("Data user for",df_22$X2.2.4),"Data user",as.character(df_22$X2.2))

# Count multiple listings into individual work sectors
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

df_22_stacked <- stack(setNames(strsplit(as.character(df_22$X2.2),';'),df_22))
df_22_summary <- as.data.frame(table(df_22_stacked$values))
df_22_summary$per <- df_22_summary$Freq / no_of_respondents * 100

df_221_summary <- as.data.frame(table(df_221))
df_221_summary$per <- df_221_summary$Freq / 155 * 100

df_221_summary <- df_221_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(df_221 = factor(df_221, unique(df_221)))


df_222_summary <- as.data.frame(table(df_222))
df_222_summary$per <- df_222_summary$Freq / 35 * 100

df_222_summary <- df_222_summary %>%
  arrange(per) %>%               # sort your dataframe
  mutate(df_222 = factor(df_222, unique(df_222)))

df_223_summary <- as.data.frame(table(df_223))

# Define ggplot blank theme for pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# PIE chart for work sectors
bp_2 <- ggplot(df_2_summary, aes(x="", y=per, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  guides(fill=guide_legend(reverse=TRUE, title="", label.position='left')) 

pie_2 <- bp + coord_polar('y', start=0) + scale_fill_brewer(palette='Spectral') + 
  blank_theme + theme(axis.text.x=element_blank(), legend.position="left",legend.text=element_text(size=14), legend.text.align = 1)
pie_2

# Bar chart for research institutes
bp_21 <- ggplot(data=df_21_summary,aes(x='', y=per, fill=df_21)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_fill_brewer(palette='Blues') +
  blank_theme + theme(legend.text=element_text(size=12), axis.text.x=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE, title=""))

bp_21

# Bar chart for data user / data provider
bp_22 <- ggplot(data=df_22_summary,aes(x='', y=per, fill=Var1)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_fill_brewer(palette='Blues') +
  blank_theme + theme(legend.text=element_text(size=12),axis.text.x=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE, title=""))

bp_22

# PIE chart for data user overview
bp_221 <- ggplot(df_221_summary, aes(x="", y=per, fill=df_221))+
  geom_bar(width = 1, stat = "identity") +
  guides(fill=guide_legend(reverse=TRUE, title="", label.position='left')) 

pie_221 <- bp_221 + coord_polar('y', start=0) + scale_fill_brewer(palette='Spectral') + 
  blank_theme + theme(axis.text.x=element_blank(), legend.position="left",legend.text=element_text(size=14), legend.text.align = 1)
pie_221

# PIE chart for data provider overview
bp_222 <- ggplot(df_222_summary, aes(x="", y=per, fill=df_222))+
  geom_bar(width = 1, stat = "identity") +
  guides(fill=guide_legend(reverse=TRUE, title="", label.position='right')) 

pie_222 <- bp_222 + coord_polar('y', start=0) + scale_fill_brewer(palette='Spectral') + 
  blank_theme + theme(axis.text.x=element_blank(), legend.position="right",legend.text=element_text(size=14), legend.text.align = 0)
pie_222
