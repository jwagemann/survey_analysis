#install.packages('likert')
install.packages(ddply)
library(likert)
library(plyr)
require(ggplot2)
library(reshape2)
library(RColorBrewer)

setwd('/Users/julia_wagemann/Documents/github//survey_analysis/')

df_new <- read.csv('./data/20190131_final_results_header_modified.csv', header=TRUE, na.string="")


# What applications are you interested in doing with Big Earth Data?

df_likert_35 <- df_new[, c('X3.5.machine.learning',
                        'X3.5.web.applications',
                        'X3.5.data.visualizations',
                        'X3.5.global.regional.analyses',
                        'X3.5.Analysis.over.long.time.spans',
                        'X3.5.time.series.analysis')]

levels_35 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very much interested')


#df_likert_37 <- df_new[, c('X3.7.Combination.data.sources',
#                        'X37.Production.value.added.products',
#                        'X3.7.Consumption.open.data',
#                        'X3.7.Sharing.results',
#                        'X3.7.Importance.task.parallelisation')]

df_likert_43 <- df_new[, c('X4.3.Download.service',
                           'X4.3.cloud.computing.infrastructure',
                           'X4.3.ogc.service',
                           'X4.3.custom.api.opendap',
                           'X4.3.virtual.research.infrastructure',
                           'X4.3.data.cube.technology',
                           'X4.3.spatial.array.database')]

levels_43 <- c('I currently use this kind of service', 'I would like to use or continue to use this kind of service in the future', 'I am not interested in this kind of service')

# How satisfied are you with the current data access service you use?
df_likert_44 <- df_new[, c('X4.4.download.service',
                        'X4.4.cloud.computing.infrastructure',
                        'X4.4.ogc.service',
                        'X4.4.custom.api.opendap',
                        'X4.4.virtual.research.infrastructure',
                        'X4.4.data.cube.technology',
                        'X4.4.spatial.array.database')]

levels_44 <- c('Very dissatisfied', 'Dissatisfied', 'Neither satisfied nor dissatisfied', 'Satisfied', 'Very satisfied')


# How do you process and analyse data? (Never, Sometimes, Always)
df_likert_45 <- df_new[, c('X4.5.cloud.code.editor',
                        'X4.5.code.routines.access.cloud.services',
                        'X4.5.code.routines.python.r',
                        'X4.5.geospatial.software',
                        'X4.5.Other')]

levels_45 <- c('Never', 'Sometimes', 'Always')

# Please rate how important the follwoing tasks are for you?
df_likert_46 <- df_new[, c('X4.6.server.cloud.processing',
                        'X4.6.parallel.computing',
                        'X4.6.time.series.retrieval',
                        'X4.6.Download.large.data.volumes',
                        'X4.6.standard.data.access',
                        'X4.6.web.applications.visualization',
                        'X4.6.data.discovery',
                        'X4.6.interoperability')]

levels_46 <- c('Not at all important', 'Not important', 'Neither not important nor important', 'Important', 'Very important')

# What are currently the greates obstacles accessing and working with Big Earth Data?
df_rating_51 <- df_new[,c('X5.1.growing.data.volume',
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

df_likert_51 <- data.frame(list(ifelse(df_rating_51==1,'No obstacle at all', 
                       ifelse(df_rating_51==2, 'No obstacle', 
                              ifelse(df_rating_51==3, 'Neither no obstacle nor an obstacle', 
                                     ifelse(df_rating_51==4, 'An obstacle', 
                                            ifelse(df_rating_51==5, 'A great obstacle', 'test')))))))

levels_51 <- c('No obstacle at all', 'No obstacle', 'Neither no obstacle nor an obstacle', 'An obstacle', 'A great obstacle')


# How much are you interested in migrating your processing tasks to a cloud service?
df_likert_61 <- df_new[, c('X6.1')]

levels_61 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very interested')

# How strong do you consider the following security aspects as a risk of cloud services?
df_likert_65 <- df_new[, c('X6.5.data.integrity',
                        'X6.5.data.breaches',
                        'X6.5.data.loss',
                        'X6.5.service.unavailability',
                        'X6.5.data.security',
                        'X6.5.other')]

levels_65 <- c('No risk at all','Might be a risk, but not important for me', 'Risk', 'Major risk')


df_likert_35_ord <- lapply(df_likert_35, function(x) ordered(x, levels = levels_35))
df_likert_35_res <- do.call(data.frame, df_likert_35_ord)

df_likert_43_ord <- lapply(df_likert_43, function(x) ordered(x, levels = levels_43))
df_likert_43_res <- do.call(data.frame, df_likert_43_ord)

df_likert_44_ord <- lapply(df_likert_44, function(x) ordered(x, levels = levels_44))
df_likert_44_res <- do.call(data.frame, df_likert_44_ord)

df_likert_45_ord <- lapply(df_likert_45, function(x) ordered(x, levels = levels_45))
df_likert_45_res <- do.call(data.frame, df_likert_45_ord)

df_likert_46_ord <- lapply(df_likert_46, function(x) ordered(x, levels = levels_46))
df_likert_46_res <- do.call(data.frame, df_likert_46_ord)

df_likert_51_ord <- lapply(df_likert_51, function(x) ordered(x, levels = levels_51))
df_likert_51_res <- do.call(data.frame, df_likert_51_ord)

df_likert_61_res <- data.frame(ordered(df_likert_61, levels = levels_61))


df_likert_65_ord <- lapply(df_likert_65, function(x) ordered(x, levels = levels_65))
df_likert_65_res <- do.call(data.frame, df_likert_65_ord)

names(df_likert_35_res) <- c(
  X3.5.machine.learning="Machine-learning / Deep learning",
  X3.5.web.applications="Interactive web applications",
  X3.5.data.visualizations="Data visualizations",
  X3.5.global.regional.analyses="Global / regional data analysis",
  X3.5.Analysis.over.long.time="Analyses over long time spans",
  X3.5.time.series.analysis="Time-series analysis")

names(df_likert_43_res) <- c(
  X4.3.Download.service = 'Download Service from respective data providers',
  X4.3.cloud.computing.infrastructure = 'Cloud computing infrastructure',
  X4.3.ogc.service = 'via an OGC web service, e.g. WMS or WCS',
  X4.3.custom.api.opendap = 'via  a custom API or an OpeNDAP server from a respective data provider',
  X4.3.virtual.research.infrastructure = 'via a Virtual Research Infrastructure',
  X4.3.data.cube.technology = 'via a Data Cube technology',
  X4.3.spatial.array.database = 'via a spatial or array database')

names(df_likert_44_res) <- c(
  X4.4.download.service = 'Download Service from respective data providers',
  X4.4.cloud.computing.infrastructure = 'Cloud computing infrastructure',
  X4.4.ogc.service = 'via an OGC web service, e.g. WMS or WCS',
  X4.4.custom.api.opendap = 'via  a custom API or an OpeNDAP server from a respective data provider',
  X4.4.virtual.research.infrastructure = 'via a Virtual Research Infrastructure',
  X4.4.data.cube.technology = 'via a Data Cube technology',
  X4.4.spatial.array.database = 'via a spatial or array database')

names(df_likert_45_res) <- c(
  X4.5.cloud.code.editor = "with a code editor in the cloud",
  X4.5.code.routines.access.cloud.services = 'with code-based processing routines accessing cloud services, using provided APIs',
  X4.5.code.routines.python.r = 'with code-based processing routines on a local machine, e.g. a local installation of Python or R',
  X4.5.geospatial.software = 'wiht a geospatial software on a local machine, e.g. QGIS',
  X4.5.other = 'Other'
)

names(df_likert_46_res) <- c(
  X4.6.server.cloud.processing = "Server-/Cloud-based processing",
  X4.6.parallel.computing = "Parallel computing of algorithms",
  X4.6.time.series.retrieval = "Time-series retrieval",
  X4.6.Download.large.data.volumes = "Download of large volumes of data",
  X4.6.standard.data.access = "Access data with a standard protocol, e.g. WMS, WCS",
  X4.6.web.applications.visualization = "On-demand data access, e.g. to create web applications to visualise final and intermediary results",
  X4.6.data.discovery = "Easier data discovery",
  X4.6.interoperability = "Interoperability between different data sets and data systems")

names(df_likert_51_res) <- c(
  X5.1.growing.data.volume = 'Growing data volume',
  X5.1.limited.processing.capacity = 'Limited processing capacity',
  X5.1.complex.data.formats = 'Complex data formats',
  X5.1.non.standard.data = 'Data are dissemintated in a non-standardised way',
  X5.1.data.discovery = 'Data discovery (finding data sets)',
  X5.1.data.access.systems = 'Data access systems',
  X5.1.data.complexity = 'Data complexity',
  X5.1.data.combination = 'Combining different kind of geospatial data, e.g. EO data with meteorological data and shapefiles',
  X5.1.too.many.data.platforms = 'Too many data platforms and portals',
  X5.1.restricted.data.services = 'Data services are too restricted to access large volumes of data',
  X5.1.lack.of.tools = 'Lacking easy-to-use tools for access, preprocessing, visualisation and evaluation',
  X5.1.data.services.cost = 'Cost of data services (cost for non-open data or cost for processing services)'
)

names(df_likert_61_res) <- c(
  X6.1="How much are you interested in migrating your processing tasks to a cloud service")

names(df_likert_65_res) <- c(
  X6.5.data.integrity="Data integrity",
  X6.5.data.breaches="Data breaches",
  X6.5.data.loss="Data loss",
  X6.5.service.unavailability="Service unavailability",
  X6.5.data.security="Security of private / restricted data",
  X6.5.other="Other")

likertObj_35 <- likert(df_likert_35_res)
likertObj_43 <- likert(df_likert_43_res)
likertObj_44 <- likert(df_likert_44_res)
likertObj_45 <- likert(df_likert_45_res)
likertObj_46 <- likert(df_likert_46_res)
likertObj_51 <- likert(df_likert_51_res)
likertObj_61 <- likert(df_likert_61_res)
likertObj_65 <- likert(df_likert_65_res)



summary(likertObj_35)
summary(likertObj_44)
summary(likertObj_45)
summary(likertObj_46)
summary(likertObj_51)
summary(likertObj_61)
summary(likertObj_65)

plot(likertObj_35)
plot(likertObj_44)
plot(likertObj_45)
plot(likertObj_46)
plot(likertObj_44, ordered=T,wrap=30, text.size=5)
test <- plot(likertObj_61, wrap=40, text.size=5)





data_applications <- plot(likertObj_35, centered=FALSE, include.center=TRUE, text.size=4, panel.arrange='h', digits=1, wrap=15, include.histogram=TRUE)
data_applications + theme(legend.text=element_text(size=12),
                          axis.text = element_text(size=14))

data_systems <- plot(likertObj_43, centered=FALSE, ,ordered=TRUE, text.size=4, panel.arrange='h', digits=3, wrap=30, colors=c('cornsilk2','darkseagreen3','gray97'))
data_systems+ theme(legend.text=element_text(size=12),
                    legend.direction="vertical",
                    axis.text = element_text(size=14))

data_system_satisfaction <- plot(likertObj_44, ordered=TRUE,wrap=30, text.size=3, include.center=TRUE)
data_system_satisfaction +theme(legend.text=element_text(size=12),
                                legend.direction="horizontal",
                                axis.text = element_text(size=14))

data_challenges <- plot(likertObj_51, ordered=TRUE,wrap=30, text.size=, include.center=FALSE)
data_challenges +theme(legend.text=element_text(size=12),
                                legend.direction="horizontal",
                                axis.text = element_text(size=9.5))
#plot(likertObj, ordered=FALSE) #Specify the exact order of the y-axis

#plot(likertObj, centered=FALSE, wrap=30)
#plot(likertObj, center=4, wrap=30)
#plot(likertObj, center=2, wrap=30)
#plot(likertObj, center=3, include.center=FALSE, wrap=30)

#plot(likertObj_45, center=1.5, include.center=FALSE, wrap=20)
#plot(likertObj, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)

#test + theme(legend.text=element_text(size=12),aspect.ratio=1/2,
#             axis.text = element_text(size=12))

#challenges <- plot(likertObj_51, ordered=TRUE,wrap=40, test.size=3.5)
#challenges + theme(legend.text=element_text(size=10),
#                   axis.text = element_text(size=10))


#satisfaction <- plot(likertObj_44, ordered=TRUE,wrap=20, test.size=3.5)
#satisfaction + theme(legend.text=element_text(size=10),
#                   axis.text = element_text(size=10))

ymin=0
text.size=3
ggplot(reshape, aes(y=value, x=Item, group=Item)) + 
  geom_bar(stat='identity', aes(fill=variable)) + 
  ylim(c(-5,105)) + 
  coord_flip() +
  scale_fill_manual('Response', values=brewer.pal(5, "BrBG"), 
                    breaks=levels(reshape$variable), 
                    labels=levels(reshape$variable)) +
  ylab('Percentage') +
  xlab('') +
  theme_light() #+ theme(legend.position='top')

