#install.packages('likert')
install.packages(ddply)
library(likert)
library(plyr)

setwd('/Users/julia_wagemann/Documents/Notebooks/phd/survey_analysis/')

df_new <- read.csv('20190131_final_results_header_modified.csv', header=TRUE, na.string="")

df_likert_35 <- df_new[, c('X3.5.machine.learning',
                        'X3.5.web.applications',
                        'X3.5.data.visualizations',
                        'X3.5.global.regional.analyses',
                        'X3.5.Analysis.over.long.time.spans',
                        'X3.5.time.series.analysis',
                        'X3.5.other')]

levels_35 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very much interested')


#df_likert_37 <- df_new[, c('X3.7.Combination.data.sources',
#                        'X37.Production.value.added.products',
#                        'X3.7.Consumption.open.data',
#                        'X3.7.Sharing.results',
#                        'X3.7.Importance.task.parallelisation')]

df_likert_44 <- df_new[, c('X4.4.download.service',
                        'X4.4.cloud.computing.infrastructure',
                        'X4.4.ogc.service',
                        'X4.4.custom.api.opendap',
                        'X4.4.virtual.research.infrastructure',
                        'X4.4.data.cube.technology',
                        'X4.4.spatial.array.database',
                        'X4.4.other')]

levels_44 <- c('Very dissatisfied', 'Dissatisfied', 'Neither satisfied nor dissatisfied', 'Satisfied', 'Very satisfied')


df_likert_45 <- df_new[, c('X4.5.cloud.code.editor',
                        'X4.5.code.routines.access.cloud.services',
                        'X4.5.code.routines.python.r',
                        'X4.5.geospatial.software',
                        'X4.5.Other')]

levels_45 <- c('Never', 'Sometimes', 'Always')


df_likert_46 <- df_new[, c('X4.6.server.cloud.processing',
                        'X4.6.parallel.computing',
                        'X4.6.time.series.retrieval',
                        'X4.6.Download.large.data.volumes',
                        'X4.6.standard.data.access',
                        'X4.6.web.applications.visualization',
                        'X4.6.data.discovery',
                        'X4.6.interoperability')]

levels_46 <- c('Not at all important', 'Not important', 'Neither not important nor important', 'Important', 'Very important')


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
                        'X5.1.data.services.cost',
                        'X5.1.other')]

df_likert_51 <- data.frame(list(ifelse(df_rating_51==1,'No obstacle at all', 
                       ifelse(df_rating_51==2, 'No obstacle', 
                              ifelse(df_rating_51==3, 'Neither no obstacle nor an obstacle', 
                                     ifelse(df_rating_51==4, 'An obstacle', 
                                            ifelse(df_rating_51==5, 'A great obstacle', 'test')))))))

levels_51 <- c('No obstacle at all', 'No obstacle', 'Neither no obstacle nor an obstacle', 'An obstacle', 'A great obstacle')


df_likert_61 <- df_new[, c('X6.1')]

levels_61 <- c('Not at all interested', 'Not interested', 'Neither not interested nor interested', 'Interested', 'Very interested')


df_likert_65 <- df_new[, c('X6.5.data.integrity',
                        'X6.5.data.breaches',
                        'X6.5.data.loss',
                        'X6.5.service.unavailability',
                        'X6.5.data.security',
                        'X6.5.other')]

levels_65 <- c('No risk at all','Might be a risk, but not important for me', 'Risk', 'Major risk')


df_likert_35_ord <- lapply(df_likert_35, function(x) ordered(x, levels = levels_35))
df_likert_35_res <- do.call(data.frame, df_likert_35_ord)

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
  X3.5.time.series.analysis="Time-series analysis",
  X3.5.other="Other")

names(df_likert_44_res) <- c(
  X4.4.download.service = 'Download Service from respective data providers',
  X4.4.cloud.computing.infrastructure = 'Cloud computing infrastructure',
  X4.4.ogc.service = 'via an OGC web service, e.g. WMS or WCS',
  X4.4.custom.api.opendap = 'via  a custom API or an OpeNDAP server from a respective data provider',
  X4.4.virtual.research.infrastructure = 'via a Virtual Research Infrastructure',
  X4.4.data.cube.technology = 'via a Data Cube technology',
  X4.4.spatial.array.database = 'via a spatial or array database',
  X4.4.other = 'Other')

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
  X5.1.data.services.cost = 'Cost of data services (cost for non-open data or cost for processing services)',
  X5.1.other = 'Other')

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
plot(likertObj_51, ordered=TRUE)
plot(likertObj_61)
plot(likertObj_65, center=2, include.center=TRUE)

plot(likertObj, ordered=FALSE) #Specify the exact order of the y-axis

plot(likertObj, centered=FALSE, wrap=30)
plot(likertObj, center=4, wrap=30)
plot(likertObj, center=2, wrap=30)
plot(likertObj, center=3, include.center=FALSE, wrap=30)

plot(likertObj_45, center=1.5, include.center=TRUE, wrap=20)
plot(likertObj, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)


