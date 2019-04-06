install.packages('tidyr') 
install.packages('dplyr')
install.packages('ggplot2')
load('tidyr')
load('ggplot')
library('dplyr')
library('ggplot2')

wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

surveyData <- read.csv('./20190131_final_results.csv', header = TRUE)

dataUse <- surveyData[,14:21]

A = tidyr::separate(dataUse,col=1,sep="[;]", into=c("A1","A2"), remove=TRUE)
B = tidyr::separate(dataUse,col=2,sep="[;]", into=c("B1","B2"), remove=TRUE)
C = tidyr::separate(dataUse,col=3,sep="[;]", into=c("C1","C2"), remove=TRUE)
D = tidyr::separate(dataUse,col=4,sep="[;]", into=c("D1","D2"), remove=TRUE)
E = tidyr::separate(dataUse,col=5,sep="[;]", into=c("E1","E2"), remove=TRUE)
F = tidyr::separate(dataUse,col=6,sep="[;]", into=c("F1","F2"), remove=TRUE)
G = tidyr::separate(dataUse,col=7,sep="[;]", into=c("G1","G2"), remove=TRUE)

dataUse_freq <- A[,c("A1","A2")]
dataUse_freq <- cbind(dataUse_freq,B[,c("B1","B2")])
dataUse_freq <- cbind(dataUse_freq,C[,c("C1","C2")])
dataUse_freq <- cbind(dataUse_freq,D[,c("D1","D2")])
dataUse_freq <- cbind(dataUse_freq,E[,c("E1","E2")])
dataUse_freq <- cbind(dataUse_freq,F[,c("F1","F2")])
dataUse_freq <- cbind(dataUse_freq,G[,c("G1","G2")])

dataUse_freq$A2 <- ifelse(dataUse_freq$A1=="Dataset I would like to use in the future", dataUse_freq$A1, dataUse_freq$A2)
dataUse_freq$A1 <- ifelse(dataUse_freq$A1=="Dataset I would like to use in the future", NA, dataUse_freq$A1)
dataUse_freq$B2 <- ifelse(dataUse_freq$B1=="Dataset I would like to use in the future", dataUse_freq$B1, dataUse_freq$B2)
dataUse_freq$B1 <- ifelse(dataUse_freq$B1=="Dataset I would like to use in the future", NA, dataUse_freq$B1)
dataUse_freq$C2 <- ifelse(dataUse_freq$C1=="Dataset I would like to use in the future", dataUse_freq$C1, dataUse_freq$C2)
dataUse_freq$C1 <- ifelse(dataUse_freq$C1=="Dataset I would like to use in the future", NA, dataUse_freq$C1)
dataUse_freq$D2 <- ifelse(dataUse_freq$D1=="Dataset I would like to use in the future", dataUse_freq$D1, dataUse_freq$D2)
dataUse_freq$D1 <- ifelse(dataUse_freq$D1=="Dataset I would like to use in the future", NA, dataUse_freq$D1)
dataUse_freq$E2 <- ifelse(dataUse_freq$E1=="Dataset I would like to use in the future", dataUse_freq$E1, dataUse_freq$E2)
dataUse_freq$E1 <- ifelse(dataUse_freq$E1=="Dataset I would like to use in the future", NA, dataUse_freq$E1)
dataUse_freq$F2 <- ifelse(dataUse_freq$F1=="Dataset I would like to use in the future", dataUse_freq$F1, dataUse_freq$F2)
dataUse_freq$F1 <- ifelse(dataUse_freq$F1=="Dataset I would like to use in the future", NA, dataUse_freq$F1)
dataUse_freq$G2 <- ifelse(dataUse_freq$G1=="Dataset I would like to use in the future", dataUse_freq$G1, dataUse_freq$G2)
dataUse_freq$G1 <- ifelse(dataUse_freq$G1=="Dataset I would like to use in the future", NA, dataUse_freq$G1)

freqs <- lapply(dataUse_freq, function(x) as.data.frame(table(x)))

dataType <- c(rep("Climate reanalysis", 2), rep('Meteorological forecasts',2), rep("Seasonal forecasts",2), rep("Environmental forecasts",2), rep("Earth Observations",2), rep("Other geospatial data",2), rep("Value-added products",2))
use <- rep(c("Current", "Future"),7)
sums <- c(freqs$A1[2,2],freqs$A2[,2],freqs$B1[2,2],freqs$B2[,2],freqs$C1[2,2],freqs$C2[,2],freqs$D1[2,2],freqs$D2[,2],freqs$E1[2,2],freqs$E2[,2],
          freqs$F1[2,2],freqs$F2[,2],freqs$G1[2,2],freqs$G2[,2])

data_use_sums <- data.frame(dataType, use, sums)
tiff("test.tiff", units="in", width=740, height=700, res=150)
# insert ggplot code

# Grouped
ggplot(data_use_sums, aes(fill=use, y=sums, x=use)) + 
  geom_bar(stat="identity",width=0.7) +
  facet_wrap(dataType~., ncol=1) +
  coord_flip() +
  scale_fill_manual("use", values = c("Current"='#3678DB', "Future"='#98BCFC')) +
  labs(x="Use", y="Counts", title="Current and future data use", Colour="Data use") +
  theme(legend.title=element_blank())

dev.off()
  