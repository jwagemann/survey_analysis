library(tidyr)
wd <- setwd('/Users/julia_wagemann/Documents/github/survey_analysis/')

surveyData <- read.csv('./data/20190611_final_results_header_modified.csv', header = TRUE, na.strings="")
no_of_respondents <- nrow(surveyData)
dataUse <- surveyData[,14:21]
dataUse_other <- surveyData[,22]

# Split columns and seperate columns with multiple responses
A = tidyr::separate(dataUse,col=1,sep="[;]", into=c("A1","A2"), remove=TRUE)
B = tidyr::separate(dataUse,col=2,sep="[;]", into=c("B1","B2"), remove=TRUE)
C = tidyr::separate(dataUse,col=3,sep="[;]", into=c("C1","C2"), remove=TRUE)
D = tidyr::separate(dataUse,col=4,sep="[;]", into=c("D1","D2"), remove=TRUE)
E = tidyr::separate(dataUse,col=5,sep="[;]", into=c("E1","E2"), remove=TRUE)
F = tidyr::separate(dataUse,col=6,sep="[;]", into=c("F1","F2"), remove=TRUE)
G = tidyr::separate(dataUse,col=7,sep="[;]", into=c("G1","G2"), remove=TRUE)

# Bring all split columns together into one data frame
dataUse_freq <- A[,c("A1","A2")]
dataUse_freq <- cbind(dataUse_freq,B[,c("B1","B2")])
dataUse_freq <- cbind(dataUse_freq,C[,c("C1","C2")])
dataUse_freq <- cbind(dataUse_freq,D[,c("D1","D2")])
dataUse_freq <- cbind(dataUse_freq,E[,c("E1","E2")])
dataUse_freq <- cbind(dataUse_freq,F[,c("F1","F2")])
dataUse_freq <- cbind(dataUse_freq,G[,c("G1","G2")])

# Set all second columns to NA, as we expect that data that is currently used is inteded to be used in the future as well.
dataUse_freq$A2 <- NA
dataUse_freq$B2 <- NA
dataUse_freq$C2 <- NA
dataUse_freq$D2 <- NA
dataUse_freq$E2 <- NA
dataUse_freq$F2 <- NA
dataUse_freq$G2 <- NA


#Bring current and future use responses in seperate columns
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


