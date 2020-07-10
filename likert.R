library(likert)
library(psych)
library(haven)
library(tidyverse)

kezia_data <- read_sav("Auswertung_Pilottest&Nacherhebung.sav")
likert_kezia <- select(kezia_data, matches("^[A0|B0].*"))
likert_kezia <- as.data.frame(likert_kezia)

for (i in 1:length(likert_kezia)) {
  likert_kezia[,i] <- as.character(likert_kezia[,i])
  likert_kezia[,i] <- factor(likert_kezia[,i],
                             levels = c("1", "2", "3", "4", "5"),
                             ordered = TRUE)
}

Input =("
  Pooh    Piglet  Tigger
        3       2       4
        5       4       4
        4       2       4
        4       2       4
        4       1       5
        4       2       3
        4       3       5
        4       2       4
        5       2       4
        5       3       3
        ")

Data = read.table(textConnection(Input),header=TRUE)


### Change Likert scores to factor and specify levels

Data$Pooh = factor(Data$Pooh,
                   levels = c("1", "2", "3", "4", "5"),
                   ordered = TRUE)

Data$Piglet = factor(Data$Piglet,
                     levels = c("1", "2", "3", "4", "5"),
                     ordered = TRUE)

Data$Tigger = factor(Data$Tigger,
                     levels = c("1", "2", "3", "4", "5"),
                     ordered = TRUE)


### Double check the data frame

library(psych)

headTail(Data)

str(Data)

summary(Data)
rm(Input)

likert(Data)

Result = likert(Data)
Resul2 = likert(likert_kezia)

plot(Result,
     type="bar")

plot(Result, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

plot(Result,
    type="density",
    facet = TRUE, 
    bw = 0.5)