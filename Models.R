library(tidyverse)
library(readxl)
library(nlme)
df <- read_excel("data/Pollen data.xlsx")

Pollen_data <- df %>%
  filter(Num_malform < 90)

Pollen_data <- dfNoOut %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    code = substring(Sample, 7, 8),
    Malformation_rate = (Num_malform/600),
  ) %>%
  filter(code == "C4" | code == "T2") %>%
  rename(
    treatment = code      #Didn't manage to make column with treatment as UV or C
                          #I tried this ---> treatment = if(Frame == "1"|Frame == "3"|Frame == "5") {"C"} else {"UV"}
  )

model1<-lme(Malformation_rate ~ treatment,
            random = ~1 |Tree, data = Pollen_data)
summary(model1)

residuals = resid(model1)
qqnorm(residuals)              #Checking normal distribution of residuals
qqline(residuals)

###
#Experimentation
meanMalfFrame <- Pollen_data %>%
  group_by(Frame, treatment) %>%
  summarize(meanMalform = mean(Num_malform))%>%
  mutate(
    treatment = if(Frame == "1"|Frame == "3"|Frame == "5") {"C"} else {"UV"}
  )
view(meanMalfFrame)

meanMalfTree <- Pollen_data %>%
  group_by(Tree, Frame, code) %>%
  summarize(treeMeanMalf = mean(Num_malform)) %>%
  mutate(
    treatment = if(Frame == "1"|Frame == "3"|Frame == "5") {"C"} else {"UV"}
  )
view(meanMalfTree)

m1Tree<-lme(treeMeanMalf ~ Frame,random = ~1 |Tree, data = meanMalfTree)
summary(m1Tree)

model1Tet<-lme(Tetrad ~ treatment,
            random = ~1 |Tree, data = Pollen_data)
summary(model1Tet)
resT = resid(model1Tet)
qqnorm(resT)              #Checking normal distribution of residuals
qqline(resT)
