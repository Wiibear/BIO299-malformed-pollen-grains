library(tidyverse)
library(readxl)
library(lme4)

df <- read_excel("data/Pollen data.xlsx") 


pollenData <- df %>%
  mutate(
    tree = substring(Sample, 0, 3),   # Create column for tree number
    frame = substring(Sample, 5, 5),   # Create column for frame number
    code = substring(Sample, 7, 8),
    malformationRate = (Num_malform/600)) %>%
  filter(code == "C4" | code == "T2") %>%
  rename(
    treatment = code)    

###
#Malformation models

model1<-glmer(cbind(Num_malform, 600- Num_malform) ~ treatment + (1 | tree),
             data = pollenData, family = binomial(link= "logit"))
summary(model1)

model2<-glmer( Num_malform ~ treatment + (1| tree),
              data = pollenData, family = poisson(link= "log"))
summary(model2)


### 
#Tetrad model

model1Tet<-glmer(Tetrad ~ treatment + (1|tree),        #with outliers
                 data = pollenData, family = poisson(link = "log"))
summary(model1Tet)

noOutlier <- pollenData %>%
  filter(Tetrad < 60) %>%
  filter(Tetrad != 31)

model2Tet<-glmer(Tetrad ~ treatment + (1|tree),        #without outliers
                 data = noOutlier, family = poisson(link = "log"))
summary(model2Tet)

###
#Histograms
meanMalftree <- pollenData %>%
  group_by(tree, treatment) %>%
  summarize(meanMalform = mean(Num_malform))%>%
  mutate(meanMalform = ceiling(meanMalform))
view(meanMalftree)


ggplot(meanMalftree, aes(tree, meanMalform), group = treatment)+
  geom_col() +
  facet_wrap(~treatment, nrow = 1)


meanTetrad <- pollenData %>%
  group_by(tree, treatment) %>%
  summarize(meanTet = mean(Tetrad)) %>%
  mutate(meanTet = ceiling(meanTet))

meanTetNoOut <- noOutlier %>%
  group_by(tree, treatment) %>%
  summarize(meanTet = mean(Tetrad)) %>%
  mutate(meanTet = ceiling(meanTet))

ggplot(meanTetrad, aes(tree, meanTet), group = treatment)+
  geom_col() +
  facet_wrap(~treatment, nrow = 1)  
