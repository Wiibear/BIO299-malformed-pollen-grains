library(tidyverse)
library(readxl)
library(nlme)
df <- read_excel("data/Pollen data.xlsx")

dfNoOut <- filter(df, Num_malform < 90 & Tetrad < 60)

Pollen_data <- df %>%
  filter(Num_malform < 90)

Pollen_data <- dfNoOut %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    code = substring(Sample, 7, 8),
    Malformation_rate = (Num_malform/600)) %>%
  filter(code == "C4" | code == "T2") %>%
  rename(
    treatment = code      #Didn't manage to make column with treatment as UV or C
                          #I tried this ---> treatment = if(Frame == "1"|Frame == "3"|Frame == "5") {"C"} else {"UV"}
  )                       

# Alistair comment. This model looks correct to me!  Except perhaps we should be using a generalised liner model instead because ther residuals of the malformation rate cannot be less than zero 
# (i.e. they are non-normally distributed)
model1<-lme(Malformation_rate ~ treatment,
            random = ~1 |Tree, data = Pollen_data)
summary(model1)

residuals = resid(model1)
qqnorm(residuals)              #Checking normal distribution of residuals
qqline(residuals)

# Although maybe we can specify the correct error distribution. 
# I think instead I would should use a binomial error distribution. 
# But this means we need to use a different package to fit the model. 
# I have never used glmer before but I think this is correct model specification

install.packages("lme4")
library(lme4)
model2<-glmer(cbind(Num_malform, 600- Num_malform) ~ treatment + (1 | Tree),
             data = Pollen_data, family = binomial(link= "logit"))
summary(model2)

# Or modelling them as count data
model3<-glmer( Num_malform ~ treatment + (1 | Tree),
              data = Pollen_data, family = poisson(link= "log"))
summary(model3)


###
#Experimentation
meanMalfFrame <- Pollen_data %>%
  group_by(Frame, treatment) %>%
  summarize(meanMalform = mean(Num_malform))%>%
  mutate(
    treatment = if(Frame == "1"|Frame == "3"|Frame == "5") {"C"} else {"UV"}
  ) %>% 
  mutate(meanMalform = ceiling(meanMalform))
view(meanMalfFrame)

# Here I corrected it, so you have a glm and poisson error distribution. You don't need the random effects if you specify it in this way
m1Tree<-glm(meanMalform ~ treatment, data = meanMalfFrame, family = poisson(link = "log"))
summary(m1Tree)


### I'll leave this for you to play with yourself

model1Tet<-lme(Tetrad ~ treatment,
            random = ~1 |Tree, data = Pollen_data)
summary(model1Tet)
resT = resid(model1Tet)
qqnorm(resT)              #Checking normal distribution of residuals
qqline(resT)
