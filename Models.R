library(tidyverse)
library(readxl)
library(lme4)
library(effects)

df <- read_excel("data/Pollen data.xlsx")

pollenData <- df %>%
  rename(
    Malformation = Num_malform) %>%
  mutate(
    tree = substring(Sample, 0, 3),   # Create column for tree number
    frame = substring(Sample, 5, 5),   # Create column for frame number
    code = substring(Sample, 7, 8)) %>% # Create column for treatment code
  filter(code == "C4" | code == "T2") %>% # Filter only the relevant treatments
  filter(tree != "PS8") %>% # Removing the tree with only control samples
  mutate(
    treatment = if_else(code == "C4", "Control", "UV-B")) # Translating treatment code to treatment type

###
#Malformation models

model1<-glmer(cbind(Malformed, 600- Malformed) ~ treatment + (1 | tree),
             data = pollenData, family = binomial(link= "logit"))
summary(model1)

model2<-glmer( Malformed ~ treatment + (1| tree),
              data = pollenData, family = poisson(link= "log"))
summary(model2)


### 
#Tetrad model

model1Tet<-glmer(Tetrad ~ treatment + (1|tree),        #with outliers
                 data = pollenData, family = poisson(link = "log"))
summary(model1Tet)

noOutlier1 <- pollenData %>%
  filter(Tetrad < 60) %>%
  filter(Tetrad != 31)

noOutlier2 <- pollenData %>%
  filter(Tetrad < 30)
 
noOutlier3 <- pollenData %>%
  filter(Tetrad < 60)



model2Tet<-glmer(Tetrad ~ treatment + (1|tree),        #without 1 outlier
                 data = noOutlier1, family = poisson(link = "log"))
summary(model2Tet)

model3Tet <-glmer(Tetrad ~ treatment + (1|tree),        #without 2 outliers
                  data = noOutlier2, family = poisson(link = "log"))
summary(model3Tet)

model4Tet <- glmer(Tetrad ~ treatment + (1|tree),        #without 2 outliers
                  data = noOutlier3, family = poisson(link = "log"))
summary(model4Tet)

###
#Histograms
meanMalftree <- pollenData %>%
  group_by(tree, treatment) %>%
  summarize(meanMalform = mean(Malformed))%>%
  mutate(meanMalform = ceiling(meanMalform))
  
ggplot(meanMalftree, aes(tree, meanMalform, fill = treatment)) +
  labs(title = "Mean malformed pollen per tree UV-B treatment vs control") +
  geom_col(position = "dodge") +
  scale_fill_manual(values = alpha(c("lightgoldenrod3", "mediumpurple2"))) +
  theme(plot.title = element_text((family = "sans")))
  

meanTetrad <- pollenData %>%
  group_by(tree, treatment) %>%
  summarize(meanTet = mean(Tetrad)) %>%
  mutate(meanTet = ceiling(meanTet))

meanTetNoOut <- noOutlier2 %>%
  group_by(tree, treatment) %>%
  summarize(meanTet = mean(Tetrad)) %>%
  mutate(meanTet = ceiling(meanTet))

ggplot(meanTetNoOut, aes(tree, meanTet, fill = treatment))+
  labs(title = "Mean number of tetrads per tree UV-B treatment vs control",
       subtitle = "Removed outliers") +                                  #No outliers
  geom_col(position = "dodge") +
  ylab("Mean number of tetrads") +
  xlab("Tree")
  

ggplot(meanTetrad, aes(tree, meanTet, fill = treatment)) +               
  labs(title = "Mean number of tetrads per tree UV-B treatment vs control") + #With outliers
  geom_col(position = "dodge") +
  ylab("Mean number of tetrads") +
  xlab("Tree")
  

###
#allEffects plot

plot(allEffects(model2))
plot(allEffects(model1Tet))
plot(allEffects(model2Tet))
plot(allEffects(model3Tet))


###
#Visualisations of models

tib <- as_tibble(allEffects(model2))
mMal <- tib$treatment
tib1 <- as_tibble(allEffects(model1Tet))
mTet1 <- tib1$treatment
tib2 <- as_tibble(allEffects(model2Tet))
mTet2 <- tib2$treatment
tib3 <- as_tibble(allEffects(model3Tet))
mTet3 <- tib3$treatment

ggplot(mMal, aes(x = treatment, fit, fill = treatment)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se), size = 0.2,  width = 0.3) +
  ylab("Approximated global mean of Malformed") +
  xlab("Treatment")
  
p2 <- ggplot(mTet1, aes(x = treatment, fit, fill = treatment)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se), size = 0.2,  width = 0.3) +
  ylab("Approximated global mean of tetrads") +
  xlab("Treatment") +
  ylim(0, 15)

p1 <- ggplot(mTet2, aes(x = treatment, fit, fill = treatment)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se), size = 0.2,  width = 0.3) +
  ylab("Approximated global mean of Tetrad") +
  xlab("Treatment") +
  ylim(0, 12)

ggplot(mTet3, aes(x = treatment, fit, fill = treatment)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se), size = 0.2,  width = 0.3) +
  ylab("") +
  xlab("") +
  guides(fill = FALSE)
  
###
# Tables


library(sjPlot)
tab_model(model2, show.ci = FALSE,
          title = "Summary of generalized mixed effect models",
          show.icc = FALSE, show.se = TRUE,
          show.est = TRUE, show.r2 = FALSE,
          show.reflvl = TRUE, transform = NULL)
m <- tab_model(model1Tet, model4Tet, show.ci = FALSE,
          title = "Summary of Malformed GLMM",
          show.icc = FALSE, show.se = TRUE,
          show.est = TRUE, show.r2 = FALSE,
          show.reflvl = TRUE, show.std = TRUE, 
          dv.labels = c("Tetrad model, Tetrad model without outliers"))
 


