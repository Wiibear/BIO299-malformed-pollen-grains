library(tidyverse)
library(readxl)
library(nlme)

df <- read_excel("data/Pollen data.xlsx")

dfNoOut <- filter(df, Num_malform < 90 & Tetrad < 60)

Pollen_data <- dfNoOut %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    code = substring(Sample, 7, 8),
    Malformation_rate = (Num_malform/600)) %>%
  filter(code == "C4" | code == "T2") %>%
  mutate(
    treatment = code)
  


c_data <- Pollen_data %>% #Data from control
  filter(code == 'C4') 

t_data <- Pollen_data %>% #Data from UV-B code        
  filter(code == 'T2') 


###
#Means and medians
  
median(t_data$Malformation_rate)  
median(c_data$Malformation_rate)      
median(Pollen_data$Malformation_rate) 
                                      
mean(t_data$Malformation_rate)
mean(c_data$Malformation_rate)
mean(c_no_out$Malformation_rate)         
mean(Pollen_data$Malformation_rate) 


###
#Checking if normally distributed

qqnorm(t_data$Tetrad)
qqline(t_data$Tetrad)

qqnorm(c_data$Tetrad)
qqline(c_data$Tetrad)

qqnorm(c_no_out$Tetrad)


qqline(c_no_out$Tetrad)

qqnorm(c_no_out$Num_malform)
qqline(c_no_out$Num_malform)

qqnorm(t_data$Num_malform)
qqline(t_data$Num_malform)

qqnorm(c_data$Num_malform)
qqline(c_data$Num_malform)


### 
#Scatterplots

ggplot(Pollen_data, aes(Tetrad, Num_malform,)) +   #Both control and UV-B code in one
  geom_point(aes(shape = code)) +
  geom_abline() 

ggplot(Pollen_data, aes(Tetrad, Num_malform,)) +   #Facet wrap of control and UV-B code
  geom_point(aes(shape = code)) +
  geom_abline() +
  facet_wrap(~ code, nrow=1)

ggplot(Pollen_data, aes(Num_malform, Tetrad)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ Tree, nrow = 2)


  
###
#Boxplots

ggplot(Pollen_data ,aes(code, Tetrad, group = code)) +#Comparison of tetrads
  geom_boxplot() 

ggplot(Pollen_data ,aes(code, Num_malform, group = code)) +  #Comparison of malformations
  geom_boxplot()

ggplot(Pollen_data ,aes(Frame, Num_malform, group = Frame)) + # Comparison between frames with regards to number of malformations
  geom_boxplot() 

ggplot(Pollen_data ,aes(Frame, Tetrad, group = Frame)) + # Comparison of frames in regards to tetrads
  geom_boxplot() 

ggplot(Pollen_data, aes(code, Malformation_rate, group = code)) + #Visualisation of malformation rate
  geom_boxplot()
 
###
# Correlation tests
  
cor.test(Pollen_data$Num_malform, Pollen_data$Tetrad, #All data
           method = "pearson")

cor.test(t_data$Num_malform, t_data$Tetrad, #Only code data
         method = "pearson")

cor.test(c_data$Num_malform, c_data$Tetrad, # Only control data
         method = "pearson")

###
#t-test
t.test(c_data$Num_malform, t_data$Num_malform, alternative = "less")    #two sample t-test with all the data

linMod <- lm(Num_malform ~ Tetrad, data = Pollen_data)
summary(linMod)
