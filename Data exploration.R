library(tidyverse)
library(readxl)

df <- read_excel("data/Pollen data.xlsx")

Pollen_data <- df %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    Treatment_full = substring(Sample, 7, 8), # Create column for treatment type with number
    Treatment = substring(Sample, 7, 7), # Create column for treatment type 
    Malformation_rate = (Num_malform/600),
    Tetrad_rate = Tetrad/600,
    Tet_to_malf_rate = Tetrad/Num_malform
  )


c_tetrad <- Pollen_data %>% #Data from control
  filter(Treatment == 'C')

t_data <- Pollen_data %>% #Data from UV-B treatment        
  filter(Treatment == 'T') 

wo_outlier <- Pollen_data %>%
  filter(Num_malform < 50 & Tetrad < 60) #Removing the largest outliers
view(wo_outlier)



###
#Checking if normally distributed

qqnorm(t_data$Tetrad)
qqline(t_data$Tetrad)

qqnorm(c_data$Tetrad)
qqline(c_data$Tetrad)

qqnorm(t_data$Num_malform)
qqline(t_data$Num_malform)

qqnorm(c_data$Num_malform)
qqline(c_data$Num_malform)

### 
#Scatterplots

ggplot(Pollen_data, aes(Tetrad, Num_malform,)) +   #Both control and UV-B treatment in one
  geom_point(aes(shape = Treatment)) +
  geom_abline() 

ggplot(Pollen_data, aes(Tetrad, Num_malform,)) +   #Facet wrap of control and UV-B treatment
  geom_point(aes(shape = Treatment)) +
  geom_abline() +
  facet_wrap(~ Treatment, nrow=1)

ggplot(Pollen_data, aes(Num_malform, Tetrad)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ Tree, nrow = 2)


  
###
#Boxplots

ggplot(Pollen_data ,aes(Treatment, Tetrad, group = Treatment)) +#Comparison of tetrads
  geom_boxplot() +
  stat_compare_means()

ggplot(Pollen_data ,aes(Treatment, Num_malform, group = Treatment)) +  #Comparison of malformations
  geom_boxplot() +

ggplot(Pollen_data ,aes(Frame, Num_malform, group = Frame)) + # Comparison between frames with regards to number of malformations
  geom_boxplot() 

ggplot(Pollen_data ,aes(Frame, Tetrad, group = Frame)) + # Comparison of frames in regards to tetrads
  geom_boxplot() 

ggplot(Pollen_data, aes(Treatment, Tet_to_malf_rate, group = Treatment)) + #Comparison of the rate of tetrads to malformations
  geom_boxplot()
 
###
# Correlation tests
  
cor.test(Pollen_data$Num_malform, Pollen_data$Tetrad, #All data
           alternative = "greater", 
           method = "kendall")

cor.test(t_data$Num_malform, t_data$Tetrad, #Only control data
         alternative = "greater",
         method = "kendall")

cor.test(c_data$Num_malform, c_data$Tetrad, # Only treatment data
         alternative = "greater", 
         method = "kendall")



