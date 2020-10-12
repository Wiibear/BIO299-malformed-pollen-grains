library(tidyverse)
library(readxl)

df <- read_excel("data/Pollen data.xlsx")

#Data wrangling

Pollen_data <- df %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    Treatment_full = substring(Sample, 7, 8), # Create column for treatment type with number
    Treatment = substring(Sample, 7, 7), # Create column for treatment type 
  )


c_data <- Pollen_data %>% #Data from control
  filter(Treatment == 'C')

t_data <- Pollen_data %>% #Data from UV-B treatment        
  filter(Treatment == 'T')

wo_outlier <- Pollen_data %>%   #Removing the most egregious outliers
  filter(Num_malform < 50 & Tetrad < 60)

c_no_out <- wo_outlier %>% #Data from control without the two big outliers
  filter(Treatment == 'C')

c_no_out2 <- wo_outlier%>% #Data from control without the 3 biggest outliers
  filter(Treatment == 'C' & Num_malform < 30)

# linear models
linear_mod_og <-lm(Num_malform ~ Tetrad, Pollen_data) # lm of original data
summary(linear_mod)

linear_mod_t <-lm(Num_malform ~ Tetrad, t_data) # lm of only the UV-B treatment
summary(linear_mod_t)

linear_mod_c <- lm(Num_malform ~ Tetrad, c_data) # lm of only the control treatment
summary(linear_mod_c)

lin_c_no_out <- lm(Num_malform ~ Tetrad, c_no_out) # lm of only the control treatment
summary(linear_mod_c)

linear_mod <-lm(Num_malform ~ Tetrad, wo_outlier) # lm with the most egregious outliers removed
summary(linear_mod)



