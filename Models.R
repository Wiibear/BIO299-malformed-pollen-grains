library(tidyverse)
library(readxl)
library(MASS)
df <- read_excel("data/Pollen data.xlsx")

Pollen_data <- df %>%
  mutate(
    Tree = substring(Sample, 0, 3),   # Create column for tree number
    Frame = substring(Sample, 5, 5),   # Create column for frame number
    Treatment_full = substring(Sample, 7, 8), # Create column for treatment type with number
    Treatment = substring(Sample, 7, 7)
  )

c_data <- Pollen_data %>% #Data from control
  filter(Treatment == 'C')

t_data <- Pollen_data %>% #Data from UV-B treatment        
  filter(Treatment == 'T')


linear_mod <-glm(Num_malform ~ Tetrad, Pollen_data, family = gaussian)
summary(linear_mod)

linear_mod_t <-glm(Num_malform ~ Tetrad, t_data, family = gaussian)
summary(linear_mod_t)

linear_mod_c <- glm(Num_malform ~ Tetrad, c_data, family = gaussian)
summary(linear_mod_c)

view(t_data)
view(c_data)
