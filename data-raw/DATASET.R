## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = F)

library(palmerpenguins)
library(missForest)
penguinsi <- penguins
penguinsi <- data.frame(missForest(as.data.frame(penguinsi))$ximp)
setwd("C:\\Users\\Billy Tian\\Desktop\\BIS 557_Computational Statistics\\bis557")
use_data(penguinsi) #add dataset into the package
