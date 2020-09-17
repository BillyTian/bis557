## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

setwd("C:\\Users\\Billy Tian\\Desktop\\BIS 557_Computational Statistics\\homework-1")
#Read in a csv file
lm_patho <- read.csv("lm_patho.csv", header = T)

setwd("C:\\Users\\Billy Tian\\Desktop\\BIS 557_Computational Statistics\\bis557")
#Add dataset into the package
use_data(lm_patho)
