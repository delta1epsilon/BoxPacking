# TODO:
# 1. Write Tests

library(dplyr)
library(rgl)

# Note: make sure that your working directory is in the project directory
source('R/objects_definition.R')
source('R/plotcube.R')
source('R/EMS.R')
source('R/operations_on_chromosomes.R')
source('R/operations_on_boxes.R')
source('R/PackBoxes.R')
source('R/PerformBinPacking.R')

# create containers
c1 <- Container(length = 2, height = 2, width = 2)
c2 <- Container(length = 2, height = 2, width = 2)

# create boxes
b1 <- Box(length = 0.5, height = 0.5, width = 0.5)
b2 <- Box(length = 1, height = 0.5, width = 0.5)
b3 <- Box(length = 0.5, height = 0.5, width = 0.5)
b4 <- Box(length = 0.5, height = 0.5, width = 0.5)
b5 <- Box(length = 0.5, height = 0.5, width = 0.5)
b6 <- Box(length = 2, height = 0.5, width = 0.5)
b7 <- Box(length = 1, height = 0.5, width = 0.5)
b8 <- Box(length = 1, height = 0.5, width = 0.5)
b9 <- Box(length = 0.5, height = 0.5, width = 0.5)
b10 <- Box(length = 0.5, height = 0.5, width = 0.5)
b11 <- Box(length = 1.5, height = 1.5, width = 1.5)
b12 <- Box(length = 1.5, height = 0.5, width = 0.5)
b13 <- Box(length = 1, height = 1, width = 1)
b14 <- Box(length = 1, height = 1, width = 1)

boxes <- list(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
containers <- list(c1, c2)

# Bin Packing
solution <- 
    PerformBinPacking(containers = containers, 
                      boxes = boxes,
                      n_iter = 4, 
                      population_size = 30, 
                      elitism_size = 5,
                      crossover_prob = 0.5, 
                      mutation_prob = 0.5,
                      verbose = TRUE, 
                      plotSolution = TRUE
                      )



