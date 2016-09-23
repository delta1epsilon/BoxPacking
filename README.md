# 3D Bin Packing Problem


```
# create containers
containers <- list()
n_containers <- 4

for (i in 1:n_containers) {
    containers <- c(containers,
                    Container(length = 2, height = 2, width = 2)
                    )
}


# create boxes
boxes <- list()
n_boxes <- 20

for (i in 1:n_boxes) {
    length <- sample(c(0.4, 0.5, 1), 1)
    height <- sample(c(0.4, 0.5, 1), 1)
    width <- sample(c(0.4, 0.5, 1), 1)

    boxes <- c(boxes,
               Box(length = length, height = height, width = width)
               )
}

# Box Packing
solution <-
    PerformBoxPacking(containers = containers,
                      boxes = boxes,
                      n_iter = 4,
                      population_size = 20,
                      elitism_size = 5,
                      crossover_prob = 0.5,
                      mutation_prob = 0.5,
                      verbose = TRUE,
                      plotSolution = TRUE
                      )
```

![](giphy.gif)
