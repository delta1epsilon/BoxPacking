#' Create a Chromosome
#'
#' @param n_boxes      - An integer
#' @param n_containers - An integer
#' @return A list with first element BPS (Box Plasement Sequence) 
#'         and second element CLS (Container Loading Sequence) and
#'         their appropriate sequences
CreateChromosome <- function (n_boxes, n_containers) {
    chromosome <- list(BPS = sample(1:n_boxes), CLS = sample(1:n_containers))
    return(chromosome)
}


#' Create 4 Initial Custom Chromosomes
#'
#' @param boxes - A list of objects of class Box
#' @return A list of 4 chromosomes 
CustomChromosomeInitialization <- function (boxes, n_containers) {
    chromosomes <- list()

    boxes_length <- sapply(boxes, function(x) x@length)
    boxes_width <- sapply(boxes, function(x) x@width)
    boxes_height <- sapply(boxes, function(x) x@height)
    boxes_volume <- sapply(boxes, function(x) x@length * x@height * x@width)
    
    # sort descending according to length
    ind_by_length <- order(boxes_length, decreasing = TRUE)
    chromosome <- list(BPS = ind_by_length, CLS = sample(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))

    # sort descending according to height
    ind_by_height <- order(boxes_height, decreasing = TRUE)
    chromosome <- list(BPS = ind_by_height, CLS = sample(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))

    # sort descending according to width
    ind_by_width <- order(boxes_width, decreasing = TRUE)
    chromosome <- list(BPS = ind_by_width, CLS = sample(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))

    # sort descending according to width
    ind_by_volume <- order(boxes_volume, decreasing = TRUE)
    chromosome <- list(BPS = ind_by_volume, CLS = sample(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))

    return(chromosomes)
}


#' Initialize a Population
#'
#' @param population_size - An integer
#' @param n_containers    - An integer
#' @param boxes           - A list of objects of class Box
#' @return A list of chromosomes 
InitializePopulation <- function (population_size, 
                                  n_containers, 
                                  boxes) {
    # get number of boxes
    n_boxes <- length(boxes)

    # create 4 custom chromosomes
    population <- 
        CustomChromosomeInitialization(boxes = boxes, 
                                       n_containers = n_containers
                                       )

    if (population_size <= 4) {
        population <- population[1:population_size]
    } else {
        n_chromosomes_to_create <- population_size - 4
        for (i in 1:n_chromosomes_to_create) {
            chromosome <- CreateChromosome(n_boxes = n_boxes, n_containers = n_containers)
            population <- c(population, list(chromosome))
        }
    }

    return(population)
}


#' Perform Elitism: Select the best Chromosomes within population 
#'
#' @param fitness        - A vector with chromosomes scores   
#' @param elitism_size - An integer
#' @return A vector of size 'elitism_size' with indeces of 
#'         best performing chromosomes
PerformElitism <- function (fitness, 
                            elitism_size) {
    best_chromosomes_ind <- order(fitness)[1:elitism_size]
    return(best_chromosomes_ind)
}


#' Perform Selection
#'
#' @param population - A list of chromosomes
#' @param fitness    - A vector
#' @return A list of chromosomes of the same size as population
PerformSelection <- function (population, fitness) {
    population_size <- length(population)
    new_population <- list()

    for (i in 1:population_size) {
        chromosomes_ind <- sample(1:population_size, 2)
        better_chromosome_ind <- which.min(fitness[chromosomes_ind])
        new_population <- c(new_population, population[chromosomes_ind[better_chromosome_ind]])
    }

    return(new_population)
}


#' Mutate a chromosome
#'
#' @param chromosome - A list with BPS and CLS
#' @return A chromosome
MutateChromosome <- function (chromosome) {
    n_boxes <- length(chromosome$BPS) 
    n_containers <- length(chromosome$CLS)

    if (n_boxes <= 2) {
        chromosome$BPS <- rev(chromosome$BPS)
    } else {
        replace_boxes_ind <- sample(1:n_boxes, 2)
        chromosome$BPS[replace_boxes_ind] <- rev(chromosome$BPS[replace_boxes_ind])
    }
    
    if (n_containers <= 2) {
        chromosome$CLS <- rev(chromosome$CLS)            
    } else {
        replace_containers_ind <- sample(1:n_containers, 2)
        chromosome$CLS[replace_containers_ind] <- rev(chromosome$CLS[replace_containers_ind])
    }

    return(chromosome)
}


#' Perform Mutation
#'
#' @param population    - A list of chromosomes
#' @param mutation_prob - A numeric in [0; 1]; A probability for chromosome mutation  
#' @return A list of chromosomes
PerformMutation <- function (population, mutation_prob) {
    population_size <- length(population)

    # choose chromosomes for mutation with probability 'mutation_prob'
    chromosomes_ind_to_mutate <- rbinom(n = population_size, size = 1, prob = mutation_prob)
    
    not_mutated_chromosomes <- population[chromosomes_ind_to_mutate == 0]
    chromosomes_to_mutate <- population[chromosomes_ind_to_mutate == 1]

    # Perform Chromosomes Mutation
    mutated_chromosomes <- lapply(chromosomes_to_mutate, MutateChromosome)

    new_population <- c(not_mutated_chromosomes, mutated_chromosomes)
    return(new_population)
}


#' Crossover 2 Chromosomes
#'
#' @param parent1 - A chromosome
#' @param parent2 - A chromosome
#' @return A chromosome
CrossoverChromosomes <- function (parent1, parent2) {
    n_boxes <- length(parent1$BPS)
    n_containers <- length(parent1$CLS)

    # randomly choose 2 cutting points for BPS 
    cutting_box_points <- sample(1:n_boxes, 2)
    cut_box_i <- min(cutting_box_points)
    cut_box_j <- max(cutting_box_points)

    # randomly choose 2 cutting points for CLS
    cutting_con_points <- sample(1:n_containers, 2)
    cut_con_i <- min(cutting_con_points)
    cut_con_j <- max(cutting_con_points)

    # create a child
    child <- list(BPS = rep(0, n_boxes), CLS = rep(0, n_containers))

    # copy genes between cutting points from parent1 to child
    child$BPS[(cut_box_i+1):cut_box_j] <- parent1$BPS[(cut_box_i+1):cut_box_j]
    child$CLS[(cut_con_i+1):cut_con_j] <- parent1$CLS[(cut_con_i+1):cut_con_j]

    # get indeces of missing genes 
    box_ind <- ifelse(cut_box_j != n_boxes,
                      list(c((cut_box_j + 1):n_boxes, 1:cut_box_i)),
                      list(c(1:cut_box_i))
                      )
    con_ind <- ifelse(cut_con_j != n_containers,
                      list(c((cut_con_j + 1):n_containers, 1:cut_con_i)),
                      list(c(1:cut_con_i))
                      )

    # fill missing genes
    child$BPS[unlist(box_ind)] <- setdiff(parent2$BPS, child$BPS)
    child$CLS[unlist(con_ind)] <- setdiff(parent2$CLS, child$CLS)

    return(child)
}


#' Perform Crossover
#'
#' @param mating_pool    - A list of chromosomes to crossover
#' @param crossover_prob - A numeric in [0; 1]; A probability for chromosome crossover  
#' @return A list of chromosomes
PerformCrossover <- function (mating_pool, crossover_prob) {
    next_population <- list()

    while (length(mating_pool) != 0) {
        if (length(mating_pool) > 1) {
            ind <- sample(1:length(mating_pool), 2)

            if (rbinom(1, 1, crossover_prob) == 1) {
                child1 <- CrossoverChromosomes(mating_pool[[ind[1]]], mating_pool[[ind[2]]])
                child2 <- CrossoverChromosomes(mating_pool[[ind[2]]], mating_pool[[ind[1]]])
                
                next_population <- c(next_population, list(child1, child2))
                mating_pool <- mating_pool[-ind]
            } else {
                next_population <- c(next_population, mating_pool[ind])
                mating_pool <- mating_pool[-ind]
            }
        } else if (length(mating_pool) == 1) {
            next_population <- c(next_population, mating_pool)
            mating_pool <- list()
        }
    }

    return(next_population)
}
