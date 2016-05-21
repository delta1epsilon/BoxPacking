#' Perform Bin Packing
#' 
#' @param containers      - A list of objects of class Container
#' @param boxes           - A list of objects of class Box
#' @param n_iter          - An integer; Number of iterations
#' @param population_size - An integer; Number of Chromosomes in each generation
#' @param elitism_size    - An integer; Number of the best chromosomes to be 
#'                          choosen to next generaion   
#' @param crossover_prob  - A numeric in [0; 1]; A probability for chromosome crossover  
#' @param mutation_prob   - A numeric in [0; 1]; A probability for chromosome mutation  
#' @param verbose         - Logical; Whether to print info during program execution
#' @param plotSolution    - Logical; Whether to plot a Packing Solution
#'
#' @return A Packing Solution list 
PerformBinPacking <- function (containers, 
                               boxes,
                               n_iter, 
                               population_size, 
                               elitism_size,
                               crossover_prob, 
                               mutation_prob,
                               verbose = FALSE, 
                               plotSolution = FALSE) {

    # TODO: think about case with 1-2 boxes, 1-2 containers

    # TODO: write verifications of arguments
    if (elitism_size < 0) {
        stop('Elitism size cant be negative')
    } else if (elitism_size == 0) {
        print('Bad choice of elitism size')
    }
    if (length(containers) == 0) {
        stop('Specify containers')
    }
    if (length(boxes) == 0) {
        stop('Specify boxes')
    }
    if (n_iter <= 0) {
        stop('Number of iterations cant be <= 0')
    }
    if (population_size <= 0) {
        stop('Population size cant be <= 0')
    } #else if (population_size > 0 & population_size < .) {
    #      print('Bad choice for Population Size')
    # }
    if (crossover_prob < 0 | crossover_prob > 1) {
        stop('crossover_prob must be in [0;1]')
    } else if (crossover_prob == 0) {
        print('Not the best choice for crossover_prob')
    }
    if (mutation_prob < 0 | mutation_prob > 1) {
        stop('mutation_prob must be in [0;1]')
    } else if (mutation_prob == 0) {
        print('Not the best choice for mutation_prob')
    }

    n <- length(containers)  # number of containers
    m <- length(boxes)  # number of boxes

    # Initialization 
    population <- InitializePopulation(population_size = population_size, 
                                       n_containers = n, 
                                       boxes = boxes
                                       ) 
    chromosome_fitness <- rep(0, population_size)

    elitism_chromosomes <- list()
    elitism_chromosomes_fitness <- c()

    for (iter in 1:n_iter) {
        if (verbose) {
            cat('Iteration:', iter, 'out of ', n_iter, '\n')
        }

        population_size <- length(population)
        for (chromosome_i in 1:population_size) {
            if (verbose) {
                cat('  Chromosome:', chromosome_i, 'out of ', population_size, '\n')
            }

            chromosome <- population[[chromosome_i]]

            # perform packing
            packing_solution <- 
                PackBoxes(boxes = boxes, 
                          containers = containers, 
                          box_packing_sequence = chromosome$BPS,
                          container_loading_sequence = chromosome$CLS 
                          )

            # calculate fitness of current chromosome 
            chromosome_fitness[chromosome_i] <- CalculateFitness(packing_solution)
        } 

        population <- c(population, elitism_chromosomes)
        chromosome_fitness <- c(chromosome_fitness, elitism_chromosomes_fitness)

        # Select the best chromosomes to next generation 
        best_chromosomes_ind <- 
            PerformElitism(chromosome_fitness, 
                           elitism_size
                           )

        elitism_chromosomes <- population[best_chromosomes_ind]
        elitism_chromosomes_fitness <- chromosome_fitness[best_chromosomes_ind]

        # remove elitism chromosomes from the population 
        population <- population[-best_chromosomes_ind]
        chromosome_fitness <- chromosome_fitness[-best_chromosomes_ind]

        # Selection
        mating_pool <- PerformSelection(population, fitness = chromosome_fitness)

        # Crossover
        crossovered_chromosomes <- PerformCrossover(mating_pool, crossover_prob = crossover_prob)

        # Mutation
        population <- PerformMutation(crossovered_chromosomes, mutation_prob = mutation_prob)
    }

    population <- c(population, elitism_chromosomes)
    chromosome_fitness <- c(chromosome_fitness, elitism_chromosomes_fitness)

    # choose solution of packing after all iterations
    best_chromosome <- population[[which.min(chromosome_fitness)]]
    best_chromosome_packing_solution <- 
        PackBoxes(boxes = boxes, 
                  containers = containers, 
                  box_packing_sequence = best_chromosome$BPS,
                  container_loading_sequence = best_chromosome$CLS 
                  )    

    if (plotSolution) {
        PlotPackingSolution(best_chromosome_packing_solution)
    }

    return(best_chromosome_packing_solution)
}
