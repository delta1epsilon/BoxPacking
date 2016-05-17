#' Perform Bin Packing
#' 
#' @param containers      - A list of objects of class Container
#' @param boxes           - A list of objects of class Box
#' @param n_iter          - An integer; Number of iterations
#' @param population_size - An integer; Number of Chromosomes in each generation
#' @param selection_size  - An integer; Number of the best chromosomes to be 
#'                          choosen to next generaion   
#' @param verbose         - logical; Whether to print info during program execution
#' @param plotSolution    - Logical; Whether to plot a Packing Solution
#'
#' @return A Packing Solution list 
PerformBinPacking <- function (containers, 
                               boxes,
                               n_iter, 
                               population_size, 
                               selection_size,
                               verbose = FALSE, 
                               plotSolution = FALSE) {

    n <- length(containers)  # number of containers
    m <- length(boxes)  # number of boxes

    # initialize list of best chromosomes and their fitness vector
    # to be passed to next generation
    selected_chromosomes <- list()
    selected_chromosomes_fitness <- c()

    for (iter in 1:n_iter) {
        if (verbose) {
            cat('Iteration:', iter, 'out of ', n_iter, '\n')
        }

        # number of chromosomes that are going to be created on each iteration
        n_chromosomes_to_create <- population_size - length(selected_chromosomes)
        adjust <- population_size - n_chromosomes_to_create

        # initialize population chromosomes and their fitness as best  
        # chromosomes and their fitness from previous generation
        population_chromosomes <- selected_chromosomes
        chromosome_fitness <- c(selected_chromosomes_fitness, rep(0, n_chromosomes_to_create))

        for (chromosome_i in 1:n_chromosomes_to_create) {
            if (verbose) {
                cat('  Chromosome:', chromosome_i, 'out of ', n_chromosomes_to_create, '\n')
            }

            # add new chromosome to the population on current generation
            if (iter == 1 & chromosome_i <= 4) {  # generate 4 special chromosomes in first generation
                chromosome <- CustomChromosomeInitialization(boxes, n_containers = n, sort_by = chromosome_i)
            } else {
                chromosome <- CreateChromosome(n_boxes = m, n_containers = n)
            }
            population_chromosomes <- c(population_chromosomes, list(chromosome))

            # perform packing
            packing_solution <- 
                PackBoxes(boxes = boxes, 
                          containers = containers, 
                          box_packing_sequence = chromosome$BPS,
                          container_loading_sequence = chromosome$CLS 
                          )

            # calculate fitness of current chromosome 
            chromosome_fitness[chromosome_i + adjust] <- CalculateFitness(packing_solution)
        } 

        # Select the best chromosomes to next generation 
        best_chromosomes_ind <- 
            PerformChromosomeSelection(chromosome_fitness, 
                                       selection_size
                                       )
        selected_chromosomes <- population_chromosomes[best_chromosomes_ind]
        selected_chromosomes_fitness <- chromosome_fitness[best_chromosomes_ind]
    }

    # choose solution of packing after all iterations
    best_chromosome <- population_chromosomes[[which.min(chromosome_fitness)]]
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
