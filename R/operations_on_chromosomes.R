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



# TODO: modify PerformChromosomeSelection function to do 
#       more advanced selection

#' Perform Selection of the best Chromosomes within population 
#'
#' @param fitness        - A vector with chromosomes scores   
#' @param selection_size - An integer
#' @return A vector of size 'selection_size' with indeces of 
#'         best performing chromosomes
PerformChromosomeSelection <- function (fitness, 
                                        selection_size) {
    best_chromosomes_ind <- order(fitness)[1:selection_size]
    return(best_chromosomes_ind)
}
