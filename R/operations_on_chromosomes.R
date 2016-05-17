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
#' @param boxes   - A list of objects of class Box
#' @param sort_by - An integer (in 1:4) 
#' @return A chromosome 
CustomChromosomeInitialization <- function (boxes, n_containers, sort_by) {
    if (sort_by == 1) {
        boxes_length <- sapply(boxes, function(x) x@length)
        
        # sort descending according to length
        ind_by_length <- order(boxes_length, decreasing = TRUE)
        chromosome <- list(BPS = ind_by_length, CLS = sample(1:n_containers))
    } else if (sort_by == 2) {
        boxes_height <- sapply(boxes, function(x) x@height)
    
        # sort descending according to height
        ind_by_height <- order(boxes_height, decreasing = TRUE)
        chromosome <- list(BPS = ind_by_height, CLS = sample(1:n_containers))
    } else if (sort_by == 3) {
        boxes_width <- sapply(boxes, function(x) x@width)
    
        # sort descending according to width
        ind_by_width <- order(boxes_width, decreasing = TRUE)
        chromosome <- list(BPS = ind_by_width, CLS = sample(1:n_containers))
    } else if (sort_by == 4) {
        boxes_volume <- sapply(boxes, function(x) x@length * x@height * x@width)

        # sort descending according to width
        ind_by_volume <- order(boxes_volume, decreasing = TRUE)
        chromosome <- list(BPS = ind_by_volume, CLS = sample(1:n_containers))        
    } else {
        stop('The argument sort_by must be in 1:4')
    }

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
