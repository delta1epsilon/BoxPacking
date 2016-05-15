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
