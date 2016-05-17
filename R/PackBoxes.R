# TODO: write a function for choosing best box rotation


#' Pack the Boxes into the Containers
#'
#' @param boxes                      - A list of objects of class Box 
#' @param containers                 - A list of objects of class Container
#' @param box_packing_sequence       - A vector
#' @param container_loading_sequence - A vector
#' @return A list of Containers and corresponding Boxes placed into 
#'         this Containers 
PackBoxes <- function (boxes, 
                       containers, 
                       box_packing_sequence, 
                       container_loading_sequence) {

    n <- length(containers)  # number of containers
    m <- length(boxes)  # number of boxes

    if (n == 0) {
        stop('Specify containers')
    }
    if (m == 0) {
        stop('Specify boxes')
    }

    #' Create an empty Packing Solution
    #'
    #' @param containers - A list of containers
    #' @return A list
    CreatePackingSolution <- function (containers) {
        packing_solution <- list()
        for (i in 1:length(containers)) {
            packing_solution <- c(packing_solution, list(containers[i]))
        }

        return(packing_solution)
    }

    # create empty packing solution
    packing_solution <- CreatePackingSolution(containers)

    # initialize box placement to FALSE
    placed_boxes <- rep(FALSE, m)

    for (con_i in 1:n) {  # for each container
        container_ind <- container_loading_sequence[con_i]  # get index of container from chromosome

        for (box_i in 1:m) {  # for each box
            box_ind <- box_packing_sequence[box_i]  # get index of box from chromosome

            if (placed_boxes[box_ind]) {
                # the box is already placed, move to next one
                next
            } else {
                # the box isn't placed yet

                # get box
                box <- boxes[[box_ind]]

                # get containers EMS
                con_EMS <- packing_solution[[container_ind]][[1]]@ems  # a list

                # prioritize container's EMS
                con_EMS <- PrioritizeEMS(con_EMS)

                for (ems in con_EMS) {  # for each EMS in the container
                    if (CheckIfBoxFitsIntoEMS(box, ems)) {

                        # 1. choose best box placement
                        box <- PerformPlacementSelection(box, ems)

                        # 2. place box in ems: set box origin to EMS origin 
                        box@origin <- ems@origin 

                        # 3. write placement in packing solution 
                        packing_solution[[container_ind]] <- 
                            c(packing_solution[[container_ind]], box)

                        # 4. update EMS for the container
                        packing_solution[[container_ind]][[1]]@ems <- 
                            UpdateEMS(packing_solution[[container_ind]][[1]]@ems, box)

                        # 5. mark the box as placed:
                        placed_boxes[box_ind] <- TRUE

                        break
                    }                           
                }
            }
        }
    }

    return(packing_solution)
}


#' Calcuate a Fitness of Packing Solution (Percent of Wasted Space)
#'
#' @param packing_solution - A list
#' @return A number between 0 and 1
CalculateFitness <- function (packing_solution) {

    #' Calculate volume of an object
    #'
    #' @param object - An object of class Box or Container
    #' @return A numeric
    CalculateVolume <- function (object) {
        volume <- object@length * object@height * object@width
        return(volume)
    }

    container_volume <- 0
    boxes_volume <- 0

    for (i in 1:length(packing_solution)) {
        if (length(packing_solution[[i]]) == 1) {
            # the Container is empty
            next
        } else {
            container_volume <- container_volume + CalculateVolume(packing_solution[[i]][[1]])
            
            for (j in 2:length(packing_solution[[i]])) {
                boxes_volume <- boxes_volume + CalculateVolume(packing_solution[[i]][[j]])
            }
        }
    }

    # calculate percent of wasted space
    fitness <- 1 - (boxes_volume / container_volume)
    return(fitness)
}
