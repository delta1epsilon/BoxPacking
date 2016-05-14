# TODO: write EMS prioritization function
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
                con_EMS <- packing_solution[[container_ind]][[1]]@ems

                for (ems in con_EMS) {  # for each EMS in the container
                    if (box@length <= ems@length & 
                        box@height <= ems@height & 
                        box@width <= ems@width) {

                        # 1. place box in ems: set box origin to EMS origin 
                        box@origin <- ems@origin 

                        # 2. write placement in packing solution 
                        packing_solution[[container_ind]] <- 
                            c(packing_solution[[container_ind]], box)

                        # 3. update EMS for the container
                        packing_solution[[container_ind]][[1]]@ems <- 
                            UpdateEMS(packing_solution[[container_ind]][[1]]@ems, box)

                        # 4. mark the box as placed:
                        placed_boxes[box_ind] <- TRUE

                        break
                    }                           
                }
            }
        }
    }

    return(packing_solution)
}
