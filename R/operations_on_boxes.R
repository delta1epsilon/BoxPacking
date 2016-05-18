#' Check If the Box fits into the EMS
#'
#' @param box - An object of class Box
#' @param ems - An object of class EMS
#' @return TRUE/FALSE
CheckIfBoxFitsIntoEMS <- function (box, ems) {
    # check if the box originaly fits
    fit1 <-
        (box@length <= ems@length & 
         box@height <= ems@height & 
         box@width <= ems@width)
    
    # check if the box fits after rotation 1
    fit2 <-
        (box@height <= ems@length & 
         box@length <= ems@height & 
         box@width <= ems@width)

    # check if the box fits after rotation 2
    fit3 <-
        (box@width <= ems@length & 
         box@height <= ems@height & 
         box@length <= ems@width)
    
    fits <- (fit1 | fit2 | fit3)
    return(fits)
}


#' Perform Best Placement Selection
#'
#' @param box - An object of class Box
#' @param ems - An object of class EMS
#' @return An object of class Box
PerformPlacementSelection <- function (box, ems) {
    possible_rotations <- list()
    possible_margins <- list()
    
    # original fit
    if (box@length <= ems@length & 
        box@height <= ems@height & 
        box@width <= ems@width) {
        margins <- c(ems@length - box@length, 
                     ems@height - box@height,
                     ems@width - box@width
                    )
        possible_rotations <- c(possible_rotations, list(box))
        possible_margins <- c(possible_margins, list(margins))
    }
    
    # rotation 1
    if (box@height <= ems@length & 
        box@length <= ems@height & 
        box@width <= ems@width) {
        margins <- c(ems@length - box@height, 
                     ems@height - box@length,
                     ems@width - box@width
                    )
        rotated_box <- box
        rotated_box@length <- box@height 
        rotated_box@height <- box@length 
                
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
    }
    
    # rotation 2
    if (box@width <= ems@length & 
        box@height <= ems@height & 
        box@length <= ems@width) {
        margins <- c(ems@length - box@width, 
                     ems@height - box@height,
                     ems@width - box@length
                    )
        rotated_box <- box
        rotated_box@length <- box@width 
        rotated_box@width <- box@length
        
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
    }    

    if (length(possible_rotations) == 0) {
        stop('The box does not fit into the EMS')
    } else {
        # select rotation with smalest margin
        best_ind <- which.min(sapply(possible_margins, min))    
        rotated_box <- possible_rotations[[best_ind]]
    }

    return(rotated_box)
}
