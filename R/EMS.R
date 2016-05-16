#' Create Empty Maximal Spaces
#'
#' @param container - An object of class 'Container' or 'EMS' 
#' @param box       - An object of class 'Box'
#'  
#' @return A list of instance of class EMS or empty list
#' @examples 
#' container <- Container(width = 2, length = 4, height = 2)
#' box <- Box(width = 1, length = 1, height = 1, origin = c(0, 0,0))
#' 
#' CreateEMS(container, box)
CreateEMS <- function (container, 
                       box) {
    
    # stop if box origin is not specified
    if (length(box@origin) == 0) {
        stop('Specify origin for the box')
    }

    #' Check if the box is otside the container
    #'
    #' @param container - An object of class 'Container' or 'EMS'
    #' @param box       - An object of class 'Box'
    #' @return TRUE/FALSE 
    CheckIfBoxIsOutside <- function (container, box) {
        # get top and bottom vertexes of the container
        container_vertex1 <- container@origin
        container_vertex2 <- container@origin + c(container@length, container@height, container@width)

        # get 4 vertexes of the box
        box_vertex1 <- box@origin
        box_vertex2 <- box@origin + c(0, box@height, 0)
        box_vertex3 <- box@origin + c(0, 0, box@width)
        box_vertex4 <- box@origin + c(box@length, 0, 0)

        is_outside <- 
            (container_vertex1[2] >= box_vertex2[2] |
             container_vertex2[2] <= box_vertex1[2] |
             container_vertex2[1] <= box_vertex1[1] |
             container_vertex1[1] >= box_vertex4[1] |
             container_vertex1[3] >= box_vertex3[3] |
             container_vertex2[3] <= box_vertex1[3])

        return(is_outside)
    }
    
    #' Check if EMS has nonzero parameters
    #'
    #' @param EMS - An instance of class EMS
    #' @return TRUE if EMS is valid (all parameters are nonzero) otherwise FALSE
    CheckIfEMSvalid <- function (EMS_object) {
        if (EMS_object@length <= 0 | EMS_object@height <= 0 | EMS_object@width <= 0 ) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }


    if (CheckIfBoxIsOutside(container, box)) {
        return(list())        
    }

    ems_list <- list()
    
    # EMS 1:
    EMS1 <- EMS(origin = container@origin, 
                length = box@origin[1] - container@origin[1],
                height = container@height,
                width = container@width
                )
    
    if (CheckIfEMSvalid(EMS1)) {
        ems_list <- c(ems_list, EMS1)
    }
    
    # EMS 2:
    EMS2 <- EMS(origin = container@origin, 
                length = container@length,
                height = container@height,
                width = box@origin[3] - container@origin[3]
                )
    
    if (CheckIfEMSvalid(EMS2)) {
        ems_list <- c(ems_list, EMS2)
    }
    
    # EMS 3:
    EMS3 <- EMS(origin = c(box@origin[1], container@origin[2:3]) + c(box@length, 0, 0), 
                height = container@height,
                width = container@width
                )
    EMS3@length <- ((container@origin + c(container@length, 0, 0)) - EMS3@origin)[1]
    
    if (CheckIfEMSvalid(EMS3)) {
        ems_list <- c(ems_list, EMS3)
    }
    
    # EMS 4:
    EMS4 <- EMS(origin = c(container@origin[1:2], box@origin[3]) + c(0, 0, box@width), 
                length = container@length,
                height = container@height
                )
    EMS4@width <- ((container@origin + c(0, 0, container@width)) - EMS4@origin)[3]
    
    if (CheckIfEMSvalid(EMS4)) {
        ems_list <- c(ems_list, EMS4)
    }
    
    # EMS 5:
    EMS5 <- EMS(origin = c(container@origin[1], box@origin[2], container@origin[3]) + c(0, box@height, 0), 
                length = container@length,
                width = container@width
                )
    EMS5@height <- ((container@origin + c(0, container@height, 0)) - EMS5@origin)[2]
    
    if (CheckIfEMSvalid(EMS5)) {
        ems_list <- c(ems_list, EMS5)
    }
    
    # EMS 6:
    EMS6 <- EMS(origin = container@origin, 
                length = container@length,
                height = box@origin[2] - container@origin[2],
                width = container@width
                )
    
    if (CheckIfEMSvalid(EMS6)) {
        ems_list <- c(ems_list, EMS6)
    }

    return(ems_list)
}


#' Check if an EMS is totally inscribed by other EMS
#'
#' @param ems_to_check - An object of class EMS to check if it's in ems
#' @param ems          - An object of class EMS
#' @return TRUE if ems_to_check is inside ems otherwise FALSE
CheckIfEMSisInsideOtherEMS <- function (ems_to_check, ems) {
    # get 2 vertexes of ems_to_check
    ems_to_check_vertex1 <- ems_to_check@origin
    ems_to_check_vertex2 <- 
        ems_to_check@origin + c(ems_to_check@length, ems_to_check@height, ems_to_check@width)

    # get 2 vertexes of ems
    ems_vertex1 <- ems@origin
    ems_vertex2 <- ems@origin + c(ems@length, ems@height, ems@width)

    ems_is_inside <- 
        all(ems_to_check_vertex1 >= ems_vertex1) &
        all(ems_to_check_vertex2 <= ems_vertex2)

    if (ems_is_inside) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


#' Remove those EMS from list which are inside other EMS
#'
#' @param ems_list - A list of objects of class EMS
#' @return An updated list of EMS objects
EliminateEMSList <- function (ems_list) {
    sequence <- 1:length(ems_list)
    ind_remove <- c()

    for (i in sequence) {
        ems <- ems_list[[i]]

        for (j in setdiff(sequence, i)) {
            if (CheckIfEMSisInsideOtherEMS(ems, ems_list[[j]])) {
                # the ems in inside other ems
                ind_remove <- c(ind_remove, i)
                break
            }
        }
    }

    if (length(ind_remove) != 0) {
        ems_list <- ems_list[-ind_remove]
    }

    return(ems_list) 
}


#' Check If the Box equals to the Container
#'
#' @param box - An object of class Box
#' @param ems - An object of class EMS
#' @return TRUE/FALSE
CheckIfBoxEqualsEMS <- function (box, ems) {
    BoxEqualsEMS <- 
        (all(box@origin == ems@origin) &
         box@length == ems@length &
         box@height == ems@height &
         box@width == ems@width
         )
    return(BoxEqualsEMS)
}


#' Update list of container's EMS after box is placed
#'
#' @param ems_list - A list of objects of class EMS 
#' @param box      - An object of class Box
#'
#' @return A list of objects of class EMS
UpdateEMS <- function (ems_list, box) {
    new_ems_list <- ems_list

    # indeces of EMS that are going to be updated and 
    # therefore replaced by it's update 
    ind_to_remove <- c()

    for (i in 1:length(ems_list)) {
        ems <- ems_list[[i]]

        new_ems <- CreateEMS(ems, box)
        if (length(new_ems) != 0 | CheckIfBoxEqualsEMS(box, ems)) {
            ind_to_remove <- c(ind_to_remove, i)
            new_ems_list <- c(new_ems_list, new_ems)
        }
    } 

    # remove EMS that were updated
    if (length(ind_to_remove) != 0) {
        new_ems_list <- new_ems_list[-ind_to_remove]
    }
    
    # remove EMS that are inside other EMS
    new_ems_list <- EliminateEMSList(new_ems_list)

    return(new_ems_list)
}


#' Prioritize EMS in EMS list (sort EMS by distance to the box origin)
#'
#' @param ems_list - A list with objects of class EMS
#' @return A list with objects of class EMS
PrioritizeEMS <- function (ems_list) {

    #' Calculate distance (Euclidean) from EMS origin to Box origin
    #' 
    #' @param ems - An object of class EMS
    #' @return A numeric
    CalculateDistanceToBoxOrigin <- function (ems) {
        distance <- sqrt(sum(ems@origin^2)) 
        return(distance)
    }

    # calculate distance for each EMS in ems_list
    distances <- sapply(ems_list, CalculateDistanceToBoxOrigin)

    # compute EMS order
    ems_order <- order(distances)

    # prioritize EMS
    ems_list <- ems_list[ems_order]
    return(ems_list)
}
