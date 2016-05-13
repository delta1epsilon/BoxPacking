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
    
    return(ems_list)
}


# TODO: fix a bug related to EMS that 'are in the air', 
#       what means we can't place a box in such EMS


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
        if (length(new_ems) != 0) {
            ind_to_remove <- c(ind_to_remove, i)
            new_ems_list <- c(new_ems_list, new_ems)
        }
    } 

    # remove EMS that were updated
    if (length(ind_to_remove) != 0) {
        new_ems_list <- new_ems_list[-ind_to_remove]
    }
    
    return(new_ems_list)
}
