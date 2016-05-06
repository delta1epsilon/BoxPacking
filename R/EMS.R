#' Create Empty Maximal Spaces
#'
#' @param container - An object of class 'Container' or 'EMS' 
#' @param box       - An object of class 'Box'
#'  
#' @return A list of 5 instance of class EMS or NULL
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

    # check if box is inside container
    box_vertex <- box@origin + c(0, box@height, 0)  # top vertex of the box
    if (!(# check by length axis
        (box_vertex[1] >= container@origin[1] & 
         box_vertex[1] < (container@origin[1] + container@length)
        ) &
        # check by height axis
        (box_vertex[2] >= container@origin[2] & 
         box_vertex[2] < (container@origin[2] + container@height)
        ) &
        # check by width axis
        (box_vertex[3] >= container@origin[3] & 
         box_vertex[3] < (container@origin[3] + container@width)
        )
    )
    ) {
        # box is outside the container
        return(list(NULL, NULL, NULL, NULL, NULL))
    }
    
    #' Check if EMS has nonzero parameters
    #'
    #' @param EMS - An instance of class EMS
    #' @return TRUE if EMS is valid (all parameters are nonzero) otherwise FALSE
    CheckIfEMSvalid <- function (EMS_object) {
        if (EMS_object@length == 0 | EMS_object@height == 0 | EMS_object@width == 0 ) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
    
    # EMS 1:
    EMS1 <- EMS(origin = container@origin, 
                length = box@origin[1] - container@origin[1],
                height = container@height,
                width = container@width
                )
    
    if (!CheckIfEMSvalid(EMS1)) {
        EMS1 <- NULL
    }
    
    # EMS 2:
    EMS2 <- EMS(origin = container@origin, 
                length = container@length,
                height = container@height,
                width = box@origin[3] - container@origin[3]
                )
    
    if (!CheckIfEMSvalid(EMS2)) {
        EMS2 <- NULL
    }
    
    # EMS 3:
    EMS3 <- EMS(origin = c(box@origin[1], container@origin[2:3]) + c(box@length, 0, 0), 
                height = container@height,
                width = container@width
                )
    EMS3@length <- ((container@origin + c(container@length, 0, 0)) - EMS3@origin)[1]
    
    if (!CheckIfEMSvalid(EMS3)) {
        EMS3 <- NULL
    }
    
    # EMS 4:
    EMS4 <- EMS(origin = c(container@origin[1:2], box@origin[3]) + c(0, 0, box@width), 
                length = container@length,
                height = container@height
                )
    EMS4@width <- ((container@origin + c(0, 0, container@width)) - EMS4@origin)[3]
    
    if (!CheckIfEMSvalid(EMS4)) {
        EMS4 <- NULL
    }
    
    # EMS 5:
    EMS5 <- EMS(origin = c(container@origin[1], box@origin[2], container@origin[3]) + c(0, box@height, 0), 
                length = container@length,
                width = container@width
                )
    EMS5@height <- ((container@origin + c(0, container@height, 0)) - EMS5@origin)[2]
    
    if (!CheckIfEMSvalid(EMS5)) {
        EMS5 <- NULL
    }
    
    return(list(EMS1, EMS2, EMS3, EMS4, EMS5))
}
