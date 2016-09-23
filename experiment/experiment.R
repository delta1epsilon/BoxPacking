c1 <- Container(length = 10, height = 10, width = 10)

container <- c1
n_iter <- 5

GenerateExperiment <- function (container, n_iter = 5) {
    # initialize empty list of Boxes
    boxes <- list()
    boxes_volume <- 0
    
    # empty_space <- 1  # percent of empty space
    # i = 1
    
    # while (empty_space > 0.2) {
    for (i in 1:n_iter) {
        cat('Iteration: ', i, '\n')
        # i <- i + 1
        
        # randomly shoose EMS
        ems_ind <- sample(1:length(container@ems), 1)
        ems <- container@ems[[ems_ind]]
        
        # randomly create a Box in ems
        box <- GenerateBox(ems)
        
        # place box in EMS
        box@origin <- ems@origin
        
        boxes <- c(boxes, box)
        
        # update percent of empty space
        empty_space <- CalculateEmptySpace(container, boxes)
        cat('  Empty space: ', empty_space, '\n')
        
        # update container EMS
        container@ems <- UpdateEMS(container@ems, box)
    }
    
    cat('making EMS boxes ...', '\n')
    
    while (empty_space > 0) {
        cat('Iteration: ', i, '\n')
        i <- i + 1
    
        # randomly shoose EMS
        ems_ind <- sample(1:length(container@ems), 1)
        ems <- container@ems[[ems_ind]]
        
        box <- Box(origin = ems@origin, 
                   length = ems@length,
                   height = ems@height, 
                   width = ems@width
                   )
        
        if (length(container@ems) > 1) {
            # update container EMS
            container@ems <- UpdateEMS(container@ems, box)
        }
        
        boxes <- c(boxes, box)
        
        # update percent of empty space
        empty_space <- CalculateEmptySpace(container, boxes)
        cat('  Empty space: ', empty_space, '\n')
        
    }
    
    return(boxes)
}


PlotBoxes <- function (container, boxes) {
    RGLInit(new.device = T)
    PlotCube(container)
    for (box in boxes) {
        PlotCube(box)    
    }
}

GenerateBox <- function (ems) {
    
    GenerateParameter <- function (ems_parameter) {
        # create vector of possible parameters
        x <- seq(0, (ems_parameter * 3) / 4, length.out = 6)[-1]
        
        # set distribution
        prob = c(0.1, 0.1, 0.15, 0.25, 0.4)
        
        # randomly choose parameter 
        parameter <- sample(x = x,
                            size = 1,
                            prob = prob
                            )
        return(parameter)
    }

    length <- GenerateParameter(ems@length)
    height <- GenerateParameter(ems@height)
    width <- GenerateParameter(ems@width)
    
    box <- 
        Box(length = length,
            height = height,
            width = width
            )

    return(box)
}




CalculateEmptySpace <- function (container, boxes) {
    #' Calculate volume of an object
    #'
    #' @param object - An object of class Box or Container
    #' @return A numeric
    CalculateVolume <- function (object) {
        volume <- object@length * object@height * object@width
        return(volume)
    }
    
    container_volume <- CalculateVolume(container)

    if (length(boxes) == 0) {
        boxes_volume <- 0
    } else {
        boxes_volume <- sum(sapply(boxes, CalculateVolume))
    }
    
    empty_space <- 1 - (boxes_volume / container_volume)
    return(empty_space)
}

