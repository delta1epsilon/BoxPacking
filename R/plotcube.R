library(rgl)
library(dplyr)

#' A custom function to initialize RGL device
#' 
#' @param new.device - A logical value. If TRUE, creates a new device
#' @param width the width of the device
RGLInit <- function(new.device = FALSE, width = 500) { 
    if (new.device | rgl.cur() == 0) {  # rgl.cur(): returns active device ID
        rgl.open()
        par3d(windowRect = 50 + c(0, 0, width, width))
        rgl.bg(color = "white")
    }
    rgl.viewpoint(theta = 40, phi = 20)
}

#' Plot 3D cube 
#' 
#' @param object - An object of class 'Container', 'Box' or 'EMS'
#' @param plot_origin  - logical, whether to plot point at the origin
#' @examples 
#' 
#' RGLInit(new.device = T)  # create new device with specific settings
#' # plot a container
#' container <- Container(width = 2, length = 4, height = 2)
#' PlotCube(container)
#' 
#' # plot a box 
#' box <- Box(width = 1, length = 1, height = 1, origin = c(0, 0,0))
#' PlotCube(box)
PlotCube <- function (object,
                      plot_origin = TRUE, ...) {
    
    origin <- object@origin 
    length <- object@length
    height <- object@height
    width <- object@width
        
    if (width <= 0) {
        stop('Spicify argument: width')
    } else if (length <= 0) {
        stop('Spicify argument: length')
    } else if (height <= 0) {
        stop('Spicify argument: height')
    }
    
    if (class(object) == 'Container') {
        origin <- c(0, 0, 0)
    }
    
    vertex1 <- origin
    vertex2 <- origin + c(0, height, width)
    vertex3 <- origin + c(length, height, 0)
    vertex4 <- origin + c(length, 0, width)
    
    # create data frame with coordinates of lines
    # to be joined to form a cube
    lines <- data.frame(vertex1, origin + c(0, height, 0))
    lines <- lines %>% cbind(data.frame(vertex1, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex1, origin + c(0, 0, width)))
    
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(0, 0, width)))
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(0, height, 0)))
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(length, height, width)))
    
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(0, height, 0)))
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(length, height, width)))
    
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(0, 0, width)))
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(length, height, width)))
    
    lines <- t(lines)
    
    if (class(object) == 'Container') {
        cube_color <- 'black'
    } else {
        # randomly select color for cube
        colors <- c('blue', 'red', 'green', 'orange')
        cube_color <- sample(colors, 1)
    }
    
    # plot cube
    segments3d(lines, line_antialias = TRUE, color = cube_color, ...)
    if (plot_origin) {
       points3d(x = 0, y = 0, z = 0, color = 'red', size = 7)
    }
}
