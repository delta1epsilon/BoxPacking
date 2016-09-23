#' An s4 class to represent a Container
#'
#' @slot origin A length-three vector 
#' @slot length A numeric
#' @slot height A numeric
#' @slot width A numeric
#'
#' @examples 
#' # create a container with size 2 x 2 x 2
#' c1 <- Container(length = 2, height = 2, width = 2)
#'
#' @export Container
Container <- setClass('Container',
                      slots = c(origin = 'numeric',
                                length = 'numeric',
                                height = 'numeric',
                                width = 'numeric',
                                ems = 'list'  # list of instances of class EMS 
                                ),
                      prototype = list(origin = c(0, 0, 0)),
                      validity = function (object) {  # make sure that all parameters are positive
                              if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                                  return("A number <= 0 for one of the parameters was given.")
                              }
                              return(TRUE)
                          }
                      )

# create method 'initialize' for Container class to create 
# slot EMS (initial EMS is whole Container) when
# an instance of class Container is being created 
#' @export 
setMethod('initialize', 
          'Container',
          function (.Object, ...) {
              .Object <- callNextMethod()
              .Object@ems <- 
                  list(
                      EMS(origin = .Object@origin,
                          length = .Object@length,
                          height = .Object@height,
                          width = .Object@width
                          )
                      )
              return(.Object)
          }
          )

#' An s4 class to represent a Box
#'
#' @slot origin A length-three vector 
#' @slot length A numeric
#' @slot height A numeric
#' @slot width A numeric
#' @slot weight A numeric
#'
#' @examples 
#' # create a box with size 1 x 1 x 1
#' b1 <- Box(length = 1, height = 1, width = 1)
#'
#' @export Box
Box <- setClass('Box',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric',
                          weight = 'numeric'
                          ),
                validity = function (object) {  # make sure that all parameters are positive
                        if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                            return("A number <= 0 for one of the parameters was given.")
                        }
                        return(TRUE)
                    }
                ) 

# define class for Empty Maximal Spaces
#' @export EMS
EMS <- setClass('EMS',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric'
                          )
                )
