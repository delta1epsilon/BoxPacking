# define class for containers
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

# define class for boxes
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
