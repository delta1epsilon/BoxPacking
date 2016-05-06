# define class for containers
Container <- setClass('Container',
                      slots = c(origin = 'numeric',
                                length = 'numeric',
                                height = 'numeric',
                                width = 'numeric'
                                ),
                      prototype = list(origin = c(0, 0, 0)),
                      validity = function (object) {  # make sure that all parameters are positive
                              if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                                  return("A number <= 0 for one of the parameters was given.")
                              }
                              return(TRUE)
                          }
                      )

# define class for boxes
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
EMS <- setClass('EMS',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric'
                          )
                )
