# define class for containers
Container <- setClass('Container',
                      slots = c(origin = 'numeric',
                                length = 'numeric',
                                height = 'numeric',
                                width = 'numeric'
                                ),
                      prototype = list(origin = c(0, 0, 0))
                      )

# define class for boxes
Box <- setClass('Box',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric',
                          weight = 'numeric'
                          )
                ) 

# define class for Empty Maximal Spaces
EMS <- setClass('EMS',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric'
                          )
                )
