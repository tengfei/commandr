## Command objects represent a high-level operation

setClass("Command")

### possibly supported methods
## rev(x)
## active(), active<-()

## returns a widget for controlling and viewing this object
setGeneric("widget", function(object, ...) standardGeneric("widget"))

## returns a widget containing an interactive visualization of the
## specified data in the context of the specified protocol
setGeneric("explore", function(object, protocol, ...)
           standardGeneric("explore"))

## how an object is identified in a user interface
setGeneric("dispName", function(object, ...) standardGeneric("dispName"))
## by default, the name of the object
setMethod("dispName", "ANY", function(object) class(object))
