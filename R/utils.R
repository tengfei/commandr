capitalize <- function(str) {
  if (length(str) && nchar(str))
      substring(str, 1, 1) <- toupper(substring(str, 1, 1))
  str
}
decapitalize <- function(str) {
  ## Dont't capitalize ALL CAPS, e.g. abbreviations
  if (str != toupper(str)) {
    substring(str, 1, 1) <- tolower(substring(str, 1, 1))
  }
  str
}

findSubclasses <- function(Class, where = topenv(parent.frame())) {
### FIXME: apparently @subclasses is not filled in across packages
### So we have to brute-force search all class definitions here
  classes <- getClasses(where, TRUE)
  classes[unlist(lapply(classes, extends, Class))]
}

### The '...' correspond to "perform time" arguments -- not parameters
### It would not be consistent for a protocol to override its own parameters
callNextProtocol <- function(...) {
  data <- get(names(formals(sys.function(sys.parent())))[1], parent.frame())
  env <- sys.frame(sys.parent(3))
  do.call("callNextMethod", list(object=quote(object), data, ...), envir = env)
}
