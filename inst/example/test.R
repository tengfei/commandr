library(commandr)
opt <- new('Operation')
getClass('Operation')
getClass('OptionalFunction')

obj <- 10
myfun1 <- function(a){
  obj<<-obj+a
}
myfun2 <- function(a){
  obj<<-obj-a
}

com <- quoteCommand(do=myfun1,undo=myfun2)
com@do()(1)
com <- rev(com)
rev(com)

setStage("stage1","stage 1","character")
setProtocol("convert","con ver t",representation())
