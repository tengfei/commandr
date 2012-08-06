## operation

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
print(obj)
com@undo()(1)
print(obj)
com <- rev(com)
com@do()(1)
print(obj)
rev(com)

## stage
## Stage("rap")   ## return 'StageRap'
obj <- setStage("stage1","stage 1","character")
## role

## 1. need to define a pipeline
## let's make an simple pipeline
## remove NA --> then compute statistics, could be mean/sd

## 2. need to define stage and protocols for each stage
stage1 <- setStage("removeMissing", "Remove missing values", "numeric", "numeric")
setProtocol("filt", "filt out missing", representation(weight = "numeric"),
            function(obj){
              obj[!is.na(obj)]
            }, "removeMissing")
stage2 <- setStage("compute", "compute statistics", "numeric", "numeric")
setProtocol("mean", "compute mean", representation(weight = "numeric"),
            mean, stage2)
setProtocol("sd", "compute sd", representation(weight = "numeric"),
            sd, stage2)

stage3 <- setStage("sort2chr", "sort data", "numeric", "character")
setProtocol("hard", "hard d", representation(no = "logical"),
            function(obj, no, ...){
              obj <- as.character(sort(obj))
            }, "sort2chr")
obj.p <- Protocol("removeMissing", "filt")
obj.p <- Protocol("compute", "mean")
class(obj.p)
is(obj.p, "Protocol")
undebug(StageForProtocol)
stage(obj.p)
role(stage(obj.p))
method(obj.p)

## 3. make a pipline
obj.pipe <- Pipeline(Protocol("removeMissing", "filt"),
                     Protocol("compute", "mean"),
                     Protocol("sort2chr", "hard"))

## accessors
parameters(obj.pipe)
protocol(obj.pipe, "removeMissing")
findProtocols(obj.pipe, "removeMissing")
inType(obj.pipe)
outType(obj.pipe)
pipeline(obj.pipe)
pipeline(obj.pipe, "numeric", "numeric")
pipeline(obj.pipe, "numeric", "character")




setClass("testEx", contains = c("character", "PipelineData"))
testEx <- function(){
  protos <- list(Protocol("removeMissing", "filt"),
                 Protocol(""))
}

## accessor for Pipeline
computeProto
computeProro <- ""

slotNames(protocolClass("compute", "mean"))

x <- rnorm(100)
compute(x, "mean")
getMethod("compute", "numeric")
## 
setProtocol("convert","con ver t",representation())

