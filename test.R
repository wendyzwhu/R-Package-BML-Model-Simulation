####Change the working directory
dir = "d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\stat242assignment3"
setwd(dir)
library(BML)

######test function
system("R CMD SHLIB -o BMLTEST.so test.c movetest.c")
dyn.load("BMLTEST.so")
is.loaded("R_BML")
is.loaded("moveCars")
###the function is compiled and in the load table

getCarLocations =
  function(g)
  {
    w = (g != "")
    i = row(g)[w]
    j = col(g)[w]
    pos = cbind(i, j)
    structure(pos, dimnames = list(g[pos], c("i", "j")))
  }


###test sample grid###
g = createBMLGrid(100,100,c(50,50),0.5)
gi = matrix(match(g[[1]],c("red","blue"),0L),nrow(g[[1]]),ncol(g[[1]]))
location = getCarLocations(gi)
red = location[ rownames(location) == "1", ]
blue = location[ rownames(location) == "2", ]
result = .C("R_BML",
         grid = gi, matrix(as.integer(0), nrow(gi), ncol(gi)), as.integer(dim(gi)),
         red, as.integer(nrow(red)), blue, as.integer(nrow(blue)),
         velocity = integer(2*100),1000L)
 
gridfinal <- result$grid
class(gridfinal) = c("BMLGrid", "matrix")

plot.BMLGrid=
  function(x, xlab = "", ylab = "", ...)
  {
    if(typeof(x) == "character")
      z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
    else
      z = x
    image(t(z), col = c("white", "red", "blue"),
          axes = FALSE, xlab = xlab, ylab = ylab, ...)
    box()
  }

plot.BMLGrid(gridfinal)
plot.BMLGrid(g[[1]])
test = runBMLGrid(g,1000)
gridToIntegerGrid(test[[1]]) == gridfinal

gridToIntegerGrid = function(x){
  matrix(match(x, c("red", "blue"), 0L), nrow(x), ncol(x)) 
}


crunBMLGrid =
  function(status, numIter = 1000L, check = FALSE)
  {
    grid = status[["grid"]]
    k = class(grid)
    gi = gridInterger(grid) 
    velocity = matrix(0L, as.integer(numIter), 2L,
                      dimnames = list(NULL, c("red", "blue")))
    pos = getCarLocations(gi)
    red = pos[rownames(pos) == "1",]
    blue = pos[rownames(pos) == "2",]
    ans = .C("R_BML", gi, grid = gi, dim(gi),
             red = red, nrow(red),
             blue = blue, nrow(blue),
             velocity = velocity,
             as.integer(numIter))
    ans = ans[c("grid", "velocity")]
    class(ans$grid) = k
    ans
  }

test = runBMLGrid(g,1000)
gtest = createBMLGrid(100,100,c(50,50),0.5)
ggtest = crunBMLGrid(gtest)
plot.BML(gtest)
plot.BML(ggtest)

plot.BMLGrid(gtest[[1]])
plot.BMLGrid(ggtest[[1]])

##Test the system.time
g100 <- createBMLGrid(100,100, ncars = c(red = 50, blue = 50),0.5)
tm_C = system.time(o <- crunBMLGrid(g100))
tm_R = system.time(o <- runBMLGrid(g100,100L))
#> tm_C
#2.96 0.10 3.05
#> tm_R
#43.04  4.84 48.64 
##100*100
#> tm_C
#用户 系统 流逝 
#0.03 0.00 0.03 
#> tm_R
#用户 系统 流逝 
#0.44 0.06 0.51 

#Make Comparison for different density and dimensions
exploreGrid =
  function(r = 100, c = 100, ncars = c(50,50),rho = 0.33, numIter = 1000, plot = TRUE)
  {
    grid = createBMLGrid(r,c,ncars,rho)
    g.out = crunBMLGrid(grid, numIter)
    if(plot) {
      plot(grid)
      plot(g.out$grid)
    }
    invisible(list(initial = grid,
                   final = g.out$grid,
                   velocity = g.out$velocity))
  }

par(mfrow = c(1, 3))
z = exploreGrid(100,100,c(50,50),0.5, plot = TRUE)
plot(rowSums(z$velocity), type = "l",
     main = "Number of cars moving in each pair of time steps")


###Different density 
set.seed(12323)
vardensity = lapply(c(.25, .33, .38, .38, .55, .65),
                function(vardensity)
                createBMLGrid(1000,1000, 1000*1000*vardensity, vardensity))
timingsDensity = lapply(c(1:6), function(x) system.time(o <- crunBMLGrid(vardensity[[x]])))
#> timingsDensity
#[[1]]
#用户 系统 流逝 
#2.98 0.10 3.09 

#[[2]]
#用户 系统 流逝 
#3.26 0.11 3.44 

#[[3]]
#用户 系统 流逝 
#3.07 0.07 3.16 

#[[4]]
#用户 系统 流逝 
#3.11 0.05 3.15 

#[[5]]
#用户 系统 流逝 
#3.86 0.07 4.02 

#[[6]]
#用户 系统 流逝 
#3.30 0.03 3.33 


tm_C = system.time(o <- crunBMLGrid(g)
tm_R = system.time(o <- runBMLGrid(g,100L))

###Different grid size 
set.seed(12323)
varGrid = lapply(c(10,50,100,500,1000,8000),
                    function(varGrid)
                      createBMLGrid(varGrid,varGrid, varGrid*varGrid*0.33, 0.33))
timingsGrid = lapply(c(1:6), function(x) system.time(o <- crunBMLGrid(varGrid[[x]])))

#> timingsGrid
#[[1]]
#用户 系统 流逝 
#0    0    0 

#[[2]]
#用户 系统 流逝 
#0.00 0.00 0.01 

#[[3]]
#用户 系统 流逝 
#0.03 0.02 0.05 

#[[4]]
#用户 系统 流逝 
#0.71 0.05 0.84 

#[[5]]
#用户 系统 流逝 
#3.10 0.09 3.19 

#[[6]]
#用户   系统   流逝 
#210.65  14.36 239.29 

##Profiling
Rprof(tmp <- tempfile())
crunBMLGrid(createBMLGrid(1000,1000,1000*0.33,0.33))
Rprof()
summaryRprof(tmp)

require(profr)
require(ggplot2)
x = profr(crunBMLGrid(createBMLGrid(1000,1000,1000*0.33,0.33)))
ggplot(x)
ggplot.profr(parse_rprof(tmp))

##revised and improve by compiling tools
testGrid = createBMLGrid(100,100,100*0.33,0.33)
library(compiler)
cruncomplier = cmpfun(crunBMLGrid)
library(microbenchmark)
compare <- microbenchmark(cruncomplier(testGrid), crunBMLGrid(testGrid), times = 100)
autoplot(compare)

##RPackage
library(devtools)
library(roxygen2)
setwd(dir)
setwd("./BML")
devtools::check()
setwd("..")
install("BML")

##Unit test
document()
