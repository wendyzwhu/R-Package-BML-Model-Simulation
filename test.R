
######test
dyn.load("HelloB.so")
is.loaded("helloB")
HelloB <- function(){
  result <- .C("helloB",
               greeting = "")
  return(result$greeting)
}
######test


getCarLocations =
  function(g)
  {
    w = (g != "")
    i = row(g)[w]
    j = col(g)[w]
    pos = cbind(i, j)
    structure(pos, dimnames = list(g[pos], c("i", "j")))
  }


setwd('d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242/stat242assignment3')
system('R CMD SHLIB -o BML2.so BML3.c movecars.c')
dyn.load("BML2.so")
stopifnot(is.loaded("R_BML"))

###test sample grid###
g = createBMLGrid(1000,1000,c(500,500),0.5)
gi = matrix(match(g[[1]],c("red","blue"),0L),nrow(g[[1]]),ncol(g[[1]]))
location = getCarLocations(g[[1]])
red = location[ rownames(location) == "red", ]
blue = location[ rownames(location) == "blue", ]
result = .C("R_BML",
         grid = gi, matrix(as.integer(0), nrow(gi), ncol(gi)), as.integer(dim(gi)),
         red, as.integer(nrow(red)), blue, as.integer(nrow(blue)),
         1000L, FALSE, velocity = integer(2*1000))
 
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

gridInterger = function(x){
  matrix(match(x, c("red", "blue"), 0L), nrow(g), ncol(g))
  
}
crunBMLGrid =
  function(grid, numIter = 100L, check = FALSE)
  {
    k = class(grid)
    gi = gi ##error
    velocity = matrix(0L, as.integer(numIter), 2L,
                      dimnames = list(NULL, c("red", "blue")))
    pos = getCarLocations(gi)
    red = pos[rownames(pos) == "1",]
    blue = pos[rownames(pos) == "2",]
    ans = .C("R_BML", gi, grid = gi, dim(gi),
             red = red, nrow(red),
             blue = blue, nrow(blue),
             as.integer(numIter), FALSE,
             velocity = velocity)
    ans = ans[c("grid", "velocity")]
    class(ans$grid) = k
    ans
  }
crunBMLGrid(g[[1]])

##Test the system.time
g100 <- createBMLGrid(100,100, ncars = c(red = 50, blue = 50),0.5)
tm_C = system.time(o <- crunBMLGrid(g[[1]]))
tm_R = system.time(o <- runBMLGrid(g,100L))

#Make Comparison for different density 

