#1)
#v <- c(x, y)
#v_dot <- Av
A<- matrix(c(.03, .02, .01, .04), 2); A
#initial balance
v0 <- c(2, .5)
P <- eigen(A)$vectors; P
PInv <- solve(P);
eigen(A)$values

Aexp <- function(t) P%*%diag(c(exp(.05*t),exp(.02*t)))%*%PInv

plot(NULL, xlim = c(0,5),ylim = c(0,5), xlab = "", ylab = "", axes = FALSE, asp = 1, pch = 20)
axis(1, pos = 0); axis(2, pos = 0)
mtext(paste("x.dot  =", S[1,1],"x + ", S[1,2], "y"),1,1)
mtext(paste("y.dot  =", S[2,1],"x + ", S[2,2], "y"),1,2)

#Choose an initial value for the vector v

#Choose a sequence of times at which to plot the solution
times <- seq(from = 0, to =10, by = 1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20 )

plotvec = function(p) {
  velocity <- S%*%p
  arrows(p[1],p[2],p[1]+ 0.1*velocity[1], p[2]+ 0.1*velocity[2], length = 0.1)
}

#We can draw a map of the vector field.
#Every vector is tangent to one of the solution curves.
for (x in -5:5)
  for (y in -3:3)
    plotvec(c(x,y))
