#code for Figure 3b example fitness curves

library(numDeriv)
library(mvtnorm)

########################################################
#selection gradients
b = 0.7
bx = 0
be = -0.1
bxe = 0
q = -0.3
qx = 0
qe = -0.3
qxe = -0.3
qx2 = 0
qe2 = -0
qxe2 = -0.1

#stochastic environments
mean_x = 0 #mean partner phenotype
mean_e = 0 #mean ecological factor
v_x = 1 #var
v_e = 1
c_xe = 0
s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
            sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))

fc = function(x, s1, s2){
    w = 1 + 
      b * x  + 
      bx * x * s1 + 
      be * x  * s2 + 
      bxe * x * s1 * s2 +
      #divide by 2 for gamma
      q * 0.5 *  x^2 +
      qx * 0.5 * x^2 * s1 +
      qe * 0.5 * x^2 * s2 + 
      qxe * 0.5 * x^2 * s1 * s2 +
      #divide by 2 for gamma
      qx2 * 0.5 * x^2 * 0.5  * s1^2
      qe2 * 0.5 * x^2  * 0.5  * s2^2
      qxe2 * 0.5 * x^2 * 0.5 * s1^2 * s2^2  
    return(w) }

#plot of laying date
  par(mfrow = c(2,2))
#x is hatchling count; y is fitness; low neighbor density (s1= 0)
curve(fc(x, s1 =  0, s2 = 0.7), from = -4, to = 4, col = "red", lwd = 3,
      xaxt = "n", yaxt = "n", xlab = "", ylab ="", ylim = c(-2,2), xaxs = "i")
abline(h = fc(x=4, s1 =  0, s2 = 0.7), lty = "dashed", col = "red")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.7, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "red")

curve(fc(x, s1 =  0, s2 = 0.5), from = -4, to = 4, col = "darkorange", add = T, lwd = 3)
abline(h = fc(x=4, s1 =  0, s2 = 0.5), lty = "dashed", col = "darkorange")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.5, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "darkorange")

curve(fc(x, s1 =  0, s2 = 0.3), from = -4, to = 4, col = "blue", lwd = 3, add = T)
abline(h = fc(x=4, s1 =  0, s2 = 0.3), lty = "dashed", col = "blue")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.3, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "blue")

#higher neighbor density (s1 = 0.5)
curve(fc(x, s1 =  0.7, s2 = 0.7), from = -4, to = 4, col = "red", lwd = 3,
      xaxt = "n", yaxt = "n", xlab = "", ylab ="", ylim = c(-2,2), xaxs = "i")
abline(h = fc(x=4, s1 =  0.7, s2 = 0.7), lty = "dashed", col = "red")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.7, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "red")

curve(fc(x, s1 =  0.7, s2 = 0.5), from = -4, to = 4, col = "darkorange", add = T, lwd = 3)
abline(h = fc(x=4, s1 =  0.7, s2 = 0.5), lty = "dashed", col = "darkorange")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.5, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "darkorange")

curve(fc(x, s1 =  0.7, s2 = 0.3), from = -4, to = 4, col = "blue", lwd = 3, add = T)
abline(h = fc(x=4, s1 =  0.7, s2 = 0.3), lty = "dashed", col = "blue")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.3, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "blue")

########################################################
#selection gradients
b = 0
bx =  0
be =  0
bxe = 0
q = -0.1
qx = 0
qe = 0.2
qxe = 0.1
qx2 = 0
qe2 = -0
qxe2 = 0.1

#stochastic environments
mean_x = 0 #mean partner phenotype
mean_e = 0 #mean ecological factor
v_x = 1 #var
v_e = 1
c_xe = 0
s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
            sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))

fc = function(x, s1, s2){
  w = 1 + 
    b * x  + 
    bx * x * s1 + 
    be * x  * s2 + 
    bxe * x * s1 * s2 +
    #divide by 2 for gamma
    q * 0.5 *  x^2 +
    qx * 0.5 * x^2 * s1 +
    qe * 0.5 * x^2 * s2 + 
    qxe * 0.5 * x^2 * s1 * s2 +
    #divide by 2 for gamma
    qx2 * 0.5 * x^2 * 0.5  * s1^2
  qe2 * 0.5 * x^2  * 0.5  * s2^2
  qxe2 * 0.5 * x^2 * 0.5 * s1^2 * s2^2  
  return(w) }

#plot of laying date
#x is hatchling count; y is fitness; low neighbor density (s1= 0)
curve(fc(x, s1 =  0, s2 = 0.7), from = -4, to = 4, col = "red", lwd = 3,
      xaxt = "n", yaxt = "n", xlab = "", ylab ="", ylim = c(-1,3), xaxs = "i")
abline(h = fc(x=4, s1 =  0, s2 = 0.7), lty = "dashed", col = "red")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.7, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "red")

curve(fc(x, s1 =  0, s2 = 0.5), from = -4, to = 4, col = "darkorange", add = T, lwd = 3)
abline(h = fc(x=4, s1 =  0, s2 = 0.5), lty = "dashed", col = "darkorange")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.5, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "darkorange")

curve(fc(x, s1 =  0, s2 = 0.3), from = -4, to = 4, col = "blue", lwd = 3, add = T)
abline(h = fc(x=4, s1 =  0, s2 = 0.3), lty = "dashed", col = "blue")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0, s2 = 0.3, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "blue")

#higher neighbor density (s1 = 0.5)
curve(fc(x, s1 =  0.7, s2 = 0.7), from = -4, to = 4, col = "red", lwd = 3,
      xaxt = "n", yaxt = "n", xlab = "", ylab ="", ylim = c(-1,3), xaxs = "i")
abline(h = fc(x=4, s1 =  0.7, s2 = 0.7), lty = "dashed", col = "red")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.7, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "red")

curve(fc(x, s1 =  0.7, s2 = 0.5), from = -4, to = 4, col = "darkorange", add = T, lwd = 3)
abline(h = fc(x=4, s1 =  0.7, s2 = 0.5), lty = "dashed", col = "darkorange")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.5, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "darkorange")

curve(fc(x, s1 =  0.7, s2 = 0.3), from = -4, to = 4, col = "blue", lwd = 3, add = T)
abline(h = fc(x=4, s1 =  0.7, s2 = 0.3), lty = "dashed", col = "blue")
opt =  optimize(fc, interval = c(-4, 4), s1 =  0.7, s2 = 0.3, maximum = TRUE)$maximum
abline(v = opt, lty = "dashed", col = "blue")
