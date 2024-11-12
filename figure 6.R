library(mvtnorm)
library(numDeriv)

#adaptive evolution of IGEs
par(mfrow=c(2,3), mar = c(2, 3, 2, 3))  

#change in IGEs functions
IGE_v = function(mean_u, mean_p, mean_h, 
                   g_u, g_p, g_h, g_ph, v_x, v_e){
    g_i = mean_u^2 * (g_p + v_x * g_h) + 
      g_u * (mean_p^2 + g_p + v_e * (mean_h^2 + g_h) ) +
      2 * v_e * (mean_u^2 * g_ph + g_u * (mean_p * mean_h + g_ph))
    return(g_i)
  }
IGE_c = function(mean_u, v_u, p_par, h_par){
    eseq = c(-2,-1,0,1,2)
    esm = list()
    for(i in 1:length(eseq)){
      es = eseq[[i]]
      s = rnorm(1e4, mean_u, v_u)
      gi = (p_par + h_par * es) * s
      esm[[i]] = mean(gi)
    }
    return(esm)
  }
    
#no ecological change
{
#plot of landscape
b = 0.6
bx = 0
be = 0
bxe = 0
q = 0.7
qx = 0
qe = 0
qxe = 0
qx2 = 0
qe2 = 0
qxe2 = 0

Wbar = function(x){
  #fitness model on character states  
  w = 1 + 
    b * x  + 
    bx * x * s1 + 
    be * x  * s2 + 
    bxe * x * s1 * s2 +
    #divide by 2 for gamma
    q * 1/2 * x^2 +
    qx * 1/2 * x^2 * s1 +
    qe * 1/2 * x^2 * s2 + 
    qxe * 1/2 * x^2 * s1 * s2 +
    #divide by 2 for gamma
    qx2 * 1/4 * x^2 *  s1^2 +
    qe2 * 1/4 * x^2  * s2^2 +
    qxe2 * 1/8 * x^2 * (s1^2  * s2^2 ) 
  
  return(mean(w)) }

#plot of fitness function
s1=s2=0
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, ylim = c(0,5), col = "seagreen3",
        lwd = 3, ylab = " ", xlab = " ", xaxt = "n", xaxs="i")
axis(1, at=c(-1, 0, 1, 2), las = 1)
s1 = 1
  bx = 0.5
  qx = 0.5
  qx2 = 0.5
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, lwd = 2, col = "lightgreen", add = T, lty = "dashed")
  s1 = -1
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, lwd = 2, col = "springgreen4", add = T, lty = "dashed")

#fitness function
resp_f = function(mean_srn,s){
  x = rep(0,4)
  Wbar = function(x){
    u = mean_srn[[1]]
    p = mean_srn[[2]]
    d = mean_srn[[3]]
    h = mean_srn[[4]]
    
    #character states from RNs  
    z = (u + x[1] + p * mean(s[,1]) + d * mean(s[,2]) + h + mean(s[,1]) * mean(s[,2]) ) + 
      (p + x[2] ) * (s[,1] - mean(s[,1])) + (d + x[3]) * (s[,2] - mean(s[,2])) +
      (h + x[4]) * (s[,1] - mean(s[,1])) * (s[,2] - mean(s[,2]))
    
    #fitness model on character states  
    w = 1 + 
      b * z  + 
      
      bx * (z - mean(z)) * s[,1] + 
      be * (z - mean(z))  * s[,2] + 
      bxe * (z - mean(z)) * s[,1] * s[,2] +
      
      #divide by 2 for gamma
      q * 1/2 * (z^2 - var(z)) +
      qx * 1/2 * (z^2 - var(z)) * s[,1] +
      qe * 1/2 * (z^2 - var(z)) * s[,2] + 
      qxe * 1/2 * (z^2 - var(z)) * s[,1] * s[,2] +
      #divide by 2 for gamma
      qx2 * 1/4 * (z^2 - var(z)) *  (s[,1]^2 - var(s[,1])) +
      qe2 * 1/4 * (z^2 - var(z))  *   (s[,2]^2 - var(s[,2]) ) +
      qxe2 * 1/8 * (z^2 - var(z)) * ((s[,1]^2 - var(s[,1])) * (s[,2]^2 - var(s[,2]))) 
    
    return(mean(w)) }
  first.derivatives = data.frame(b = grad(func = Wbar, x = x))
  second.derivatives = data.frame(hessian(func = Wbar, x = x))
  denom = Wbar(x = x)
  
  rownames(first.derivatives) = c("mu","psi","delta", "phi")
  colnames(second.derivatives) = rownames(second.derivatives) = c("mu","psi","delta", "phi")
  
  return(
    list(round(first.derivatives/denom,2),
         round(second.derivatives/denom,2)))
}

#response (freq.-dep)
b = 0.6
bx = 0
be = 0
bxe = 0
q = 0.7
qx = 0
qe = 0
qxe = 0
qx2 = 0
qe2 = 0
qxe2 = 0
bx = 0
qx2 = 0

#loop function
resgen = function(gen, m_init, v_init, v_e){
  bl = list()
  gl = list()
  ms = list()
  distm = data.frame(mean = rep(0,gen+1), sd = rep(0,gen+1))
  giv = list()
  gim = list()
  
  #initial distribution
  mean_srn = matrix(m_init, ncol = 1)
  ms[[1]] = mean_srn
  v_u = v_init[[1]]
  v_p = v_init[[2]]
  v_d = v_init[[3]]
  v_h = v_init[[4]]
  g = matrix(c(v_u,0,0,0,
               0,v_p,0,0,
               0,0,v_d,0,
               0,0,0,v_h), nrow = 4, ncol = 4)
  mean_x = mean_srn[1,]
  mean_e = 0 #mean ecological factor
  v_x = g[1,1]
  v_e = v_e
  c_xe = 0
  s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
              sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
  par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
  z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
  distm[1,] = c(mean(z), var(z))
  
  giv[[1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
              g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
  
  gim[[1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
  
  #gen 1
  resp1 = resp_f(mean_srn, s)
  bl[[1]] = resp1[[1]]
  gl[[1]] = resp1[[2]]
  mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
  ms[[2]] = mean_srn
  g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                              as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
  mean_x = mean_srn[1,]
  mean_e = 0
  v_x = g[1,1]
  v_e = v_e
  c_xe = 0
  s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
              sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
  par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
  z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
  distm[2,] = c(mean(z), var(z))
  
  giv[[2]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                   g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
  
  gim[[2]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
  
  
  
  if(gen > 1){
    for(i in 2:gen){
      mean_x = mean_srn[1,] #mean partner phenotype
      mean_e = 0 #mean ecological factor
      v_x = g[1,1] #var
      v_e = v_e
      c_xe = 0
      s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      resp1 = resp_f(mean_srn, s)
      bl[[i]] = resp1[[1]]
      gl[[i]] = resp1[[2]]
      mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
      ms[[i+1]] = mean_srn
      g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                  as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
      
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[i + 1,] = c(mean(z), var(z))
      
      giv[[i+1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                       g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
      
      gim[[i+1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
    }
  }

  return(list(bl, gl, ms, distm, giv, gim))
}

#mean change plot (different variances)
v_par = c(0.3, 0.5)
for(i in 1:length(v_par)){

  bx = 0.5
  qx = 0.5
  qx2 = 0.5
  fdr2 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[i]],v_par[[i]],0,v_par[[i]]), v_e = v_par[[i]])
  
  bx = 0
  qx = 0
  qx2 = 0
  nfdr = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[i]],v_par[[i]],0,v_par[[i]]), v_e = v_par[[i]])

#plot mean change
if(i == 1){plot(NA, main = " ", ylab = " ", xlim = c(1,6), ylim = c(0,3.2))
  points(fdr2[[4]][,1], cex = 1, pch = 21, bg = "seagreen3", col = "seagreen3")
  lines(fdr2[[4]][,1], lwd = 2, pch = 21, col = "seagreen3", lty = "solid")
  }
else{plot(NA, main = " ", ylab = " ", xlim = c(1,6), ylim = c(0,3.2))
  points(fdr2[[4]][,1], cex = 1, pch = 21, bg = "seagreen3", col = "seagreen3")
  lines(fdr2[[4]][,1], lwd = 2, pch = 21, col = "seagreen3", lty = "solid")}
points(nfdr[[4]][,1], cex = 1, pch = 21, bg = "black")
lines(nfdr[[4]][,1], lwd = 2, pch = 21, col = "black", lty = "solid")
}

}

#ecological change
{  
#plot of landscape
b = 0.6
bx = 0
be = 0
bxe = 0
q = 0.7
qx = 0
qe = 0
qxe = 0
qx2 = 0
qe2 = 0
qxe2 = 0

Wbar = function(x){
  #fitness model on character states  
  w = 1 + 
    b * x  + 
    bx * x * s1 + 
    be * x  * s2 + 
    bxe * x * s1 * s2 +
    
    q * 1/2 * x^2 +
    qx * 1/2 * x^2 * s1 +
    qe * 1/2 * x^2 * s2 + 
    qxe * 1/2 * x^2 * s1 * s2 +
    
    qx2 * 1/4 * x^2 *  s1^2 +
    qe2 * 1/4 * x^2  * s2^2 +
    qxe2 * 1/8 * x^2 * (s1^2  * s2^2 ) 
  return(mean(w)) }

#plot of fitness function
s1=s2=0
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, ylim = c(0,5), col = "seagreen3",
      lwd = 3, ylab = " ", xlab = " ", xaxt = "n", xaxs = "i")
axis(1, at=c(-1, 0, 1, 2), las = 1)
s1 = 1; s2 = 1
bx = 0.5
bxe = -0.4
qx = 0.5
qxe = -0.4
qx2 = 0.5
qxe2 = -0.4
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, lwd = 2, col = "lightgreen", add = T, lty = "dashed")
s1 = -1; s2 = 1
curve(Vectorize(Wbar)(x), from = -1, to = 2.5, lwd = 2, col = "springgreen4", add = T, lty = "dashed")

#fitness function
resp_f = function(mean_srn,s){
  x = rep(0,4)
  Wbar = function(x){
    u = mean_srn[[1]]
    p = mean_srn[[2]]
    d = mean_srn[[3]]
    h = mean_srn[[4]]
    
    #character states from RNs  
    z = (u + x[1] + p * mean(s[,1]) + d * mean(s[,2]) + h + mean(s[,1]) * mean(s[,2]) ) + 
      (p + x[2] ) * (s[,1] - mean(s[,1])) + (d + x[3]) * (s[,2] - mean(s[,2])) +
      (h + x[4]) * (s[,1] - mean(s[,1])) * (s[,2] - mean(s[,2]))
    
    #fitness model on character states  
    w = 1 + 
      b * z  + 
      
      bx * (z - mean(z)) * s[,1] + 
      be * (z - mean(z))  * s[,2] + 
      bxe * (z - mean(z)) * s[,1] * s[,2] +
      
      q * 1/2 * (z^2 - var(z)) +
      qx * 1/2 * (z^2 - var(z)) * s[,1] +
      qe * 1/2 * (z^2 - var(z)) * s[,2] + 
      qxe * 1/2 * (z^2 - var(z)) * s[,1] * s[,2] +

      qx2 * 1/4 * (z^2 - var(z)) *  (s[,1]^2 - var(s[,1])) +
      qe2 * 1/4 * (z^2 - var(z))  *   (s[,2]^2 - var(s[,2]) ) +
      qxe2 * 1/8 * (z^2 - var(z)) * ((s[,1]^2 - var(s[,1])) * (s[,2]^2 - var(s[,2]))) 
    
    
    return(mean(w)) }
  first.derivatives = data.frame(b = grad(func = Wbar, x = x))
  second.derivatives = data.frame(hessian(func = Wbar, x = x))
  denom = Wbar(x = x)
  
  rownames(first.derivatives) = c("mu","psi","delta", "phi")
  colnames(second.derivatives) = rownames(second.derivatives) = c("mu","psi","delta", "phi")
  
  return(
    list(round(first.derivatives/denom,2),
         round(second.derivatives/denom,2)))
}

#response (freq.-dep)
b = 0.6
bx = 0
be = 0
bxe = 0
q = 0.7
qx = 0
qe = 0
qxe = 0
qx2 = 0
qe2 = 0
qxe2 = 0
bx = 0
qx2 = 0

#loop function
eco_gen = 0.25
resgen = function(gen, m_init, v_init, v_e){
  bl = list()
  gl = list()
  ms = list()
  distm = data.frame(mean = rep(0,gen+1), sd = rep(0,gen+1))
  
  #initial distribution
  mean_srn = matrix(m_init, ncol = 1)
  ms[[1]] = mean_srn
  v_u = v_init[[1]]
  v_p = v_init[[2]]
  v_d = v_init[[3]]
  v_h = v_init[[4]]
  g = matrix(c(v_u,0,0,0,
               0,v_p,0,0,
               0,0,v_d,0,
               0,0,0,v_h), nrow = 4, ncol = 4)
  mean_x = mean_srn[1,]
  mean_e = 0 #mean ecological factor
  v_x = g[1,1]
  v_e = v_e
  c_xe = 0
  s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
              sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
  par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
  z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
  distm[1,] = c(mean(z), var(z))
  
  #gen 1
  resp1 = resp_f(mean_srn, s)
  bl[[1]] = resp1[[1]]
  gl[[1]] = resp1[[2]]
  mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
  ms[[2]] = mean_srn
  g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                              as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
  mean_x = mean_srn[1,]
  mean_e = mean_e + eco_gen
  v_x = g[1,1]
  v_e = v_e
  c_xe = 0
  s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
              sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
  par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
  z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
  distm[2,] = c(mean(z), var(z))
  
  if(gen > 1){
    for(i in 2:gen){
      mean_x = mean_srn[1,] #mean partner phenotype
      mean_e = mean_e + eco_gen #mean ecological factor
      v_x = g[1,1] #var
      v_e = v_e
      c_xe = 0
      s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      resp1 = resp_f(mean_srn, s)
      bl[[i]] = resp1[[1]]
      gl[[i]] = resp1[[2]]
      mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
      ms[[i+1]] = mean_srn
      g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                  as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
      
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[i + 1,] = c(mean(z), var(z))
    }
  }
  
  return(list(bl, gl, ms, distm))
}

#mean change plot (different variances)
v_par = c(0.3, 0.5)
for(i in 1:length(v_par)){

  bx = 0.5
  bxe = -0.4
  qx = 0.5
  qxe = -0.4
  qx2 = 0.5
  qxe2 = -0.4
  fdr2 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[i]],v_par[[i]],0,v_par[[i]]), v_e = v_par[[i]])
  
  bx = 0
  bxe = 0
  qx = 0
  qxe = 0
  qx2 = 0
  qxe2 = 0
  nfdr = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[i]],v_par[[i]],0,v_par[[i]]), v_e = v_par[[i]])
  
  #plot mean change
  if(i == 1){plot(NA, main = " ", ylab = " ", xlim = c(1,6), ylim = c(0,3.3))
    points(fdr2[[4]][,1], cex = 1, pch = 21, bg = "seagreen3", col = "seagreen3")
    lines(fdr2[[4]][,1], lwd = 2, pch = 21, col = "seagreen3", lty = "solid")
    }
  else{plot(NA, main = " ", ylab = " ", xlim = c(1,6), ylim = c(0,3.3))
       points(fdr2[[4]][,1], cex = 1, pch = 21, bg = "seagreen3", col = "seagreen3")
       lines(fdr2[[4]][,1], lwd = 2, pch = 21, col = "seagreen3", lty = "solid")
       }

  points(nfdr[[4]][,1], cex = 1, pch = 21, bg = "black")
  lines(nfdr[[4]][,1], lwd = 2, pch = 21, col = "black", lty = "solid")
  
}

}

##############################################
#IGEs
par(mfrow=c(2,6), mar = c(3, 2, 3, 1)) 
  
#no ecological change
{
    #fitness function
    resp_f = function(mean_srn,s){
      x = rep(0,4)
      Wbar = function(x){
        u = mean_srn[[1]]
        p = mean_srn[[2]]
        d = mean_srn[[3]]
        h = mean_srn[[4]]
        
        #character states from RNs  
        #character states from RNs  
        z = (u + x[1] + p * mean(s[,1]) + d * mean(s[,2]) + h + mean(s[,1]) * mean(s[,2]) ) + 
          (p + x[2] ) * (s[,1] - mean(s[,1])) + (d + x[3]) * (s[,2] - mean(s[,2])) +
          (h + x[4]) * (s[,1] - mean(s[,1])) * (s[,2] - mean(s[,2]))
        
        #fitness model on character states  
        w = 1 + 
          b * z  + 
          
          bx * (z - mean(z)) * s[,1] + 
          be * (z - mean(z))  * s[,2] + 
          bxe * (z - mean(z)) * s[,1] * s[,2] +
          
          q * 1/2 * (z^2 - var(z)) +
          qx * 1/2 * (z^2 - var(z)) * s[,1] +
          qe * 1/2 * (z^2 - var(z)) * s[,2] + 
          qxe * 1/2 * (z^2 - var(z)) * s[,1] * s[,2] +
          
          qx2 * 1/4 * (z^2 - var(z)) *  (s[,1]^2 - var(s[,1])) +
          qe2 * 1/4 * (z^2 - var(z))  *   (s[,2]^2 - var(s[,2]) ) +
          qxe2 * 1/8 * (z^2 - var(z)) * ((s[,1]^2 - var(s[,1])) * (s[,2]^2 - var(s[,2]))) 
        
        return(mean(w)) }
      first.derivatives = data.frame(b = grad(func = Wbar, x = x))
      second.derivatives = data.frame(hessian(func = Wbar, x = x))
      denom = Wbar(x = x)
      
      rownames(first.derivatives) = c("mu","psi","beta", "phi")
      colnames(second.derivatives) = rownames(second.derivatives) = c("mu","psi","beta", "phi")
      
      return(
        list(round(first.derivatives/denom,2),
             round(second.derivatives/denom,2)))
    }
    
    #response (freq.-dep)
    b = 0.6
    bx = 0
    be = 0
    bxe = 0
    q = 0.7
    qx = 0
    qe = 0
    qxe = 0
    qx2 = 0
    qe2 = 0
    qxe2 = 0
    bx = 0
    qx2 = 0
    
    #loop function
    resgen = function(gen, m_init, v_init, v_e){
      bl = list()
      gl = list()
      ms = list()
      distm = data.frame(mean = rep(0,gen+1), sd = rep(0,gen+1))
      giv = list()
      gim = list()
      
      #initial distribution
      mean_srn = matrix(m_init, ncol = 1)
      ms[[1]] = mean_srn
      v_u = v_init[[1]]
      v_p = v_init[[2]]
      v_d = v_init[[3]]
      v_h = v_init[[4]]
      g = matrix(c(v_u,0,0,0,
                   0,v_p,0,0,
                   0,0,v_d,0,
                   0,0,0,v_h), nrow = 4, ncol = 4)
      mean_x = mean_srn[1,]
      mean_e = 0 #mean ecological factor
      v_x = g[1,1]
      v_e = v_e
      c_xe = 0
      s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[1,] = c(mean(z), var(z))
      
      giv[[1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                       g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
      
      gim[[1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
      
      #gen 1
      resp1 = resp_f(mean_srn, s)
      bl[[1]] = resp1[[1]]
      gl[[1]] = resp1[[2]]
      mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
      ms[[2]] = mean_srn
      g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                  as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
      mean_x = mean_srn[1,]
      mean_e = 0
      v_x = g[1,1]
      v_e = v_e
      c_xe = 0
      s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[2,] = c(mean(z), var(z))
      
      giv[[2]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                       g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
      
      gim[[2]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
      
      
      
      if(gen > 1){
        for(i in 2:gen){
          mean_x = mean_srn[1,] #mean partner phenotype
          mean_e = 0 #mean ecological factor
          v_x = g[1,1] #var
          v_e = v_e
          c_xe = 0
          s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
                      sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
          resp1 = resp_f(mean_srn, s)
          bl[[i]] = resp1[[1]]
          gl[[i]] = resp1[[2]]
          mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
          ms[[i+1]] = mean_srn
          g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                      as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
          
          par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
          z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
          distm[i + 1,] = c(mean(z), var(z))
          
          giv[[i+1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                             g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
          
          gim[[i+1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
        }
      }
      
      return(list(bl, gl, ms, distm, giv, gim))
    }
    
    #mean change plot (different variances)
    v_par = c(0.3, 0.5)
    bx = 0.5
    qx = 0.5
    qx2 = 0.5
    fdr1 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[1]],v_par[[1]],0,v_par[[1]]), v_e = v_par[[1]])
    fdr2 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[2]],v_par[[2]],0,v_par[[2]]), v_e = v_par[[2]])
    
  plot(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]),  main = " ", ylab = " ", xlim = c(-2,2), ylim = c(0,1))
  points(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]), lwd = 2, pch = 21, bg = "seagreen3", col = "seagreen3", lty = "solid")
  lines(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]), lwd = 2, pch = 21, col = "seagreen3", lty = "solid")

  for(d in 2:(gen)){
    plot(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]),  main = " ", ylab = " ", xlim = c(-2,2), ylim = c(0,1))
    points(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]), lwd = 2, pch = 21, bg = "seagreen3", col = "seagreen3", lty = "solid")
    lines(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]), lwd = 2, pch = 21, col = "seagreen3", lty = "solid")
 }
  }  

#ecological change
{
    #fitness function
    resp_f = function(mean_srn,s){
      x = rep(0,4)
      Wbar = function(x){
        u = mean_srn[[1]]
        p = mean_srn[[2]]
        d = mean_srn[[3]]
        h = mean_srn[[4]]
        
        #character states from RNs  
        z = (u + x[1] + p * mean(s[,1]) + d * mean(s[,2]) + h + mean(s[,1]) * mean(s[,2]) ) + 
          (p + x[2] ) * (s[,1] - mean(s[,1])) + (d + x[3]) * (s[,2] - mean(s[,2])) +
          (h + x[4]) * (s[,1] - mean(s[,1])) * (s[,2] - mean(s[,2]))
        
        #fitness model on character states  
        w = 1 + 
          b * z  + 
          
          bx * (z - mean(z)) * s[,1] + 
          be * (z - mean(z))  * s[,2] + 
          bxe * (z - mean(z)) * s[,1] * s[,2] +
          
          #divide by 2 for gamma
          q * 0.5 * (z^2 - var(z)) +
          qx * 0.5 * (z^2 - var(z)) * s[,1] +
          qe * 0.5 * (z^2 - var(z)) * s[,2] + 
          qxe * 0.5 * (z^2 - var(z)) * s[,1] * s[,2] +

          qx2 * 1/4 * (z^2 - var(z)) *  (s[,1]^2 - var(s[,1])) +
          qe2 * 1/4 * (z^2 - var(z))  *   (s[,2]^2 - var(s[,2]) ) +
          qxe2 * 1/8 * (z^2 - var(z)) * ((s[,1]^2 - var(s[,1])) * (s[,2]^2 - var(s[,2]))) 
        
        
        return(mean(w)) }
      first.derivatives = data.frame(b = grad(func = Wbar, x = x))
      second.derivatives = data.frame(hessian(func = Wbar, x = x))
      denom = Wbar(x = x)
      
      rownames(first.derivatives) = c("mu","psi","delta", "phi")
      colnames(second.derivatives) = rownames(second.derivatives) = c("mu","psi","delta", "phi")
      
      return(
        list(round(first.derivatives/denom,2),
             round(second.derivatives/denom,2)))
    }
    
    #response (freq.-dep)
    b = 0.6
    bx = 0
    be = 0
    bxe = 0
    q = 0.7
    qx = 0
    qe = 0
    qxe = 0
    qx2 = 0
    qe2 = 0
    qxe2 = 0
    bx = 0
    qx2 = 0
    
    #loop function
    eco_gen = 0.25
    resgen = function(gen, m_init, v_init, v_e){
      bl = list()
      gl = list()
      ms = list()
      distm = data.frame(mean = rep(0,gen+1), sd = rep(0,gen+1))
      giv = list()
      gim = list()
      
      #initial distribution
      mean_srn = matrix(m_init, ncol = 1)
      ms[[1]] = mean_srn
      v_u = v_init[[1]]
      v_p = v_init[[2]]
      v_d = v_init[[3]]
      v_h = v_init[[4]]
      g = matrix(c(v_u,0,0,0,
                   0,v_p,0,0,
                   0,0,v_d,0,
                   0,0,0,v_h), nrow = 4, ncol = 4)
      mean_x = mean_srn[1,]
      mean_e = 0 #mean ecological factor
      v_x = g[1,1]
      v_e = v_e
      c_xe = 0
      s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[1,] = c(mean(z), var(z))
      
      giv[[1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                       g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
      
      gim[[1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
      
      #gen 1
      resp1 = resp_f(mean_srn, s)
      bl[[1]] = resp1[[1]]
      gl[[1]] = resp1[[2]]
      mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
      ms[[2]] = mean_srn
      g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                  as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
      mean_x = mean_srn[1,]
      mean_e = 0 + eco_gen
      v_x = g[1,1]
      v_e = v_e
      c_xe = 0
      s = rmvnorm(2e5, mean = c(mean_x, mean_e), 
                  sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
      par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
      z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
      distm[2,] = c(mean(z), var(z))
      
      giv[[2]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                       g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
      
      gim[[2]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
      
      
      
      if(gen > 1){
        for(i in 2:gen){
          mean_x = mean_srn[1,] #mean partner phenotype
          mean_e = 0 #mean ecological factor
          v_x = g[1,1] #var
          v_e = v_e + eco_gen
          c_xe = 0
          s = rmvnorm(1e5, mean = c(mean_x, mean_e), 
                      sigma = matrix(c(v_x, c_xe, c_xe, v_e), nrow = 2, ncol= 2))
          resp1 = resp_f(mean_srn, s)
          bl[[i]] = resp1[[1]]
          gl[[i]] = resp1[[2]]
          mean_srn = mean_srn + as.matrix(g) %*% as.matrix(resp1[[1]]) 
          ms[[i+1]] = mean_srn
          g = g + as.matrix(g) %*% (as.matrix(resp1[[2]]) - 
                                      as.matrix(resp1[[1]]) %*% t(as.matrix(resp1[[1]]))) %*% as.matrix(g)
          
          par = rmvnorm(2e5, mean = t(mean_srn), sigma = as.matrix(g))
          z = par[,1] + par[,2] * s[,1] + par[,3] * s[,2] + par[,4] * s[,1] * s[,2]
          distm[i + 1,] = c(mean(z), var(z))
          
          giv[[i+1]] = IGE_v(mean_u = mean_srn[[1]], mean_p = mean_srn[[2]], mean_h = mean_srn[[4]],
                             g_u = g[1,1], g_p = g[2,2], g_h = g[4,4], g_ph = g[2,4], v_x = v_x, v_e = v_e)
          
          gim[[i+1]] = IGE_c(mean_u = mean_srn[[1]], v_u = g[1,1], p_par = par[,2], h_par = par[,4])
        }
      }
      
      return(list(bl, gl, ms, distm, giv, gim))
    }
    
    #mean change plot (different variances)
    v_par = c(0.3, 0.5)
    bx = 0.5
    bxe = -0.4
    qx = 0.5
    qxe = -0.4
    qx2 = 0.5
    qxe2 = -0.4
    fdr1 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[1]],v_par[[1]],0,v_par[[1]]), v_e = v_par[[1]])
    fdr2 = resgen(gen = 6, m_init = rep(0,4), v_init = c(v_par[[2]],v_par[[2]],0,v_par[[2]]), v_e = v_par[[2]])
    
    plot(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]),  main = " ", ylab = " ", xlim = c(-2,2), ylim = c(0,1))
    points(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]), lwd = 2, pch = 21, bg = "seagreen3", col = "seagreen3", lty = "solid")
    lines(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[1]]), lwd = 2, pch = 21, col = "seagreen3", lty = "solid")

    for(d in 2:(gen)){
      plot(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]),  main = " ", ylab = " ", xlim = c(-2,2), ylim = c(0,1))
      points(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]), lwd = 2, pch = 21, bg = "seagreen3", col = "seagreen3", lty = "solid")
      lines(x = c(-2,-1,0,1,2), y = unlist(fdr1[[6]][[d]]), lwd = 2, pch = 21, col = "seagreen3", lty = "solid")
  }
  }

