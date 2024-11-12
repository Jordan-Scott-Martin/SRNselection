#numeric methods can be used to replicate relationships 
#between selection gradients on character states and SRNs
#note that gradients are estimated with stochastic error
#see Table S1 for corresponding analytic results

library(numDeriv)
library(mvtnorm)

#selection gradients
#change values from zero to see how terms influence
#corresponding selection gradients on SRN parameters
b = 0 #directional
bu = 0
bx = 0
bux = 0
q = 0 #quadratic
qu = 0
qx = 0
qux = 0
quux = 0
quxx = 0
quu = 0
qxx = 0
quuxx = 0

#stochastic environments
mean_u = 0 #social state
mean_x = 0 #mean ecological state
v_u = 1 #change variances to see how fluctuations
v_x = 1 #corresponding selection gradients on SRN parameters
c_ux = 0
e = rmvnorm(1e4, mean = c(mean_u, mean_x), 
            sigma = matrix(c(v_u, c_ux, c_ux, v_x), nrow = 2, ncol= 2))

Wbar = function(x){
  
  #character states from RNs  
  z = x[1] + x[2] * e[,1] + x[3] * e[,2] + x[4] * e[,1] *e[,2]
  
  #fitness model on character states  
  w = 1 + 
        b * z  + 
        bu * z * e[,1] + 
        bx * z  * e[,2] + 
        bux * z * e[,1] * e[,2] +
        
        q * 1/2 * (z^2 - var(z)) +
        qu * 1/2 * (z^2 - var(z)) * e[,1] +
        qx * 1/2 * (z^2 - var(z)) * e[,2] + 
        qux * 1/2 * (z^2 - var(z)) * (e[,1] * e[,2]) +
       
        quux * 1/4 * (z^2 - var(z)) * ((e[,1]^2 - var(e[,1]))) * e[,2] + 
        quxx * 1/4 * (z^2 - var(z)) * ((e[,2]^2 - var(e[,2]))) * e[,1] +  
        quu * 1/4 * (z^2 - var(z)) *  (e[,1]^2 - var(e[,1])) +
        qxx * 1/4 * (z^2 - var(z))  *   (e[,2]^2 - var(e[,2]) ) +
        quuxx * 1/8 * (z^2 - var(z)) * ((e[,1]^2 - var(e[,1])) * (e[,2]^2 - var(e[,2]))) 
    
  return(mean(w)) }

#calculate selection gradients on SRNs
x = c(0, 0, 0, 0)
first.derivatives = data.frame(b = grad(func = Wbar, x = x))
second.derivatives = data.frame(hessian(func = Wbar, x = x))
denom = Wbar(x = x)

rownames(first.derivatives) = c("mu","psi","delta", "phi")
colnames(second.derivatives) = rownames(second.derivatives) = c("mu","psi","delta", "phi")

round(first.derivatives/denom,2) #betas
(round(second.derivatives/denom,2)) #gammas
