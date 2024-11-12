#effects of social plasticity on directional selection
db_x = 0
u = p = d = h = 0
v_u = 1
v_p = 1
v_d = 0
v_h = 0
v_s = 1
v_x = 0

#assume no genetic correlations
resp_f = function(x, db_s, db_sx){
  du = x * v_u
  dp = db_s * v_s * v_p
  dd = db_x * v_x * v_d
  dh = db_sx * v_s * v_x * v_h
  
  n1 = u + p * u + d * x + h * u * x
  n2 = (u + du) + (p + dp) * (u + du) + (d + dd) * x + (h + dh) * (u + du) * x
  adp = n2-n1
  return(adp)
}

#genetic correlations
resp_fg = function(x, db_s, g_up){
  du = x * v_u + g_up * db_s
  dp = db_s * v_s * v_p + g_up * du
  dd = 0
  dh = 0
  
  #n1 = u + p * u + d * x + h * u * x
  #n2 = (u + du) + (p + dp) * (u + du) + (d + dd) * x + (h + dh) * (u + du) * x
  #adp = n2-n1
  #return(adp)
  return(du + p * du + dp * u + dp * du)
}

par(mfrow=c(1,3), mar = c(2, 2.2, 2, 2))
curve(resp_fg(x, db_s = -0.3, g_up = -0.3), from = -0.3, to = 0.3, lwd = 3, col = "red", ylim = c(-0.55, 0.55), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(resp_fg(x, db_s = 0, g_up = -0.3), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.55, 0.55), add = T, lty = "dotted")
curve(resp_fg(x, db_s = 0.3, g_up = -0.3), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.55, 0.55), add = T)
  
curve(resp_f(x, db_s = -0.3, db_sx = 0), from = -0.3, to = 0.3, lwd = 3, col = "red", ylim = c(-0.55, 0.55), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(resp_f(x, db_s = 0, db_sx = 0), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.55, 0.55), add = T, lty = "dotted")
curve(resp_f(x, db_s = 0.3, db_sx = 0), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.55, 0.55), add = T)
  
curve(resp_fg(x, db_s = -0.3, g_up = +0.3), from = -0.3, to = 0.3, lwd = 3, col = "red", ylim = c(-0.55, 0.55), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(resp_fg(x, db_s = 0, g_up = +0.3), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.55, 0.55), add = T, lty = "dotted")
curve(resp_fg(x, db_s = 0.3, g_up = +0.3), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.55, 0.55), add = T)

#effects of social plasticity on quadratic selection
db_x = 0
u = p = d = h = 0
v_u = 1
v_p = 1
v_d = 0
v_h = 0
v_s = 1
v_x = 0

#genetic correlations
resp_fg = function(x, db_s, db_s2, g_up){
  G = matrix( c(v_u, g_up, g_up, v_p) , nrow = 2, ncol = 2)
  gp = matrix( c(x, db_s * v_s, db_s * v_s, db_s2 * v_s^2) , nrow = 2, ncol = 2)
  
  dG = G %*% gp %*% G
  dvu = dG[1,1]
  dvp = dG[2,2]
  vpu_1 = v_p * v_u
  vpu_2 = (v_p + dvp) * (v_u + dvu)
  dvpu = vpu_2 - vpu_1
  
  n1 = v_u + vpu_1 
  n2 = (v_u + dvu) + (vpu_1 + dvpu) 
  adp = n2-n1
  return(adp / 2)
}
ff = Vectorize(resp_fg)

par(mfrow=c(1,3), mar = c(2, 2.2, 2, 2))
curve(ff(x, db_s = -0.15, db_s2 = -0.15, g_up = -0.3), from = -0.3, to = 0.3, 
      lwd = 3, col = "red", ylim = c(-0.6, 0.6), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(ff(x, db_s = 0, db_s2 = 0, g_up = -0.3), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.5, 0.5), add = T, lty = "dotted")
curve(ff(x, db_s = 0.15, db_s2 = 0.15, g_up = -0.3), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.5, 0.5), add = T)
  
curve(ff(x, db_s = -0.15, db_s2 = -0.15, g_up = 0), from = -0.3, to = 0.3, 
      lwd = 3, col = "red", ylim = c(-0.6, 0.6), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(ff(x, db_s = 0, db_s2 = 0, g_up = 0), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.7, 1), add = T, lty = "dotted")
curve(ff(x, db_s = 0.15, db_s2 = 0.15, g_up = 0), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.5, 0.6), add = T)

curve(ff(x, db_s = -0.15, db_s2 = -0.15, g_up = 0.3), from = -0.3, to = 0.3, 
      lwd = 3, col = "red", ylim = c(-0.6, 0.6), yaxt = "n")
axis(2, at=c(-0.4, -0.2, 0, 0.2, 0.4), las = 2)
curve(ff(x, db_s = 0, db_s2 = 0, g_up = 0.3), from = -0.3, to = 0.3, lwd = 3, ylim = c(-0.5, 0.6), add = T, lty = "dotted")
curve(ff(x, db_s = 0.15, db_s2 = 0.15, g_up = 0.3), from = -0.3, to = 0.3, lwd = 3, col = "blue", ylim = c(-0.5, 0.6), add = T)
