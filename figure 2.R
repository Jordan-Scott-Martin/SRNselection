#function for calculating evolvability based on context dependent IGEs
evo_fun = function(m_mu, m_delta, x, m_phi, v_mu, v_delta, v_psi, v_phi, v_x, 
                   c_mu.delta, c_mu.psi, c_mu.phi, c_psi.phi){
          v_d = v_mu + v_x * (m_delta^2 + v_delta + m_mu^2 * v_phi + v_mu * (m_phi^2 + v_phi))
          v_i = m_mu^2 * (v_psi + v_x * v_phi) + v_mu * (x^2 + v_psi + v_x * (m_phi^2 + v_phi) ) +
                2 * v_x * ( v_mu * (x * m_phi + c_psi.phi) + (m_mu^2 * c_psi.phi ))
          c_di = (v_mu + c_mu.delta) * (x + v_x * m_phi) + 
                  v_x * ( m_phi * m_mu * c_mu.psi + m_mu * x * c_mu.phi + c_mu.psi*c_mu.phi +
                            v_x * (2 * (m_mu * m_phi * c_mu.phi) + c_mu.phi^2) )
          
          e = (v_d + v_i + 2 * c_di)
          return(e)
}

m_mu = 0
m_delta_s = c(0,0,0.5)
m_phi_s = c(0,0,0.5)
v_mu = 0.3
v_psi_s = c(0.1, 0.3)
v_delta_s = v_psi_s
v_phi_s = v_psi_s
v_x = v_psi_s
cor_l = -0.3
cor_h = 0.3
c_mu.psi_l = c(cor_l, cor_h) * sqrt(v_mu*v_psi_s[1])
c_mu.psi_h = c(cor_l, cor_h) * sqrt(v_mu*v_psi_s[2])
c_mu.delta_l = c_mu.psi_l
c_mu.delta_h = c_mu.psi_h
c_mu.phi_l = c_mu.psi_l
c_mu.phi_h = c_mu.psi_h
c_psi.phi_l = c(cor_l, cor_h) * sqrt(v_psi_s[1]*v_phi_s[1])
c_psi.phi_h = c(cor_l, cor_h) * sqrt(v_psi_s[2]*v_phi_s[2])
fromx = -0.5
tox = 0.5
ymin = 0
ymax = 1.3
col=c("green","slateblue")
type=c("solid","dashed")

par(mfrow=c(2,2))

#C1, V1,1####
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[1], c_mu.psi = c_mu.psi_l[1],
              c_mu.phi = c_mu.phi_l[1], c_psi.phi = c_psi.phi_l[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[1], 
     lwd = 3, ylim = c(ymin,ymax),  xaxt="n",yaxt="n")
v_d = v_mu + v_x[1] * (m_delta_s[1]^2 + v_delta_s[1] + m_mu^2 * v_phi_s[1] + v_mu * (m_phi_s[1]^2 + v_phi_s[1]))     
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[1]) 
axis(1, at = c(fromx,0,tox), las = 1)
axis(2, at = c(0,0.4,0.8,1.2), las = 1)

v_d = v_mu + v_x[1] * (m_delta_s[3]^2 + v_delta_s[1] + m_mu^2 * v_phi_s[1] + v_mu * (m_phi_s[3]^2 + v_phi_s[1]))
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[2])
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[1], c_mu.psi = c_mu.psi_l[1],
              c_mu.phi = c_mu.phi_l[1], c_psi.phi = c_psi.phi_l[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)
 
#C2
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[2], c_mu.psi = c_mu.psi_l[2],
              c_mu.phi = c_mu.phi_l[2], c_psi.phi = c_psi.phi_l[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[1], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[2], c_mu.psi = c_mu.psi_l[2],
              c_mu.phi = c_mu.phi_l[2], c_psi.phi = c_psi.phi_l[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

#C1, V1,2####
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[1], c_mu.psi = c_mu.psi_h[1],
              c_mu.phi = c_mu.phi_h[1], c_psi.phi = c_psi.phi_h[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[1], 
     lwd = 3, ylim = c(ymin,ymax),  xaxt="n",yaxt="n")
v_d = v_mu + v_x[1] * (m_delta_s[1]^2 + v_delta_s[2] + m_mu^2 * v_phi_s[2] + v_mu * (m_phi_s[1]^2 + v_phi_s[2]))     
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[1]) 
axis(1, at = c(fromx,0,tox), las = 1)
axis(2, at = c(0,0.4,0.8,1.2), las = 1)

v_d = v_mu + v_x[1] * (m_delta_s[3]^2 + v_delta_s[2] + m_mu^2 * v_phi_s[2] + v_mu * (m_phi_s[3]^2 + v_phi_s[2]))
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[2])
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[1], c_mu.psi = c_mu.psi_h[1],
              c_mu.phi = c_mu.phi_h[1], c_psi.phi = c_psi.phi_h[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)
 
#C2
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[2], c_mu.psi = c_mu.psi_h[2],
              c_mu.phi = c_mu.phi_h[2], c_psi.phi = c_psi.phi_h[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[1], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[1], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[2], c_mu.psi = c_mu.psi_h[2],
              c_mu.phi = c_mu.phi_h[2], c_psi.phi = c_psi.phi_h[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

#C1, V2,1####
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[1], c_mu.psi = c_mu.psi_l[1],
              c_mu.phi = c_mu.phi_l[1], c_psi.phi = c_psi.phi_l[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[1], 
     lwd = 3, ylim = c(ymin,ymax),  xaxt="n",yaxt="n")
v_d = v_mu + v_x[2] * (m_delta_s[1]^2 + v_delta_s[1] + m_mu^2 * v_phi_s[1] + v_mu * (m_phi_s[1]^2 + v_phi_s[1]))     
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[1]) 
axis(1, at = c(fromx,0,tox), las = 1)
axis(2, at = c(0,0.4,0.8,1.2), las = 1)

v_d = v_mu + v_x[2] * (m_delta_s[3]^2 + v_delta_s[1] + m_mu^2 * v_phi_s[1] + v_mu * (m_phi_s[3]^2 + v_phi_s[1]))
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[2])
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[1], c_mu.psi = c_mu.psi_l[1],
              c_mu.phi = c_mu.phi_l[1], c_psi.phi = c_psi.phi_l[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)
 
#C2
curve(evo_fun(m_mu = m_mu, x, m_delta = m_delta_s[1], m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[2], c_mu.psi = c_mu.psi_l[2],
              c_mu.phi = c_mu.phi_l[2], c_psi.phi = c_psi.phi_l[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[1], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[1], v_psi = v_psi_s[1], v_phi = v_phi_s[1],
              c_mu.delta = c_mu.delta_l[2], c_mu.psi = c_mu.psi_l[2],
              c_mu.phi = c_mu.phi_l[2], c_psi.phi = c_psi.phi_l[2]),
     from = -0.5, to = 0.5, ylab = "e", col = col[2], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)


#C1, V2,2####
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[1], x, m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[1], c_mu.psi = c_mu.psi_h[1],
              c_mu.phi = c_mu.phi_h[1], c_psi.phi = c_psi.phi_h[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[1], 
     lwd = 3, ylim = c(ymin,ymax),  xaxt="n",yaxt="n")
v_d = v_mu + v_x[2] * (m_delta_s[1]^2 + v_delta_s[2] + m_mu^2 * v_phi_s[2] + v_mu * (m_phi_s[1]^2 + v_phi_s[2]))     
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[1]) 
axis(1, at = c(fromx,0,tox), las = 1)
axis(2, at = c(0,0.4,0.8,1.2), las = 1)

v_d = v_mu + v_x[2] * (m_delta_s[3]^2 + v_delta_s[2] + m_mu^2 * v_phi_s[2] + v_mu * (m_phi_s[3]^2 + v_phi_s[2]))
abline(h = v_d, col = "darkgrey", lwd = 2, lty = type[2])
curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[1], c_mu.psi = c_mu.psi_h[1],
              c_mu.phi = c_mu.phi_h[1], c_psi.phi = c_psi.phi_h[1]),
     from = fromx, to = tox, ylab = "e", col = col[1], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)
 
#C2
curve(evo_fun(m_mu = m_mu, x, m_delta = m_delta_s[1], m_phi = m_phi_s[1], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[2], c_mu.psi = c_mu.psi_h[2],
              c_mu.phi = c_mu.phi_h[2], c_psi.phi = c_psi.phi_h[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[1], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)

curve(evo_fun(m_mu = m_mu, m_delta = m_delta_s[3], x, m_phi = m_phi_s[3], 
              v_mu = v_mu, v_x = v_x[2], v_delta = v_delta_s[2], v_psi = v_psi_s[2], v_phi = v_phi_s[2],
              c_mu.delta = c_mu.delta_h[2], c_mu.psi = c_mu.psi_h[2],
              c_mu.phi = c_mu.phi_h[2], c_psi.phi = c_psi.phi_h[2]),
     from = fromx, to = tox, ylab = "e", col = col[2], lty = type[2], 
     lwd = 3, ylim = c(0,1.2),  xaxt="n",yaxt="n", add = T)
