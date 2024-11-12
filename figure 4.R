#this code is only for plotting relationships
#see 'numeric methods.R' code to reproduce results

par(mfrow=c(3,4), mar = c(2, 2, 2, 2))

#beta
{
  #b_mu
  v_x = 0.7
  curve(return(x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  
  #b_psi
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #b_delta
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #b_phi
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
}

#diag quad
{
  #q_mu
  v_x = 0.7
  curve(return(x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #q_psi
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x*v_x^3), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x*v_x^3), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #q_delta
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(x*v_x^3), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(x*v_x^3), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #q_phi
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#e480b5", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#80e4af", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#f790e4", lwd = 3)
  curve(return(2*x*v_x^4), from = -0.5, to = 0.5, add = T, col = "#af51e4", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#e480b5", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#80e4af", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#f790e4", lty = "dashed")
  curve(return(2*x*v_x^4), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#af51e4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
}

#offdiag quad
{
  #empty spot
  plot(NA, xlim = c(-0.5, 0.5), xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)

  #q * mu
  v_x = 0.7
  curve(return(x*v_x), from = -0.5, to = 0.5, lwd = 3, col = "#809fe4", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#a380e3", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#a34875", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#eebf6c", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#6ceee6", lwd = 3)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col = "#b486f8", lwd = 3)
  v_x = 1.3
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#809fe4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#a380e3", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#a34875", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#eebf6c", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#6ceee6", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#b486f8", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #q * x
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#809fe4", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, col = "#d9ccf1", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#a34875", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#eebf6c", lwd = 3)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col = "#6ceee6", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#b486f8", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#809fe4", lty = "dashed")
  curve(return(x*v_x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#d9ccf1", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#a34875", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#eebf6c", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#6ceee6", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#b486f8", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
  
  #q * mu * x
  v_x = 0.7
  curve(return(x-x), from = -0.5, to = 0.5, lwd = 3, col = "#809fe4", xlab = "", ylab ="", 
        ylim = c(-1,1), xaxt='n', yaxt='n')
  axis(side = 1, at=c(-0.5,0,0.5))
  axis(side = 2, at=c(-1,0,1), las=1)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#d9ccf1", lwd = 3)
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, col =  adjustcolor("#a34875",alpha.f = 1), lwd = 3)
  curve(return(x*v_x^2+0.05), from = -0.5, to = 0.5, add = T, col =  adjustcolor("#eebf6c", alpha.f = 1), lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#6ceee6", lwd = 3)
  curve(return(x-x), from = -0.5, to = 0.5, add = T, col = "#b486f8", lwd = 3)
  v_x = 1.3
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#809fe4", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#d9ccf1", lty = "dashed")
  curve(return(x*v_x^2), from = -0.5, to = 0.5, add = T, lwd = 3, col =  adjustcolor("#a34875",alpha.f = 1), lty = "dashed")
  curve(return(x*v_x^2+0.05), from = -0.5, to = 0.5, add = T, lwd = 3, col =  adjustcolor("#eebf6c", alpha.f = 1), lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#6ceee6", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "#b486f8", lty = "dashed")
  curve(return(x-x), from = -0.5, to = 0.5, add = T, lwd = 3, col = "black")
}