#SRN showing socioecological plasticity
#x: partner trait value; y: focal trait value
#p: social plasticity; dp: socioecological plasticity
#two plots: low and high density
library(ggplot2)
library(cowplot)

##################################################################
#low density

fy = function(int,x, p, dp) {
        y = int + (p + dp) * x
        return(y)}
fyr = function(int,p, dp, sd){
  sq = fy(int,seq(0,1,0.1),p,dp)
  yr = rnorm(length(sq), mean = sq, sd = sd)
  return(yr)}

int = 0 + c(0.1, 0.2, 0.3)
p = 0.3
dp = c(-0.2,0,0.2)
sds = 0.1

dfl = data.frame(id = rep(1:3, each =11), 
                 y = NA, yr = NA)
idy = list()
idyr = list()
for(i in 1:length(dp)){
idy[[i]] = fy(int[[i]],seq(0,1,0.1),p, dp[[i]])
idyr[[i]] = fyr(int[[i]],p, dp[[i]], sds)  
}

dfl = data.frame(id = rep(1:3, each =11), x = seq(0,1,0.1),
                 y = unlist(idy), yr = unlist(idyr))

col = c("pink","purple","purple4")

p1 =
ggplot(dfl, aes(x = x, y = yr))+
  scale_x_continuous(expand = c(0.01,0))+
  geom_point(aes(color = as.factor(id), alpha = 0.25))+
  scale_color_manual(values = col)+
  scale_y_continuous(limits = c(0,1))+
  geom_abline(intercept = int, slope = p + dp, color = col, lwd = 1.5)+
  theme(
    panel.border=element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
    strip.background = element_blank(),
    panel.background= element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank())+
  guides(color = "none", fill = "none", alpha = "none")


##################################################################
#high density

int = 0.2 + c(0.1, 0.2, 0.3)
p = 0.3
dp = c(0.1,-0.15,-0.4)
sds = 0.05

dfl = data.frame(id = rep(1:3, each =11), 
                 y = NA, yr = NA)
idy = list()
idyr = list()
for(i in 1:length(dp)){
  idy[[i]] = fy(int[[i]],seq(0,1,0.1),p, dp[[i]])
  idyr[[i]] = fyr(int[[i]],p, dp[[i]], sds)  
}

dfl = data.frame(id = rep(1:3, each =11), x = seq(0,1,0.1),
                 y = unlist(idy), yr = unlist(idyr))

col = c("pink","purple","purple4")

p2 =
ggplot(dfl, aes(x = x, y = yr))+
  scale_x_continuous(expand = c(0.01,0))+
  geom_point(aes(color = as.factor(id), alpha = 0.25))+
  scale_color_manual(values = col)+
  scale_y_continuous(limits = c(0,1))+
  geom_abline(intercept = int, slope = p + dp, color = col, lwd = 1.5)+
  theme(
    panel.border=element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
    strip.background = element_blank(),
    panel.background= element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
    )+
  guides(color = "none", fill = "none", alpha = "none")

####################################################
#combine plots
p3 = plot_grid(p1,p2)
p3