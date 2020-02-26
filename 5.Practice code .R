#plot the fit of the model
d.t <- cbind.data.frame(Presence, SVL, Island)
head(d.t)
nrow(d.t)
tt <- 654/2
d.t$sex <- rep( c("M", "F"), 327 )
head(d.t)

str(d.t)

View(d.t)

# # try to plot just those points 

plot(d.t$Presence, d.t$SVL,  xaxt = "n", type = "n", xlab = "Competition", ylab = 
       "SVL")

# text for left of the graph 
text(0.1, 32, "Absent", cex = 0.80, xpd = TRUE)
text(0.2, 30, "Males", cex = 0.80, xpd = TRUE)
text(0.01, 30, "Females", cex = 0.80, xpd = TRUE)
text(0.01, 34, "Osprey", cex = 0.75, xpd = TRUE)

#============================seperating by island===========================

male.f <- d.t$SVL[d.t$sex=="F" & d.t$Presence == 0 & d.t$Island=="osprey"]
points(rep(0.01, length(male.f)) , male.f)


#==================try to make a for loop to do each island====================
for(i in 6){
  
  male.f <- d.t$SVL[d.t$sex=="F" & d.t$Presence == 0 & d.t$Island==i]
  points(rep(0.01+i 0.05 , length(male.f)) , male.f)
  
  
  
  
}
  







##points for absence 
males.no <- d.t$SVL[d.t$sex == "M" & d.t$Presence == 0] #male svl in absence 
points(jitter(rep(0.2, length(males.no)), 3) , males.no)# plot the points 
females.no <- d.t$SVL[d.t$sex=="F" & d.t$Presence==0] #female svl in absence 
points(jitter(rep(0.01, length(females.no)), 3) , females.no) #plot the ponts 


# text for right of the graph 
text(0.85, 33, "Present", xpd = TRUE)
text(0.95, 30, "Males", xpd = TRUE)
text(0.75, 30, "Females", xpd = TRUE)


##points for Presence 
males.yes <- d.t$SVL[d.t$sex == "M" & d.t$Presence == 1] #male svl in presence
points(jitter(rep(0.94, length(males.yes)), 1) , males.yes)# plot the points 
females.yes <- d.t$SVL[d.t$sex=="F" & d.t$Presence==1] #female svl in presence
points(jitter(rep(0.75, length(females.yes)), 1),females.yes) #plot the ponts 

