#   =================Make Figure 4 =======================================

x.lim <- c(0, 7)#max(x.islands)+1 )
y.lim <- c(1,5)# range of Mass

n.islands <- unique(d.m1$Island)


# pch 16 for females and 1 for Males
#pch.t <- c(16, 1)

plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "na")

text(6.5, -1.5, "Present", xpd = TRUE)

text(19.5, -1.5, "Absent", xpd = TRUE)


#d.m1 <- d.m[d.m$Presence =="0",]


for(i in 1:length(cat.presence)){
  #i <- 1
  
  d.m1 <- d.m[d.m$Presence == cat.presence[i],]
  # now with the presence group i we go and loop through all islands
  n.islands <- unique(d.m1$Island)
  for(j in 1:length(n.islands)){
   # j <- 1
    # select island j
    d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
    
    
    
    
    
    # ============================= points  =========================
    d.f <- d.m.2[d.m.2$Sex == "f",]
    
    #x.islands <-  c(seq(1,3,2), seq(5,7, 2))
    ######### Mean male and female for absence####### 
    
    points(1, mean(d.f$Mass), pch= 16, cex= 1)
    
    # means for  males 
    d.f <- d.m.2[d.m.2$Sex == "m",]
    
    points(2, mean(d.f$Mass), pch= 16, cex= 1)
    
    
    
    
    
  }
}
    
    
    ################ mean points for presence ###################################
    
    d.m1 <- d.m[d.m$Presence =="0",]
n.islands <- unique(d.m1$Island)
    for(l in 1:length(n.islands)){
    #l <- 1
    # select island j
    d.m.2 <- d.m1[d.m1$Island == n.islands[l],]
    
    #d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
    d.f <- d.m.2[d.m.2$Sex == "f",]
    # means for presence females 
    
    points(5,mean(d.f$Mass), pch= 16, cex= 1)
    
    
    # means for  males 
    d.f <- d.m.2[d.m.2$Sex == "m",]
    
    points(6,mean(d.f$Mass), pch= 16, cex= 1)
    
  }



###########################  END   ##########################################















# =============================== for the males ============================
d.f <- d.m.2[d.m.2$Sex == "m",]


print(x.islands[islands.tot] + delta.sex, mean(d.f$Mass))

# =========== Put the island names at the bottom of the graph ==============

# ========================End the loop ========================================    
}

}

# ========================== Add legend to the plot ============================

legend(12,6.5, legend = c("Female", "Male"), pch = c(16,1), cex = 0.75,
       xpd = TRUE)

#====================mean means===================================

plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "n")


