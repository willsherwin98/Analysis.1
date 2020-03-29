#
# This script is where I create and store figures for the Anole data 
#
# create data frame that I am working with 
d.t <- cbind.data.frame(Presence, SVL, Island, Sex)
head(d.t)
nrow(d.t)
tt <- 594/2
head(d.t)

str(d.t)

View(d.t)

# ===========rename the islands so they graph nicely =========================

d.t$Island <- gsub("line.cedars", "Line Cedars", d.t$Island)
d.t$Island <- gsub("hornet", "Hornet", d.t$Island)
d.t$Island <- gsub("yin", "Yin", d.t$Island)
d.t$Island <- gsub("osprey", "Osprey", d.t$Island)
d.t$Island <- gsub("yang", "Yang", d.t$Island)
d.t$Island <- gsub("crescent", "Crescent", d.t$Island)
d.t$Island <- gsub("n.twin", "N.Twin", d.t$Island)
d.t$Island <- gsub("lizard", "Lizard", d.t$Island)
d.t$Island <- gsub("hook", "Hook", d.t$Island)
d.t$Island <- gsub("s.twin", "S.Twin", d.t$Island)
d.t$Island <- gsub("channel", "Channel", d.t$Island)
d.t$Island <- gsub("pine", "Pine", d.t$Island)


########################### GRAPH FOR SVL ######################################

#  two loops: one for presence and one for island
cat.presence <- rev(unique(d.t$Presence)) # start with 0 (absence)
islands.tot <- 1  # counter to keep track where we are on the X axis
all.islands <- unique(d.t$Island)

#======================== figure specific parameters ===========================
space.groups <- 2   # space between the absence and presence groups
delta.sex <- 0.3 # distance between sexes in plot
noise <- 0.075     #  to create jitter
delta.r <- c(delta.sex + noise, delta.sex - noise)
x.islands <- c(seq(1,11,2), seq(15,24, 2))
seq
x.lim <- c(0, max(x.islands)+1 )
y.lim <- c(33, 65)# range of SVL 
cex.text <- 0.7

# pch 16 for females and 1 for Males
pch.t <- c(16, 1)

#========================= Open as PDF to store in folder======================

pdf(file =paste(gr.path, "Final.SVL.pdf", sep = ""), width = 8, 
    height = 4) # open as a pdf and save to graph folder.


# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL ", xlab = "", 
     las =  1, xaxt = "n")

#add text to the x axis 

# text for left of the graph 
text(6.5, 25, "Present", xpd = TRUE)

#text for the right side of the graph 
text(19.5, 25, "Absent", xpd = TRUE)

# ================== loop to get into presence or absence ======================
for(i in 1:length(cat.presence)){
    # i <- 1
  
 d.t1 <- d.t[d.t$Presence == cat.presence[i],]
  # now with the presence group i we go and loop through all islands
 n.islands <- unique(d.t1$Island)

for(j in 1:length(n.islands)){
   # j <- 2
 # select island j
d.t.2 <- d.t1[d.t1$Island == n.islands[j],]
    
 # ============================= points for females =========================
    d.f <- d.t.2[d.t.2$Sex == "f",]
    x.f <- runif(nrow(d.f), x.islands[islands.tot] - delta.r[1], x.islands[islands.tot] - delta.r[2] )           
    points(x.f, d.f$SVL, pch = pch.t[1]) #, col = col.t[islands.tot] )
    points( x.islands[islands.tot] - delta.sex, mean(d.f$SVL), pch = "—", col =
              "red") # the last line is for the mean 

   
# =============================== for the males ============================
    d.f <- d.t.2[d.t.2$Sex == "m",]
    x.f <- runif(nrow(d.f), x.islands[islands.tot] + delta.r[2], x.islands[islands.tot] + delta.r[1] )           
    points(x.f, d.f$SVL, pch = pch.t[2]) #, col = col.t[islands.tot] )
    points( x.islands[islands.tot] + delta.sex, mean(d.f$SVL), pch = "—", col =
              "red") # the last line is for the mean 
    
    
    # =========== Put the island names at the bottom of the graph ==============
    text(x.islands[islands.tot], y.lim[1]+1,  substr( unlist(n.islands[j]), 1,3),
         cex = cex.text)
    islands.tot <-  islands.tot + 1
# ========================End the loop ========================================    
  }
 
}
 

# ========================== Add legend to the plot ============================

legend(12,74.5, legend = c("Female", "Male"), pch = c(16,1), cex = 0.75,
       xpd = TRUE)

#=============================Close the pdf====================================
dev.off() #close pdf 



############################## END SVL #########################################


############################ GRAPH FOR MASS ###################################


#create data frame that I am working with 
d.m <- cbind.data.frame(Presence, Mass, Island, Sex)
head(d.m)
nrow(d.m)
tt <- 594/2
head(d.m)

str(d.m)

View(d.m)


# ===========rename the islands so they graph nicely =========================

d.m$Island <- gsub("line.cedars", "Line Cedars", d.m$Island)
d.m$Island <- gsub("hornet", "Hornet", d.m$Island)
d.m$Island <- gsub("yin", "Yin", d.m$Island)
d.m$Island <- gsub("osprey", "Osprey", d.m$Island)
d.m$Island <- gsub("yang", "Yang", d.m$Island)
d.m$Island <- gsub("crescent", "Crescent", d.m$Island)
d.m$Island <- gsub("n.twin", "N.Twin", d.m$Island)
d.m$Island <- gsub("lizard", "Lizard", d.m$Island)
d.m$Island <- gsub("hook", "Hook", d.m$Island)
d.m$Island <- gsub("s.twin", "S.Twin", d.m$Island)
d.m$Island <- gsub("channel", "Channel", d.m$Island)
d.m$Island <- gsub("pine", "Pine", d.m$Island)


#  two loops: one for presence and one for island
cat.presence <- rev(unique(d.m$Presence)) # start with 0 (absence)
islands.tot <- 1  # counter to keep track where we are on the X axis
all.islands <- unique(d.m$Island)

space.groups <- 2   # space between the absence and presence groups
delta.sex <- 0.3 # distance between sexes in plot
noise <- 0.075     #  to create jitter
delta.r <- c(delta.sex + noise, delta.sex - noise)
x.islands <- c(seq(1,11,2), seq(15,24, 2))

x.lim <- c(0, max(x.islands)+1 )
y.lim <- c(0, 5)# range of Mass
cex.text <- 0.7

# pch 16 for females and 1 for Males
pch.t <- c(16, 1)

#========================= Open as PDF to store in folder======================

pdf(file =paste(gr.path, "Final.Mass.pdf", sep = ""), width = 8, 
    height = 4) # open as a pdf and save to graph folder.


# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "n")

#add text to the x axis 

# text for left of the graph 
text(6.5, -1.5, "Present", xpd = TRUE)

#text for the right side of the graph 
text(19.5, -1.5, "Absent", xpd = TRUE)

for(i in 1:length(cat.presence)){
  # i <- 1
  
  d.m1 <- d.m[d.m$Presence == cat.presence[i],]
  # now with the presence group i we go and loop through all islands
  n.islands <- unique(d.m1$Island)
  for(j in 1:length(n.islands)){
    # j <- 2
    # select island j
    d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
    
    # ============================= points for females =========================
    d.f <- d.m.2[d.m.2$Sex == "f",]
    x.f <- runif(nrow(d.f), x.islands[islands.tot] - delta.r[1], x.islands[islands.tot] - delta.r[2] )           
    points(x.f, d.f$Mass, pch = pch.t[1]) 
    points( x.islands[islands.tot] - delta.sex, mean(d.f$Mass), pch = "—", col =
              "red") # the last line is for the mean 

    # =============================== for the males ============================
    d.f <- d.m.2[d.m.2$Sex == "m",]
    x.f <- runif(nrow(d.f), x.islands[islands.tot] + delta.r[2], x.islands[islands.tot] + delta.r[1] )           
    points(x.f, d.f$Mass, pch = pch.t[2]) 
    points( x.islands[islands.tot] + delta.sex, mean(d.f$Mass), pch = "—", col =
              "red") # the last line is for the mean 
    
    # =========== Put the island names at the bottom of the graph ==============
    text(x.islands[islands.tot], y.lim[1]+0.25,  substr( unlist(n.islands[j]), 1,3),
         cex = cex.text)
    islands.tot <-  islands.tot + 1
    # ========================End the loop ========================================    
  }
  
}

# ========================== Add legend to the plot ============================

legend(12,6.5, legend = c("Female", "Male"), pch = c(16,1), cex = 0.75,
       xpd = TRUE)

#=============================Close the pdf====================================
dev.off() #close pdf 

############################ End Mass #########################################




#=======================Plot the means for mass and SVL ======================
##############   Plot the means for SVL  ################################################

# open as a PDF 
pdf(file =paste(gr.path, "Means.svl,mass", sep = ""), width = 5, 
    height = 5)




#specify data 
d.t <- cbind.data.frame(Presence, SVL, Island, Sex)
#======================== figure specific parameters ===========================

x.lim <- c(0.5,3.53)
y.lim <- c(45, 60)# range(d.t$SVL)
cex.text <- 0.7


# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL ", xlab = "", 
     las =  1, xaxt = "n")

# text for left of the graph 
text(3, 40, "Present", xpd = TRUE) 
text(2.73, 43, "Males", xpd = TRUE)
text(3.33, 43, "Females", xpd = TRUE)

text(0.97, 40, "Absent", xpd = TRUE)
text(1.3, 43, "Males", xpd = TRUE)
text(0.70, 43, "Females", xpd = TRUE)

text(2,64, "A", xpd=TRUE)


#========================== POINTS FOR MALE AND FEMALE MEANS=================
# MEANS FOR FEMALE ABSENT  
cat.presence <- rev(unique(d.t$Presence))

d.t1 <- d.t[d.t$Presence == cat.presence[1],]

d.F.0 <- d.t1[d.t1$Sex== "f",]

points(0.70,mean(d.F.0$SVL), pch = 8)

# MEANS FOR FEMALE PRESENT 
d.t2 <- d.t[d.t$Presence == cat.presence[2],]

d.F.1 <- d.t2[d.t2$Sex== "f",]

points(3.33,mean(d.F.1$SVL), pch = 8)

# MALE ABSENT  
d.t3 <- d.t[d.t$Presence == cat.presence[1],]

d.M.0 <- d.t3[d.t3$Sex== "m",]

points(1.3,mean(d.M.0$SVL), pch = 8)

#MALE PRESENCE  
d.t4 <- d.t[d.t$Presence == cat.presence[2],]

d.M.1 <- d.t4[d.t4$Sex== "m",]

points(2.73, mean(d.M.1$SVL), pch = 8)




##############   Plot the means for MASS  ################################################




#======================== figure specific parameters ===========================

x.lim <- c(0.5, 3.53 )
y.lim <- c(0, 5)# range of Mass
cex.text <- 0.

# pch 16 for females and 1 for Males
pch.t <- c(16)

#specify the data frame 
d.t <- cbind.data.frame(Presence, Mass, Island, Sex)

# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "n")

# text for left of the graph 
text(3, - 2, "Present", xpd = TRUE, cex = 3) 
text(2.73, -0.75, "Males", xpd = TRUE, cex = 3)
text(3.33, -0.75, "Females", xpd = TRUE, cex = 3)

text(0.97, -2, "Absent", xpd = TRUE)
text(1.3, -0.75, "Males", xpd = TRUE)
text(0.70, -0.75, "Females", xpd = TRUE)

text(2,6, "B", xpd=TRUE)


#========================== POINTS FOR MALE AND FEMALE MEANS=================
# MEANS FOR FEMALE ABSENT  
cat.presence <- rev(unique(d.t$Presence))

d.t1 <- d.t[d.t$Presence == cat.presence[1],]

d.F.0 <- d.t1[d.t1$Sex== "f",]

points(0.70,mean(d.F.0$Mass), pch = 8)

# MEANS FOR FEMALE PRESENT 
d.t2 <- d.t[d.t$Presence == cat.presence[2],]

d.F.1 <- d.t2[d.t2$Sex== "f",]

points(3.33,mean(d.F.1$Mass), pch = 8)

# MALE ABSENT  
d.t3 <- d.t[d.t$Presence == cat.presence[1],]

d.M.0 <- d.t3[d.t3$Sex== "m",]

points(1.3,mean(d.M.0$Mass), pch = 8)

#MALE PRESENCE  
d.t4 <- d.t[d.t$Presence == cat.presence[2],]

d.M.1 <- d.t4[d.t4$Sex== "m",]

points(2.73, mean(d.M.1$Mass), pch = 8)

#close pdf 
dev.off()

