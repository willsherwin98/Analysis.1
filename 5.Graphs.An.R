
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
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Snout Vent Length (mm) ", xlab = "", 
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
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass (grams)", xlab = "", 
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





##################################### Mean Of Means ############################
# Create PDF 
pdf(file =paste(gr.path, "Mean.Means", sep = ""), width = 8, 
    height = 5) # open as a pdf and save to graph folder.
# plot side by side 
#par(mar=c(3,4,1,0.5))
par(mar=c(3,4,0.3,0.5))
par(mfrow=c(2,1))
######################### MEANS FARMER CODED MASS ##############################
#create data frame that I am working with 
d.m <- cbind.data.frame(Presence, Mass, Island, Sex)
nrow(d.m)
head(d.m)
str(d.m)

#View(d.m)



x.lim <- c(0, 4.5)
y.lim <- c(1, 4)# range of Mass


#First make the empty plot 
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass (g)", xlab = "", 
     las =  1, xaxt = "na")

# text for left of the graph 
#text(1.3, 0.75, "Absent", xpd = TRUE) 
#text(1.75, 0.5, "Males", xpd = TRUE)
#text(0.75, 0.5, "Females", xpd = TRUE)

#text(3.5, 0.75, "Present", xpd = TRUE)
#text(4, 0.5, "Males", xpd = TRUE)
#text(3, 0.5, "Females", xpd = TRUE)

text(4.4,3.9, "A", xpd=TRUE)


# select the point that i want to plot.

################################# Female presence ############################

# select presence vs absence 
x <- 3
d.m1 <- d.m[d.m$Presence == 1,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Yin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$Mass), pch = 1, cex= 1)

fpYIN <- mean(d.f$Mass)


# Yang # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)


fpYAN <- mean(d.f$Mass)

# N. Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)
fpTwin <- mean(d.f$Mass)

# Lizard # 

d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

fpLizard <- mean(d.f$Mass)



# Hook # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

fpHook <- mean(d.f$Mass)


#Channel 

d.m.2 <- d.m1[d.m1$Island == n.islands[6],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

fpChan<- mean(d.f$Mass)



# Mean of means 
FP <- c(fpYIN,fpYAN,fpTwin,fpLizard,fpHook,fpChan)
points(x,mean(FP), pch = 17, cex= 1)

fpm <- mean(FP)

############################# Female Absence ##################################
x <- 1
d.m1 <- d.m[d.m$Presence == 0,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Hornet # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$Mass), pch = 1, cex= 1)

faHorn <- mean(d.f$Mass)

# Osprey # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

faOsp <- mean(d.f$Mass)

# Crescent # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

faCres <- mean(d.f$Mass)

# S Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

faStwin <- mean(d.f$Mass)
# Pine # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

faPine <- mean(d.f$Mass)

# Mean of the means 
FA <- c(faHorn,faOsp,faCres,faStwin,faPine)
points(x,mean(FA), pch = 17, cex= 1)
fam <- mean(FA)
############################ Male Presence ####################################



# select presence vs absence 
x <- 3.5
d.m1 <- d.m[d.m$Presence == 1,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Yin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$Mass), pch = 1, cex= 1)

mpYin <- mean(d.f$Mass)

# Yang # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

mpYan <- mean(d.f$Mass)


# N. Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

mpNtwin <- mean(d.f$Mass)

# Lizard # 

d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

mpLiz <- mean(d.f$Mass)


# Hook # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

mpHoo <- mean(d.f$Mass)


#Channel 

d.m.2 <- d.m1[d.m1$Island == n.islands[6],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)
mpChan <- mean(d.f$Mass)

# mean of means 
MP <- c(mpYin,mpYan,mpNtwin,mpLiz,mpHoo,mpChan)
points(x,mean(MP), pch = 17, cex= 1)
mpm <- mean(MP)

############################# Male Absence #####################################
x <- 1.5

d.m1 <- d.m[d.m$Presence == 0,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Hornet # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$Mass), pch = 1, cex= 1)
maHorn <- mean(d.f$Mass)

# Osprey # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

maOsp <- mean(d.f$Mass)

# Crescent # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

maCres <- mean(d.f$Mass)

# S Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

maStwin <- mean(d.f$Mass)
# Pine # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$Mass), pch = 1, cex= 1)

maPine <- mean(d.f$Mass)

#mean of means 
MA <- c(maHorn,maOsp,maCres,maStwin,maPine)
points(x,mean(MA), pch = 17, cex= 1)

mam <- mean(MA)


####################### Mean of Means SVL ######################################

#create data frame that I am working with 
d.m <- cbind.data.frame(Presence, SVL, Island, Sex)
nrow(d.m)
head(d.m)
str(d.m)

#View(d.m)
x.lim <- c(0, 4.5)
y.lim <- c(40, 60)# range of Mass


#First make the empty plot 
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL (mm)", xlab = "", 
     las =  1, xaxt = "na")

# text for left of the graph 
text(1.3, 38, "Absent", xpd = TRUE) 
text(1.65, 35.3, "Males", xpd = TRUE)
text(0.90, 35.3, "Females", xpd = TRUE)

text(3.4, 38, "Present", xpd = TRUE)
text(3.75, 35.3, "Males", xpd = TRUE)
text(3, 35.3, "Females", xpd = TRUE)

text(4.4,58, "B", xpd=TRUE)


# select the point that i want to plot.

################################# Female presence ############################

# select presence vs absence 
x <- 3
d.m1 <- d.m[d.m$Presence == 1,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Yin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$SVL), pch = 1, cex= 1)

fpYIN <- mean(d.f$SVL)


# Yang # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)


fpYAN <- mean(d.f$SVL)

# N. Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)
fpTwin <- mean(d.f$SVL)

# Lizard # 

d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

fpLizard <- mean(d.f$SVL)



# Hook # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

fpHook <- mean(d.f$SVL)


#Channel 

d.m.2 <- d.m1[d.m1$Island == n.islands[6],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

fpChan<- mean(d.f$SVL)



# Mean of means 
FP <- c(fpYIN,fpYAN,fpTwin,fpLizard,fpHook,fpChan)
points(x,mean(FP), pch = 17, cex= 1)

fps <- mean(FP)

############################# Female Absence ##################################
x <- 1
d.m1 <- d.m[d.m$Presence == 0,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Hornet # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$SVL), pch = 1, cex= 1)

faHorn <- mean(d.f$SVL)

# Osprey # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

faOsp <- mean(d.f$SVL)

# Crescent # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

faCres <- mean(d.f$SVL)

# S Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

faStwin <- mean(d.f$SVL)
# Pine # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "f",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

faPine <- mean(d.f$SVL)

# Mean of the means 
FA <- c(faHorn,faOsp,faCres,faStwin,faPine)
points(x,mean(FA), pch = 17, cex= 1)
fas <- mean(FA)
############################ Male Presence ####################################



# select presence vs absence 
x <- 3.5
d.m1 <- d.m[d.m$Presence == 1,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Yin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$SVL), pch = 1, cex= 1)

mpYin <- mean(d.f$SVL)

# Yang # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

mpYan <- mean(d.f$SVL)


# N. Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

mpNtwin <- mean(d.f$SVL)

# Lizard # 

d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

mpLiz <- mean(d.f$SVL)


# Hook # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

mpHoo <- mean(d.f$SVL)


#Channel 

d.m.2 <- d.m1[d.m1$Island == n.islands[6],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)
mpChan <- mean(d.f$SVL)

# mean of means 
MP <- c(mpYin,mpYan,mpNtwin,mpLiz,mpHoo,mpChan)
points(x,mean(MP), pch = 17, cex= 1)
mps <- mean(MP)

############################# Male Absence #####################################
x <- 1.5

d.m1 <- d.m[d.m$Presence == 0,]  # select presence vs absence

n.islands <- unique(d.m1$Island) # islands of presence v absence 


# Hornet # 
d.m.2 <- d.m1[d.m1$Island == n.islands[1],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

# plot the points on the graph.

points(x,mean(d.f$SVL), pch = 1, cex= 1)
maHorn <- mean(d.f$SVL)

# Osprey # 
d.m.2 <- d.m1[d.m1$Island == n.islands[2],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

maOsp <- mean(d.f$SVL)

# Crescent # 
d.m.2 <- d.m1[d.m1$Island == n.islands[3],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

maCres <- mean(d.f$SVL)

# S Twin # 
d.m.2 <- d.m1[d.m1$Island == n.islands[4],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

maStwin <- mean(d.f$SVL)
# Pine # 
d.m.2 <- d.m1[d.m1$Island == n.islands[5],] #select island 

d.f <- d.m.2[d.m.2$Sex == "m",] # select the sex 

points(x,mean(d.f$SVL), pch = 1, cex= 1)

maPine <- mean(d.f$SVL)

#mean of means 
MA <- c(maHorn,maOsp,maCres,maStwin,maPine)
points(x,mean(MA), pch = 17, cex= 1)

mas <- mean(MA)


# Turn Off PDF 
dev.off()

#=========================================== mean outputs SVL =====================
Means.out <- capture.output(fps,fas,mps,mas) #capture the output 
difM <-capture.output(fps - fas, mps - mas)
cat(Means.out, file= paste(an.path, "All.means.SVL.txt" , sep = "")) 
cat(difM, file= paste(an.path, "Diff.means.SVL.txt" , sep = ""))

#=========================================== mean outputs SVL =====================
Means.out <- capture.output(fpm,fam,mpm,mam) #capture the output 
difM <-capture.output(fpm - fam, mpm - mam)
cat(Means.out, file= paste(an.path, "All.means.mass.txt" , sep = "")) 
cat(difM, file= paste(an.path, "Diff.means.mass.txt" , sep = ""))

