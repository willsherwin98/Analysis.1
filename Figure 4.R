# Create PDF 
pdf(file =paste(gr.path, "Mean.Means", sep = ""), width = 8, 
    height = 5) # open as a pdf and save to graph folder.
# plot side by side 
par(mar=c(3,4,1,0.5))
par(mfrow=c(2,1))
######################### MEANS FARMER CODED MASS ############################################
#create data frame that I am working with 
d.m <- cbind.data.frame(Presence, Mass, Island, Sex)
nrow(d.m)
head(d.m)
str(d.m)

View(d.m)



x.lim <- c(0, 4.5)
y.lim <- c(1, 4)# range of Mass
 

#First make the empty plot 
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "na")

# text for left of the graph 
text(1.3, 0.75, "Absent", xpd = TRUE) 
text(1.75, 0.5, "Males", xpd = TRUE)
text(0.75, 0.5, "Females", xpd = TRUE)

text(3.5, 0.75, "Present", xpd = TRUE)
text(4, 0.5, "Males", xpd = TRUE)
text(3, 0.5, "Females", xpd = TRUE)

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
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL", xlab = "", 
     las =  1, xaxt = "na")

# text for left of the graph 
text(1.3, 38, "Absent", xpd = TRUE) 
text(1.75, 36.5, "Males", xpd = TRUE)
text(0.75, 36.5, "Females", xpd = TRUE)

text(3.5, 38, "Present", xpd = TRUE)
text(4, 36.5, "Males", xpd = TRUE)
text(3, 36.5, "Females", xpd = TRUE)

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




# Turn Off PDF 
dev.off()