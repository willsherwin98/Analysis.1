
unique(An.d$sex)
length(An.d$Bones.sex[An.d$Bones.sex=="m"])
length(An.d$Bones.sex[An.d$Bones.sex=="f"])

length(An.d$sex[An.d$sex =="m"])
length(An.d$sex[An.d$sex =="f"])

unique(An.d$island)

length(unique(An.d$island,An.d$presence[An.d$presence=="0"]))

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
#for(i in 6){
  i <- 0.01
  male.f <- d.t$SVL[d.t$sex=="F" & d.t$Presence == 0 & d.t$Island==i]
  points(rep(i+0.02, length(male.f)) , male.f)
  
  
  
  
}

# =========================coding with Thor====================================


#=======Change the names of the headers so that the x axis reads nicely.======== 
d.t <- cbind.data.frame(Presence, SVL, Island, Sex)

head(d.t)
nrow(d.t)
tt <- 654/2
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

d.t$Island
View(d.t)
head(d.t)




# goinng to make two loops: one for presence and one for island

#library(RColorBrewer)
        
cat.presence <- rev(unique(d.t$Presence)) # start with 0 (= absence)
islands.tot <- 1  # counter to keep track where we are on the X axis
all.islands <- unique(d.t$Island)

#======================== figure specific parameters ===========================
space.groups <- 2   # space between the absence and presence groups
delta.sex <- 0.3 #0.15 # 1/2 distance between sexes in plot
noise <- 0.075     # used to create some jitter
delta.r <- c(delta.sex + noise, delta.sex - noise)
x.islands <- c(seq(1,9,2), seq(13,25, 2))

x.lim <- c(0, max(x.islands)+1 )
y.lim <- c(33, 65)# range(d.t$SVL)
cex.text <- 0.7

col.t <- grey.colors(length(all.islands), 0.2, 0.8, alpha = 0.8)


# <- brewer.pal(12, "Dark2")

#col.t <- c("blue", "red", "black", "maroon2", "purple", NA, NA, "orange", "gray",
 #          "yellow", "pink", "dark green", "fire brick", "dark blue") # NEEDS TO BE SAME NUMBER OF ISLANDS + space.groups
# so build in two  numbers for the space between the absence and presence

# Make each island i different pch 
#pch.t <- c(15,16,17,18,19,20,NA,NA,4,1,0,2,3,5,6)

# pch 16 for females and 1 for Males
pch.t <- c(16, 1)

# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL ", xlab = "", 
     las =  1, xaxt = "n")

#add text to the x axis 

# text for left of the graph 
text(8, 25, "Absent", xpd = TRUE)

#text for the right side of the graph 
text(15, 25, "Present", xpd = TRUE)



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
    
    #plot(20,50, cex = 1, pch = "——")
    
   print(islands.tot)
    #put islands into the graph. 
    #text(x.islands[islands.tot], y.lim[1]+1,  substr( unlist(n.islands[j]), 1,2),
        # cex = cex.text)
   #islands.tot <-  islands.tot + 1
    
    # =========== Put the island names at the bottom of the graph ==============
   text(x.islands[islands.tot], y.lim[1]+1,  substr( unlist(n.islands[j]), 1,3),
        cex = cex.text)
   islands.tot <-  islands.tot + 1
   
    
  }
  # add the number of islands in this round to counter for islands
  #islands.tot <- islands.tot + space.groups
  
}
  
# ========================== Add legend to the plot ============================
legend(22.2,78.1, legend = c("Female", "Male"), pch = c(16,1), cex = 0.75,
       xpd = TRUE)


#legend(24.21,73.5, legend = c("Female", "Male"), pch = c(16,1), cex = 0.75,
# xpd = TRUE)   

# this legend lines up on the lines to the right of the graph on the pdf 




1











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



##############   Plot the means for SVL  #########################


par(mfrow=c(2,2))

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
















##############   Plot the means for SVL  ################################################

# open as a PDF 
pdf(file =paste(gr.path, "Means.svl.mass", sep = ""), width = 30, 
    height = 30)

#plot both at the same time 

#par(mar=c(3,4,1,0.5))
par(mar=c(3,4,1,0.5))
#par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
#specify data 
d.t <- cbind.data.frame(Presence, SVL, Island, Sex)
#======================== figure specific parameters ===========================

x.lim <- c(0.5,3.53)
y.lim <- c(40, 65)# range(d.t$SVL)
cex.text <- 0.7


# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "SVL ", xlab = "", 
     las =  1, xaxt = "n")

# text for left of the graph 
#text(3, 40, "Present", xpd = TRUE) 
#text(2.73, 43, "Males", xpd = TRUE)
#text(3.33, 43, "Females", xpd = TRUE)

#text(0.97, 40, "Absent", xpd = TRUE)
#text(1.3, 43, "Males", xpd = TRUE)
#text(0.70, 43, "Females", xpd = TRUE)

text(3.5,63.5, "A", xpd=TRUE)


#========================== POINTS FOR MALE AND FEMALE MEANS=================
# MEANS FOR FEMALE ABSENT  
cat.presence <- rev(unique(d.t$Presence))

d.t1 <- d.t[d.t$Presence == cat.presence[1],]

d.F.0 <- d.t1[d.t1$Sex== "f",]

length(d.F.0$SVL)
points(rep(0.70,length(d.F.0$SVL)),d.F.0$SVL, pch = 1)# all points 


points(0.70,mean(d.F.0$SVL), pch = 15, col = "red") # means 

# MEANS FOR FEMALE PRESENT 
d.t2 <- d.t[d.t$Presence == cat.presence[2],]

d.F.1 <- d.t2[d.t2$Sex== "f",]
length(d.F.1$SVL)
points(rep(3.33,length(d.F.1$SVL)),d.F.1$SVL, pch = 1)# all points 

points(3.33,mean(d.F.1$SVL), pch = 15, col = "red")#means 

# MALE ABSENT  
d.t3 <- d.t[d.t$Presence == cat.presence[1],]

d.M.0 <- d.t3[d.t3$Sex== "m",]
length()
points(rep(1.3,length(d.M.0$SVL)),d.M.0$SVL, pch = 1)#points 

points(1.3,mean(d.M.0$SVL), pch = 15, col = "red")# means 

#MALE PRESENCE  
d.t4 <- d.t[d.t$Presence == cat.presence[2],]

d.M.1 <- d.t4[d.t4$Sex== "m",]

points(rep(2.73,length(d.M.1$SVL)), d.M.1$SVL, pch = 1)# points 

points(2.73, mean(d.M.1$SVL), pch = 15, col = "red") #means 



##############   Plot the means for MASS  ################################################


AbF <- 47.29263#mean absent female 
PrF <- 47.79616# mean presence female 
AbM <- 56.58027 #mean absent male 
PrM <- 56.91857 #mean presence male
AbF-PrF
AbM-PrM

#======================== figure specific parameters ===========================

x.lim <- c(0.5, 3.53 )
y.lim <- c(0, 5)# range of Mass
cex.text <- 0.7

# pch 16 for females and 1 for Males
pch.t <- c(16)

#specify the data frame 
d.t <- cbind.data.frame(Presence, Mass, Island, Sex)

# ==================== plot an empty figure space =============================
plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
     las =  1, xaxt = "n")

# text for left of the graph 
text(3, - 2, "Absent", xpd = TRUE) 
text(2.73, -0.75, "Males", xpd = TRUE)
text(3.33, -0.75, "Females", xpd = TRUE)

text(0.97, -2, "Present", xpd = TRUE)
text(1.3, -0.75, "Males", xpd = TRUE)
text(0.70, -0.75, "Females", xpd = TRUE)

text(3.5,4.5, "B", xpd=TRUE)


#========================== POINTS FOR MALE AND FEMALE MEANS=================
#mean difference for mass 
AbF <- 2.001316#mean absent female 
PrF <- 2.011111# mean presence female 
AbM <- 3.175824 #mean absent male 
PrM <- 3.238819 #mean presence male
AbF-PrF
AbM-PrM

# MEANS FOR FEMALE ABSENT  
cat.presence <- rev(unique(d.t$Presence))

d.t1 <- d.t[d.t$Presence == cat.presence[1],]

d.F.0 <- d.t1[d.t1$Sex== "f",]

points(rep(0.70,length(d.F.0$Mass)),d.F.0$Mass,pch=1) # all the points 

points(0.70,mean(d.F.0$Mass), pch = 15, col = "red") # just the means 

# MEANS FOR FEMALE PRESENT 
d.t2 <- d.t[d.t$Presence == cat.presence[2],]

d.F.1 <- d.t2[d.t2$Sex== "f",]

points(rep(3.33,length(d.F.1$Mass)), d.F.1$Mass, pch = 1)

points(3.33,mean(d.F.1$Mass), pch = 15, col = "red")

# MALE ABSENT  
d.t3 <- d.t[d.t$Presence == cat.presence[1],]

d.M.0 <- d.t3[d.t3$Sex== "m",]

points(rep(1.3,length(d.M.0$Mass)),d.M.0$Mass, pch = 1) # all point 
  
points(1.3,mean(d.M.0$Mass), pch = 15, col = "red")# means 

#MALE PRESENCE  
d.t4 <- d.t[d.t$Presence == cat.presence[2],]

d.M.1 <- d.t4[d.t4$Sex== "m",]

points(rep(2.73,length(d.M.1$Mass)), d.M.1$Mass, pch = 1)# all points 

points(2.73, mean(d.M.1$Mass), pch = 15, col = "red")#just the means 

#close pdf 
dev.off()



















############################# Means of the Means of the island##################
cat.presence <- rev(unique(d.m$Presence)) # start with 0 (absence)
islands.tot <- 1 # counter to keep track where we are on the X axis
all.islands <- unique(d.m$Island)

space.groups <- 2   # space between the absence and presence groups
delta.sex <- 0.3 # distance between sexes in plot
noise <- 0.075     #  to create jitter
delta.r <- c(delta.sex + noise, delta.sex - noise)
x.islands <- c(seq(1,11,2), seq(15,24, 2))

x.lim <- c(0, max(x.islands)+1 )
y.lim <- c(0, 5)# range of Mass
cex.text <- 0.7

#for(i in 1:length(cat.presence)){
  # i <- 1
  
 # d.m1 <- d.m[d.m$Presence == cat.presence[i],]
  # now with the presence group i we go and loop through all islands
  
  
 # n.islands <- unique(d.m1$Island)
  
  
  
  #   =================Make Figure 4 =======================================
 
   x.lim <- c(0, 10)#max(x.islands)+1 )
  y.lim <- c(1,5)# range of Mass
  
  
  # pch 16 for females and 1 for Males
  #pch.t <- c(16, 1)
  
  plot(NA, xlim = x.lim, ylim = y.lim, ylab = "Mass", xlab = "", 
       las =  1, xaxt = "na")
  
  
  
  #d.m1 <- d.m[d.m$Presence =="0",]
  
  
  for(i in 1:length(cat.presence)){
     #i <- 1
    
    d.m1 <- d.m[d.m$Presence == cat.presence[i],]
    # now with the presence group i we go and loop through all islands
    n.islands <- unique(d.m1$Island)
  
    
    for(j in 1:length(n.islands)){
       #j <- 1
      # select island j
      d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
  
  
  
  
    
    # ============================= points  =========================
    d.f <- d.m.2[d.m.2$Sex == "f",]
    
      #x.islands <-  c(seq(1,3,2), seq(5,7, 2))
     ######### Mean male and female for absence####### 
    x.f <- runif(nrow(d.f), x.islands[islands.tot] + delta.r[2], x.islands[islands.tot] + delta.r[1] )  
    
    #points(x.islands[islands.tot] - delta.sex, mean(d.f$Mass), pch = 16, cex =
              1)
   
   points(x.f, mean(d.f$Mass), pch= 16, cex= 1)
   
   # means for  males 
   d.f <- d.m.2[d.m.2$Sex == "m",]
   
 points(x.islands[islands.tot] - delta.sex, mean(d.f$Mass), pch = 16, cex =
            1)
   
   #points(5, mean(d.f$Mass), pch= 16, cex= 1)
   
    
    }
  }
   ################ mean points for presence ###################################
   
   #d.m1 <- d.m[d.m$Presence =="1",]
   #for(j in 1:length(n.islands)){
     #j <- 1
     # select island j
     d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
  
    #d.m.2 <- d.m1[d.m1$Island == n.islands[j],]
   d.f <- d.m.2[d.m.2$Sex == "f",]
    # means for presence females 
   
   points(6,mean(d.f$Mass), pch= 16, cex= 1)
   
   
   # means for  males 
   d.f <- d.m.2[d.m.2$Sex == "m",]
   
   points(8,mean(d.f$Mass), pch= 16, cex= 1)
 
    }
  }
     
     
    

    
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


