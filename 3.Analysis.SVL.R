#========================Factorial analysis=====================================

#Ho1 - Sex will have no effect on size 
#Ho2 - island will have no effect on size
#Ho3 - competition will have no effect on size
#Ho4 - presence and island will have no effect on size 

#Make the variables easier to work with 
Sex <- An.d$sex
SVL <- An.d$svl
Island <- An.d$island
Presence <- An.d$presence


#================Visualize the data============================================
# first view the data in a bar plot 
barplot(tapply(SVL,list(Presence,Sex), mean), beside = T,
        col = c("red","blue","green","pink"), ylim = c(0,80))

#inspect the mean values of our two factors 
tapply(SVL,list(Presence,Sex),mean)
#The means do not significantly differ between islands 

#create a density plot to view the data between sex and island  
ggplot(An.d, aes(x = svl)) + geom_density() + facet_wrap(island ~
                                                           sex)

#now between sex and and presence 
ggplot(An.d, aes(x = svl)) + geom_density() + facet_wrap(presence ~
                                                           sex)
#Save this density plot to our graphs folder. 
pdf(file =paste(gr.path, "Sex.presence.SVL.pdf", sep = ""), width = 5, 
    height = 5) # open as a pdf and save to graph folder.
ggplot(An.d, aes(x = svl)) + geom_density() + facet_wrap(presence ~
                                                           sex)
dev.off() #close pdf 

#From the initial visualization of the data it appears that on average males are 
#larger than females but there is no difference between presence and no presence 

#==========================Model the Data======================================
# Now fit a factorial analysis of varience
model <- aov(SVL~Presence  + Sex * Island)
summary.lm(model)

#Try to simplify the model. 
model.s <-  aov(SVL~Presence+Sex+Island)
summary.lm(model.s)

#Model.1 with presence + sex
model.1 <- aov(SVL~Presence+Sex)
summary.lm(model.s)

# model 2 with comp + island 
model.2 <- aov(SVL~Presence+Island)
summary.lm(model.2)

#model 3 effect of comp
model.3 <- aov(SVL~Presence)
summary.lm(model.3)

#model 4 effect of island
model.4 <- aov(SVL~Island)
summary.lm(model.4)

#model 5 effect of sex
model.5 <- aov(SVL~Sex)
summary.lm(model.5)

#=================Determin which model is has an Effect=========================

AIC.sum.SVL <- AIC(model,model.1,model.2,model.3,model.4,model.5)
#from this we can see that model  and model 5 have the most effect which makes sense
#because our data does not show any signs of presence or island having an effect 

#=================Run a mixed effect model on the data=========================
#interaction plot to see the interaction between variables 

interaction.plot(Presence,Sex, SVL, fun=mean)

#we can see that presence and mean size of SVL between the sexes have no interaction 

interaction.plot(Island,Sex, SVL, fun=mean)
#same is true with islands. 


#This is so that distributions that must be non-zero can make sense of my
# data
res <- SVL +1 
qqp(res,"norm") # norm distribution fits my data best for in this context. 
# since my data  is normally distributed I can use a linear mixed model

#check lognorm distribuution 
qqp(res,"lnorm")

#Run a LMM

lmm.SVL <- lmer(svl~presence+sex +(1|island), data = An.d, REML = FALSE)

summary(lmm.SVL)
0#we can see that our random effect is not 0 so it has some sort of effect. 

#see the significance 
Anova(lmm.SVL)
#from this we can see the presence of a congener does not have an effect on SVL 
#and that sex is the main factor that influences SVL in Anole lizards. 



#========Here is where we put all our results in our results folder.======== 
#AIC output 
sum.AIC.SVL <- capture.output(AIC.sum.SVL) #capture the output of the summary of model.t . 
cat(sum.AIC.SVL, file= paste(an.path, "AIC.Models.SVL.txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  

#LMM model output 
sum.LMM.SVL <- capture.output(summary(lmm.SVL))
cat(sum.LMM.SVL, file= paste(an.path, "LMM.SVL.txt" , sep = ""))

#Anova of LMM output 
sum.AN.LM.SVL <- capture.output(Anova(lmm.SVL))
cat(sum.AN.LM.SVL, file= paste(an.path, "Anova.lm.SVL.txt" , sep = ""))










#======================Plot the linear effect model============================

#plot the fit of the model
d.t <- cbind.data.frame(Presence, SVL)
head(d.t)
nrow(d.t)
tt <- 654/2
d.t$sex <- rep( c("M", "F"), 327 )
head(d.t)

str(d.t)


# # try to plot just those points 

plot(d.t$Presence, d.t$SVL,  xaxt = "n", type = "n", xlab = "Competition", ylab = 
       "SVL")

# text for left of the graph 
text(0.1, 33, "Absent", xpd = TRUE)
text(0.2, 30, "Males", xpd = TRUE)
text(0.01, 30, "Females", xpd = TRUE)


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


#==================Save this plot to our graphs folder.======================== 
pdf(file =paste(gr.path, "Main.SVL.pdf", sep = ""), width = 5, 
    height = 5) # open as a pdf and save to graph folder.

plot(d.t$Presence, d.t$SVL,  xaxt = "n", type = "n", xlab = "Competition", ylab = 
       "SVL")

# text for left of the graph 
text(0.1, 33, "Absent", xpd = TRUE)
text(0.2, 30, "Males", xpd = TRUE)
text(0.01, 30, "Females", xpd = TRUE)

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

dev.off() #close pdf 



# Add the mean line through each data point (doesn't make sense to me)
# the means were taken from earlier in the code right after examing the bar plot
# those means show mean SVL for males and females in the presence and absence of
# congener, not sure why they dont seem to fit the plotted data here. 


#segments(0.90, 56.63088, x1= 1,)  #males present 
#segments(0.75, 47.66894, x1= 0.70,) #females present 

#segments(0.15, 56.58027, x1= 0.25,) #males absent 
#segments(0.01, 47.29263, x1= 0.06,) #females absent






