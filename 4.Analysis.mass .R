
# ========================Factorial analysis=====================================

#Ho1 - Sex will have no effect on size 
#Ho2 - island will have no effect on size
#Ho3 - competition will have no effect on size
#Ho4 - presence and island will have no effect on size 

#Make the variables easier to work with 
Sex <- An.d$sex
Mass <- An.d$Mass
Island <- An.d$island
Presence <- An.d$presence


#================Visualize the data============================================
# first view the data in a bar plot 
barplot(tapply(Mass,list(Presence,Sex), mean), beside = T,
        col = c("red","blue","green","pink"), ylim = c(0,80))

#inspect the mean values of our two factors 
tapply(Mass,list(Presence,Sex),mean)
#The means do not significantly differ between islands 

#create a density plot to view the data between sex and island  
ggplot(An.d, aes(x = Mass)) + geom_density() + facet_wrap(island ~sex)


densityPlot(presence~sex, svl, show.bw =FALSE, data=An.d)

#now between sex and and presence 
ggplot(An.d, aes(x = Mass)) + geom_density() + facet_wrap(presence~sex)
#Save this density plot to our graphs folder. 
pdf(file =paste(gr.path, "Sex.presence.Mass.pdf", sep = ""), width = 5, 
    height = 5) # open as a pdf and save to graph folder.
ggplot(An.d, aes(x = Mass)) + geom_density() + facet_wrap(presence ~sex)
dev.off() #close pdf 

#From the initial visualization of the data it appears that on average males are 
#larger than females but there is no difference between presence and no presence 

#==========================Model the Data======================================
# Now fit a factorial analysis of varience
model <- aov(Mass~Presence  + Sex * Island)

summary.lm(model)


#Try to simplify the model. 
model.s <-  aov(Mass~Presence+Sex+Island)
summary.lm(model.s)

#Model.1 with presence + sex
model.1 <- aov(Mass~Presence+Sex)
summary.lm(model.s)

# model 2 with comp + island 
model.2 <- aov(Mass~Presence+Island)
summary.lm(model.2)

#model 3 effect of comp
model.3 <- aov(Mass~Presence)
summary.lm(model.3)

#model 4 effect of island
model.4 <- aov(Mass~Island)
summary.lm(model.4)

#model 5 effect of sex
model.5 <- aov(Mass~Sex)
summary.lm(model.5)

#=================Determin which model is has an Effect=========================

AIC.sum.Mass <- AIC(model,model.1,model.2,model.3,model.4,model.5)
#from this we can see that models 1 and 5 have the most effect which makes sense
#because our data does not show any signs of presence or island having an effect 

#=================Run a mixed effect model on the data=========================
#interaction plot to see the interaction between variables 

interaction.plot(Presence,Sex, Mass, fun=mean)
#we can see that presence and mean size of Mass between the sexes have no interaction 

interaction.plot(Island,Sex, Mass, fun=mean)
#same is true with islands. 


#This is so that distributions that must be non-zero can make sense of my
# data
res <- Mass +1 
qqp(res,"norm") # norm distribution fits my data best for in this context. 
# since my data  is normally distributed I can use a linear mixed model

#check lognorm distribuution 
qqp(res,"lnorm")

#Run a LMM
#library(lme4)

lmm.Mass <- lmer(Mass~presence+sex +(1|island), data = An.d, REML = FALSE)

summary(lmm.Mass)
#we can see that our random effect is not 0 so it has some sort of effect. 

#see the significance 
Anova(lmm.Mass)
#from this we can see the presence of a congener does not have an effect on Mass 
#and that sex is the main factor that influences Mass in Anole lizards. 



#========Here is where we put all our results in our results folder.======== 
#AIC output 
sum.AIC.Mass <- capture.output(AIC.sum.Mass) #capture the output of the summary of model.t . 
cat(sum.AIC.Mass, file= paste(an.path, "AIC.Models.Mass.txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  

#LMM model output 
sum.LMM.Mass <- capture.output(summary(lmm.Mass))
cat(sum.LMM.Mass, file= paste(an.path, "LMM.Mass.txt" , sep = ""))

#Anova of LMM output 
sum.AN.LM.Mass <- capture.output(Anova(lmm.Mass))
cat(sum.AN.LM.Mass, file= paste(an.path, "Anova.lm.Mass.txt" , sep = ""))










#======================Plot the linear effect model============================

#plot the fit of the model
d.t.2 <- cbind.data.frame(Presence, Mass)
head(d.t.2)
nrow(d.t.2)
tt <- 654/2
d.t.2$sex <- rep( c("M", "F"), 327 )
head(d.t.2)

str(d.t.2)


# # try to plot just those points 

plot(d.t.2$Presence, d.t.2$Mass,  xaxt = "n", type = "n", xlab = "Competition", ylab = 
       "Mass")

# text for left of the graph 
text(0.18, 0.9, "Absent", xpd = TRUE) 
text(0.28, 0.7, "Males", xpd = TRUE)
text(0.05, 0.7, "Females", xpd = TRUE)

##points for absence 
males.no.2 <- d.t.2$Mass[d.t.2$sex == "M" & d.t.2$Presence == 0] #male svl in absence 
points(jitter(rep(0.28, length(males.no.2)), 3) , males.no.2)# plot the points 
females.no.2 <- d.t.2$Mass[d.t.2$sex=="F" & d.t.2$Presence==0] #female svl in absence 
points(jitter(rep(0.05, length(females.no.2)), 3) , females.no.2) #plot the ponts 


# text for right of the graph 
text(0.85, 0.9, "Present", xpd = TRUE)
text(0.95, 0.7, "Males", xpd = TRUE)
text(0.75, 0.7, "Females", xpd = TRUE)


##points for Presence 
males.yes.2 <- d.t.2$Mass[d.t.2$sex == "M" & d.t.2$Presence == 1] #male mass in presence
points(jitter(rep(0.94, length(males.yes.2)), 1) , males.yes.2)# plot the points 
females.yes.2 <- d.t.2$Mass[d.t.2$sex=="F" & d.t.2$Presence==1] #female mass in presence
points(jitter(rep(0.75, length(females.yes.2)), 1),females.yes.2) #plot the ponts 

#==================Save this plot to our graphs folder.======================== 
pdf(file =paste(gr.path, "Main.Mass.pdf", sep = ""), width = 5, 
    height = 5) # open as a pdf and save to graph folder.


plot(d.t.2$Presence, d.t.2$Mass,  xaxt = "n", type = "n", xlab = "Competition", ylab = 
       "Mass")

# text for left of the graph 
text(0.18, 0.9, "Absent", xpd = TRUE) 
text(0.28, 0.7, "Males", xpd = TRUE)
text(0.05, 0.7, "Females", xpd = TRUE)

##points for absence 
males.no.2 <- d.t.2$Mass[d.t.2$sex == "M" & d.t.2$Presence == 0] #male svl in absence 
points(jitter(rep(0.28, length(males.no.2)), 3) , males.no.2)# plot the points 
females.no.2 <- d.t.2$Mass[d.t.2$sex=="F" & d.t.2$Presence==0] #female svl in absence 
points(jitter(rep(0.05, length(females.no.2)), 3) , females.no.2) #plot the ponts 


# text for right of the graph 
text(0.85, 0.9, "Present", xpd = TRUE)
text(0.95, 0.7, "Males", xpd = TRUE)
text(0.75, 0.7, "Females", xpd = TRUE)


##points for Presence 
males.yes.2 <- d.t.2$Mass[d.t.2$sex == "M" & d.t.2$Presence == 1] #male mass in presence
points(jitter(rep(0.94, length(males.yes.2)), 1) , males.yes.2)# plot the points 
females.yes.2 <- d.t.2$Mass[d.t.2$sex=="F" & d.t.2$Presence==1] #female mass in presence
points(jitter(rep(0.75, length(females.yes.2)), 1),females.yes.2) #

dev.off() #close pdf 



# Add the mean line through each data point (doesn't make sense to me)
#segments(0.90, 56.63088, x1= 1,)  #males present 
#segments(0.75, 47.66894, x1= 0.70,) #females present 

#segments(0.15, 56.58027, x1= 0.25,) #males absent 
#segments(0.01, 47.29263, x1= 0.06,) #females absent
