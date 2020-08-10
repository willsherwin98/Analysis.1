#======================= Model selection =======================================
install.packages("bbmle")
library(bbmle)
####################### variables 
#jaw.l <- sc.t$Jaw.length.sc
#width <- sc.t$Width.sc
#height <- sc.t$Height.sc
#out.l <- sc.t$Outlever.sc
#open.in <- sc.t$Open.inlever.sc
#close.in <- sc.t$Close.inlever.sc
#head.l <- sc.t$Head.length.sc
sex <- sc.t$sex
pres <- sc.t$presence
isl <- sc.t$island
trait <-  c("Jaw.length.sc","Width.sc", "Height.sc", "Outlever.sc", 
            "Open.inlever.sc", "Close.inlever.sc","Head.length.sc")
var.names <- colnames(sc.t)[12:18]



##################### Model 1 with presence + sex ##############################
i <- 1
for(i in 1:7){
  M1x <- aov(sc.t[,trait[i]]~pres+sex)
  
  # M1.out <- capture.output(summary.lm(M1x)) #capture the output of the summary of model.3 . 
  
  #cat(M1.out, file= paste(an.path,var.names[i],"m1.lm.txt" , sep = "")) 
  
  
  
  ######################## model 2 with comp + island ############################
  j <- 1
  for(j in 1:7){
    M2x <- aov(sc.t[,trait[j]]~pres+isl)
    
    #M2.out <- capture.output(summary.lm(M2x)) #capture the output of the summary of model.3 . 
    
    #cat(M2.out, file= paste(an.path,var.names[j],"m2.lm.txt" , sep = ""))
    
    
    
    #########################  model 3 effect of competition  ##############################
    t <- 1
    for(t in 1:7){
      M3x <- aov(sc.t[,trait[t]]~pres)
      
      #M3.out <- capture.output(summary.lm(M3x)) #capture the output of the summary of model.3 . 
      
      #cat(M3.out, file= paste(an.path,var.names[t],"m3.lm.txt" , sep = ""))
      
      
      ######################### model 4 effect of island ###########################
      e <- 1
      for(e in 1:7){
        M4x <- aov(sc.t[,trait[e]]~isl)
        
        #M4.out <- capture.output(summary.lm(M4x)) #capture the output of the summary of model.3 . 
        
        # cat(M4.out, file= paste(an.path,var.names[e],"m4.lm.txt" , sep = ""))
        
        
        ######################## model 5 effect of sex   ################################
        y <- 1
        for(y in 1:7){
          M5x <- aov(sc.t[,trait[y]]~sex)
          
          # M5.out <- capture.output(summary.lm(M5x)) #capture the output of the summary of model.3 . 
          
          #cat(M5.out, file= paste(an.path,var.names[y],"m5.lm.txt" , sep = ""))
          
          aic.out <- capture.output(AICctab(M1x,M2x,M3x,M4x,M5x, weights = TRUE)) # Here is where we compare all the models (AIC)
          cat(aic.out,file=paste(an.path,var.names[y],"aic.txt", sep = ""))
        }
      } 
    }
  }
}


#========================= End 




###########         Models for SD of trophic traits ################

# model.1  sc trait ~  sex +# species + (1|island) 

# model.2  sc trait ~sex * # species + (1|island) 

# model.3 sc trait ~sex + # species + sex * # species + (1|island)

#==========================Variables for the model loop.======================== 
expl.variable <-  c("Jaw.length.sc","Width.sc", "Height.sc", "Outlever.sc", 
                    "Open.inlever.sc", "Close.inlever.sc","Head.length.sc")  #The position of each explanatory column
var.names <- colnames(sc.t)[12:18]  #here is where the names are assigned

#==================== loop for model 1 =========================================
# model.1  sc trait ~  sex +# species + (1|island)  ## W/O interaction 
i <- 1
for(i in 1:7){ #create a for loop to test each trophic trait 
  
  model.1 <- lmer(sc.t[,expl.variable[i]] ~ sc.t$sex + sc.t$presence + 
                    (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each explanatory variable.
  
  #sum.t <- capture.output(summary(model.1)) #capture the output of the summary of model.1 . 
  
  #cat(sum.t, file= paste(an.path,var.names[i],"M1.txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  
  
  
  
  
  
  #==================== loop for model 2 ================================================
  # model.2  sc trait ~sex * # species + (1|island) ## With interaction of presence 
  
  j <- 1
  for(j in 1:7){ #create a for loop to test each tropic trait 
    
    model.2 <- lmer(sc.t[,expl.variable[j]] ~ sc.t$sex * sc.t$presence + 
                      (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each trait.
    
    #sum.t <- capture.output(summary(model.2)) #capture the output of the summary of model.2 . 
    
    #cat(sum.t, file= paste(an.path,var.names[j],"M2.txt" , sep = "")) # paste as a txt file in the results folder.  
    
    ##############       End loop for model 2        ##############
    
    
    
    #==================== loop for model 3 ================================================
    # model.3 sc trait ~sex + # species + sex * # species + (1|island) 
    
    t <- 1
    for(t in 1:7){ #create a for loop to for each trophic trait.  
      
      model.3 <- lmer(sc.t[,expl.variable[t]] ~ sc.t$sex + sc.t$presence + sc.t$sex
                      * sc.t$presence + (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each sc trait .
      
      
      #sum.t <- capture.output(summary(model.3)) #capture the output of the summary of model.3 . 
      
      #cat(sum.t, file= paste(an.path,var.names[t],"M3.txt" , sep = "")) # paste as a txt file in the results folder.  
      
      AIC.OUT <- capture.output(AICctab(model.1,model.2,model.3, weights = TRUE)) # Here is where we compare all the models (AIC)
      cat(AIC.OUT,file=paste(an.path,var.names[i],"ModAic.txt", sep = ""))
    }
  }
}

##############       End loop for models 1-3       ##############



# Here is the full model selection#
#
#
#


hist(sc.t$Jaw.length)


# standardize the explanatory varaible (presence)

presence2 <- scale(sc.t$presence, center = TRUE, scale = TRUE)


# fit a basic lm

lm1 <- lm(Jaw.length~presence2, data = sc.t)
summary(lm1)

#prelimanary plot 
prelim.plot <- ggplot(sc.t, aes(x = presence2, y = Jaw.length)) + geom_point() +
  geom_smooth(method ="lm")


# plot the residuals 
plot(lm1, which = 1)

#plot a qqplot 
plot(lm1, which = 2) # qq are wayy off 


# is our data independent? examine if they are similar across islands. 

boxplot(Jaw.length ~ island, data = sc.t) # does not appear to be much variation 


#look at data split by island 

(split.p <- ggplot(aes(presence,  Jaw.length), data = sc.t)+ geom_point() + 
    facet_wrap(~ island)+ xlab("presence")+ ylab("Jaw")) # jaw length apears to be unnafected by presence of a congener 


#see how much varaition is explanied by the random effect of island

mix.lm <- lmer(Jaw.length ~ sex * presence2 + (1|island), data = sc.t)
summary(mix.lm) 

#check assumptions 
plot(mix.lm)

#qq
qqnorm(resid(mix.lm))
qqline(resid(mix.lm))  





##         Models for SD of trophic traits ################

# model.1  sc trait ~  sex + # species + (1|island) 

# model.2  sc trait ~ sex * # species + (1|island) 

# model.3 sc trait ~sex + # species + sex * # species + (1|island)

#==========================Variables for the model loop.======================== 
expl.variable <-  c("Jaw.length.sc","Width.sc", "Height.sc", "Outlever.sc", 
                    "Open.inlever.sc", "Close.inlever.sc","Head.length.sc")  #The position of each explanatory column
var.names <- colnames(sc.t)[12:18]  #here is where the names are assigned

#==================== loop for model 1 =========================================
# model.1  sc trait ~  sex +# species + (1|island)  ## W/O interaction 
i <- 1
for(i in 1:7){ #create a for loop to test each trophic trait 
  
  model.1 <- lmer(sc.t[,expl.variable[i]] ~ sc.t$sex + sc.t$presence + 
                    (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each explanatory variable.
  
  #sum.t <- capture.output(summary(model.1)) #capture the output of the summary of model.1 . 
  
  #cat(sum.t, file= paste(an.path,var.names[i],"M1.txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  
  
  
  
  
  
  #==================== loop for model 2 ================================================
  # model.2  sc trait ~sex * # species + (1|island) ## With interaction of presence 
  
  j <- 1
  for(j in 1:7){ #create a for loop to test each tropic trait 
    
    model.2 <- lmer(sc.t[,expl.variable[j]] ~ sc.t$sex * sc.t$presence + 
                      (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each trait.
    
    #sum.t <- capture.output(summary(model.2)) #capture the output of the summary of model.2 . 
    
    #cat(sum.t, file= paste(an.path,var.names[j],"M2.txt" , sep = "")) # paste as a txt file in the results folder.  
    
    ##############       End loop for model 2        ##############
    
    
    
    #==================== loop for model 3 ================================================
    # model.3 sc trait ~sex + # species + sex * # species + (1|island) 
    
    t <- 1
    for(t in 1:7){ #create a for loop to for each trophic trait.  
      
      model.3 <- lmer(sc.t[,expl.variable[t]] ~ sc.t$sex + sc.t$presence + sc.t$sex
                      * sc.t$presence + (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each sc trait .
      
      
      #sum.t <- capture.output(summary(model.3)) #capture the output of the summary of model.3 . 
      
      #cat(sum.t, file= paste(an.path,var.names[t],"M3.txt" , sep = "")) # paste as a txt file in the results folder.  
      
      AIC.OUT <- capture.output(AICctab(model.1,model.2,model.3, weights = TRUE)) # Here is where we compare all the models (AIC)
      cat(AIC.OUT,file=paste(an.path,var.names[i],"ModAic.txt", sep = ""))
    }
  }
}

##############       End loop for models 1-3       ##############

install.packages("sjstats")
library(sjstats)

effectsize::standardize_parameters(model = model.2)


