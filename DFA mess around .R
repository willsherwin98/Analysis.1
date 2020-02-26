#LDFA mess around 

# Data 
Bones <- read.csv("~/Desktop/Quest/Quest Year 4 /Semester 2/Keystone /KEYSTONE /Data/ Bones.csv")


#clean Data 
#First, make a data frame including only the relevent colums and check 
#that it worked using the head function 
data.DFA <-data.frame(Bones$island, Bones$sagrei.present,Bones$sex,Bones$whole.head,
                    Bones$snout, Bones$braincase.width, Bones$head.width.retroarticulars,
                    Bones$head.width.jugals, Bones$head.width.quadrates,Bones$real.lower.jaw.length)
head(data.DFA)


# Get rid of the missing data points and check that it works using the structure 
# function 

# change unk's to NAs
data.DFA[data.DFA == "unk"] <- NA
#take out NA values and see if it worked
An.DFA <- na.omit(data.DFA)
View(An.DFA)


library(MASS)
# Fit the model
model <- lda(Bones.island~., data = An.DFA)

plot(model)
# Make predictions
predictions <- model >  predict(An.DFA)
# Model accuracy
mean(predictions$class==test.transformed$Species)