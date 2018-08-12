# importing data into R environment. the session directory has been set up 
# using the session tab.

car_set <- read.csv("CarPrice_Assignment.csv")
View(car_set)
str(car_set)

# calling all the required packages required later here itself

require(tidyr)   # [1]
library(plyr)    # [2]
require(ggplot2) # [3]
require(dplyr) # [4]

# derived metrics

#[1]
car_set <- car_set %>% separate(CarName, c('CAR_MAKE','NAME_OF_CAR'),sep = " ")
View(car_set)
str(car_set)
car_set$CAR_MAKE <- as.factor(car_set$CAR_MAKE)
str(car_set)  

# data cleaning starts here  
# the dataset had some bad values in the CAR_MALE & enginetype categorical variables - the same category was being represnted
# using multiple levels. Which is wrong & hence needed to be replaced keeping the categorical nature intact.
# this was achieved by using the levels function on the CAR_MAKE variable.
# I checked several sources & saw there is no such enginetype as 'DOHCV' type. It is most probably a typing mistake & hence an outlier 
# source - https://www.caranddriver.com/porsche/cayenne. Based on this, I have converted the DONCV type to DOHC. 

levels(car_set$CAR_MAKE)

levels(car_set$CAR_MAKE) <- tolower(levels(car_set$CAR_MAKE)) 

#[2]
revalue(car_set$CAR_MAKE, c("vokswagen" = "volkswagen")) -> car_set$CAR_MAKE
revalue(car_set$CAR_MAKE, c("vw" = "volkswagen")) -> car_set$CAR_MAKE
revalue(car_set$CAR_MAKE, c("toyouta" = "toyota")) -> car_set$CAR_MAKE
revalue(car_set$CAR_MAKE, c("maxda" = "mazda")) -> car_set$CAR_MAKE

levels(car_set$CAR_MAKE)

revalue(car_set$enginetype, c("dohcv" = "dohc")) -> car_set$enginetype

levels(car_set$enginetype)

# na check. This is a very important step in data cleaning. The outcome of this will say for each variable if there are NA values present / not.
# under each variable, we see false. hence no NA values present.


apply(car_set, 2, function(x) any(is.na(x)))


# check for duplicated records. This is another very important step in data cleaning process.
# the 0 in the return value goes to show that there are no duplicates in the data set.

sum(duplicated(car_set))


# Car_ID-This Variable is similar to Row_number 
# The Car_ID variable is not required in the analysis. It is okay to remove the column for linear regression fitting

summary(car_set$car_ID)
car_set<- car_set[,-1]
View(car_set)

# similarly, we are not going to use the column NAME_OF_CAR column anywhere in our model building exercise.
# Hence, it makes complete sense to drop it right here from the dataset

summary(car_set$NAME_OF_CAR)
car_set <- car_set[, -3]
View(car_set)



# Performing EDA to find corellation between predictor/dependat variables & identify outliers to remove them.
# For each of the categorical variables, I have created boxplots and have found relation between the values and the predictor 
# variable- price & drawn conclusions about the data from this. For the numeric variables, in addition to this, 
# I have used the scatter plot & quantile to find data points that vary randomly in between consecutive quantile values & 
# thus, these have been treated as outliers. 


# Symboling-Insurance Risk Rating[Pretty Safe-Risky on a scale of -3 to 3] 
# It is Categorical in nature. Though after importing it is int, the change to categorical will be handled later 

table(car_set$symboling)

# From the summary it is clear that there are very few cars with very safe rating.
# Majority of the cars have a rating of either 0 or +1

#[3]
symboling_price<- ggplot(data = car_set, aes(x=as.factor(car_set$symboling), y=car_set$price))+ geom_boxplot() + xlab("Symboling")+ ylab("Pricing") + ggtitle("Symboling vs. Price")
symboling_price

# the boxplot goes on to show that 1st quartile values of symboling & pricing are negatively correlated till symboling =  1
# past that, the two start to show positive correlation.till symboling = +3


# Car Company Analysis- <Categorical Variable>

table(car_set$CAR_MAKE)

# From the frequency table it is clear that in the dataset, there is only one car from the company = Mercury. 
# Followed <=3 cars from Alfa-Romero, chevrolet, Jaguar and Renault. 
# Toyota has the most number of cars in the dataset.


company_price<- ggplot(data = car_set, aes(x=car_set$CAR_MAKE, y=car_set$price))+ geom_boxplot()+ xlab("Car Company Name")+ ylab("Price in USD") + ggtitle("Plt2. Car Company vs. Price")
company_price

# From the price boxplot it is clear that the brands with the lowest priced cars belong to CHEVROLET
# while most expensive vehicles in the dataset (i.e. >30,000 USD) 
# belong to BUIC, JAGUAR, PORSCHE and BMW. It is difficult to say what is the median of all the data in the set looking at the plot only

# Fuel Type Analysis- <Categorical Variable>

table(car_set$fueltype)

# There are a lot more cars in the dataset runnning on Gas than which are running on Diesel

fueltype_price<- ggplot(data = car_set, aes(x=car_set$fueltype, y=car_set$price))+ geom_boxplot()+ xlab("Fuel-Type")+ ylab("Price in USD") + ggtitle("Fuel-Type vs. Price")
fueltype_price

# The median price of Gas vehicles is lower than that of Diesel Vehicles. However, 
# due to a few cars, the skewness of this parameter is pretty high and the highest priced cars are infact those that run on GAS though the majority of them are priced lower than median price of Diesel!
# GAS type vehicles also seems to have some values that are far off from the median of these types of car


#  Aspiration- <Categorical Variable>

table(car_set$aspiration)

# There are a lot more standard aspiration vehicles than Turbo aspirated in the dataset.

aspirate_price<- ggplot(data = car_set, aes(x=car_set$aspiration, y=car_set$price))+ geom_boxplot()+ xlab("Engine Aspiration")+ ylab("Price in USD") + ggtitle("Engine Aspiration vs. Price")
aspirate_price

# 75th percentile of standard aspirated vehicles have a price lower than the median price of turbo aspirated vehicles. 

# Number of Doors- <Categorical Variable>

table(car_set$doornumber)

# There is almost an equal spread of 2 and 4 doored vehicles in the dataset.

doornumber_price<- ggplot(data = car_set, aes(x=car_set$doornumber, y=car_set$price))+ geom_boxplot()+ xlab("Number of Doors")+ ylab("Price in USD") + ggtitle("Number of Doors vs. Price")
doornumber_price

# Two and Four Door vehicles are almost equally priced.
# There are however some outliers in the price of both 4 & 2 door vehicles. 
# as expected, the most expensive vehicles in the dataset have two doors.

# Carbody Type- <Categorical Variable>

table(car_set$carbody)

# Sedans and Hatchbacks contribute to most of the vehicles in this dataset.

carbody_price<- ggplot(data = car_set, aes(x=car_set$carbody, y=car_set$price))+ geom_boxplot()+ xlab("Type of car body")+ ylab("Price in USD") + ggtitle("Carbody vs. Price")
carbody_price

# Hatchback vehicles have the lowest median price of vehicles in the data set sedans seems to have the highest skewness
# in the dataet. Hardtop category of vehicles have the highest pricing


# Drivewheel Type- <Categorical Variable>

table(car_set$drivewheel)

# Front wheel drive and rear wheel drive vehicles contribute to almost 95% of the vehicles in the dataset.

drivewheel_price<- ggplot(data = car_set, aes(x=car_set$drivewheel, y=car_set$price))+ geom_boxplot()+ xlab("Drivewheel Type")+ ylab("Price in USD") + ggtitle("Drivewheel vs. Price")
drivewheel_price

# Vehicles with Rearwheel drive demand the highest prices. The median price of vehicles with RwD is significantly high.
# Also rearwheel & frontwheel drive vehicles have quite a bit of skewness in the dataset. Frontwheel drive (FWD) vehicles having the lowest median price.

# Engine Location- <Categorical Variable>

table(car_set$enginelocation)


# Almost all vehicles in the dataset have engines placed in the front of the vehicle. 
# The number of cars with engine in the rear makes it seemingly insignificant in the dataset


engineloc_price<- ggplot(data = car_set, aes(x=car_set$enginelocation, y=car_set$price))+ geom_boxplot()+ xlab("Engine Location")+ ylab("Price in USD") + ggtitle("Engine Location vs. Price")
engineloc_price 

# Howver the cars with engine in the rear are the ones that are priced pretty high. 
# The median vaue is way higher than cars with engine in the front. Though, interestingly enough, the highest priced model 
# seems to be having a front engine


# Wheelbase Analysis- <Numeric Variable>
# 99%  of the dataset have a wheel base between 86.6-115.544

summary(car_set$wheelbase)
boxplot(car_set$wheelbase)
title(main = "Boxplot of Wheelbase", ylab = "Wheelbase")
quantile(car_set$wheelbase, seq(0,1,0.01))

# Outlier Treatment: There is a significant jump in the wheelbase between 99-100 percentile therefore we will cap
# wheelbase at 99th Percentile with 115.544 units.

car_set$wheelbase[which(car_set$wheelbase > 115.54)]<- 115.54

# Plotting a Scatterplot

wheelbas_price<- ggplot(data = car_set, aes(x=car_set$wheelbase, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Wheelbase")+ ylab("Price in USD") + ggtitle("Wheelbase vs. Price")
wheelbas_price

# This scatter plot tells us that the lowest price range is for vehicles having a wheelbase of 
# around 95 following which the price increases with increase in wheelbase. Initially the two are negatively co-rellated till 
# wheelbase of size 95. Post which, it is positively co-rellated

# Carlength Analysis- <Numeric Variable>
# Data seems to be evenly spread out with one lower range outlier - 141.1
# majority of the data lies between 166.3 - 183.1

summary(car_set$carlength)
boxplot(car_set$carlength)
title(main = "Boxplot of Carlength", ylab = "Carlength")
quantile(car_set$carlength, seq(0,1,0.01))

# Outlier Treatment is not required

#Plotting a Scatterplot

carleng_price<- ggplot(data = car_set, aes(x=car_set$carlength, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carlength")+ ylab("Price in USD") + ggtitle("Carlength vs. Price")

carleng_price

# There seems to be a general trend increase in the price of the vehicle with higher carlength.
# car Price & car length seems to be positively co-rellated.

# Carwidth Analysis- <Numeric Variable>
# Majority of vehicles seem to have a carwidth between 64-68 units. 

summary(car_set$carwidth)
boxplot(car_set$carwidth)
title(main = "Boxplot of Carwidth", ylab = "Carwidth")
quantile(car_set$carwidth, seq(0,1,0.01))

# No sudden spikes therefore outlier treatment is not required.

# plotting a Scatterplot

carwidth_price<- ggplot(data = car_set, aes(x=car_set$carwidth, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carwidth")+ ylab("Price in USD") + ggtitle("Carwidth vs. Price")
carwidth_price

# There seems to be a general trend increase in the price of the vehicle with increase in carwidth.
# Both of these seem to be positively co-rellated

# Carheight Analysis- <Numeric Variable>
#Vehicle height seems to be evenly spread with no sudden spikes. 
# Majority of vehicle heights are between 52-55.5

summary(car_set$carheight)
boxplot(car_set$carheight)
title(main = "Boxplot of Carheight", ylab = "Carheight")
quantile(car_set$carheight, seq(0,1,0.01))

# plotting a scatterplot between Carheight & vehicle price

carheight_price<- ggplot(data = car_set, aes(x=car_set$carheight, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carheight")+ ylab("Price in USD") + ggtitle("Plt14. Carheight vs. Price")
carheight_price

# The plot shows some periodic fluctuation with a low price for vehicles with a height between 51-55.
# Followed by and increase in price between 55-57.5 then followed by a drop in price. too much fluctuation to deduce anything significant

# Curbweight Analysis- <Numeric Variable>
# Majority of the vehicles seem to have a curb weight between 2200-3000 kg. 
# When observing the spread there is a slight peak in weight between 0-1st percentile. 
# however, it is okay to be left alone

summary(car_set$curbweight)
boxplot(car_set$curbweight)
title(main = "Boxplot of Curbweight", ylab = "Curbweight")
quantile(car_set$curbweight, seq(0,1,0.01))

# plotting a scatterplot

curbwt_price<- ggplot(data = car_set, aes(x=car_set$curbweight, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Curbweight")+ ylab("Price in USD") + ggtitle("Plt15. Curbweight vs. Price")
curbwt_price

# From the scatter plot it is clear that the price of the vehicle increases with an increase in the curbweight.
# Both of these are positively co-rellated

# Enginetype Analysis- <Catagorical Variable>

table(car_set$enginetype)

# Vehicles with OHC (overhead head cam) and OHCF engine type contribute to 80% of the vehivles in the dataset.


# Cylinder Number Analysis- <Categorical Variable>
# Vehicles with four and six cylinder engines contribute to 90% of the dataset. 
# There is one record entry for a vehicle having 3 or 12 cylinders respetively.


table(car_set$cylindernumber)

# Engine Size Analysis- <Numeric Variable>

summary(car_set$enginesize)
boxplot(car_set$enginesize)
title(main = "Boxplot of Engine Size", ylab = "Engine Size")
quantile(car_set$enginesize, seq(0,1,0.01))

# There are significant spikes in the engine size 2-3 percentile, 49-50 percentile and 98-99%.
# However, the engine size is collinear
# with number of cylinders and for each discrete value increase in the number of cylinders there might
# be significant spike in engine size. Trying to cap these values might disturb the collinearity relation
# and hence I have let the values remain as is


enginesize_cylindernumber <- plot(x = car_set$cylindernumber, y = car_set$enginesize)

# from the above boxplot, we can justify out theory of the relationship between number of cylinders & enginesize 
# the heaviest engine size is the one with the highest number of cylinders - 12
# and hence no outlier treatment is needed for this variable


# Fuel System Analysis- <Categorical Variable>
# MPFI and 2BBL fuel injection systems Contribute to a majority number of the vehicles in the dataset

table(car_set$fuelsystem)

# Bore-Ratio Analysis- <Numeric Variable>
# From the percentile summary and the boxplot it is clear that engine bore ratio is fairly evenly spread 
# out with a majority of vehicles having bore ratio between 3.15-3.58

summary(car_set$boreratio)
boxplot(car_set$boreratio)
title(main = "Boxplot of Engine Bore Ratio", ylab = "Bore Ratio")
quantile(car_set$boreratio, seq(0,1,0.01))

# scatter plot

borerat_price<- ggplot(data = car_set, aes(x=car_set$boreratio, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Engine Bore Ratio")+ ylab("Price") + ggtitle("Engine Bore Ratio vs. Price")
borerat_price

# From the scatterplot it is clear that there seems to be a general trend increase in price with an 
# increase in bore-ratio untill about Engine-Bore Ratio = 3.0. Till then the 2 variables seem to be negatively
# corellated to each other

# Stroke Length Analysis- <Numeric Variable>
# There is a significant jump in in stoke length from 1-2 percentile

summary(car_set$stroke)
boxplot(car_set$stroke)
title(main = "Boxplot of Engine Stroke Length", ylab = "Stroke Length")
quantile(car_set$stroke, seq(0,1,0.01))


# Outlier Treatment:
car_set$stroke[which(car_set$stroke<2.64)]<-2.64

# scatter plot

stroke_vol_price<- ggplot(data = car_set, aes(x=car_set$stroke, y=car_set$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Stroke/Volume Inside the Engine")+ ylab("Price") + ggtitle("Engine Stroke vs. Price")
stroke_vol_price

# the corellation between stroke & price is highly unpredictable until about Stroke value 3.4
# after that, there is a clear positive corellation
# Compression Ratio- <Numeric Variable>

summary(car_set$compressionratio)
boxplot(car_set$compressionratio) 
title(main = "Boxplot of Engine Compression Ratio", ylab = "Compression Ratio")
quantile(car_set$compressionratio, seq(0,1,0.01))

# From the engine compression ratio boxplot and quantile summary it is clear that majority of the datapoints 
# are between 8.6-9.4.
# However there is a significant jump in the compression ratio from the 90-91 percentile. 
# Therefore we will cap all outlier values at 10.94

# Outlier Treatment:

car_set$compressionratio[which(car_set$compressionratio>10.94)]<-10.94

# post outlier treatment boxplot

boxplot(car_set$compressionratio)

# a much more compressed variable

# Horsepower Analysis- <Numeric Variable>

summary(car_set$horsepower)
boxplot(car_set$horsepower) 
title(main = "Boxplot of Horsepower", ylab = "Horsepower")
quantile(car_set$horsepower, seq(0,1,0.01))

# The boxplot and the percentile summary show that majority of vehicles have horsepower between 70-116 hp. 
# However there is a significant jump at 97-98%. So, the outliers must be removed & all values above 184 
# must be capped to 184

car_set$horsepower[which(car_set$horsepower > 184.0)]<- 184.0

# Peak RPM Analysis- <Numeric Variable>

summary(car_set$peakrpm)
boxplot(car_set$peakrpm) 
title(main = "Boxplot of Peak RPM", ylab = "Peak RPM")
quantile(car_set$peakrpm, seq(0,1,0.01))

# The peak rpm is almost evenly distributed with majority of the dataset having peak rpm between 4800-5600 rpm. 
# However there is a significant spike between 99-100 percentlie. All values above 6000 must be capped at 6000

car_set$peakrpm [which(car_set$peakrpm > 6000)]<- 6000


# City MPG- <Numeric Variable>

summary(car_set$citympg)
boxplot(car_set$citympg) 
title(main = "Boxplot of City MPG", ylab = "City MPG")
quantile(car_set$citympg, seq(0,1,0.01))

# The boxplot and the percentile summary suggests that majority of the vehicles in the dataset have a city mileage 
# between 19-31 mpg. However there is a significant spike at 98-99 percentile. All values above 38 must be capped at 38

car_set$citympg [which(car_set$citympg > 38)] <- 38

# Highway MPG- <Numeric Variable>

summary(car_set$highwaympg)
boxplot(car_set$highwaympg) 
title(main = "Boxplot of Highway MPG", ylab = "Highway MPG")
quantile(car_set$highwaympg, seq(0,1,0.01))

# From the boxplot and the percentile summary it is clear that majority of vehicles have a highway 
# mileage between 25-34 mpg. However, there is a spike between 98-99% percentile. All values above 46.92 must be capped at 46.92

# Outlier Treatment

car_set$highwaympg [which(car_set$highwaympg > 46.92)] <- 46.92



# EDA for all the dependent attributes in the dataset has been performed. 
# All outliers have been identified and treated accordingly. 






# converting categorical to numeric

summary(car_set$fueltype)
levels(car_set$fueltype)<-c(1,0)
car_set$fueltype <- as.numeric(levels(car_set$fueltype))[car_set$fueltype]
class(car_set$fueltype)
summary(car_set$fueltype)


summary(car_set$aspiration)
levels(car_set$aspiration)<-c(1,0)
car_set$aspiration <- as.numeric(levels(car_set$aspiration))[car_set$aspiration]
class(car_set$aspiration)
summary(car_set$aspiration)

summary(car_set$doornumber)
levels(car_set$doornumber)<-c(1,0)
car_set$doornumber <- as.numeric(levels(car_set$doornumber))[car_set$doornumber]
class(car_set$doornumber)
summary(car_set$doornumber)

summary(car_set$enginelocation)
levels(car_set$enginelocation)<-c(1,0)
car_set$enginelocation <- as.numeric(levels(car_set$enginelocation))[car_set$enginelocation]
class(car_set$enginelocation)
summary(car_set$enginelocation)

# deriving new columns from the available set
# After doing some research, I found CC to be a viable new column & indicative of a car's proeperty
# the value of CC is calculated by multiplying the value 15.5 to horsepower. 
# the factor to be multiplied is between 14 & 17. So we use the mean. 

car_set$CC <- car_set$horsepower* 15.5
View(car_set)

# In the automobile sector the power(hp)/curbweight ratio is an important parameter. Usually vehicles with a 
# high power to weight ratio are performance vehicles and will therefore be priced higher than regular vehicles.

car_set$powerwtrat<- round(car_set$horsepower/car_set$curbweight, 3)
View(car_set)

# Carbody Atrribute has 5 levels. We will now create 4 dummy variable and then attach it to 
# the main dataframe. this exercise will involve removing the 1st dummy variable that is created as a result
# of using the model.matrix command as for a variable with n different levels, we need n-1 levels of dummy variables

car_set$carbody<- as.factor(car_set$carbody)


temp_dum1<- data.frame(model.matrix(~carbody, data = car_set))
temp_dum1<- temp_dum1[,-1]
car_set<- cbind(car_set[,-6], temp_dum1)


# Drivewheel Attribute has 3 levels. We will create 2 dummy variables and attach it 
# to the main dataframe. this exercise will involve removing the 1st dummy variable that is created as a result
# of using the model.matrix command as for a variable with n different levels, we need n-1 levels of dummy variables


table(car_set$drivewheel)
car_set$drivewheel<- as.factor(car_set$drivewheel)

# We have converted it to a factor. Now we will create 2 dummy variables and store it in temp_dum dataframe.

temp_dum2<- data.frame(model.matrix(~drivewheel, data = car_set))
temp_dum2<- temp_dum2[,-1]
car_set<- cbind(car_set[,-6],temp_dum2)
View(car_set)

# Symboling is a scaled insurance risk rating given to a car. It assigns values based on a scale of -3 to +3 synonymous to Pretty Safe to Risky. 
# In our dataset we have ratings from -2 to +3. I have converted the numeric type of levels to character types

table(car_set$symboling)
car_set$symboling<- as.factor(car_set$symboling)

# We have converted it to a factor. Now we will create 5 dummy variables and store it in temp_dum dataframe.

temp_dum3<- data.frame(model.matrix(~symboling, data = car_set))
temp_dum3<- temp_dum3[,-1] 
names(temp_dum3)<- c("symboling_level[-1]","symboling_level[0]", "symboling_level[+1]", "symboling_level[+2]", "symboling_level[+3]")
car_set<- cbind(car_set[,-1], temp_dum3)


# Car Company has 22 levels with each level representing a car company name. 
# We will create 21 dummy variables and attach it to the maindataframe

table(car_set$CAR_MAKE)
car_set$CAR_MAKE<- as.factor(car_set$CAR_MAKE)
temp_dum4<- data.frame(model.matrix(~CAR_MAKE, data = car_set))
temp_dum4<- temp_dum4[,-1]
car_set<- cbind(car_set[,-1], temp_dum4)
View(car_set)


# Car Engine Type has 6 levles with each level representing the type of engine used in the vehicle. 
# We will create 5 dummy variables and attach it to the maindataframe

table(car_set$enginetype)
car_set$enginetype<- as.factor(car_set$enginetype)
temp_dum5<- data.frame(model.matrix(~enginetype, data = car_set))
temp_dum5<- temp_dum5[,-1]
car_set<- cbind(car_set[,-10], temp_dum5)
View(car_set)

# Cylinder Number has 7 levels with each level representing the number of cylinders in the engine of the vehicle. We will create 6 dummy variables and attach it to the main dataframe

table(car_set$cylindernumber)
car_set$cylindernumber<- as.factor(car_set$cylindernumber)
temp_dum6<- data.frame(model.matrix(~cylindernumber, data = car_set))
temp_dum6<- temp_dum6[,-1]
car_set<- cbind(car_set[,-10], temp_dum6)


# Fuel System has 8 levels with each level representing the fuel injection system in the vehicle. We will create 7 dummy variables and attach it to the main dataframe


table(car_set$fuelsystem)
car_set$fuelsystem<- as.factor(car_set$fuelsystem)
temp_dum7<- data.frame(model.matrix(~fuelsystem, data = car_set))
temp_dum7<- temp_dum7[,-1]
car_set<- cbind(car_set[,-11], temp_dum7)


# Model Building

# The first step to model building is to make sure that we have all the variables as numeric type 

str(car_set)

# Some of the variables need conversion to numeric type. Namely - curbweight, enginesize, horsepower, peakrpm, ciympg & highwaympg


car_set$curbweight<- as.numeric(car_set$curbweight)
car_set$enginesize<- as.numeric(car_set$enginesize)
car_set$horsepower<- as.numeric(car_set$horsepower)
car_set$peakrpm<- as.numeric(car_set$peakrpm)
car_set$citympg<- as.numeric(car_set$citympg) 
car_set$highwaympg<- as.numeric(car_set$highwaympg)


# Setting the seed value for repeatability
set.seed(100)

#Since the total dataset has 205 observations we will split it into testing and training dataset using 
# the 70%-30% rule. 70% of the data will be used to train the model & 30% of the data will be udes as testing data.

training_indices<- sample(1:nrow(car_set), 0.7*nrow(car_set))
trng_set<- car_set[training_indices,]
test_set<- car_set[-training_indices,]

View(trng_set)

# first model

model_1<- lm(price~., data = trng_set)
summary(model_1)


# model 1 - adjusted R square is 0.9712 & multiple R square value is 0.9835

install.packages("MASS")
install.packages("car") 
require(MASS)
aicfilter<- stepAIC(model_1, direction = "both")

aicfilter

# after running STEP AIC, we see 21 variables can be removed straightwawy
# model 2

model_2 <- lm(price ~ CAR_MAKEhonda + stroke + `symboling_level[0]` + cylindernumbersix + fuelsystemspdi
              + `symboling_level[+3]` + CAR_MAKEchevrolet + CAR_MAKEsaab + CAR_MAKEporcshce + cylindernumberfive
              + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + carbodysedan + enginelocation + carbodyhatchback
              + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
              + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
              + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
              + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

require(car)
vif(model_2)


# looking at the VIF values, I can see the variables cylindernumberfive,cylindernumbersix, 
# cylindernumberfour, carbodysedan, enginelocation, carbodyhatchback, horsepower
# to be probable candidates of removing. However, I will check their respective
# p- values before I remove them

summary(model_2)

# cylindernumberfive, cylindernumbersix will be removed. as they are having no stars& p-value is much > .05

model_3 <- lm (price ~ CAR_MAKEhonda + stroke + `symboling_level[0]` + fuelsystemspdi
                             + `symboling_level[+3]` + CAR_MAKEchevrolet + CAR_MAKEsaab + CAR_MAKEporcshce 
                             + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + carbodysedan + enginelocation + carbodyhatchback
                             + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
                             + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                             + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                             + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)



vif(model_3)


# carbodysedan,carbodyhatchback, horsepower, powerwtrat,enginesize needs to be checked for p-vlue

summary(model_3)

# removing carbodysedan,carbodyhatchback as these two are comparitively having lower p-value 

model_4 <- lm (price ~ CAR_MAKEhonda + stroke + `symboling_level[0]` + fuelsystemspdi
               + `symboling_level[+3]` + CAR_MAKEchevrolet + CAR_MAKEsaab + CAR_MAKEporcshce 
               + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + enginelocation 
               + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
               + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
               + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
               + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)


vif(model_4)

# powerwtrat, enginesize, horsepower, fuelsystemmpfi, carwidth, CAR_MAKEhonda should be checked

summary(model_4)

# CAR_MAKEhonda needs to be removed

model_5 <- lm (price ~  stroke + `symboling_level[0]` + fuelsystemspdi
    + `symboling_level[+3]` + CAR_MAKEchevrolet + CAR_MAKEsaab + CAR_MAKEporcshce 
    + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + enginelocation 
    + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
    + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
    + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
    + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)



vif(model_5)

# from here on, we can see that there are no such variables with very high VIF value & also very high p-value
# hence, from the next iteration I am going to only rely on p-values 


summary(model_5)

# going by the p values, CAR_MAKEchevrolet seems to be the best variable to be removed.

model_6 <- lm (price ~  stroke + `symboling_level[0]` + fuelsystemspdi
               + `symboling_level[+3]` + CAR_MAKEsaab + CAR_MAKEporcshce 
               + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + enginelocation 
               + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
               + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
               + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
               + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_6)
vif(model_6)

# there is still a good value of adjusted R square in comparison to the previous iteration of 0.9724. 
# the p value this time is 0.9727

# symboling_level[3] will be removed next.


model_7 <- lm (price ~  stroke + `symboling_level[0]` + fuelsystemspdi
               + CAR_MAKEsaab + CAR_MAKEporcshce 
               + carbodyhardtop + fuelsystemmpfi + cylindernumberfour + enginelocation 
               + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
               + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
               + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
               + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)


summary(model_7)
vif(model_7)

# carbodyhardtop should be removed & checked next. the p value from the previous iteration seems to do just fine. from 0.9729 to 0.9727

model_8 <- lm (price ~  stroke + `symboling_level[0]` + fuelsystemspdi
               + CAR_MAKEsaab + CAR_MAKEporcshce 
               + fuelsystemmpfi + cylindernumberfour + enginelocation 
               + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
               + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
               + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
               + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)


summary(model_8)
vif(model_8)

# removing stroke now from the model next

model_9 <- lm (price ~ `symboling_level[0]` + fuelsystemspdi
               + CAR_MAKEsaab + CAR_MAKEporcshce 
               + fuelsystemmpfi + cylindernumberfour + enginelocation 
               + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
               + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
               + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
               + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_9)
vif(model_9)

# removing CAR_MAKEporcshce in the next iteration 

model_10 <- lm (price ~  stroke + `symboling_level[0]` + fuelsystemspdi
                + CAR_MAKEsaab + fuelsystemmpfi + cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_10)
vif(model_10)

# no significant change in R squared value 
# removing the variable stroke next

model_11 <- lm (price ~  `symboling_level[0]` + fuelsystemspdi
                + CAR_MAKEsaab + fuelsystemmpfi + cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_11)
vif(model_11)
# no significant change in R squared value
# `symboling_level[0]` removing next


model_12 <- lm (price ~ fuelsystemspdi + CAR_MAKEsaab + fuelsystemmpfi + cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)


summary(model_12)

vif(model_12)


# CAR_MAKEsaab will be removed next

model_13 <- lm (price ~    fuelsystemspdi + fuelsystemmpfi + cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + carbodywagon + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_13)
vif(model_13)

# fuelsystemspdi, carbodywagon will be removed next 

model_14 <- lm (price ~ fuelsystemmpfi + cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_14)
vif(model_14)

# Removing  fuelsystemmpfi as very high p value

model_15 <- lm (price ~  cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + drivewheelrwd + boreratio + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_15)
vif(model_15)
# the adjusted R square value reduced from 0.9714 to 0.9643 
# but it is a acceptable difference

# next boreratio needs to be removed

model_16 <- lm (price ~  cylindernumberfour + enginelocation 
                + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + drivewheelrwd + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_16)
vif(model_16)

# next cylindernumberfour will be removed

model_17 <- lm (price ~  enginelocation + CAR_MAKEporsche + fuelsystem2bbl + peakrpm + drivewheelrwd + horsepower
                + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_17)
vif(model_17)

# dropping peakrpm 

model_18 <- lm (price ~  enginelocation + CAR_MAKEporsche + fuelsystem2bbl + drivewheelrwd + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)


summary(model_18)
vif(model_18)

# removing fuelsystem2bbl 

model_19 <- lm (price ~  enginelocation + CAR_MAKEporsche + drivewheelrwd + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + aspiration + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_19)
vif(model_19)

# aspiration, CAR_MAKEporsche will be removed

model_20 <- lm (price ~  enginelocation + drivewheelrwd + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + enginetyperotor
                + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_20)
vif(model_20)
# adjusted R square value changes from .9619 to .9587. This is a permissable change
# now will need to drop enginetyperotor, drivewheelrwd as these are having single stars.

model_21 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEdodge + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + powerwtrat + CAR_MAKEmitsubishi + enginesize + CAR_MAKEbmw , data = trng_set)

summary(model_21)
vif(model_21)

# CAR_MAKEdodge to be dropped. adjusted R square value is still comparable

model_22 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEvolkswagen + CAR_MAKEtoyota + CAR_MAKEplymouth + powerwtrat + CAR_MAKEmitsubishi + enginesize 
                + CAR_MAKEbmw , data = trng_set)

summary(model_22)

# CAR_MAKEplymouth, CAR_MAKEvolkswagen to be dropped next 

model_23 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEnissan + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEmazda
                + CAR_MAKEbuick + CAR_MAKEtoyota + powerwtrat + CAR_MAKEmitsubishi + enginesize 
                + CAR_MAKEbmw , data = trng_set)

summary(model_23)
vif(model_23)

# now CAR_MAKEnissan, CAR_MAKEmazda are to be removed

model_24 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEpeugeot + CAR_MAKErenault + carwidth + CAR_MAKEsubaru + CAR_MAKEbuick + CAR_MAKEtoyota + powerwtrat + CAR_MAKEmitsubishi + enginesize 
                + CAR_MAKEbmw , data = trng_set)

summary(model_24)
vif (model_24)

# next model will be built removing CAR_MAKErenault, CAR_MAKEmitsubishi

model_25 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEpeugeot + carwidth + CAR_MAKEsubaru + CAR_MAKEbuick + CAR_MAKEtoyota + powerwtrat + enginesize 
                + CAR_MAKEbmw , data = trng_set)

summary(model_25)
vif(model_25)

# removing enginesize 

model_26 <- lm (price ~  enginelocation + horsepower + enginetypeohc + CAR_MAKEpeugeot + carwidth + CAR_MAKEsubaru + CAR_MAKEbuick + CAR_MAKEtoyota + powerwtrat
                + CAR_MAKEbmw , data = trng_set)

summary(model_26)

# I will stick to this model now. Reason being the Multiple R square & adjusted R square values are extremely close to each other
# also, all the involved variables have 3 stars & really low p values
# now the model will be put to test - using the Test data set

View(test_set)

# the original Price column needs to be removed & in it's place, the Price_predicted column is being added

test_set$Price_predicted <- predict(model_26,test_set[,-18])


cal_correl <- cor(test_set$price, test_set$Price_predicted, use = "everything")
cal_correl

cal_rsquare <- (cal_correl)^2
cal_rsquare

# FINAL COMMENTS - 
# the cal_correl calculation comes out to be .9248, shows that the price predicted is pretty close to the actual pricing 
# also, the cal_rsquare calculation of .855 goes on to show that our model can account for 85.5% of the variability of prices of vehicles.

# now to find the confidence & margin of error of the model, I have tried to find the error percentage 
# per predicted value & then try to find the percentage distribution of error

test_set$pred_error_value <- abs(test_set$price - test_set$Price_predicted)

test_set$pred_error_percentage <- round(((test_set$pred_error_value / test_set$price) * 100), 2)

# setting cars_Id for easy comparison
test_set <- tibble::rowid_to_column(test_set, "cars_ID")

View(test_set)

# plot of error percentage in price prediction per Car_Id. The model is able to predict most of the car prices below the range of 20% error
# also, there is no fixed pattern to the error values. This goes on to show that the model is fair to use

error_ <- ggplot(test_set, aes(x=test_set$cars_ID, y=test_set$pred_error_percentage))+geom_point()+ xlab("cars_ID")+ ylab("Predicted Price Error in %") + ggtitle("Plot Showing Random distribution of Predicted Error Percentage")
error_

error_frequency <- ggplot(test_set, aes(x = test_set$pred_error_percentage))+ geom_histogram() + xlab("Predicted Price Error in %") + ylab("Frequency of Occurance") 
error_frequency

error_v <- ggplot(test_set, aes(x=test_set$cars_ID, y=test_set$pred_error_value))+geom_point()+ xlab("cars_ID")+ ylab("Predicted Price Error Value") + ggtitle("Plot Showing Random distribution of Predicted Error")
error_v


# Major takeaways - 

# 1) the final model finalised to predict the car prices is model_26. Contrary to general belief, out of the 10 most influential
# variables used to determine Car price, 4 of them are not even a innate attribute of the vehicle, but it's the brand name!
# this goes on to show that if Geely Auto wants to sell cars at a higher profit margin, they should focus a good portion of their time & effort on brand marketing
# brand & awarness as they should focus on the vehicle's core attributes
# 2) the selected variables in the model describe the price of a car till about 92% taking into account a permissable 
# variance of about 85.5%
