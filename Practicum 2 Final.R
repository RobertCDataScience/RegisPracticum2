library(readxl)
library(ISLR)
library(tree)
stockx_df <- read_excel('/Users/robertcarter/Desktop/StockX-Data-Contest-2019-3.xlsx')


shoedata <- as.data.frame(stockx_df)

head(shoedata)

names(shoedata) <- tolower(gsub("\\.", "_", names(shoedata)))
names(shoedata) <- gsub("\\(", "", names(shoedata))
names(shoedata) <- gsub("\\)", "", names(shoedata))
names(shoedata) <- gsub(" ", "_", names(shoedata)) 

names(shoedata)

unique(shoedata$brand)

summary(shoedata)

attach(shoedata)

slr <- lm(sale_price ~ brand, data = shoedata)
summary(slr)


lmshoes <- lm(sale_price ~ brand  + retail_price + shoe_size, data=shoedata)

lmshoes <- lm(sale_price ~ brand + release_date + retail_price + shoe_size, data=shoedata)

summary(lmshoes)

#Does brand affect resale price
#The mean will not be different between brnads
#The mean will be diferent between brands
mean(shoedata$sale_price)
mean(shoedata$retail_price)

tapply(shoedata$sale_price, shoedata$brand, mean)
tapply(shoedata$retail_price, shoedata$brand, mean)

boxplot(retail_price ~ brand)
boxplot(sale_price ~ brand)

oneway.test(sale_price~brand)
##########################
aov.out = aov(sale_price~brand, data=shoedata)
summary(aov.out)

TukeyHSD(aov.out)


#The is no interaction between Brand and Retail Price
#There is interaction between Brand and Retail Price

res.aov3 <- aov(sale_price ~ brand * retail_price, data = shoedata)
summary(res.aov3)

TukeyHSD(res.aov3, which = "brand")

#The is no interaction between release date and shoe size
#There is interaction between release date and shoes size
res.aov4 <- aov(sale_price ~ brand * shoe_size, data =shoedata)
summary(res.aov4)

TukeyHSD(res.aov4, which = "brand")

#Predict brand
shoedata$brand_bi <- ifelse(shoedata$brand == 'Off-White', 0, 1)
glm.fit <- glm(brand_bi ~ sale_price, data = shoedata, family = binomial)
summary(glm.fit)


#######################
anova(glm.fit, test='Chisq') 

glm.probs <- predict(glm.fit,type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Yeezy", "Off-White")

table(glm.pred,shoedata$brand_bi)


#Breaking data into train and test data for the decisoin tree
#Starting with shuffling the data since the order dates are in order
#Creating new data set, making the original data set
shoedata <- as.data.frame(stockx_df)
shoedata2 <- shoedata

head(shoedata2, 20)


#Creating a variable that determines if the sale price is higher than the average sale price. Doing this determine the categories that best preict
#sale price
shoedata2$high_sale <- ifelse(shoedata2$sale_price >= 371, 'Yes', 'No')
shoedata2$high_sale <- as.factor(shoedata2$high_sale)

#Creating  a decision tree based on high sale
summary(shoedata2)
tree.shoedata2 = tree(high_sale~.-sale_price, data=shoedata2)
summary(tree.shoedata2)

plot(tree.shoedata2)
text(tree.shoedata2, pretty = 0)

tree.shoedata2

#This is where the shuffle happens. This generates a random list of the data. Mixing the data will be important in our decision tree.
set.seed(101)
shuffle_index <- sample(1:nrow(shoedata2))
head(shuffle_index)

#Creating the new dataset with the mixed up data set.
shoedata2<- shoedata2[shuffle_index, ]

#Breaking the data into train and test datasets. Splitting the data 80/20
smp_siz = floor(0.8*nrow(shoedata2))
train_ind = sample(seq_len(nrow(shoedata2)),size = smp_siz)  # Randomly identifies ther ows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =shoedata2[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=shoedata2[-train_ind,]

summary(train)

#plotting the train data
tree.train = tree(high_sale~.-sale_price, data=train)
plot(tree.train)
text(tree.train, pretty=0)

#Predict using test data
tree.pred = predict(tree.train, test, type="class")
with(test, table(tree.pred, high_sale))

