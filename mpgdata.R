require(dplyr)

mpgdata = read.table('D:\\AutoMPG\\auto-mpg.data')
names(mpgdata) <- c('mpg','cylinders','displacement','horsepower','weight','acceleration','model year','origin','car name')
plot(mpgdata)

par(mfrow = c(2,3))
plot(newdata$weight, newdata$mpg)
plot(1/newdata$weight,newdata$mpg)
plot(newdata$displacement, newdata$mpg)
plot(1/newdata$displacement,newdata$mpg)
plot(1/sqrt(newdata$displacement),newdata$mpg)

mpg1 = mpgdata[mpgdata$horsepower <= 50,]
mpg2 = mpgdata[mpgdata$horsepower > 50,]
reg1 = dplyr::select(mpg1, mpg, weight.inv, displacement.inv, acceleration, horsepower, year_class, cylinders, origin)
reg2 = dplyr::select(mpg2, mpg, weight.inv, displacement.inv, acceleration, horsepower, year_class, cylinders, origin)
null1  = lm(mpg ~ 1, data = reg1)
full1 = lm(mpg ~ ., data = reg1)
null2 = lm(mpg ~ 1, data = reg2)
full2 = lm(mpg ~ ., data = reg2)


full1 = lm(mpg~., data = reg1)
bc = boxcox(full1)
lambda1 = with(bc, x[which.max(y)])
lambda1
reg1$mpg.bc = (reg1$mpg^lambda1 - 1)/lambda1
reg1 = dplyr::select(reg1, -one_of(c('mpg')))
full1 = lm(mpg.bc ~ ., data = reg1)
summary(full1)

reg2 = dplyr::select(reg2, -one_of(c('acceleration', 'origin')))
full2 = lm(mpg~., data = reg2)
bc = boxcox(full2)
lambda2 = with(bc, x[which.max(y)])
lambda2
reg2$mpg.bc = (reg2$mpg^lambda2 - 1)/lambda2
reg2 = dplyr::select(reg2, -one_of(c('mpg')))
full2 = lm(mpg.bc ~ ., data = reg2)
summary(full2)