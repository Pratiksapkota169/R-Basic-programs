###vectors,data,matrices and subsetting
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
x*y  #element wise operation of vectors
x/y
x^y
x[2] #accessing elements of vectors
x[2:3]
x[-2] # - indicates removes the element 2 from x and return the subsetted vector
x[-c(1,2)]
z=matrix(seq(1:12),4,3)
z
z[3:4,2:3]
z[,2:3]
z[,1] # this became a vector and dropped its matrix status
z[,1,drop=FALSE] #subsetting to maintain matrix status and not vector as above
dim(z)
ls()
rm(y)
ls()
###Generating random data and graphics
x=runif(50)  ##random unifrom generating 50 random values
y=rnorm(50)  ##random normal variables
plot(x,y)
plot(x,y,xlab="Random Uniform", ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1)) #power- set row =2 for the plot screen is half than before for plot
plot(x,y)
hist(y)
par(mfrow=c(1,1))

###Reading in data
Auto=read.csv("Auto.csv",header= TRUE)
names(Auto)
dim(Auto)
class(Auto) #class of order (dataframes where columns can have different kind factors, variables,matrices,continous)
summary(Auto) #statistics for numerical,summary for categorical
plot(Auto$cylinders,Auto$mpg)
attach(Auto) # auto variables becomes accessible in  workspace. We have attached auto data frame in workspace
search() ##various workspaces
plot(cylinders,mpg)
cylinders~as.factor(cylinders)


