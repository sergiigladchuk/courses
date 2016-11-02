################################################################################
# This is code for the course Basic Introduction to R
# This covers most of what you need to know to get started with R but probably 
# not everything and there might well be errors in here that need to be corrected, 
# so stay alert!!!!!
# Create a folder somewhere on your computer and save this file to it together
# with the other files in the folder RIntroCourse that you got at the course
# start
# Add the path to the folder below (in the function setwd()) within quotes
# and be careful with the slashes (right kind and number!!!) It should look as
# it does below.
# You can choose to use this particular file during the course OR create one of
# your own (start a new script in you editor, save it in your working directory,
# (i.e. your newly created folder!) and write things yourself. I WOULD STRONGLY
# RECOMMEND THE LATTER...you will learn more and you can make your own (better?)
# comments etc. etc. If you choose the latter you can still have the pdf version
# of this file to look at, it is among the files you just got...
# I hope you will enjoy the course and using R
# Good luck!
# Martin Stjernman 2012-10-14
#
# Modified by Jessica Abbott 2016-10-29
################################################################################



rm(list=ls())

##### Setting up working directory #############################################
setwd("C:/Users/Jessica/Documents/stats course/R intro")      
# you have to find out the path to your folder on your computer...and be aware 
# of the slashes!

getwd()

##### Assignments and objects #################################################
x <- 4    # takes the value 4 and assigns it to object/variable x
x         # to see what is in object x just type it in the console

(x <- 4 + 8)  # entering parentheses around an assignment prints the result in
# the console directly (i.e. you don't need to write x in
# the console

# Try to write code clearly. Be careful with spaces...

x<-4	# Nothing is shown, this is as expected…
x     #...and we see that x has been assigned the value 4
# But if we put the space in the wrong place...
x< -4    # outputs [1] FALSE

#...we've obviously made a relational comparison

# Not only single values (numbers) can be assigned
(y <- 1:100)       # y is assigned a vector of numbers from 1 to 100

m <- mean(y)     # Take THE RESULT from the function mean() applied to the
# vector y and assign it to object m
m

# To see what objects we have created in the workspace we can use ls()
ls()	# function objects() work as well


# We can remove objects using function rm()
rm(x)      # we can also use rm('x') see further below
ls()      # object x is gone

rm(list=c('m', 'y'))  # we can remove more than one object at the same time
# (NOTE we need to have object names within quotes here,
# see ?rm)
ls()

# If we want to remove all objects in the workspace at the same time
x <- 1:10
y <- 2*x
ls()

rm(list=ls())
ls()
# It is good practice to start new scripts with  rm(list=ls()) to make sure the
# workspace is clean

### Saving and loading objects
x <- 1:10
y <- c(3,7,3,5,3,3)
mx <- mean(x)
my <- mean(y)
save(x, y, file='OnlyXY.Rdata')
save(list=ls(), file='All.Rdata')
save(list=c('mx', 'my'), file='MxMy.Rdata')
rm(list=ls())
ls()
load('All.Rdata')
ls()
rm(list=ls())
load('OnlyXY.Rdata')
ls()
load('MxMy.Rdata')
ls()


##### Vectors ##################################################################
x <- c(2, 1, 7, 4, 34)      # c() means combine
x <- c(1:5, x, 56)          # we can combine numbers with the vector object x
# and assign this to x again
x
y <- c('I', 'think', 'programming', 'is', 'fun') # a vector of character strings
# (length 5)
y                                                 
sq <- 1:10                   # A sequence from 1 to 10 in steps of 1
l <- x > 4                   # a vector of logicals
l
# If the elements you try to put in a vetcor is not of the same type, everything
# will be "forced" to be of the "lowest" type
z <- c(1:5, TRUE, 'atextstring')   # lowest type here is character so everything
# is forced into that
z
u <- c(1:5, TRUE)                  # here numeric is the lowest type
u
n <- c(a=1, b=4, c=56)             # named elements
n
# You can also construct "empty" vectors of a specified length using
# vector(mode, length) e.g.
v <- vector('logical', 3)
v
##### Sequences and repetitions ################################################
1:10              # makes a sequence of (integer) values from 1 to 10
# (i.e. by step = 1)
seq(from=1, to=10, by=0.1)   # If we want shorter steps
seq(from=1, to=10, length.out=9) #If we want a specific length of the result

seq(along=x)	        #Gives a sequence of "count" of the elements in x
# (NOTE, can (and should) be used instead of 1:length(x))

rep(1:10, times=2)
rep(1:10, times=10:1)
rep(1:4, each=3)
rep(1:4, each = 2, length.out = 10) 	# 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3) 	     # length 24, 3 complete replications

##### Matrices #################################################################

X <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 3)      # data entered column-wise
X
Y <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 3, byrow = TRUE) # data entered row-wise
Y
Z <- matrix(c(4, 7, 3, 8, 9, 2), ncol = 3)
Z
TY <- t(Y)
TY
t(Z) == Y                # t() transposes the matrix i.e. change rows to columns

# matrix with dimnames
Z2 <- matrix(c(4, 7, 3, 8, 9, 2), ncol = 3,
             dimnames=list(c('row1', 'row2'), c('col1', 'col2', 'col3')))
Z2
##### Arrays ###################################################################

A <- array(1:60, dim = c(3, 5, 4))    # gives a 3-dimensional array with 3 rows,
A                                      # 5 columns and 4 "sheets"
A2 <- array(1:60, dim = c(3, 5, 2, 2)) # gives a 4-dimensional array
A2
##### Indexing vectors, matrices and arrays ####################################
x
x[3]    # gets the 3rd element of vector x
x[c(1, 5, 6)] # indexing by a vector, gets element number 1 5 and 6 in vector x
x[x > 3]      # indexing by a logical vector
x[x==1]   # another example
x[]     # gets all elements of vector x; is basically the same as writing x
x[-3]   # gets everything in x BUT the third element
X
X[2, 1] # gets the element in row 2 column 1 of matrix X
X[, 2]  # gets all elements in column 2 of matrix X
X[2, ]  # gets all elements of row 2 of matrix X
X[5]    # indexing MULTI-DIMENSIONAL structures with a SINGLE index, X[i] or
# A[i] will return the ith sequential element (counted column-wise)
# of matrix X  or A respectively
A[, 2, 3] # gets all elements of column 2 in "sheet" 3 of array A
n
n['b']  # indexing by name, gets the element named 'b' in vector n
Z2
Z2['row1', 'col2'] # works similarly for dimnames in matrices, gets the element
# on row named 'row1' column named 'col2'


##### Lists ####################################################################
L <- list(a=1, b=1:3, c=matrix(1:4, 2), d=x > 4, e=y)   # Note that we can have
# elements of various
# types in the same list
L
length(L)       # Try to understand why the length of the list is what it is...


##### Dataframes ###############################################################
#Create a dataset (dataframe) using data.frame() on "manually" input data

Names <- c('Martin', 'Emily', 'Katharina', 'Nicholas', 'Susanne', 'Mikael',
           'Åke', 'Annelie')

ID <- c(37, 20, 2, 5, 11, 17, 22, 13)

Gender <- c('Male', 'Female', 'Female', 'Male', 'Female', 'Male',
            'Male', 'Female')

# In the following we will use the random number generator to get some random
# numbers for our dataset. We set a seed for the random number genarator
# such that we all get the same random samples!
set.seed(1234)
Score <- runif(8, 5, 20)           # Some random scores from a uniform
# distribution in the interval 5 to 20

Height <- rnorm(8, ifelse(Gender=='Male', 1.815, 1.668), 0.06)
# some random numbers from a normal distribution with
# mean for males of 1.815 and a mean for females  of 1.668 and a standard
# deviation of 0.06

Group <- c(1, 3, 2, 2, 3, 3, 3, 2)

dat <- data.frame(Firstname=Names, ID=ID, Sex=Gender, Score=Score,
                  Height=Height, Group=Group)    # We use the data.frame() function to put
# it all together
dat
# If we already had a list with the data...
d.list <- list(Firstname=Names, ID=ID, Sex=Gender, Score=Score,
               Height=Height, Group=Group)

#...we can use as.data.frame() to convert into a dataframe
dat.list <- as.data.frame(d.list)

# Some comparisons of a list vs a dataframe
str(dat)       # shows the structure of the object (the dataframe in this case)

str(d.list)    # the structure of the list, compare with the above
str(dat.list)  # Looks the same as str(dat) telling us that as.data.frame worked
head(dat)      # Shows the first 6 (default) cases in the dataframe, we can show
# the first n rows of the dataframe by using
# head(dataframename, n)...and we can see the last rows by using
# tail(dataframename) in a similar manner

head(d.list)         # shows the first 6 ELEMENTS of the list, compare also to
# head(as.list(1:10))

summary(dat)     # the summary function applied on a data.frame object
summary(d.list)  # the summary function on a list
class(dat)       # the class of an object tells functions how to treat the
# object. Many functions have specific methods for certain
# objects

mode(dat)    # the mode of an object describes how (of what type)
# the object is stored in memory or put in perhaps a more
# complicated way "the basic type of its fundamental constituents"
class(d.list)
mode(d.list)

# Further useful "descriptive" functions
names(dat)    # gets the names of the variables in dataframe dat
# (this would work to get the names of the elements of a list too)
dim(dat)      # gets the dimensions of dat. The result is a vector with first
# element is the number of rows and the second is the number
# of columns and so on(this also works for matrices, arrays etc.)

length(x)     # finds the length of object x  (works for e.g. lists, vectors,
# matrices dataframes etc. BUT the results differ depending on
# what it is

# try to understand the following results
length(X)       # a matrix
length(dat)     # a dataframe
length(d.list)  # a list

##### Indexing of lists and dataframes #########################################
# Look carefully at the differences in output to better understand
# the characteristics of indexing lists/dataframes
L
L$a     # gets element a in the list L
L['a']  # gets element a AS A LIST object
L[1]    # does the same since a is the first element in the list L
L[['a']] #(or L[[1]]); does the same as L$a
L['c']        # extracts c as a list element; check class(L['c']) and
# compare it to...
L[['c']] # ...class(L[['c']]); L[['c']] extracts c as it is (i.e. as a matrix in
# this case) and, again, L$c does the same

L[['c']][1, 2] # gets the element of row 1, column 2 in the matrix c
# which is in list L!!!

#Try using e.g. str(L$a) and str(L['a']) to compare how the different indexing
#methods differ in their output.

# Similar things work for dataframes
dat
dat$Score
dat['Score']
dat[['Score']]
dat[,'Score'] # This may seem wierd but this is how indexing on dataframes work
# apparently and make indexing of a dataframe similar to indexing
# of a matrix
dat[3, 4]     # Indexing as in a matrix, gets 3rd case of 4th variable
dat[3, 'Score']
dat[dat$Sex=='Female', ]  # Logical indexing based on the values of one of the
# variables in a dataframe is often very useful

##### Factors ##################################################################
# In the dat dataframe Group is a numeric variable but we would like to have it
# as a factor (a categorical vector)
# We make use of the factor() function
dat$Group <- factor(dat$Group, labels=c('teacher', 'PhDstudent', 'other'))
#the labels argument puts better labels on the levels...

str(dat)          # we see that Group is now a factor with levels 1, 2 and 3
dat$Group         # when you have a look at a factor you can also see its levels
levels(dat$Group) # to see (extract) the levels of a factor we use the
# levels() function

unique(dat$Group) 	#Works similarly to levels(), but can be used on other types
#of data, not only factors
##### Getting data into R ######################################################
## Using read.csv()  (we will use read.table() later)
# compensation.csv comes from M Crawley's The R book and contains data from an
# experiment on the impact of grazing by rabbits on seed production of a
# plant (Ipomopsis)
comp.dat <- read.csv('compensation.csv')   # Remember, we've already
# set working directory so
# we need not specify the
# whole path

# The statement to read.csv() may not always be this simple. Let's look at the
# help for read.csv()
?read.csv
# Let's have a look at the result
str(comp.dat)
summary(comp.dat)
names(comp.dat)
dim(comp.dat)

##### Plotting #################################################################
# Let's start with some simple "try-out" plotting
# Among the default R packages that are automatically loaded when starting R is
# the package datasets. This package contains a number of datasets made
# available by the R Development Core Team and other contributors worldwide
# To see a short description of the datasets in the package datasets use
# library(help="datasets")
# We'll start by using the trees dataset. Try typing trees in the console and
# see what happens...
# Use help(trees) to find out what this data is about
help(trees)
str(trees)      # we have a look at the dataset


## The plot function applied to different types of objects
# The plotting function (and many other functions) ouput different things
# depending on the class (type) of object(s) you supply as argument(s)
plot(trees$Girth, trees$Volume)   # A graphics window appears with a scatterplot
# with Girth on the x-axis and Volume on
# the y-axis
plot(trees)                   # the plot-function applied to a data.frame
# object invokes the plot.data.frame() function
# that produces pairwise scatterplots
# of all variables in the dataframe
# (see ?plot.data.frame)

tree.lm <- lm(trees$Volume ~ trees$Girth) # This is a statistical model
# i.e. we are doing a linear
# regression of Volume on Girth.
# Don't bother to much about it now,
# this is just to illustrate how plot
# works on a model object

plot(tree.lm)                             # Now, we can see that the plot-
# function applied on a model object
# of class 'lm' yields a number
# of diagnostic plots of the model.
# Press ENTER or click on the graph
# to get the next figure

plot(Volume ~ Girth, data=trees)        # We can use the formula representation
# (as in the linear model) to specify x
# and y values (note the reversed order)
# Here plot.formula() is invoked as the
# plotting function

plot(dat$Height)                        # Aha, here the x-values are just the
# indices of the variable values in the
# dataframe

plot(dat$Sex, dat$Height)    # Here we see what happens if we plot with a factor
# (plot.factor() is invoked which in turn invoke
# the boxplot() function)
# The boxplot may be a good way of showing the distribution of the data: the
# thick bar shows the median, the box enclose the interquartile range (25th  and
# 75th percentile) and the "whiskers" shows either min or max values (if no
# outliers are visible) or 1.5 times the interquartile range (and then you will
# see outliers as separate points)
# May require some training to understand...see also ?boxplot and ?boxplot.stats

boxplot(trees)        # boxplot applied to a dataframe shows box and whisker
# plots for each of the variables
boxplot(dat[, -7])    # NOTE that even factors are plotted with a box! In this
# case it is the numerical representation of the factor
# levels that is plotted (e.g. as.numeric(dat$Firstname))
# Maybe not very useful...ALSO NOTE we had to exclude the
# character variable dat$Res (seventh column) from the
# dataframe because this does not have a numerical
# representation and if this would have been included
# boxplot would have thrown an error (thanks Lin Yu for
# spotting this)



##### OPTIONAL - will mostly be covered later in the course ####################
##### Working with the dataframe ###############################################

## Adding data
# It is very is very simple to add new variables to a dataframe
# Let's say we wanted to add type of residency to our home made dataset dat
# First we need to create such a variable. Let's use the function sample(), see
# ?sample to understand what it does....

Residency <- sample(c('Flat' , 'House', 'Castle'), 8, replace=TRUE,
                    prob=c(0.55, 0.44, 0.01))

# Then we put it into the dataframe by assignement. To illustrate that the
# variable in the dataframe is not the same object as the object Residency we
# just created, we call it something else:
dat$Res <- Residency      # You could have done this in one command like:
# dat$Res <- sample(c('Flat' , 'House', 'Castle'), 8,
# replace=TRUE, prob=c(0.55, 0.44, 0.01))

dat                     # Yes, it is there!
str(dat)

# We can also use the function cbind(). This function combines vectors,
# dataframes etc. column-wise.
# This time we'll add food preference
Food <- sample(c('Fish' , 'Veg', 'Meat'), 8, replace=TRUE)
dat <- cbind(dat, Food)
str(dat)      # we see that the new variable got its name from the name of the
# added character vector. NOTE also the different behaviour of how
# the added character vector was treated:
# In the first example, the variable (Res) in dat is a character
# vector but in the second (Food) it was turned into a factor!
# This shows that it is important to look at the dataset
# (using f ex str()) after we have done something to it...
# You can also compare the dataframe variable dat$Food with the
# object Food to see the difference between the factor dat$Food
# and the character vector Food

str(dat$Food)
str(Food)
dat$Food
Food
class(dat$Food)    # dat$Food has class "factor" (once again, the class of an
# object gives information on how the object should be
# handled by functions etc.
class(Food)                     # Food has class "character"
mode(dat$Food)                  # dat$Food has mode "numeric" (the mode of an
# object gives information on how it is stored
# in memory)
mode(Food)                      # Food is a character vector
ls()                            # Here we can see that we have object Food and
# object dat (which has a variable called Food
# in it...)

# To add a new case we can use rbind() which works similarly to cbind() but
# combines things row-wise. NOTE, to make this work we need the new case(s) to
# be a dataframe too, with the same named variables in it
newcase <- data.frame(Firstname='Eva', ID=12, Sex='Female',
                      Score=runif(1, 5, 20), Height=rnorm(1, 1.668, 0.06), Group='other',
                      Res=sample(c('Flat' , 'House', 'Castle'), 1, replace=TRUE,
                                 prob=c(0.55, 0.44, 0.01)),
                      Food=sample(c('Fish' , 'Veg', 'Meat'), 1, replace=TRUE))

dat <- rbind(dat, newcase)
str(dat)


## Sorting data
# There are several functions to sort data
sort(dat$Score)                   # dat$Score in increasing value order
sort(dat$Score, decreasing=TRUE)  # dat$Score in decreasing value order
sort(dat$Res)                     # dat$Res in alphabetical order; REMEMBER
# dat$Res is a character vector

sort(dat$Group)                   # dat$Group in "level" order;
# REMEMBER dat$Group is a factor

rev(dat$Score)           # reverses dat$Score
rev(sort(dat$Score))     # rev() reverses the sorting of dat$Score
order(dat$Score)     # a vector of indices that when applied to e.g.
# dat$Firstname put Firstnames in the order of dat$Score...
#... like this
dat$Firstname[order(dat$Score)]
dat[order(dat$Sex, dat$Height),]  # you can sort on several variables at the
# same time, here the whole of dat is sorted
# first on Sex and then on Height

dat[order(dat$Sex, -dat$Height),] # and you can use - (subtraction sign) to
# reverse the ordering, here dat is sorted
# increasingly on Sex an decreasingly on Height

rank(dat$Height)     # the rank of each case according to Height (shortest has
# lowest rank!)

rank(dat$Food)       # the rank of each case according to the levelling of Food.
# NOTE what happens when there are ties
rank(dat$Sex)

## Subsetting
# Subsetting via indexing
dat.Females <- dat[dat$Sex=='Female',]
# Subsetting via subset()
dat.Males <- subset(dat, Sex=='Male')      # Note that when using subset() on a
# dataframe we do not need to specify
# dat$Sex...

# We can choose not to include all variables
dat.Males.red <- subset(dat, Sex=='Male', select=c(Firstname, Score, Height))
# We only want the variables Firstname, Score and Height
str(dat.Males.red)
# With indexing this would be...
dat[dat$Sex=='Male', c('Firstname', 'Score', 'Height')]

# Looking at str(dat.Females) we see that the factors Firstname and Sex has
# levels that don't occur in the data
str(dat.Females)
# This may cuase problems e.g. when plotting
plot(dat.Females$Sex, dat.Females$Score)  # males are plotted too despite there
# being no males in the data...
# To remove unused levels we can use droplevels
dat.Females <- droplevels(dat.Females)
str(dat.Females)
# Alternatively we could have used factor() on the factors with unused levels...
dat.Males$Firstname <- factor(dat.Males$Firstname)
#...and similarly for the other factors...

str(dat.Males)

## Merging
# Understanding match()!  Example from Crawley's R book
# We need some data
worms <- read.table('worms.txt', header=T)  # Data on worm counts in
worms                                       # different fields with
# varying characteristics

hcides <- read.table('herbicides.txt', header=T)  # Data on which
hcides			# herbicide works best in which type of vegetation

# We want herbicide information to be added to our worms dataframe; the matching
# variables in the two sets are Vegetation (worms) and Type (hcides)
# Let's see what match() does
match(worms$Vegetation, hcides$Type)
# We get a vector of indices as long as the variable worms$Vegetation
# where each element shows on what row in the variable hcides$Type
# the same value occur

match(hcides$Type, worms$Vegetation)  # If we switch the order of the two
# variables we get "the opposite", a
# vector as long as hcides$Type with each
# element showing the index of the row in
# worms$Vegetation where the same value
# occur AND with NA in those places where
# the value in hcides$Type couldn't be
# found in worms$Vegetation

# Now let's use this to get information on herbicide into the worms data
ix <- match(worms$Vegetation, hcides$Type)    # we assign the result from match
# to ix to make the next row
# easier to read...
worms$hbc <- hcides$Herbicide[ix]             # hcides$Herbicide is expanded by
# the index ix and assigned to
# worms$hbc
worms         # Ah, nice! Seems to work...e.g. all grasslands have hbc=Allclear

## Understanding merge()
# We need new data (again from Crawley's R book)
# There are two dataframes with data on flowering time and life form of various
# plants
# We want to put this into one dataframe, but first, read in the data
(ftimes <- read.table('fltimes.txt', header=T)) # we put it within
# parentheses so that we can see them directly in the console
(lforms <- read.table('lifeforms.txt', header=T))

# We should look at the names of the variables specifically
names(lforms)
names(ftimes)

# They have at least two names in common so let's try the simplest form of merge
merge(ftimes, lforms)             # In this case our focus is on flowering time
# so we put it first

# merge() managed to match on the correct variables apparently (see ?merge to
# try to understand how it knew what to match on...), but the resulting
# dataframe seems a bit short (few cases) compared to the original dataframes
# This is because merge() by default only combines data for those cases where
# the matching variables have data in both dataframes e.g. there is no flowering
# time info for Acer palmatum and there is no info on lifeform for Conyza
# bilbaoana, hence these species are not in the merged dataframe

# If you want all species represented in the merged data you can use the
# argument all=T
merge(ftimes, lforms, all=T)        # Now we got missing values (NA) in those
# places where there were no data in one of
# the original dataframes

# If you just want all data from the first dataframe but only the matching cases
# from the second use all.x=T
merge(ftimes, lforms, all.x=T)
# And vice versa
merge(ftimes, lforms, all.y=T)

# Let's stick with all data and assign it
all.flowers <- merge(ftimes, lforms, all=T)
# Then we want some info on seed size for these plants
# This info is in the seedwts dataset
seeds <- read.table('seedwts.txt', header=T)
seeds
# We try using merge() as plainly as possible
merge(all.flowers, seeds)
# Ooops! this was not what we wanted
# Apparently, there are no common names in the two datasets so merge() didn't
# know how to combine them and just made an "exhaustive match" (all cases in the
# first dataset were each combined with all cases in the second)

# We need to tell merge() what variables to match on
# Genus in all.flowers should be matched to name1 in seeds and species in
# all.flowers should be matched to name2
# We use the by.x and by.y arguments of merge()
merge(all.flowers, seeds, by.x=c('Genus', 'species'), by.y=c('name1', 'name2'))

# Note that the names of the matching variables are taken from the first
# dataframe



###Summarizing
## Basic
colMeans(comp.dat[,1:2])    # calculates the mean of columns 1 and 2 of comp.dat
colMeans(dat[, c(2, 4, 5)]) # calculates the mean of columns 2, 4 and 5
colMeans(dat)               # We see that dat contains some non numeric columns
# which colMeans don't like
colSums(comp.dat[,1:2])     # Calculates sums instead (as expected!)
rowMeans(dat[,4:5])         # Maybe not very useful here but just to show that
# it work for rows as well

# To find out how many observations we have in different groupings we can use
# table() (cross-tabulation)
table(dat$Sex, dat$Group)

## Generating summaries (e.g. mean, sd,...) over some grouping factor(s):
# the aggregate() and tapply() functions
aggregate(comp.dat$Fruit, by=list(comp.dat$Grazing), mean)
# The output/result of this function is a data.frame (compare to what comes out
# of tapply())

# We can summarize several variables simultaneously
aggregate(dat[,c('Score', 'Height')], by=list(dat$Sex), mean)

# Here we summarize using standard deviation instead
aggregate(dat[,c('Score', 'Height')], by=list(dat$Sex), sd)

# And here we get the number of cases for each variable (sample size!)
aggregate(dat[,c('Score', 'Height')], by=list(dat$Sex), length)

# The function tapply outputs a matrix instead of a dataframe which can be
# useful in some situations (i.e. some plot functions like matrices better...
tapply(comp.dat$Fruit, INDEX=list(comp.dat$Grazing), mean)

# We can aggregate on more than one variable
# We find a new dataset, this time with data on weight gain in cows fed on four
# different food supplements and three different grains (data is from Beckerman
# and Petchey "Getting started with R")
growth <- read.csv('growth.csv')
str(growth)             # to check that the import worked
names(growth)           # to see how the variables are named

# We want seperate means of weight gain for each combination of supplement
# and diet
aggregate(growth$gain, by=list(growth$supplement, growth$diet), mean)
# there are 12 combinations of supplement and diet (4x3)


# If we want proper names for the variables, we can name the elements of the
# lists we supply
aggregate(list(gain=growth$gain), by=list(suppl=growth$supplement,
                                          diet=growth$diet), mean)

# Grouping by more than one variable works for tapply too
tapply(growth$gain, list(growth$supplement, growth$diet), mean)

### OPTIONAL - more advance plotting ##########################################
### Let's try out some plotting on the comp.dat data to see how we can start by
# producing a simple plot and then develop it bit by bit
# Much of the following comes from the book "Getting started with R" by
# A Beckerman and O Petchey
# To refresh:
# We have an "experiment" of rabbits grazing on a plant and would like to have a
# look at how this affects fruit set
# We also have a measure of the size of the plant (measured by root diameter)
str(comp.dat)
head(comp.dat)

# We can have a quick look at the boxplot of Fruit against Grazing
plot(comp.dat$Grazing, comp.dat$Fruit)
# or if you like
boxplot(comp.dat$Fruit~comp.dat$Grazing)
# Note that you have to use the formula representation in the latter case
# What just happened (when using plot()) was that plot() recognized that the x
# values (comp.dat$Grazing) was a factor and by default plot() applied to a
# factor as x-values produces a boxplot!

# To make a histogram of Fruit
hist(comp.dat$Fruit)

# Or making different histograms for each Grazing group...
# ...this may seem a bit intimidating but is mostly to give you an idea of the
# data (and a hint into using par(), and logical indexing etc)
# ...the brave student should try to understand what everything means
# (see?par and ?hist!!!)
op <- par(mfrow=c(2, 1))    # we want two figures in the same window organised
# in two rows (and one column)
# We assign par() to an object. By doing so the
# "old" setting is saved as object op and thereby we
# can restore the plotting frame to normal
# afterwards (see last row in the following chunk of
# code)

hist(comp.dat$Fruit[comp.dat$Grazing=='Grazed'], breaks=10, main='Grazed',
     xlab='Fruit production', xlim=c(0, 120))
hist(comp.dat$Fruit[comp.dat$Grazing=='Ungrazed'], breaks=10, main='Ungrazed',
     xlab='Fruit production',  xlim=c(0, 120))
par(op)


# We start by making a barplot of fruit set for the two groups
?barplot  # Look at the help on barplot...it's rather complex, but have a look at
# the first argument height, which specifies the height of the bars

# Apparently we need a vector or matrix describing the height of each bar,
# here, tapply() is very convenient to use
(m.fruit <- tapply(comp.dat$Fruit, comp.dat$Grazing, mean))
# We want the bars in the plot to illustrate mean fruit production of the two
# grazing categories

barplot(m.fruit)         # Seems OK but the scale of the y-axis could be better.
barplot(m.fruit, ylim=c(0, 100))    # The help page shows we can specify limits
# on the y-axis by ylim -> looks better.
# What about some axis-labels
barplot(m.fruit, ylim=c(0, 100), xlab='Treatment', ylab='Fruit production')
# OK! Now we might like some illustration of the variation in fruit production

sd.fruit <- tapply(comp.dat$Fruit, comp.dat$Grazing, sd)
# tapply helps us getting the standard deviation in fruit production

# Now we would like to get the sd's into the figure as error bars.
# There are several ways (packages like Hmisc, gplots, plotrix etc.
# have specific functions for this) but arrows() is a pretty convenient function
# to use
?arrows     #arrows draws arrows(!) but we can fix the angle of the arrowhead
# such that it looks like an error bar

# arrows() needs start and stop coordinates on both the x- and y-axis
# the y-axis coordinates should be m.fruit - sd.fruit and m.fruit + sd.fruit for
# each category, respectively and the arrows should start and stop at the same
# place on the x-axis (at the middle of each bar)

# You can get the x-coordinates of the centre of each bar by assigning the
# barplot to an object (this is a special feature of some plotting functions...)
b.pl <- barplot(m.fruit, ylim=c(0, 100), xlab='Treatment',
                ylab='Fruit production')

b.pl          # Here they are! OK lets get this into the arrows function

# Note, arrows is a lower-level plot function so it only adds things to an
# existing plot
arrows(b.pl, m.fruit - sd.fruit, b.pl, m.fruit + sd.fruit, code=3,
       angle=90, length=0.1) # code specifies type of arrow(double headed),
# angle is the angle of the arrow heads and length
# is the length of the arrowheads

# Beautiful! Now as a last touch, let's add some sample sizes
# We find the sample size using tapply again and add the information using
# text() (also a low level plot function)
n.fruit <- tapply(comp.dat$Fruit, comp.dat$Grazing, length)
# REMEMBER what the length() function does...
text(b.pl, 3, paste('n = ', n.fruit))  # we use b.pl again to find the
# x-ccordinates for the text and set the
# text at y=3 to get the nicely
# into the bars  see ?text for further
# info
# paste() is a function that concatenates (character) vectors, see ?paste...

paste('n = ', n.fruit)                # ...and try this out to see what it does

# Let's try out a scatterplot using the plot() function again
# This time we will look at how root diameter relates to Fruit production
plot(comp.dat$Root, comp.dat$Fruit)         # formula alternative would be
# plot(Fruit~Root, data=comp.dat)
# Note below how we can separate the call to the plot function into several rows
# to facilitate reading...
# We add proper labels for the axes
plot(Fruit~Root, data=comp.dat,
     xlab=list('Root biomass', cex=1.5),
     ylab=list('Fruit production', cex=1.5),
     cex=2, pch=21, bg='grey')
# This time the call to xlab is a list with the text and then a specification of
# cex (character expansion) saying that the labels should be written 1.5 times
# larger than default. We also want the points to be 2 times bigger than normal
# and use a new point symbol that accomodates specification of background colour
# To change the border colour we could have used col
# For those interested I can add that you can create your own colours and use it
# instead of 'grey' above by using the rgb() function...
# ...like this: myfavcolour <- rgb(0.24,0.38,0.13) where the arguments specify
# the "strength" of red, green and blue component in the colour, respectively.
# A good resource for getting RGB-values for different colours is 
# http://www.colorpicker.com/

# Remember that we had a grouping variable Grazing in the data. To visualize
# this we could try to colour the points based on this grouping
# We can do this by specifying a vector of colours to bg instead of just one
# character string (above it was 'grey')
# There are two ways of producing this vector for the current example
# 1) Using the ifelse function
bg.col <- ifelse(comp.dat$Grazing == 'Grazed', 'green', 'red')
# Try to understand what we just did. Have a look at bg.col to see the result

# 2) Even cooler and useful when we have more than two levels of the grouping
# factor
bg.col <- c('green', 'red')   # Here we can include as many colours as we have
# levels of the grouping factor
bg.col[comp.dat$Grazing]      # We use the factor Grazing (its numeric
# representation) to index bg.col. Once again
# think hard about what this meant!
# OK, now that we have this vector of colours, we use it in the call to bg in
# the plot function
plot(Fruit~Root, data=comp.dat,
     xlab=list('Root biomass', cex=1.5),      # the call to xlab is a list with
     # the text and then a
     # specification of cex (character
     # expansion) saying that the
     # labels should be written 1.5
     # times larger than the default
     ylab=list('Fruit production', cex=1.5),
     cex=2, pch=21, bg=bg.col[Grazing])       # I decided to use the cool way!
# We only need to specify Grazing
# (instead of comp.dat$Grazing)
# since we have specified the
# data set already in the plot
# function

# And while we're on it, let's add a legend as well(I'm short on the description
# here, have a look at ?legend if you want more info
legend('topleft', legend=c('Grazed', 'Ungrazed'),
       pch=21, pt.bg=c('green', 'red'), pt.cex=2)
# Make sure you get the ordering of the strings right in the specifications to
# legend and pt.bg
# Instead of using 'topleft' to specify location of the legend box we can use
# simple x and y coordinates

# Let's send this beatiful figure to a pdf-file
pdf('Beautiful_figure.pdf', paper='a4') # We start the device (i.e. a pdf-file)
plot(Fruit~Root, data=comp.dat,              # We plot the figure
     xlab=list('Root biomass', cex=1.5),
     ylab=list('Fruit production', cex=1.5),
     cex=2, pch=21, bg=bg.col[Grazing])
legend('topleft', legend=c('Grazed', 'Ungrazed'),
       pch=21, pt.bg=c('green', 'red'), pt.cex=2) # and add a legend
dev.off()                                     # And turn the device off

# Now you should find the pdf-file in you working directory
# (If you forgot where, use getwd() !!!!)
