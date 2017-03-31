################################################################################
# Examples from http://www.statmethods.net/advgraphs/ggplot2.html
################################################################################

# ggplot2 examples
library(ggplot2)

# create factors with value labels
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears"))
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl"))

# Kernel density plots for mpg
# grouped by number of gears (indicated by color)
qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5),
      main="Distribution of Gas Milage", xlab="Miles Per Gallon",
      ylab="Density")

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am,
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")

# Separate regressions of mpg on weight for each number of cylinders
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"),
      method="lm", formula=y~x, color=cyl,
      main="Regression of MPG on Weight",
      xlab="Weight", ylab="Miles per Gallon")

# Boxplots of mpg by number of gears
# observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"),
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon") 


# http://stackoverflow.com/questions/7961865/creating-confidence-area-for-normally-distributed-scatterplot-in-ggplot2-and-r
library(reshape2)
data(tips)
# RUN REGRESSION AND APPEND PREDICTION INTERVALS
lm_fit  = lm(total_bill ~ tip, data = tips)
tips_with_pred = data.frame(tips, predict(lm_fit, interval = 'prediction'))

# PLOT WITH REGRESSION LINE, CONFIDENCE INTERVAL AND PREDICTION INTERVAL
p0 <- ggplot(tips_with_pred, aes(x = tip, y = total_bill)) + 
  geom_point() +
  geom_smooth(method = 'lm', aes(fill = 'confidence'), alpha = 0.5) +
  geom_ribbon(aes(y = fit, ymin = lwr, ymax = upr, fill = 'prediction'),
              alpha = 0.2) +
  scale_fill_manual('Interval', values = c('green', 'blue')) +
  theme(legend.position = c(0.20, 0.85))
p0



# Mosaic Plot Example
library(vcd)
mosaic(HairEyeColor, shade=TRUE, legend=TRUE) 


# Association Plot Example
library(vcd)
assoc(HairEyeColor, shade=TRUE) 

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
library(corrplot)
data(mtcars)
M <- cor(mtcars)
corrplot(M, method="circle")

corrplot(M, type="upper")



# Using different color spectrum
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(M, type="upper", order="hclust", col=col)

# Caret Package
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))