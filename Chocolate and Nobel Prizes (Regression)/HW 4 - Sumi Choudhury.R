#########################
#         HW 4          # 
#########################


# SDGB 7844: Statistical Methods and Computation I
# Sumi Choudhury

x <- read.table("nobel_chocolate.txt", header = T, sep = ",") # read the data file
View(x)
dim(x) # rows and columns
colnames(x) # obtain variable names

head(x) # check the first six rows of data
tail(x) # check the last six rows of data

# check missing data
nrow(x)
length(complete.cases(x)) 

# extract the 3 columns: country, nobel_rate, chocolate_con
data.temp <- x[,-(4:24)] 
dim(data.temp) # rows X columns
head(data.temp) # check the first six rows of table
tail(data.temp) # check the last six rows of table

x <- data.temp # replace x with data.temp
rm(data.temp)

colnames(x) <- c("Country", "Nobel_Laureates", "Chocolate_Consumption") # change the column names
colnames(x)

# compute correlation between Nobel Laureates and Chocolate Consumption
cor.Choc_NL <- cor(x$Chocolate_Consumption, x$Nobel_Laureates, use = "complete.obs")  
cor.Choc_NL

# construct scatterplot
plot(x$Chocolate_Consumption, x$Nobel_Laureates, las = T, 
     main = "Scatterplot of \nNobel Laureates vs. Chocolate Consumption",
     xlab = "Chocolate Consumption(kg/yr/capita)", ylab = "Nobel Laureates per 10 Million Population", pch =20,
     xlim = c(0,15), ylim = c(0,35))
abline(lsfit(x$Chocolate_Consumption, x$Nobel_Laureates), col = "firebrick",lwd = 1.8)
# label Sweden on the plot with arrow and text
arrows(4.5,33, 6.25, 31.857, length = 0.1, col = "firebrick") 
text(4.3,34.5, labels = "Sweden", col = "firebrick")

# add the correlation to the scatterplot
text(2,30, labels = paste("Correlation =", round(cor(x$Chocolate_Consumption, x$Nobel_Laureates, use = "complete.obs"),
                                                 digits = 5)))
abline(h=0, lty = 3, col ="forestgreen", lwd = 2) # add a line to the zero pint on y-axis

dev.off() 

# linear regression of Nobel Laureates and Chocolate Consumption
lm.x <- lm(x$Nobel_Laureates ~ x$Chocolate_Consumption)
lm.x
names(lm.x) 

dev.off()

# conduct a residual analysis to check the asssumptions
par(mfrow = c(1,2)) 

# construct the residual vs. Chocolate_Consumption plot
plot(x$Chocolate_Consumption, lm.x$residuals, las = T, main = "Residuals vs. Chocolate Consumption", 
     xlab ="Chocolate Consumption(kg/yr/capita)", ylab ="Residuals", pch =20, xlim= c(0,15) )
abline(h=0, col = "forestgreen", lty = 2)

# construct the Q-Q plot for residuals 
qqnorm(lm.x$residuals, las = T, main = "Normal Q-Q Plot for Residuals", pch = 20, col = "forestgreen")
qqline(lm.x$residuals)

# using linear regression summary for hypothesis test 
summary(lm.x)
# anova(lm.x) alternative formula


# add the regression line to the scatterplot
plot(x$Chocolate_Consumption, x$Nobel_Laureates, las = T, 
     main = "Scatterplot of \nNobel Laureates vs. Chocolate Consumption",
     xlab = "Chocolate Consumption(kg/yr/capita)", ylab = "Nobel Laureates per 10 Million Population", pch =20,
     xlim = c(0,15), ylim = c(0,35))
abline(coef(lm.x), col = "firebrick", lwd = 2) # add a regression line

dev.off()

num <- which(x$Country == "Sweden") # find out the row number of Sweden
num

# predict the expected value for Sweden
fitted.values(lm.x)[num]

# the residual of Sweden
lm.x$residuals[num]
