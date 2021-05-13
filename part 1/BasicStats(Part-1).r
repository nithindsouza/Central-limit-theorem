library(readr)
cars <- read_csv("C:\\Users\\hp\\Desktop\\CLT assi\\probability dataset\\Cars.csv")

#getting mean value
sapply(cars , mean , na.rm = TRUE)
#Mean = 34.42208

#getting SD value
sapply(cars , sd , na.rm = TRUE)
#SD = 9.131445

#a.	P (MPG > 38)
Z <- (38 - 34.42208) / 9.131445
Z

#P(MPG<40) 
Z<- (40- 34.42208) / 9.131445
Z

#Checking for Normal distribution by visualizing data
hist(cars$MPG , breaks = 10 , prob = TRUE ,
     xlab = 'X-variabel' , main = 'Density curve over histogram')
lines(density(cars$MPG))

#from the plot it is decided that cars$MPG is a normal distribution

wc_at <- read_csv("C:\\Users\\hp\\Desktop\\CLT assi\\probability dataset\\wc-at.csv")
#Checking for Normal distribution by visualizing data
#Adipose tissue
hist(wc_at$AT , breaks = 10 , prob = TRUE ,
     xlab = 'X-variabel' , main = 'Density curve over histogram')
lines(density(wc_at$AT))

#Waist circumference
hist(wc_at$Waist , breaks = 10 , prob = TRUE ,
     xlab = 'X-variabel' , main = 'Density curve over histogram')
lines(density(wc_at$Waist))

#Q10

mean_a = 5
sd_a =   3

mean_b = 7
sd_b      =   4

     #A
profit_mean = (mean_a + mean_b)/2

profit_sd= sqrt((sd_a**2)+(sd_b**2)+(mean_a - profit_mean)+(mean_b - profit_mean))


z_val_95 = qnorm((1-0.95)/2)

lower_range_rs = ((z_val_95 * profit_sd) + profit_mean)* 45 * 1000000
upper_range_rs = ( -(z_val_95 * profit_sd) + profit_mean) * 45 * 1000000
    
    #B


z_val_5= qnorm(0.05,profit_mean,profit_sd)

percentile_5 = -(z_val_5 * profit_sd + profit_mean) * 45 * 1000000

    #C
dividion_a = pnorm(0,mean_a,sd_a)
dividion_a

dividion_b = pnorm(0,mean_b, sd_b )
dividion_b