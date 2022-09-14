#2-c
s  = 0.05
mean = 37.1
sample_mean = 36 
dif_mean = 36-37.1
radn = qnorm(0.01) * s / diff_mean
n = radn*radn
n
#####################9 - a
dataf <- read.csv("E:/UT/term_3/statiscal inferense/HW/HW3/dataset/metadata.csv", header=FALSE)

df1 <- dataf[!(is.na(df$V3) | df$V3=="" | is.na(df$V4) | df$V4==""),] 

y <-df1$V3
x <- df1$V4

men <- strtoi(c(x [y == 'M']))
women <- strtoi(c(x [y == 'F']))

mean_m <- mean(men)
mean_m

mean_w <- mean(women)
mean_w

xbar <- mean_m - mean_w #point estimation

n = length(men)
m = length(women)

sn = sd(men)
sm = sd(women)

df  = n+m-2
df

#confidence interval

SP = sqrt(((n - 1) * sn^2 + (m - 1) * sm^2) / df )
SE= SP*sqrt((1/n)+(1/m))

z_star<- qnorm(0.95+(1-0.95)/2)
CI <- c(xbar-z_star*SE,xbar+z_star*SE)
CI

#p -value
z = ((abs(xbar))-0)/SE
pvalue =2*(1-pnorm(z)) 
pvalue

#9 - b


df2 <- dataf[!(is.na(dataf$V3) | dataf$V3=="" | is.na(dataf$V6) | dataf$V6=="" ),]
df2
df3 = data.frame(df2$V3,df2$V6)
df3 <- df3[-1,]

Tabel = table(df3$df2.V3,df3$df2.V6)

count=addmargins(Table)[1:2,]

#p_had
p <- count
p[1,]=round((p[1,]/p[1,3]),3)
p[2,]=round((p[2,]/p[2,3]),3)
p


#Conditons for inference

i <- 1:2
CFI=lapply(i,p,count,FUN = function(i,p,count) {
  np     = round(count[,i]*p[,i])
  n_p  = round(count[,i]*(1-p[,i]))
  print(paste(" female and male if Survival::",names(count[1,])[i],np[1],np[2], n_p[1],n_p[2]))
} )


#CI

i <- 1:2
CI=lapply(i,p,count,FUN = function(i,p,count) {
  pointestimate   = p[1,i]-p[2,i]
  SE<-sqrt(sum(p[,i]*(1-p[,i])/count[,i]))
  z_star = qnorm(0.005,lower.tail = FALSE)
  CI<- c(pointestimate-z_star*SE , pointestimate+z_star*SE)
  print(paste("CI for proportion of female and male  if Fjob::",
              names(count[1,])[i],'is (',round(CI,2)[1],',',round(CI,2)[2],')'))
})

z = ((abs(xbar))-0)/SE
pvalue =2*(1-pnorm(z)) 
pvalue


########## 10 #############
#10.a.

alpha <- 0.05
n <- 50
sig = 0.7


alpha<-.05
n<-50
sig = 0.7
plot(seq(9,13,1),pnorm(qnorm(alpha,0,1,lower.tail =TRUE) 
                        - (seq(9,13,1)-11.5)/(sig/(sqrt(n))))
     ,col="blue",type="l",xlab=  expression(paste(mu['a']))
     , ylab = expression(paste("PWR (", mu['a'],')')),)
      legend("topright",
      legend = c("n = 50,alpha = .05"),lwd = 1, col ="blue")
      
  
