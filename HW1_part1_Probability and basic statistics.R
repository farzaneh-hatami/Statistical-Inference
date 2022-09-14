########### creating a vector #########3
v <- c(57,66,72,78,79,79,81,81,82,83,84,87,88,89,90,91,92,94,95)

########### calculating median - mode - variance - std ####
#median
median.result <- median(v)
#mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
Mode(v,na.rm = FALSE)
#variance
var(v)
#sd
sd(v)
?hist

####### part 2 ###########
lowerq = quantile(v)[2]
upperq = quantile(v)[4]
iqr = upperq - lowerq #Or use IQR(data)
mild.threshold.upper = (iqr * 1.5) + upperq
mild.threshold.lower = lowerq - (iqr * 1.5)
print(v[v> mild.threshold.upper])
print(v[v< mild.threshold.lower])

?boxplot
boxplot(v)
hist(v,freq = F)
lines(density(v))

