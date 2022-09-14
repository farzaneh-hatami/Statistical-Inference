########### a ##############
df <- read.csv("E:/UT/term_3/statiscal inferense/HW/HW1/imdb.csv")
df
name <- names(df)
df$title
typeof(df$imdb_ID)
typeof(df$title)
typeof(df$year)
typeof(df$duration)
typeof(df$total_votes)
typeof(df$budget)
typeof(df$USA_gross_income)
typeof(df$worldwide_gross_income)
typeof(df$tomatometer_status)
typeof(df$audience_rating)

##########3 b##############
year <- sort(unique(df$year))
movie_c <- c(length(df$year[df$year == year[1]]))
for (i in(2:length(year))){
  print(length(df$year[df$year == year[i]]))
  movie_c <- c(movie_c , length(df$year[df$year == year[i]]))
  print(movie_c)
}
barplot(movie_c , names.arg  = year, main = "number of movies prodused yearly")

###### c #########
hist(df$USA_gross_income , main = "histogram of USA_gross_income",ylab = "Frequency" , xlab = "USA_gross_income")
########## d ########
boxplot(df$duration ~ df$tomatometer_status ,col='blue' , main = 'Distribution of movie during', ylab = "duration",xlab = 'tomatometer-status')

######## e ##########

df$type <- apply(df, 1, FUN = function(x) if(x[4] > 200) 'so long' 
                   else if(x[4] >150 && x[4] < 200) 'long' 
                   else if(x[4] > 100 && x[4] <150) 'standard' 
                   else if(x[4] <80) 'short')
type_cont <- c(length(df$type[df$type == 'so long']))
type_cont <- c(type_cont,length(df$type[df$type == 'long']))
type_cont <- c(type_cont,length(df$type[df$type == 'standard']))
type_cont <- c(type_cont, length(df$type[df$type == 'short']))

piepercent<- round(100*type_cont/sum(type_cont), 1)

pie(type_cont ,labels = piepercent,col = rainbow(length(type_cont)),main = "")
legend("topleft", c("So Long","Long","Standard","Short"), cex = 0.8,
           fill = rainbow(length(type_cont)))
###### f#####
plot(df$worldwide_gross_income , df$USA_gross_income ,main = "scatter plot of two random variable",ylab = 'USA-gross-income' , xlab = "World-wide-gross-income")
