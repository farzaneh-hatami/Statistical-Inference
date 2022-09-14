########## HW5 ################

# ________________ question 6 __________ #
df = 2 - 1 #degree of freedom

observed_true = 110 #the observed mothe wh can recognize their baby

n = 320 # total trial

p = 1/26 # probablity of a mother who recognize her child corectly


expected <- c(round(n*p) , round(n*(1-p)))
observed <- c(observed_true , n - observed_true)

khi2 <- sum(((observed  - expected)^2)/expected)
khi2

p_value <- pchisq(khi2 , df , lower.tail=FALSE)
p_value

#__________ question 7 __________ #
# H0 : eye color is independent from hair color
# HA : the eye colore is dependent from hair color
# To do a chi-square test of independence, our object has to be a matrix 
#Caith is already a matrix so we can proceed with the test.
library(MASS)
caith
r_sum <- c(rowSums(caith))
c_sum <- c(colSums(caith))
total_sum <- sum(r_sum)

expected_count <- (r_sum %*% t(c_sum)) / total_sum
expected_count

khi2 <-sum(((caith - expected_count)^2)/expected_count)
khi2

df <- (length(r_sum)-1) * (length(c_sum)-1)
df

p_value <- pchisq(khi2 , df  , lower.tail = FALSE)
p_value

chisq.test(caith)

#__________ question 8 ______________

#H0 = 0.5
#HA > 0.5
#n = 20
#p-hat = 1
n = 20 #number of observation(computers)
size = 1 #number of trialas
prob = 0.5 #probability of success in each trial
sample_size = 100000

simulation <- c()

for (val in 1: sample_size)
{
  computers <- rbinom(n, size, prob)
  n_damage <- sum(computers)
  propotion <- n_damage/n
  simulation <- append(simulation, propotion)
  #print(propotion)
}

hist(simulation)
#p-value = P(p-sim>=1 | p = 0.5)
p_value <- sum(simulation[simulation == 1])/sample_size
p_value