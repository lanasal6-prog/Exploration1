# MATH220 Project A - Exploration 1
# Dimension of the span of k random vectors in R^n
# Idea: put the k vectors as the rows of a matrix A
# The span of the vectors = row space of A, so dim(span) = rank(A)
# In R we can get the rank with qr(A)$rank
# Run in RStudio

set.seed(240) # So we get the same result every time

# The examples are from the prompt
v1 = c(1,0,2,1)
v2 = c(0,1,1,3)
v3 = c(1,1,3,4)
A = rbind(v1,v2,v3)
qr(A)$rank # Should be 2 (v3 = v1 + v2)

# The function to get the dimension of the span of k random vectors
# Type = normal. Entries from a standard normal distribution
# Type = integer. Entries chosen from 0,1 only
span_dim = function(n, k, type) {
  if(type == "normal") {
    A = matrix(rnorm(k * n), nrow = k, ncol = n)
  } else {A = matrix(sample(c(0, 1), k * n, replace = TRUE), nrow = k, ncol = n)
  }
  return(qr(A)$rank)
}

# try it once
span_dim(4, 3, "normal")

# Experiment 1: normal entries, n = 5, k from 1 to 10
# We chose n = 5 because it's small enough to check the rank by hand and k goes 
# past n so we can see what happens with more vectors than dimensions
n <- 5
ks <- 1:10
reps <- 100 # The number of sets for each k. 100 sets each, more than this and the 0/1 loop got slow

avg_normal = c()
for(k in ks) {
  results = c()
  for(i in 1:reps) {
    results[i] = span_dim(n, k, "normal")
  }
  avg_normal[k] = mean(results)
}
avg_normal

plot(ks, avg_normal, type = "b", pch = 19, col = "blue", xlab = "k (number of vectors)", 
      ylab = "average dimension of span", main = "Normal entries, n = 5", ylim = c(0, 6))
lines(ks, pmin(ks, n), lty = 2, col = "red") # The line min(n, k)
legend("bottomright", legend = c("simulation", "min(n,k)"), col = c("blue", "red"), lty = c(1, 2))

# Experiment 2: same thing but with 0/1 entries
avg_int = c()
for(k in ks) {
  results = c()
  for(i in 1:reps) {
    results[i] = span_dim(n, k, "integer")
  }
  avg_int[k] = mean(results)
}
avg_int

plot(ks, avg_int, type = "b", pch = 19, col = "darkgreen", xlab = "k (number of vectors)", 
      ylab = "average dimension of span", main = "0/1 entries, n = 5", ylim = c(0, 6))
lines(ks, pmin(ks, n), lty = 2, col = "red")
legend("bottomright", legend = c("simulation", "min(n,k)"), col = c("darkgreen", "red"), lty = c(1, 2))

# Findings
# We picked n = 5 and let k run from 1 to 10, doing 100 sets for each k.
# The random vectors were made two ways: entries from N(0,1) and entries that are just 0 or 1.
# 1. With normal entries the dimension came out as min(n, k) every time.
#    A continuous random vector has no chance of landing exactly in the span of the others.
# 2. The dimension can't be bigger than n or bigger than k, so it's always dim <= min(n, k).
# 3. With 0/1 entries the average dimension is a bit below min(n, k), because you can easily get the same 
#    vector twice or even a vector of all zeros.
#    The gap was biggest when k was close to n = 5 and when k is bigger than n we nearly always got the full dimension 5.
