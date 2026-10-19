# MATH220 Priject A - Exploration 1
# Dimension of the span of k random vectors in R^n
# Idea: put the k vectors as the rows of a matrix A
#The span of the vectors = row space of A, so dim(span) = rank(A)
# In R we can get the rank with qr(A)$rank

set.seed(240) # So we get the same result every time

# The examples are from the promt
v1 = [1,0,2,1]
v2 = [0,1,1,3]
v3 = [1,1,3,4]
A = np.array([v1,v2,v3])
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
n = 5
ks = 1:10
reps = 100 # The number of sets for each k

avg_normal = c()
for(k in ks) {
  results = c()
  for(i in 1:reps) {
    results[i] = span_dim(n, k, "normal")
  }
  avg_normal[k] = mean(results)
}
avg_normal

plot(ks, avg_normal, type = "b", pch = 19, col = "blue", xlab = "K (number of vectors)", 
      ylab = "average dimension of span", main = "Normal entries, n = 5", ylim = c(0, 6))
lines(ks, pmin(ks, n), lty = 2, col = "red") # The line(n, k)
legend("bottomright", legend = c("simulation", "min(n,k)"), col = c("blue", "red"), lty = c(1, 2))
