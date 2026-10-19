import numpy as np

#Choose range for example n from 2 to 10 k from 1 to 15

# The examples are from the promt
v1 = [1,0,2,1]
v2 = [0,1,1,3]
v3 = [1,1,3,4]
A = np.array([v1,v2,v3])
print("Rank:", np.linalg.matrix_rank(A))

# More work
