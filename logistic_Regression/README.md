This is a set of interesting code that I wrote from scratch that 
show how logistic regression are trained. Conceptually, i randomly
generated a vector of random values between 0 and 1.0, and use it 
as initial probabilities, i then converted them to odds:

  odds = {p(i)/(1-p(i)), for all i=0 .. 400}

the cool thing is that odds ranges from -inf to inf.

step t, I performed linear regression to obtain beta_hat(t):

  odds ~ beta0 + X * beta, where X is the model matrix
  
step t+1, i calculate the difference:

  delta(t+1) = y - 1/(1 + exp(-x)), where x= beta0_hat(t) + X * beta_hat(t)
  
I then updated odds:

  odds = y + lambda * delta(t+1)
  
where lambda is the learning rate.

From here, we returned to step t, and repeat the whole process
till convergence.
