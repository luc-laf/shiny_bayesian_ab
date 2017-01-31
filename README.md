A/B Bayesian Testing 
==============

This is a shiny app developed with the aim of helping MSM with Bayesian A/B testing.
--------------


It uses the Beta, Binomial conjugate setup to predict the distribution of the two variants 'success rate'. See https://www.cs.cmu.edu/~10701/lecture/technote2_betabinomial.pdf for a walk through of the maths.

At the top of the page there are sections to enter the number of 'successes' and the total volume for the two variants in the test.

After entering these the results are calculated using the button.
N.B. The prior is assumed to be uniform unless edited at the bottom of the dashboard.

The region of practical equivalence can also be entered at the bottom, it is assumed to be 0.

