# Checks of convergence for the Bayesian models 

`CheckChains1.jpg` and `CheckChains2.jpg` are the Markov chain Monte Carlo estimates for the best model in each journal / outcome / type combination. There are two chains per model and they are all stable and show good mixing. The estimates are the overall intercept (beta[1]) and the trend (beta[2]). These estimates were made using JAGS (Version 3.4.0).

`CheckChains3.jpg` are the Markov chain Monte Carlo estimates for hour of day models. They show the intercept and inverse-variance for the random effect (labelled 'taubeta'). All chains are stable and show good mixing. These estimates were made using WinBUGS (Version 1.4.3).
