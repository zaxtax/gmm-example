library('rjags')

N <- 1200
y <- c(rnorm(400, -7), rnorm(400, 3), rnorm(400, 10))
 
jags <- jags.model('gmm.jags',
                   data = list('y' = y,
                               'ones' = rep(1,3),
                               'N' = N),
                   n.chains = 1,
                   n.adapt = 10)
 
update(jags, 10)
 
jags.samples(jags,
             c('z', 'phi'),
             1)
