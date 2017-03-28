library('rjags')
library('coda')

N <- 12000
y <- c(rnorm(4000, -7), rnorm(4000, 3), rnorm(4000, 10))

 
jags <- jags.model('gmm.jags',
                   data = list('y' = y,
                               'ones' = rep(1,3),
                               'N' = N),
                   n.chains = 1,
                   n.adapt = 10)

start.time <- Sys.time()
 
update(jags, 5);

## coda.samples(jags,
##              c('z', 'phi'),
##              1)
samples <- jags.samples(jags,
                        c('z', 'phi'),
                        1);

end.time <- Sys.time()
end.time - start.time
