library('rjags')

N <- 3
y <- c(3.85, 3.4, 12.0)
 
jags <- jags.model('gmm_test.jags',
                   data = list('y' = y,
                               'ones' = c(1,1),
                               'z2' = 1,
                               'z3' = 2,
                               'N' = N),
                   n.chains = 1,
                   n.adapt = 10)
 
update(jags, 10)
 
jags.samples(jags,
             c('z1', 'phi'),
             200)
