suppressMessages(library('rjags'))
suppressMessages(library('coda'))

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
    N      <- 1200
    sweeps <- 5
    y      <- c(rnorm(400, -7), rnorm(400, 3), rnorm(400, 10))
} else {
    N      <- as.numeric(args[1])
    sweeps <- as.numeric(args[2])  
    y      <- c(rnorm(N/3, -7), rnorm(N/3, 3), rnorm(N/3, 10))
}
zTrue <- c(rep(1,N/3), rep(2,N/3), rep(3,N/3))

start.time <- Sys.time()

jags <- jags.model('gmm.jags',
                   data = list('y' = y,
                               'ones' = rep(1,3),
                               'N' = 3*floor(N/3)),
                   n.chains = 1,
                   n.adapt = 10,
                   quiet=TRUE)

start2.time <- Sys.time()
 
update(jags, sweeps);

## coda.samples(jags,
##              c('z', 'phi'),
##              1)
samples <- jags.samples(jags,
                        c('z', 'phi'),
                        1);

end.time  <- Sys.time()
duration  <- end.time - start2.time
duration2 <- end.time - start.time

remap <- as.numeric(as.factor(samples$phi))
zPred <- sapply(samples$z, function (i) remap[i])

accuracy <- length(zTrue[zPred == zTrue])/length(zTrue)

# format(duration)
cat("JAGS",
    N,
    format(accuracy),
    as.double(duration),
    as.double(duration2),
    sep=",",
    fill=TRUE)

