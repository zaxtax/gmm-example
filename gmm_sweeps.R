suppressMessages(library('rjags'))
suppressMessages(library('coda'))

suppressMessages(library('MCMCpack'))
suppressMessages(library('LaplacesDemon'))

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
   cat("gmm.R <dataSize> <sweeps> <trial>", fill=TRUE)  
} else {
    N      <- as.numeric(args[1])
    sweeps <- as.numeric(args[2])  
    trial  <- as.numeric(args[2])

phi   <- c(-7, 3, 10)
as    <- c(1,  1, 1)
theta <- as.vector(rdirichlet(1, as))
zTrue <- rcat(N, theta)
y     <- rnorm(N, phi[zTrue])

start.time <- Sys.time()

jags <- jags.model('gmm.jags',
                   data = list('y'    = y,
                               'ones' = as,
                               'N'    = N),
                   n.chains = 1,
                   n.adapt = 10,
                   quiet=TRUE)

start2.time <- Sys.time()
 
update(jags, sweeps);

## samplesC <- coda.samples(jags,
##                          c('z', 'phi'),
##                          1);
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
    format(sweeps),
    format(trial),
    #N,
    format(accuracy),
    #as.double(duration),
    #as.double(duration2),
    sep=",",
    fill=TRUE)
}
