library(tidyverse)
ceemid_ci <- readxl::read_excel(file.path("data", "CEEMID-CI_index.xlsx"))
median_volumes <- ceemid_ci %>%
    select ( report_start, country_code, median_volume ) %>%
    pivot_wider( names_from = country_code, values_from  = median_volume ) %>%
    mutate_if ( is.numeric, function(x) round(x, 0))

streamsim <- function(n = 600, mean = 1, sd = 2,
                      start_period  = 1,
                      median_vols = rep(0, 38),
                      dmean = 0, dst = 0) {

    meanlog = log (mean)
    sdlog = log (sd)

    leading_zero_vector <- rep ( 0, start_period - 1)

    missing_length <- max(0, 60 -(start_period-1 + length(median_vols)) )
    missing_length
    if (missing_length>0) {
        forecasted_median_vols <- forecast::forecast(median_vols, missing_length)
        long_tail <- round (c( leading_zero_vector, median_vols, forecasted_median_vols$mean), 0)

    } else {
        long_tail  <- c(leading_zero_vector, round(median_vols,0))
    }

    length(long_tail)
    sum(long_tail)
    if ( is.na(sum(long_tail)) ) stop ( 'Missing value in long_tail')


    remaining_count <- n - sum(long_tail[1:min(length(long_tail), 60)])
    if ( is.na(remaining_count)) stop ( "remaining count is NA")
    if ( remaining_count <0 ) stop ("remaining_count is negative")

    random_streams <- rlnorm( n = remaining_count,          # number of random numbers
                              meanlog = meanlog,  # mean
                              sdlog = sdlog)

    hh <- hist (random_streams, breaks = 59 )


    simulated_random_streams <- hh$counts
    simulated_streams <- c(rep(0, start_period-1), simulated_random_streams)

    sim_stream_length <- length( simulated_streams)
    if ( sim_stream_length < 60  ) {
        # fill up zero counts
        simulated_streams <- c(simulated_streams, rep (0, 60 - sim_stream_length))
        }

    streams <- simulated_streams[1:60] + long_tail[1:60]

    names ( streams ) <- paste0("month_", 1:60)
    mode <- which( simulated_random_streams ==max(simulated_random_streams ))[1]
    release = start_period
    peak = as.numeric(which ( streams ==max(streams))[1])
    s <- sum ( streams )
    params <- c(release, n, mean, sd, mode, peak, dmean, dst, s )

    names ( params) <- c("release", "n", "mean", "sd", "mode", "peak", "dmean", "dst" , "sum")

    distorsions =  1+rnorm(60, dmean ,dst )

    names(distorsions) = paste0("dist_", 1:60)

    as.data.frame ( cbind ( type = "rnorm",
                            t(c(params,streams, distorsions))
                            )
    )
}

simulated_df <- streamsim ( n = 60000,
                         start_period = 1,
                         median_vols = median_volumes$GB,
                         mean = 3,
                         sd = 1.2)
simulated_df

simulated_df[1, 7:(38+6)] <- 6000/60
simulated_df[1,1] <- "perenial"
simulated_df$peak[1] <- 1

st = 1.5
exp(sd^2)+2*sqrt (exp(st^2)-1)

params_1 <- expand.grid (
    c(6000, 60000, 600000),
    1:25,
    seq(1.1, 4.1, by = 1),
    seq(2.3, 1.3, -0.1),
    seq(-0.3,0, by = 0.025),
    c(0.01,0.02))

params_2 <- expand.grid (
    6000000,
    1:25,
    seq(1.1, 4.1, by = 1),
    seq(2.3, 1.3, -0.1),
    seq(0,0.3, by = 0.025),
    c(0.01,0.02)
    )

names ( params_1) <- names(params_2) <- c("n", "start_period", "mean", "st", "dmean", "dst")

params <- rbind ( params_1, params_2)

simulated_df <- streamsim(params$n[1],
                          start_period = params$start_period[1],
                          median_vols = median_volumes$GB,
                          mean = params$mean[1],
                          sd = 1.3,
                          dmean = params$dmean[1],
                          dst = params$dst[1])

cycle = 2:nrow(params)
lc <- length(cycle)
lc

cyc <- sample ( cycle, 10000, replace = FALSE)
cyc
params[cyc, ]
params$dmean[cyc]
i



for ( i in  seq_along (cyc) ) {

    chosen <- cyc[i]

    message ( i, "/ ", lc, " ", chosen,  " n= ",
              params$n[chosen], " with mean= ", params$mean[chosen],
              " release=", params$start_period[chosen],
              " distorsion=", params$dmean[chosen])

    tmp <- streamsim(params$n[chosen],
                     start_period = params$start_period[chosen],
                     median_vols = median_volumes$GB,
                     mean = params$mean[chosen],
                     sd = params$st[chosen],
                     dmean = params$dmean[chosen],
                     dst = params$dst[chosen])

    if ( i > 1 ) {
        simulated_df <- rbind ( simulated_df, tmp )
    } else {
        simulated_df <- tmp
    }

    i =100
 if ( i %% 100 == 0 ) { saveRDS(simulated_df, file.path("data", "simulation.rds"))}
}

distort <- simulated_df [ , 71:130]
simulated_df[z,11:70]

z = 16

plot (as.numeric(simulated_df[z,11:70])*as.numeric(simulated_df[z,71:130]))

as.numeric(simulated_df[z,99:128])

simulated <- streamsim ( n = 6000,
            start_period = 2,
            median_vols = median_volumes$GB,
            mean = 3,
            sd = 1.2)


data.frame (  date = median_volumes$report_start,
              streams = as.numeric( simulated[,7:(38+6)])) %>%
    ggplot ( aes ( x = date, y = streams)) +
    geom_point()


market_shares <- c( 0.25, 0.75)
market_shares2 <- c(0, 1)
market_shares1 <- c(1, 0)

prices <- matrix ( c(1,1,2,2, 0,3), nrow = 3)
prices

prices %*% market_shares
prices %*% market_shares2
prices %*% market_shares1
