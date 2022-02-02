library(tidyverse)
## Create perenial hits in random moments, and with random disturbance, but stead streams.

ceemid_ci <- readxl::read_excel(file.path("data", "CEEMID-CI_index.xlsx"))
median_volumes <- ceemid_ci %>%
    select ( report_start, country_code, median_volume ) %>%
    pivot_wider( names_from = country_code, values_from  = median_volume ) %>%
    mutate_if ( is.numeric, function(x) round(x, 0))

streamsim_perenial <- function(n = 600,
                      start_period  = 1,
                      median_vols = rep(0, 38),
                      dmean = 0, dst = 0) {

    leading_zero_vector <- rep ( 0, start_period - 1)
    monthly_volume = n/60

    missing_length <- max(0, 60 -(start_period-1 + length(median_vols)) )
    missing_length
    if (missing_length>0) {
        forecasted_median_vols <- rep(monthly_volume, missing_length)
        long_tail <- round (c( leading_zero_vector, median_vols, forecasted_median_vols), 0)

    } else {
        long_tail  <- c(leading_zero_vector, round(median_vols,0))
    }

    length(long_tail)
    sum(long_tail)
    if ( is.na(sum(long_tail)) ) stop ( 'Missing value in long_tail')


    remaining_count <- n - sum(long_tail[1:min(length(long_tail), 60)])
    if ( is.na(remaining_count)) stop ( "remaining count is NA")
    if ( remaining_count <0 ) stop ("remaining_count is negative")



    simulated_random_streams <- rep ( monthly_volume, 60)
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
    params <- c(release, n, monthly_volume, 0, mode, peak, dmean, dst, s )

    names ( params) <- c("release", "n", "mean", "sd", "mode", "peak", "dmean", "dst" , "sum")

    distorsions =  1+rnorm(60, dmean ,dst )

    names(distorsions) = paste0("dist_", 1:60)

    as.data.frame ( cbind ( type = "linear",
                            t(c(params,streams, distorsions))
                            )
    )
}

simulated_df <- streamsim_perenial ( n = 60000,
                         start_period = 1,
                         median_vols = median_volumes$GB)


params_1_linear <- expand.grid (
    c(6000, 60000, 600000),
    1:25,
    seq(-0.3,0, by = 0.025),
    c(0.01,0.02))

params_2_linear <- expand.grid (
    6000000,
    1:25,
    seq(0,0.3, by = 0.025),
    c(0.01,0.02)
    )

names ( params_1_linear) <- names(params_2_linear) <- c("n", "start_period", "dmean", "dst")

params_linear <- rbind ( params_1_linear, params_2_linear)

simulated_df <- streamsim_perenial (params_linear$n[1],
                          start_period = params_linear$start_period[1],
                          median_vols = median_volumes$GB,
                          dmean = params_linear$dmean[1],
                          dst = params$dst[1])

cycle = 2:nrow(params)
lc <- length(cycle)
lc

cyc <- sample ( cycle, 500, replace = FALSE)
i = 1
for ( i in  seq_along (cyc) ) {

    chosen <- cyc[i]

    message ( i, "/ ", length(cyc), " ", chosen,  " n= ",
              params$n[chosen], " release=", params$start_period[chosen],
              " distorsion=", params$dmean[chosen])

    tmp <- streamsim_perenial (params$n[chosen],
                     start_period = params$start_period[chosen],
                     median_vols = median_volumes$GB,
                     dmean = params$dmean[chosen],
                     dst = params$dst[chosen])

    if ( i > 1 ) {
        simulated_lin_df <- rbind ( simulated_lin_df, tmp )
    } else {
        simulated_lin_df <- tmp
    }

    i =100
 if ( i %% 100 == 0 ) { saveRDS(simulated_lin_df,
                                file.path("data", "simulation_linear.rds"))}
}

distort <- simulated_df [ , 71:130]



