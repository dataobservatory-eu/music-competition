index <- readxl::read_excel( file.path("data", "CEEMID-CI_index.xlsx"))
sim_df <- readRDS(  file.path("data", "simulation.rds") ) %>%
    mutate ( type = "rnorm")
sim_dfp <- readRDS(  file.path("data", "simulation_linear.rds") ) %>%
    mutate ( type = "linear")

reporting_periods <- as.Date(unique ( index$report_start))
volumes <- sim_df %>%
    bind_rows (sim_dfp) %>%
    select ( type, n, release, peak, starts_with ("month") ) %>%
    mutate ( song_name = glue::glue("{type}_{n}_release{release}_peak_{peak}_{1:nrow(.)}"))



select_songs_for_example <- function(seed = 2021) {
    set.seed(seed)
    song_sample_all_releases <- sample ( unique ( volumes$song_name), 10, replace = FALSE)
    song_sample_1_release  <- sample ( unique ( volumes %>%
                                                    filter ( release==1) %>%
                                                    select (song_name) %>%
                                                    unlist() %>% as.character()), 10, replace = FALSE)

    select_songs_release_1 <- volumes %>%
        filter ( .data$song_name %in% song_sample_1_release )

    select_songs_release_all <- volumes %>%
        filter ( .data$song_name %in% song_sample_all_releases )

    save ( select_songs_release_1, select_songs_release_all,
           file =  file.path(
               "data", paste0("select_songs_examples_",  Sys.Date(), ".rda")
               )
    )
    }

load ( file.path(
    "data", paste0("select_songs_examples_2021-05-04.rda")
) )

long_vols_release_1  <- select_songs_release_1 %>%
    pivot_longer ( cols = starts_with("month"),
                   values_to = "streams") %>%
    mutate ( streams = as.numeric(streams) ) %>%
    mutate ( month = as.numeric(gsub("month_", "", name)) ) %>%
    left_join ( median_price_matrix %>%
                    select ( all_of(c("report_start", "month", "GB"))),
                by = 'month') %>%
    mutate ( UK_revenue = (.data$GB * .data$streams)  )

long_vols_release_all  <- select_songs_release_all %>%
    pivot_longer ( cols = starts_with("month"),
                   values_to = "streams") %>%
    mutate ( streams = as.numeric(streams) ) %>%
    mutate ( month = as.numeric(gsub("month_", "", name)) ) %>%
    left_join ( median_price_matrix %>%
                    select ( all_of(c("report_start", "month", "GB"))),
                by = 'month') %>%
    mutate ( UK_revenue = (.data$GB * .data$streams)  )

stream_example_release_1_plot <- long_vols_release_1 %>%
    filter ( .data$streams > 0 ) %>%
    ggplot ( aes ( x = report_start,
                   y = streams,
                   group = song_name ,
                   color = song_name )) +
    geom_point() +
    geom_smooth( se = F,  method = 'loess', formula = 'y ~ x') +
    scale_color_manual ( values  = as.character(satellitereport::sr_palette())) +
    scale_y_log10( labels =  scales::comma ) +
    theme ( legend.position = "none") +
    labs ( title = "Simulated Songs With Various Levels of Popularity",
           subtitle = "10 randomly selected songs from the simulated repertoire",
           x = element_blank(), y = "monthly streams",
           caption = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021."))

ggsave ( file.path("plots", "stream_example_release_1_plot.png"),
         plot = stream_example_release_1_plot,
         units = "cm",
         width = 16,
         height = 9, dpi = 300)

stream_example_release_all_plot <- long_vols_release_all %>%
    filter ( .data$streams > 0 ) %>%
    ggplot ( aes ( x = report_start,
                   y = streams,
                   group = song_name ,
                   color = song_name )) +
    geom_point() +
    geom_smooth( se = F,  method = 'loess', formula = 'y ~ x') +
    scale_color_manual ( values  = as.character(satellitereport::sr_palette())) +
    scale_y_log10( labels =  scales::comma ) +
    theme ( legend.position = "none") +
    labs ( title = "Simulated Songs With Various Popularities & Release Dates",
           subtitle = "10 randomly selected songs with different release days",
           x = element_blank(), y = "monthly streams",
           caption = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021."))

ggsave ( file.path("plots", "stream_example_release_all_plot.png"),
         plot = stream_example_release_all_plot,
         units = "cm",
         width = 16,
         height = 9, dpi = 300)


save (stream_example_release_all_plot,stream_example_release_1_plot,
      file = file.path("plots", "simulation_example_plots.rda"))


volumes_release_1 <-volumes %>%
    filter ( release == 1 )

median_price_matrix <- index %>%
    select ( all_of(
        c( "report_start", "country_code", "median_value", "median_volume") )
        ) %>% mutate ( median_price  = median_value  / median_volume ) %>%
    select ( -all_of (c("median_value", "median_volume"))) %>%
    pivot_wider ( names_from = "country_code", values_from = "median_price") %>%
    mutate ( month = 1:nrow(.))

volume_matrix <- volumes %>%
    select ( starts_with ( "month")) %>%
    mutate_all ( as.numeric ) %>%
    as.matrix()

revenue_matrix_1 <- volume_matrix[ ,1:12] %*%  median_price_matrix$GB[1:12]
revenue_matrix_13 <- volume_matrix[ ,1:12] %*%  median_price_matrix$GB[1:12]


create_annual_domestic_revenues <- function ( volume_matrix, price_vector, start_period ) {
    annual_volume <- volume_matrix[ ,start_period:(11+start_period)]
    annual_price <- median_price_matrix$GB[start_period:(11+start_period)]
    annual_volume %*% annual_price
}

annual_revenues_GB  <- vapply ( 1:27,
                                function(x) create_annual_domestic_revenues(
                                    volume_matrix = volume_matrix,
                                    price_vector = median_price_matrix$GB,
                                    start_period  = x),
                                numeric(nrow(volume_matrix)))

annual_revenue_matrix <- annual_revenues_GB %>%
    as.tibble () %>%
    set_names ( paste0("revenue_", 1:27))

revenues_domestic_annual <- volumes  %>%
    select ( -starts_with (c("month"))) %>%
    bind_cols ( annual_revenue_matrix ) %>%
    pivot_longer ( starts_with ( "revenue_"),
                   names_to = "period",
                   values_to = "revenues") %>%
    mutate ( period = as.numeric (gsub("revenue_", "", .data$period))) %>%
    filter ( release == period )


table_revenues_domestic_1 <- volumes  %>%
    select ( -starts_with (c("month"))) %>%
    bind_cols ( annual_revenue_matrix ) %>%
    pivot_longer ( starts_with ( "revenue_"),
                   names_to = "period",
                   values_to = "revenues") %>%
    mutate ( period = as.numeric (gsub("revenue_", "", .data$period))) %>%
    filter ( release == 1, period  %in% 1:12 )

sum_table_revenues_domestic_1 <- table_revenues_domestic_1 %>%
    select ( song_name, type, n, revenues ) %>%
    mutate ( release = as.integer(release)) %>%
    mutate ( monthly_listen = paste0( "monthly ",
                                      sprintf("%.0f", as.numeric(.data$n)/60))
    ) %>%
    group_by ( monthly_listen, release )  %>%
    summarize ( mean = mean ( revenues),
                median = median ( revenues),
                max = max (revenues),
                min = min ( revenues),
                range = max - min,
                relative_range  = range / mean ,
                stdev = sd(revenues)) %>%
    mutate_if ( is.numeric, ~ round(., 2))

sprintf("%.0f", 10000)
revenue_domestic_df <- revenues_domestic_annual %>%
    mutate ( release = as.integer(release)) %>%
    mutate ( monthly_listen = paste0( "monthly ",
                                      sprintf("%.0f", as.numeric(.data$n)/60))
             ) %>%
    group_by ( monthly_listen, release )  %>%
    summarize ( mean = mean ( revenues),
                median = median ( revenues),
                max = max (revenues),
                min = min ( revenues),
                range = max - min,
                relative_range  = range / mean ,
                stdev = sd(revenues))  %>%
    left_join ( median_price_matrix %>%
                    select ( all_of(c("month", "report_start") )) %>%
                    mutate ( release = as.integer(.data$month) ),
                by = 'release') %>%
    mutate ( report_start = as.Date (.data$report_start) )

scale_color_manual ( values = c("#DB001C", "#007CBB" ))

revenue_release_day_uk_plot <- revenue_domestic_df %>%
    ggplot (
        aes ( x = report_start,
              y = mean,
              group = monthly_listen ) ) +
    geom_point () +
    geom_smooth ( method = 'lm', se = FALSE, formula = 'y ~ x', color = "#DB001C") +
    scale_x_date () +
    facet_wrap ( facets = "monthly_listen", scales = "free") +
    labs ( x = element_blank(), y = "average annual revenue",
           title = "Effect of Release Date on Revenues",
           subtitle = "Based on the CEEMID-CI median indexes in the United Kingdom only",
           caption = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021."))

#source(file.path("R", "market_share_scenarios.R"))
#random_shares <- create_random_shares(nrow(volume_matrix))

mp <- median_price_matrix %>%
    select ( all_of(c("report_start", names(random_shares))))

int_weight_price  <- as.tibble (
    as.matrix ( random_shares /100) %*% t(as.matrix ( mp[,-1]))
    )

not_use <- function(){vol_matrix <- volumes_df %>%
    select ( starts_with ( "month_") ) %>%
    as.matrix ()}

revd <- volume_matrix*d

create_annual_revenues <- function ( volume_matrix, price_matrix, start_period ) {
    annual_volume <- volume_matrix[ ,start_period:(11+start_period)]
    annual_price <- as.matrix(price_matrix[, start_period:(11+start_period)])
    rowSums(annual_volume * annual_price)
}

test_int_revenues <- create_annual_revenues( volume_matrix = volume_matrix,
                        price_matrix = int_weight_price,
                        start_period = 1 )

international_revenues <- vapply(1:27,
                                 function(x) {create_annual_revenues (volume_matrix,
                                                                      int_weight_price,
                                                                      x) },
                                 numeric(nrow(volume_matrix)) )


international_revenue_df <- volumes  %>%
    select( all_of (c("type", "n", "release"))) %>%
    bind_cols  ( as.tibble ( international_revenues ) %>%
                     set_names ( paste0("month_", 1:27 ))
     ) %>%
    pivot_longer ( starts_with ("month_"),
                   names_to = "month",
                   values_to = "weighted_revenues") %>%
    mutate ( month  = as.numeric ( gsub("month_", "", month ))) %>%
    left_join ( median_price_matrix %>%
                    select ( all_of(c("month", "report_start") )) %>%
                    mutate ( month = as.integer(.data$month) ),
                by = c('month') )  %>%
    filter ( month == .data$release ) %>%
    mutate ( report_start = as.Date (.data$report_start) ) %>%
    mutate ( monthly_listen = paste0( "monthly ",
                                      sprintf("%.0f", as.numeric(.data$n)/60))
    )  %>%
    group_by ( report_start, monthly_listen ) %>%
    summarize ( mean = mean ( weighted_revenues),
                median = median ( weighted_revenues),
                max = max (weighted_revenues),
                min = min ( weighted_revenues),
                range = max - min,
                relative_range  = range / mean ,
                stdev = sd(weighted_revenues))

dom_rev_plot <- revenue_domestic_df %>%
    ggplot ( ., aes ( x = report_start,
                      y = mean,
                      group = monthly_listen ) ) +
    geom_point () +
    geom_smooth ( method = 'lm', se = FALSE, formula = 'y ~ x', color = "#DB001C") +
    scale_x_date ( date_breaks = "6 month", date_labels = "%Y-%m" ) +
    facet_wrap ( facets = "monthly_listen", scales = "free") +
    labs ( x = element_blank(), y =element_blank(),
           subtitle = "GB only") +
    theme ( axis.text.x = element_text(size =7) )

dom_rev_plot

int_rev_plot <- international_revenue_df  %>%
    ggplot ( ., aes ( x = report_start,
                      y = mean,
                      group = monthly_listen ) ) +
    geom_point () +
    scale_x_date ( date_breaks = "6 month", date_labels = "%Y-%m" ) +
    geom_smooth ( method = 'lm',
                  se = FALSE,
                  formula = 'y ~ x', color =  "#007CBB"  ) +
    scale_x_date ( date_breaks = "6 month", date_labels = "%Y-%m" ) +
    facet_wrap ( facets = "monthly_listen", scales = "free") +
    labs ( x = element_blank(), y = element_blank(),
           subtitle = "GB, DE, NL, CH, CZ, HU") +
    theme ( axis.text.x = element_text(size =7) )

coplot <- gridExtra::grid.arrange(
    dom_rev_plot,
    int_rev_plot,
    ncol = 2,
    left = "average annual revenue",
    top= "Effect of International Diversification on Revenues",
    bottom = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021."))

scaling_factor <- 1.4
ggsave ( file.path("plots", "Effect_International_Diversification_Revenues_Coplot.png"),
         plot =coplot,
         units = "cm",
         width = 16*scaling_factor,
         height = 9*scaling_factor, dpi = 250)


all_revenues_df <- international_revenue_df %>%
    pivot_longer ( cols = c("mean", "median",
                            "max", "min",
                            "range", "relative_range", "stdev") ) %>%
    mutate ( group = "international" )  %>%
    full_join (revenue_domestic_df %>%
                   pivot_longer ( cols = c("mean", "median",
                                           "max", "min",
                                           "range", "relative_range", "stdev") ) %>%
                   mutate ( group = "domestic" )  )


effect_int_rev_plot <- all_revenues_df  %>%
    select ( report_start, monthly_listen, group, name, value ) %>%
    filter ( .data$name %in% c("max", "mean", "median", "min") ) %>%
    ggplot ( ., aes ( x = report_start,
                      y = value,
                      color = group,
                      group = monthly_listen ) ) +
    geom_point () +
    scale_x_date ( date_breaks = "6 month", date_labels = "%Y-%m" ) +
    scale_color_manual ( values = c("#DB001C", "#007CBB" )) +
    facet_grid ( monthly_listen ~ name,
                 scales = "free") +
    labs ( x = element_blank(), y = "average annual international revenue",
           color = NULL,
           title = "Effect of International Diversification on Revenues",
           subtitle = "Based on the CEEMID-CI median indexes in GB, DE, NL, CH, CZ, HU",
           caption = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021.")) +
    theme ( legend.position = "bottom"  )

ggsave ( file.path("plots", "Effect_International_Diversification_Revenue.png"),
         plot = effect_int_rev_plot,
         units = "cm", width = 16, height = 9, dpi = 300)

effect_int_risk_plot <- all_revenues_df  %>%
        select ( report_start, monthly_listen, group, name, value ) %>%
        filter ( .data$name %in% c("range", "stdev") ) %>%
        ggplot ( ., aes ( x = report_start,
                          y = value,
                          color = group,
                          group = monthly_listen ) ) +
        geom_point () +
        scale_x_date (
            date_breaks = "6 month", date_labels = "%Y-%m"
            ) +
        scale_color_manual ( values = c("#DB001C", "#007CBB" )) +
        facet_grid ( monthly_listen ~name,
                     scales = "free") +
        labs ( x = element_blank(), y = "average annual international revenue",
               color = NULL,
               title = "Effect of International Diversification on Risk Metrics",
               subtitle = "Based on the CEEMID-CI median indexes in GB, DE, NL, CH, CZ, HU",
               caption = glue::glue ("Based on {nrow (sim_df)} lognormal and {nrow(sim_dfp)} hypothetical perenial songs
                                 \ua9 Daniel Antal, music.dataobservatory.eu, 2021.")) +
        theme ( legend.position = "bottom",
                axis.text.x = element_text(size =7))

effect_int_risk_plot

ggsave ( file.path("plots", "Effect_International_Diversification_Risk.png"),
         plot =effect_int_risk_plot,
         units = "cm", width = 16, height = 9, dpi = 300)

save ( effect_int_rev_plot,
       effect_int_risk_plot,
       revenue_release_day_uk_plot,
       sum_table_revenues_domestic_1 ,
       file = file.path("plots", "revenue_scenarios.rda"))
