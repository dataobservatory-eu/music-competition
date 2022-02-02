library(tidyverse)
library(currencycode)
### currency rates ----------------------------------------------------------
currency_rates <- read.csv ( file.path ("data", "currency_rates.csv"))
index <- readxl::read_excel( file.path("data", "CEEMID-CI_index.xlsx"))
zzz <- currency_rates$EUR[2]/currency_rates$EUR[1]-1

country_prices <- index %>% select ( all_of(c("report_start", "country_code",
                            "median_value", "median_volume"))) %>%
    mutate ( median_price  = median_value / median_volume ) %>%
    select ( -c("median_value", "median_volume")) %>%
    left_join ( currencycode::currency_list %>%
                    select ( countrycode_2, currency_code) %>%
                    rename ( country_code = .data$countrycode_2)) %>%
    mutate ( currency_code = case_when (
        country_code %in% c("SI", "LT", "SK", "DE", "NL", "AT", "LV", "EE", "BA") ~  "EUR",
        TRUE ~ currency_code
    ))

europe_palette <- satellitereport::palette_europe_countries()
europe_palette["CH"] <- 'red'

price_distribution_plot <- country_prices %>%
    filter ( country_code %in% c("GB", "DE", "NL","SK", "LT", "CH", "AT", "CZ", "HU")) %>%
    filter ( median_price < 0.1) %>%
    ggplot ( aes ( x = median_price,
                    fill = country_code )) +
    geom_histogram(bins = 9 ) +
    scale_fill_manual( values = europe_palette ) +
    facet_wrap ( facets = "country_code") +
    theme ( legend.position = "bottom") +
    labs ( fill = NULL,
           title = "Distribution of Streaming Values")


country_price_wide <- country_prices %>%
    pivot_wider ( names_from = country_code, values_from = median_price )

country_price_summary <- country_prices %>%
    filter ( ! .data$country_code %in% c("BA", "SI", "RS", "MK", "AL", "ME")) %>%
    group_by ( country_code ) %>%
    summarize ( mean = mean(median_price)*100,
                stdev = sd(median_price*100),
                range = (max(median_price)-min(median_price))*100,
                abs_dev_pct = (range / mean)*100) %>%
    arrange ( -mean )
country_price_summary

currency_rates_long <- currency_rates %>%
    mutate ( report_start = as.Date( report_start )) %>%
    mutate ( reporting_period = ifelse ( report_start %in% unique(as.Date(index$report_start)),
                                         "index period", "outside period")) %>%
    pivot_longer( cols = -starts_with("report"),
                  names_to = "currency",
                  values_to = "rate" )

currency_rate_summary <- currency_rates_long  %>%
    group_by ( currency ) %>%
    summarize ( mean = mean(rate),
                stdev = sd(rate),
                range = (max(rate)-min(rate)),
                abs_dev_pct = (range / mean)*100) %>%
    arrange ( -mean )

currency_rate_environment_plot <- currency_rates_long %>%
    ggplot ( aes ( x = report_start,
                   y = rate,
                   color = reporting_period,
                   group = currency)) +
    geom_point () +
    scale_color_manual ( values = c("#DB001C", "grey80"))+
    geom_smooth( se = F, method = "lm", formula = 'y ~ x')+
    facet_wrap ( facets = "currency", scales = "free") +
    theme ( legend.position = "bottom") +
    labs ( x = element_blank(), y = element_blank(),
           color = NULL,
           title = "Currency Rate Environment",
           subtitle = "Select Exchange Rates During the CEEMID-CI Index Period",
           caption = "The linear trend line only illustrates the overal depreciation trend")

example <- data.frame (
    money  = c(100,100,100),
    exch = c(1.22, 1.11,1.34)
)

save ( currency_rate_environment_plot ,
       price_distribution_plot,
       country_price_summary ,
       europe_palette,
       currency_rate_summary,
       file = file.path("plots", "exchange_rate_plots.rda"))

gbp <- example$money / example$exch
gbp
(example$exch[1] / example$exch[2] -1 ) * example$money[1]
(example$exch[2] / example$exch[1] -1  ) * example$money[1]
(example$exch[2] / example$exch[1] ) * example$money[1]
index_period <- median_price_matrix %>%
    select ( report_start )

currency_scenarios <- currency_rates %>%
    mutate ( report_start = as.Date ( report_start )) %>%
    mutate ( period = 1:nrow(.))

index_periods <- index_period %>%
    inner_join ( currency_scenarios) %>%
    select ( all_of(c("period"))) %>%
    unlist ( ) %>% as.integer()

min ( index_periods):max ( index_periods)

currency_matrix <- currency_rates[ index_periods, -1 ]

400 / 398

alt_curr_sc <- (currency_rates[ index_periods-1, -1 ] / currency_rates[ index_periods, -1 ])


mp <- median_price_matrix %>%
    select ( all_of(c("report_start", names(random_shares))) )

mp[, -1]* alt_curr_sc


exchange_matrix = alt_curr_sc
price_matrix <- mp[,-1]

create_alternative_exchange_scenarios <- function ( timelag = 0,
                                                    random_shares = random_shares,
                                                    currency_rates = currency_rates,
                                                    mp_matrix = mp[,-1]
                                                    ) {

    exchange_factor <- (
        currency_rates[ index_periods-timelag, -1 ] / currency_rates[ index_periods, -1 ]
        )


    alternative_weight_price <- function ( random_shares = random_shares,
                                           price_matrix,
                                           exchange_matrix) {

        alt_price_matrix <- as.matrix ( price_matrix) * exchange_matrix
        as.matrix ( random_shares /100) %*% t(as.matrix ( price_matrix ))
    }

    create_annual_revenues <- function ( volume_matrix, price_matrix, start_period ) {
        annual_volume <- volume_matrix[ ,start_period:(11+start_period)]
        annual_price <- as.matrix(price_matrix[, start_period:(11+start_period)])
        rowSums(annual_volume * annual_price)
    }

    awp1 = alternative_weight_price (random_shares,
                                     price_matrix = mp[,-1],
                                     exchange_matrix = exchange_factor )

     create_annual_revenues( volume_matrix = volm,
                                 price_matrix = awp1,
                                 start_period = 1 )
}

a <- create_alternative_exchange_scenarios(timelag = 0,
                                           random_shares = random_shares,
                                           currency_rates = currency_rates,
                                           mp_matrix = mp[,-1])
b <- international_revenues_df
names ( a )
