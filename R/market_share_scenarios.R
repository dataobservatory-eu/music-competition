create_random_shares <- function (n) {

    GB_share <- runif ( n,30,70)
    DE_share <- 100-GB_share
    DE_share <- (100-GB_share) * runif (length(GB_share), 0.1, 0.5)
    NL_share <- (100-GB_share-DE_share)*runif(length(DE_share), 0.1,0.8)
    CH_share <- (100-GB_share-DE_share-NL_share)*runif(length(DE_share), 0.1,0.9)
    CZ_share <- (100-GB_share-DE_share-NL_share-CH_share)*runif(length(DE_share), 0.1,0.9)
    HU_share <- (100-GB_share-DE_share-NL_share-CH_share-CZ_share)

    tmp <- data.frame (
        GB = GB_share,
        DE = DE_share,
        NL = NL_share,
        CH = CH_share,
        CZ = CZ_share,
        HU  = HU_share
    )

    tmp
}

