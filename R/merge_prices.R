#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param gdx path to REMIND binary output, gdx format
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param REMINDyears range of REMIND timesteps
#' @param intensity_data logit level intensity data
#' @param nonfuel_costs logti level non-fuel costs
#' @param module GDX input is based on old "complex" module or new "edge_esm" module
#' @import data.table
#' @importFrom rmndt disaggregate_dt magpie2dt
#' @importFrom gdx readGDX
#' @importFrom magclass time_interpolate lowpass dimSums mbind
#' @export

merge_prices <- function(gdx, REMINDmapping, REMINDyears,
                         intensity_data,
                         nonfuel_costs,
                         module="edge_esm") {
    sector_fuel <- pih <- value <- fuel_price <- fuel_price_pkm <- EJ_Mpkm_final <- NULL
    non_fuel_price <- technology <- GDP_cap <- region <- `.` <- weight <- POP_val <- NULL
    GDP <- yearconv <- time <- year_at_yearconv <- non_fuel_price_conv <- non_fuel_price_trend <- NULL
    vehicle_type <- subsector_L3 <- subsector_L2 <- subsector_L1 <- sector <- tot_price <- NULL
    ## report prices from REMIND gdx in 2005$/MJ

    tdptwyr2dpgj <- 31.71  #TerraDollar per TWyear to Dollar per GJ
    CONV_2005USD_1990USD <- 0.67
    startyear <- 2020

    ## load entries from the gdx
    fety <- readGDX(gdx, c("entyFe", "fety"), format = "first_found")
    pebal_subset <- c("pegas")

    budget.m <- readGDX(gdx, name = "qm_budget", types = "equations", field = "m",
                        format = "first_found")[, REMINDyears[REMINDyears >= startyear],]  # Alternative: calcPrice

    interpolate_first_timesteps <- function(obj){
        ## interpolate values for 1990, 2005 and 2010
        obj = time_interpolate(obj, c(1990, seq(2005, startyear, 5)),
                             integrate_interpolated_years=T,
                             extrapolation_type = "constant")
        return(obj)
    }

    budget.m <- interpolate_first_timesteps(budget.m)
    budget.m <- lowpass(budget.m)

    if(module == "edge_esm"){

        bal_eq <- "q35_demFeTrans"
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")

        if (is.null(febal.m)){ # temporary fix for compatibility with old version
          febal.m <- readGDX(gdx, name = "qm_balFeForCesAndEs", types = "equations",
                             field = "m", format = "first_found")
        } else {
          febal.m <- dimSums(febal.m,dim=c(3.2))
        }

        febal.m <- febal.m[, REMINDyears[REMINDyears>=startyear], fety]

        if(any(febal.m > 0)){
            sprintf("Found positive marginals on %s. We correct this, but the issue should be adressed.", bal_eq)
            febal.m[febal.m > 0] <- -1e-10
        }

        febal.m <- interpolate_first_timesteps(febal.m)

        ## in some regions and time steps, 0 final energy demand for an entry could give problems
        tmp <- magclass::setNames(lowpass(lowpass(febal.m[, , "fegat"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }else{
        bal_eq <- "q_balFe"
        pebal.m <- readGDX(gdx, name = c("q_balPe", "qm_pebal"), types = "equations",
                           field = "m", format = "first_found")[, REMINDyears[REMINDyears>=startyear],
                                                                pebal_subset]
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")[, REMINDyears[REMINDyears>=startyear],
                                                                fety]
        if(any(febal.m > 0)){
            sprintf("Found positive marginals on %s. We correct this, but the issue should be adressed.", bal_eq)
            febal.m[febal.m > 0] <- -1e-10
        }

        febal.m <- interpolate_first_timesteps(febal.m)
        pebal.m <- interpolate_first_timesteps(pebal.m)

        ## Nat. Gas too cheap on PE level (4x)
        tmp <- magclass::setNames(4*lowpass(lowpass(pebal.m[, , "pegas"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }
    sprintf("Loading prices for module `%s` from eq. `%s`.", module, bal_eq)

    tmp <- mbind(tmp, magclass::setNames(lowpass(lowpass(febal.m[, , "feelt"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "elect_td_trn"))

    tmp <- mbind(tmp, magclass::setNames(lowpass(lowpass(febal.m[, , "feh2t"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "H2 enduse"))

    tmp <- mbind(tmp, magclass::setNames((dimSums(lowpass(febal.m[, , "fedie"] + febal.m[, , "fepet"]), dim=3))/(2*budget.m + 1e-10) * tdptwyr2dpgj, "refined liquids enduse"))

    tmp <- magpie2dt(tmp, regioncol = "region", yearcol = "year", datacols = "sector_fuel")
    test <- tmp[year > 2005 & value <= 0]
    if(nrow(test)){
        print(paste("Zero Fuel Prices found. Regions:", unique(test$region)))
        print("The weighted averages of non-zero regions of the corresponding fuel will be used.")
        tmp[, value := ifelse(value <= 0, mean(value[value>0]), value), by = c("year", "sector_fuel")]
    }

    ## define plug in hybrid as consuming 60% liquids and 40% electricity
    tmp_PIH <- tmp[sector_fuel %in% c("elect_td_trn", "refined liquids enduse")]
    tmp_PIH[, pih := 0.6*value[sector_fuel == "refined liquids enduse"] + 0.4*value[sector_fuel == "elect_td_trn"], by = c("region", "year")]
    tmp_PIH <- tmp_PIH[sector_fuel == "elect_td_trn"][, c("sector_fuel", "value") := list("Liquids-Electricity", NULL)]
    setnames(tmp_PIH, old = "pih", new = "value")

    fuel_price_REMIND <- rbind(tmp, tmp_PIH)

    ## rename the fuel price
    setnames(fuel_price_REMIND, old = c("value"), new = c("fuel_price"))

    ## fuel price in 2005USD/GJ -> 1990USD/EJ

    fuel_price_REMIND[, fuel_price := fuel_price * CONV_2005USD_1990USD * 1e9]

    ## join with vehicle intensity and load factor to get the 1990USD/pkm

    km_intensity <- intensity_data[year %in% REMINDyears]
    fuel_price_REMIND <- merge(fuel_price_REMIND, km_intensity, by = c("region",
        "year", "sector_fuel"), all.y = TRUE)

    ## fuel_price [$/EJ * EJ/Mpkm * Mpkm/pkm],
    tech_cost <- fuel_price_REMIND[, fuel_price_pkm := fuel_price * EJ_Mpkm_final * 1e-6]

    ## merge the non energy prices, they are $/pkm
    tech_cost <- merge(tech_cost, nonfuel_costs,
                        by = c("region", "year", "technology",
                               "vehicle_type", "subsector_L1",
                               "subsector_L2", "subsector_L3","sector"),
                        all.x = TRUE)

    ## calculate the total price
    tech_cost[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost)

}
