#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param gdx
#' @param REMINDmapping
#' @param path2intensities
#' @param path2UCD
#' @import remind
#' @import data.table
#' @importFrom rmndt toISO_dt toRegions_dt
#' @export

merge_prices <- function(gdx, REMINDmapping, path2intensities, path2UCD) {
    ## report prices from REMIND gdx in 2005$/MJ
    tdptwyr2dpgj <- 31.71  #TerraDollar per TWyear to Dollar per GJ
    
    ## load entries from the gdx
    fety <- readGDX(gdx, c("entyFe", "fety"), format = "first_found")
    pebal_subset <- c("pegas", "pecoal")
    
    febal.m <- readGDX(gdx, name = c("q_balFe", "q_febal"), types = "equations",
                       field = "m", format = "first_found")[, years, fety]
    budget.m <- readGDX(gdx, name = "qm_budget", types = "equations", field = "m",
                        format = "first_found")[, years,]  # Alternative: calcPrice
    pebal.m <- readGDX(gdx, name = c("q_balPe", "qm_pebal"), types = "equations",
                       field = "m", format = "first_found")[, years, pebal_subset]
    
    tmp <- setNames(abs(lowpass(febal.m[, , "feelt"]/(budget.m + 1e-10), fix = "both",
                                altFilter = match(2010, years))) * tdptwyr2dpgj, "Price|Final Energy|Electricity|Transport|Moving Avg (US$2005/GJ)")
    
    tmp <- mbind(tmp, setNames(abs(lowpass(febal.m[, , "feh2t"]/(budget.m + 1e-10),
                                           fix = "both", altFilter = match(2010, years))) * tdptwyr2dpgj, "Price|Final Energy|Hydrogen|Transport|Moving Avg (US$2005/GJ)"))
    tmp <- mbind(tmp, setNames(abs(lowpass(febal.m[, , "fedie"]/(budget.m + 1e-10),
                                           fix = "both", altFilter = match(2010, years))) * tdptwyr2dpgj, "Price|Final Energy|Liquids|Transport|Moving Avg (US$2005/GJ)"))
    tmp <- mbind(tmp, setNames(pebal.m[, , "pegas"]/(budget.m + 1e-10) * tdptwyr2dpgj,
                               "Price|Natural Gas|Primary Level (US$2005/GJ)"))
    tmp <- mbind(tmp, setNames(pebal.m[, , "pecoal"]/(budget.m + 1e-10) * tdptwyr2dpgj,
                               "Price|Coal|Primary Level (US$2005/GJ)"))
    
    tmp <- magpie2dt(tmp, regioncol = "region",
                     yearcol = "year", datacols = "sector_fuel")

    fuel_price_REMIND <- toISO_dt(tmp, mapping = REMINDmapping)
    
    fuel_price_REMIND <- toRegions_dt(fuel_price_REMIND, GCAM2ISO_MAPPING,
                                   datacol = "sector_fuel",
                                   valuecol = "value",
                                   strategy = "gdp")
    
    fuel_price_REMIND[, `:=`(sector_fuel, ifelse(grepl("Electricity", sector_fuel),
                                                 "elect_td_trn", sector_fuel))]
    fuel_price_REMIND[, `:=`(sector_fuel, ifelse(grepl("Hydrogen", sector_fuel),
                                                 "H2 enduse", sector_fuel))]
    fuel_price_REMIND[, `:=`(sector_fuel, ifelse(grepl("Liquids", sector_fuel), "refined liquids enduse",
                                                 sector_fuel))]
    fuel_price_REMIND[, `:=`(sector_fuel, ifelse(grepl("Gas", sector_fuel), "delivered gas",
                                                 sector_fuel))]
    fuel_price_REMIND[, `:=`(sector_fuel, ifelse(grepl("Coal", sector_fuel), "delivered coal",
                                                 sector_fuel))]
    
    ## rename the fuel price
    setnames(fuel_price_REMIND, old = c("value"), new = c("fuel_price"))
    
    ## fuel price in 2005USD/GJ -> 1990USD/EJ
    
    fuel_price_REMIND[, fuel_price := fuel_price * CONV_2005USD_1990USD * 1e9]
    
    ## apply the markup fro NG and coal (coal can be negative!!, as a workaround I
    ## make it positive and apply the markup):
    fuel_price_REMIND[, fuel_price := ifelse(sector_fuel == "delivered gas", fuel_price/0.2,
                                                fuel_price)]
    fuel_price_REMIND[, fuel_price := ifelse(sector_fuel == "delivered coal",
                                                abs(fuel_price/0.2), fuel_price)]

    if(all(fuel_price_REMIND[year == 1990]$fuel_price == 0)){
        ## if no 1990 prices are found, lets use 2005 prices and issue warning
        warning("No 1990 fuel prices found in REMIND, using 2005 prices.")
        fuel_price_REMIND[year == 1990, fuel_price := fuel_price_REMIND[year==2005]$fuel_price] 
    }
    
    stopifnot(all(fuel_price_REMIND$fuel_price > 0))
    
    ## join with vehicle intensity and load factor to get the 1990USD/pkm

    km_intensity <- readRDS(path2intensities)
    km_intensity <- km_intensity[year %in% years,]
    fuel_price_REMIND <- merge(fuel_price_REMIND, km_intensity, by = c("region",
        "year", "sector_fuel"), all.y = TRUE)


    ## fuel_price [$/EJ * EJ/Mpkm * Mpkm/pkm], 
    tech_cost2 <- fuel_price_REMIND[, fuel_price_pkm := fuel_price * EJ_Mpkm_final * 1e-6]
    tech_cost2=tech_cost2[,-c("EJ_Mpkm","EJ_Mpkm_adjusted","lambda","EJ_Mpkm")]

    ## merge the non energy prices, they are $/pkm
    non_energy_cost_result <- readRDS(path2UCD)[["non_energy_cost"]]
    tech_cost2 <- merge(tech_cost2, non_energy_cost_result,
                        by = c("region", "year", "technology",
                               "vehicle_type", "subsector_L1",
                               "subsector_L2", "subsector_L3","sector"),
                        all.x = TRUE)

    ## missing non energy price for coal, Adv-Electric and Adv-Liquids freight rail
    ## (is not in the GCAM database). Attribute the same non energy price as in
    ## Liquids Freight Rail
    tech_cost2 <- tech_cost2[, non_fuel_price := ifelse(technology %in% c("Coal",
        "Adv-Electric", "Adv-Liquid") & vehicle_type == "Freight Rail_tmp_vehicletype",
        non_fuel_price[technology == "Liquids" & vehicle_type == "Freight Rail_tmp_vehicletype"],
        non_fuel_price), by = c("region", "year")]

    ## delete the MiniCars from EU-12 (they should not be in the load factor...)
    ## tech_cost2 <- tech_cost2[!(region == "EU-12" & vehicle_type == "Mini Car"), ]

    ## calculate the total price
    tech_cost2[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost2)

}