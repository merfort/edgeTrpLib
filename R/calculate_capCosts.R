#' Evaluate the costs of REMIND technology placeholders (e.g. te_eselt_pass_sm).
#'
#' @param base_price base prices from logit calculations
#' @param Fdemand_ES normalized ES demand
#' @param stations costs of stations in $/km
#' @param EDGE2CESmap map from top level EDGE-T/GCAM categories to REMIND CES nodes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param REMINDyears range of REMIND timesteps
#' @param scenario EDGE-T scenario name
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @export

calculate_capCosts <-function(base_price, Fdemand_ES, stations,
                              EDGE2CESmap,
                              EDGE2teESmap,
                              REMINDyears,
                              scenario){

  teEs <- region <- variable <- value <- demand_F <- `.` <- subsector_L3 <- vehicle_type <- NULL
  vehicles_number <- annual_mileage <- load_factor <- demand <- technology <- cost_st_km <- NULL

  ## the non fuel price is to be calculated only for motorized entries
  Fdemand_ES=Fdemand_ES[!subsector_L3 %in% c("Walk","Cycle"),]
  base_price=base_price[!subsector_L3 %in% c("Walk","Cycle"),]
  ## merge prices and demand
  ## TODO at the moment, Hybrid Electric veh cannot be included in this calculation because they have 2 fuels (elec, liq) and cannot be mapped to one
  ## fuel only. This has to be fixed.
  data=merge(base_price,Fdemand_ES[technology != "Hybrid Electric"],all.y=TRUE,by=intersect(names(base_price),names(Fdemand_ES)))

  ## merge with mappings
  data=merge(data,EDGE2CESmap,all.x=TRUE,by=intersect(names(data),names(EDGE2CESmap)))
  data=merge(data,EDGE2teESmap,all=TRUE,by=intersect(names(data),names(EDGE2teESmap)))
  ## summarise and find the average prices
  data=data[,.(non_fuel_price=sum(non_fuel_price*demand_F/sum(demand_F))), by=c("region","year","teEs")]

  ## merge with the stations costs
  data = merge(data, stations, all = TRUE, by = c("teEs", "region", "year"))
  data[is.na(cost_st_km), cost_st_km := 0]
  ## temporarily set to 0 the station costs
  data[, cost_st_km := NULL]

  non_fuel_price = melt(data, id.vars = c("region", "year", "teEs"),
                            measure.vars = c("non_fuel_price"))

  setcolorder(non_fuel_price, c("region","year","teEs","variable","value"))

  #rows with NaNs are deleted (meaning: no price, and no demand, in the specific region/year)
  non_fuel_price=non_fuel_price[!is.nan(value),]

  non_fuel_price = approx_dt(non_fuel_price, REMINDyears,
                     xcol = "year", ycol = "value",
                     idxcols = c("region", "teEs", "variable"),
                     extrapolate=T)

  non_fuel_price=non_fuel_price[variable=="non_fuel_price",]
  non_fuel_price[,variable:=NULL]
  non_fuel_price=non_fuel_price[order(region,year,teEs)]

  return(non_fuel_price)
}
