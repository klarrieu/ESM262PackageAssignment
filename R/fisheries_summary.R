#' Summary of Fisheries Data
#'
#' This function computes the most frequently caught fish in each fishery,
#' the revenue from each fishery, the total revenue from all fisheries,
#' and optionally generates a plot of revenue by fishery.
#'
#' @param catches a matrix containing the number of each fish species caught in each location,
#' with rows for species and columns for locations
#' @param prices a data frame containing the price of each fish species
#' @param plot if set to TRUE, will generate a plot of revenue by fishery
#' @author Kenneth Larrieu
#' @import tidyverse

fisheries_summary = function(catches, prices, plot = FALSE){

  locations = colnames(catches)
  num_locs = length(locations)

  #get most frequently caught fish in each location
  maxes = rownames(catches)[apply(catches, 2, which.max)]
  maxes = data.frame(locations = colnames(catches), most_caught = maxes)

  #get revenue for each location
  catches = as.data.frame(catches)
  catches = add_column(catches, fish = as.factor(rownames(catches)))
  joined = merge(prices, catches, by = "fish", all.y = TRUE)
  rev = rep(NA, times = num_locs)

  for (i in 1:num_locs){
    rev[i] = crossprod(joined[[i+2]],joined$price)
  }

  rev = data.frame(location = locations, revenue = rev)

  #get total revenue
  rev_tot = sum(rev$revenue)

  #make plot if plot parameter is set to TRUE
  if (plot == TRUE){
    rev_plot = ggplot(data = rev, aes(x = location, y = revenue)) + geom_bar(stat="identity") +
      labs(x = "Location", y = "Revenue", title = sprintf("The total revenue is $%.2f", rev_tot))

    return(list("most_caught" = maxes,"revenues" = rev, "total_revenue" = rev_tot,
                "revenue_plot" = rev_plot))
  }

  else {
    return(list("most_caught" = maxes,"revenues" = rev, "total_revenue" = rev_tot))
  }

}
