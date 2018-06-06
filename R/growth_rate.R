#' Fish Growth Rate
#'
#' This function estimates the growth rate per day for Atlantic Cod, using a third
#' degree polynomial function of temperature (in degrees Celsius).
#' The default values of parameters a,b,c,d are obtained from
#' Experiment A in Table 2 of Bjoornsson et al.
#' @param t temperature in degrees Celsius
#' @param a constant term in estimate function
#' @param b first degree coefficient in estimate function
#' @param c second degree coefficient in estimate function
#' @param d third degree coefficient in estimate function
#' @references Bjoornsson et al. Growth model for Atlantic cod (Gadus morhua):
#' Effects of temperature and body weight on growth rate.
#' @author Kenneth Larrieu
#' @examples growth_rate(15)

#returns percent growth per day given temperature t in Celsius
growth_rate = function(t, a = -0.4970, b =  0.1656, c =  0.08588, d = -0.004266){

  G = G=a+b*t+c*t**2+d*t**3

  return(G)
}
