#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at a given speed
#' 
#' @param cdrag coefficient due to drag default=0.3 
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param p_air (kg/m3) default =1.2
#' @return power (W)

auto_power = function(m, v, A, crolling = 0.015, g = 9.8,
                      p_air = 1.2, cdrag = 0.3) {
  result = crolling * m * g * v + 0.5*A * p_air * cdrag * v^3
  return(result)
}



