#' irrigation function
#' 
#' @param precipitation (precip) (mm/day)
#' @param evapotranspiration (ET) (mm/day)
#' @param crop_type (wheat, corn, barley)
#' @returns list of relative soil moisture availability, whether that crop should be irrigated, and how much water should be given (in L).

irrigation=function(precip, ET, crop_type) {
  
  ### start with some error checking - we should never have negative ET or PE values
  ifelse (ET < 0, stop("Negative ET value supplied"), NA)
  ifelse (precip < 0, stop("Negative PE value supplied"), NA)
  ### we will also warn users if running function for non-compatible crop
  ifelse (!crop_type %in% c('wheat', 'corn', 'barley'), 
          return("Invalid crop type. Please enter either wheat, corn, or barley"), NA)
  
  
  ### use PE and ET as proxy for available soil moisture
  soil_moisture_avail = precip - ET
  
  ### how many days after rain event should you irrigate?
  should_irrigate = ifelse (soil_moisture_avail < 0, "No", "Yes") 
  
  ### how much should you irrigate?
  irrigation_amount = case_when((crop_type == 'wheat' & should_irrigate == "Yes") ~ 5,
                                (crop_type == 'corn' & should_irrigate == "Yes") ~ 3,
                                (crop_type == 'barley' & should_irrigate == "Yes") ~ 1,
                                (should_irrigate == "No") ~ 0)
    
    
  
  ## return outputs as list
  x = list(soil_moisture_avail, should_irrigate, irrigation_amount)
  return(x)
  
}

