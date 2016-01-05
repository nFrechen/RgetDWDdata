# Berechnung der Grasreferenzverdunstung
evap.net.radiation <- function(R_G=NA, sunshine=NA, doy=NA, date=NA, temp, U, latitude, albedo=0.23){
	if(all(is.na(doy)) & all(is.na(date))) stop("Either doy or date have to be given")
	if(all(is.na(doy))) doy = yday(date)
	Rn_K(R_G, temp, sunshine, doy, latitude, albedo) - Rn_L(R_G, latitude, doy, temp, U, sunshine) # [mm/d]
}

Rn_K <- function(R_G=NA, temp, sunshine=NA, doy, latitude, albedo=0.23){
	if(all(is.na(R_G)) & all(is.na(sunshine))) stop("Either global radiation (R_G) or sunshine duration (sunshine) have to be given")
	if(!all(is.na(R_G)) & !all(is.na(sunshine))) warning("sunshine duration (sunshine) is neglected since global radiation (R_G) is given")
	if(!all(is.na(R_G))) {
	}else{
		warning("Calculating global radiation (R_G) from sunshine duration")
		R_G = R_G(sunshine, doy, latitude)
		sunshine = NA
	}
	L = 249.8 - 0.242 * temp # [J/(cm^2*mm)]
	Rn_K = (1-albedo)*R_G/L # [mm/d]
}

xi <- function(doy){
	xi = 0.0172 * doy - 1.39 # [radiant] doy = day of year
}

R_0 <- function(doy, latitude){
	R_0 = 2425 + 1735 * sin(xi(doy)) + 44 * (latitude - 51.0) * (sin(xi(doy)) -1)
}

Rn_L <- function(R_G=NA, latitude, doy, temp, U, sunshine){
	if(all(is.na(R_G))) R_G = R_G(sunshine, doy, latitude)
	# phi = geographic latitude [°]
	R_L = 10.8 + 0.205 * temp # approximation
	Rn_L = R_L * (1.64*R_G/R_0(doy, latitude) - 0.22) * (0.34 - 0.0044 * sqrt(U*es(temp))) # [mm/d]
}

R_G <- function(sunshine, doy, latitude){
	S = sunshine # sunshine duration [h/d]
	# S_0 = astronomical possible sunshine duration [h/d]
	S_r = S/S_0(doy, latitude) # [-]
	R_G = R_0(doy, latitude) *(0.19 + 0.55 * S_r) # [J/(cm^2*d)]
}

S_0 <- function(doy, latitude){
	S_0 = 12.3 + sin(xi(doy)) * (4.3 + (latitude - 51.0)/6) # [h/d]
}
