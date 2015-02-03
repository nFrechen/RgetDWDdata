# Berechnung der Grasreferenzverdunstung
net.radiation <- function (sunshine, temp, U, date, latitude, albedo) {
	phi <- latitude
	alpha <- albedo
	
	xi <-  0.0172 * yday(date) - 1.39        
	
	R_0 <- 2425 + 1735 * sin(xi) + 44 * (phi - 51.0) * (sin(xi)-1)  
	
	S_0 <- 12.3 + sin(xi) * (4.3 + (phi - 51.0)/6)
	
	S_r <- sunshine/S_0 
	
	R_G <- R_0 * (0.19 + 0.55 * S_r)
	
	R_L <- 10.8 + 0.205 * temp 
	
	RnL <- R_L * (1.64*R_G/R_0-0.22) * (0.34 - 0.0044 * sqrt( U * es(temp) ))
	
	L <- 249.8 - 0.242 * temp
	
	RnK <- (1-alpha)  * R_G/L
	
	Rn <- RnK - RnL
	return(Rn)
}

gammastar <- function(v_2, gamma){
	# calculates the modified psychrometer constant
	gamma *(1 + 0.34 * v_2)
} 

es <- function(temp, ice=FALSE){
	# saturation vapor pressure over
	if(ice){
		# ice
		6.11 * exp(22.46 * temp / (272.62 + temp))
	}else{
		# liquid water
		6.11 * exp(17.62 * temp / (243.12 + temp))	
	}
} 

s <- function(temp, ice=FALSE){
	# calculates the gradient of the saturation vapor pressure curve
	es(temp, ice=ice)* 4284/(243.12 + temp)^2
} 




calcET0 <- function(temp, date, sunshine, U, v, v_height=2, latitude, albedo=0.23, gamma=0.65){
	v_2 <- v * 4.2 /(log(v_height) + 3.5)
	
	s(temp)/(s(temp) + gammastar(v_2, gamma)) * 
		net.radiation(sunshine, temp, U, date, latitude, albedo) +
		90*gamma / (s(temp) + gammastar(v_2, gamma)) * 
		es(temp) / (temp + 273) *
		(1-U/100) * v_2
}
