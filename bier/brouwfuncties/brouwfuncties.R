#' @title Plato naar Brix conversie
#' @description Zet een Plato waarde om naar een Brix waarde
#'
#' @param plato De Plato waarde
#'
#' @return De Brix waarde
#'
Plato2Brix <- function(plato) {
	#Zet plato waarde om naar Brix
	return(1.04 * plato)
}


#' @title Brix naar Plato conversie
#' @description Zet een Brix waarde om naar een Plato waarde
#'
#' @param brix De Brix waarde
#'
#' @return De Plato waarde
#'
Brix2Plato <- function(brix) {
	#Zet brix waarde om naar Plato
	return(brix / 1.04)
}


#' @title SG naar Plato conversie
#' @description Zet een SG waarde om naar een Plato waarde
#'
#' @param sg De dichtheid
#' @param methode De gebruikte methode, simpel of clerck
#'
#' @return De Plato waarde
#'
SG2Plato <- function(sg, methode = "simpel") {
	if (methode == "clerck") {
		plato = 259 - 259 / sg
	} else {
		plato = 250 * (sg - 1)
	}
	return(plato)
}


#' @title Plato naar SG conversie
#' @description Zet een Plato waarde om naar een SG waarde
#'
#' @param plato De Plato waarde
#' @param methode De gebruikte methode, simpel of clerck
#'
#' @return De dichtheid (SG)
#'
Plato2SG <- function(plato, methode = "simpel") {
	if (methode == "clerck") {
		sg = 259 / (259 - plato)
	} else {
		sg = 1 + plato / 250
	}
	return(sg)
}


#' @title Digitaal Hop rendement simpel
#' @description Bepaalt decimaal hoprendement volgens eenvoudige methode
#'
#' @param kooktijd De kooktijd in minuten (vector)
#'
#' @return Digitaal Hop Rendement (vector)
#'
DHR_Simpel <- function(kooktijd) {
	vec <- c() #initialiseer lege vector
	for (kt in kooktijd) {
		print
		interval <- cut(kt, breaks = c(0, 10, 31, 59, 300),
						labels = c(1,2,3, 4), right = FALSE)
		dhr <- switch(as.numeric(interval), 0, 0.1, (kt - 10)/200, 0.25)
		vec <- c(vec, dhr)
	}
	return(vec)
}


#' @title Digitaal Hop rendement Tinseth
#' @description Bepaalt decimaal hoprendement volgens Tinseth methode
#'
#' @param kooktijd De kooktijd in minuten (vector/numeriek getal)
#' @param sg De dichtheid (numeriek getal/vector), niet beide vector
#'
#' @return Digitaal Hop Rendement (vector)
#'
DHR_Tinseth <- function(kooktijd, sg) {
	kooktijd_factor <- (1 - exp(-0.04 * kooktijd)) / 4.15
	sg_factor <- 1.65 * 0.000125 ^ (sg - 1)
	dhr <- kooktijd_factor * sg_factor
	return(dhr)
}

#Test DHR
x <- c(5, 10, 15, 35, 60, 75, 90)
round(DHR_Simpel(x), 2)
round(DHR_Tinseth(x, sg = 1.040), 2)

# Bij Tinseth mogen niet beide parameters een vector zijn, zoals
# v1 <- c(2, 3, 4, 5)
# v2 <- c(10, 20)
