/* **************************************************************************
Institucion:					
Autor:							Edinson Tolentino
Proyecto:						Union de bases:
								100
								500
								Sumaria 
Fecha Ultima Modificacion:		2022
*****************************************************************************/

	clear all
	set more off

	//Set pathways
	glo path 	"C:\Users\et396\Dropbox" // ET
	glo enaho 	"${path}\BASES\ENAHO"
	glo main 	"${path}\Docencia\Educate\Indicadores\S2"
	glo clean 	"${main}\Data"
	glo codigos	"${enaho}\DISEL-MTPE"	// Codigos de dofiles variables

	*======================================================
	* PART I
	*==================================================
	*Paso 1 :
	*Carga de la base de datos
	*--------------------------------------------
	*Trabajando modulo de empleo
	*-----------------------------------
	u "${enaho}//2021//enaho01a-2021-500.dta", clear
	
	*Codigo de hogar
	egen codigo_hogar=concat(conglome vivienda hogar)
	do "${codigos}/5.- r1r_a.do"
	do "${codigos}/9c.- r3.do"
	do "${codigos}/15.- r6.do"
	do "${codigos}/41.- rsexo.do"
	do "${codigos}/6.- r2.do"
	do "${codigos}/8.- r2r_b.do"
	do "${codigos}/7.- reduca_nivel.do"
	do "${codigos}/41a.- rmujer.do"
	
	*renombre
	g redad=r1r_a
	label var redad "edad persona"
	keep if redad>17 & redad<65
	g redadsq = redad*redad
	*Solo jefe de hogar
	keep if p203==1
	
	* solo pea ocupada
	keep if r3==1
	
	sum r6,d
	keep if r6>`r(p5)'
	sum r6,d
	keep if r6<`r(p90)'
	
	g lnr6=ln(r6)
	label var lnr6 "log-salarios (princ + secun)"
	cls
	keep codigo_hogar lnr6 r6 redad redadsq rmujer reduca_niv
	d
	saveold "${clean}//Empleo_2021.dta",replace

	
	*Trabajando modulo de vivienda
	*-----------------------------------
	u "${enaho}//2021//enaho01-2021-100.dta", clear
	*Codigo de hogar
	egen codigo_hogar=concat(conglome vivienda hogar)
	do "${codigos}/r2.-ractivos.do"
	
	cls
	keep codigo_hogar ragua rluz
	d
	saveold "${clean}//Activos_2021.dta",replace
	
	*Temas de pobreza
	*-------------------------------------------
	u "${enaho}//2021//sumaria-2021.dta", clear
	*Codigo de hogar
	egen codigo_hogar=concat(conglome vivienda hogar)
	do "${codigos}/r0.-rpobre.do"
	
	cls
	keep codigo_hogar r*
	d
	saveold "${clean}//Pobreza_2021.dta",replace		
	
	*Union de bases de datos en una sola
	cls
	u "${clean}//Pobreza_2021.dta",clear
	merge 1:1 codigo_hogar using "${clean}//Activos_2021.dta", keep(match ) nogen
	merge 1:1 codigo_hogar using "${clean}//Empleo_2021.dta", keep(match ) nogen
	
	
	order codigo_hogar rpobre r6 lnr6 rmujer redad redadsq reduca_niv rlg rg ry rly rmiembros ragua rluz  
	keep codigo_hogar rpobre r6 lnr6 rmujer redad redadsq reduca_niv rlg rg ry rly rmiembros ragua rluz
	d
	sum
	saveold "${clean}/BD_Eleccion_2021.dta",replace	
	
	erase "${clean}//Empleo_2021.dta"	
	erase "${clean}//Activos_2021.dta"	
	erase "${clean}//Pobreza_2021.dta"	

	



