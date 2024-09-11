	cls
	clear all
	set more off
	cd "C:\Users\et396\Dropbox\Docencia\Educate\Indicadores\S2\Data"
	
	* Import datasets
	use "BD_Eleccion_2021.dta",clear
	d
	sum 
	
	*============================================================
	* PART I
	*============================================================
	* Uso de global
	glo Xs 	"redad redadsq rmujer ragua lnr6 i.reduca_niv"
	
	sum rpobre redad redadsq rmujer ragua lnr6 i.reduca_niv
	sum rpobre $Xs
		
	* Graph
	tw (kdensity lnr6 if rpobre==1) (kdensity lnr6 if rpobre==0), legend(label(1 "Pobre") label(2 "No-pobre"))
	graph box lnr6 , over(rpobre)

	
	* Pregunta 1
	*========================================================
	* Model OLS - MPL
	reg rpobre $Xs , r
	estimate store m_ols
	

	* Modelo Probit
	probit rpobre $Xs , r
	estimate store m_probit

	estimates table m_ols m_probit, b(%7.4f) stats(N aic) star
	estimates table m_ols m_probit, b(%7.4f) se(%7.4f) stats(N aic)
	
	
	
	* Pregunta 2
	*========================================================
	*No lineal - redad
	probit rpobre $Xs , r

	* relacion no lineal de la edad
	scalar edad_opt = -(_b[redad] / (2*_b[redadsq]))
	display edad_opt
	
	* Pregunta 3
	*========================================================
	probit rpobre $Xs  , r
	margins, dydx(*) 
	
	* graficos de probabilidad
	* definimos las covariables
	glo Xs 	"redad redadsq i.rmujer ragua lnr6 i.reduca_niv"
	
	probit rpobre $Xs  , r
	margins reduca_niv, atmeans
	marginsplot
	
	probit rpobre $Xs  , r
	margins i.reduca_niv#i.rmujer
	marginsplot
	
		
	* Pregunta 4
	*========================================================
	* Logit vs Probit
	probit rpobre $Xs, r
	estimate store mnl_probit
	logit rpobre $Xs , r
	estimate store mnl_logit
	estimates table mnl_probit mnl_logit, b(%7.4f) stats(N aic) star


