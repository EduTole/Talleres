
cls
clear all
glo main		"C:\Users\et396\Dropbox"
glo lecture		"${main}\Docencia\Educate\Econometria\S1"
glo Data		"${lecture}\Data"
glo Tablas			"${lecture}\Tablas"

*Carga de Data
u "${Data}/Mincer_2021.dta",clear
d

/*
*Analisis exploratorio
*-----------------------------------
*sum r6 redad rexper rmujer reduca
sum r6 reduca 

*Por percentiles 
xtile pct = r6, nq(4) 
table pct , c(mean r6 ) row col 
tabstat r6 reduca, stats(mean p5 p25 p50 p75 max min ) col(stats) 

*Grafico
*-----------------------------------
tw (scatter r6 reduca ) (lfit r6 reduca)

*Transformacion de variables
*-----------------------------------
g lnr6=log(r6)
g lnreduca=log(reduca)

*Grafico de dispersion
*-----------------------------------
tw (scatter lnr6 reduca ) (lfit lnr6 reduca), ytitle("Log-Ingresos") xtitle("Educacion (años)")
graph export "${Out}/f0.png",replace

*Grafico de caja que excluye los missing values
*-----------------------------------
graph box r6
graph box r6,  nooutside

*Grafico de caja que excluye los missing values segun sexo
*-----------------------------------
graph box r6, over(rmujer)  nooutside
table rmujer , c(mean r6 mean redad  mean rexper) row col 

*Grafico de CDF 
*-----------------------------------
cumul r6 if r6<2501 & rmujer==0, g(r6_cdf_1)
cumul r6 if r6<2501 & rmujer==1, g(r6_cdf_2)
line r6_cdf_1 r6_cdf_2 r6 if r6<2501, sort legend(label(1 "Hombre") label(2 "Mujer")) xline(1000) xtitle("Ingresos") ytitle("Proporcion de pop.")
graph export "${Out}/f1.png",replace

*Grafico de PDF 
*-----------------------------------
tw (kdensity lnr6 if rmujer==0 ) (kdensity lnr6 if rmujer==1 ) , legend(label(1 "Hombre") label(2 "Mujer")) xtitle("Log-Ingresos") ytitle("Densidad")
graph export "${Out}/f2.png",replace

tw (kdensity r6 if rmujer==0 & r6<2501) (kdensity r6 if rmujer==1 & r6<2501) , legend(label(1 "Hombre") label(2 "Mujer")) xline(1000)

*Modelos de Regresion
*-----------------------------------
*Modelo Bivariado
*-----------------------------------
reg r6 reduca 
estimate store m1
estimate table m1, b(%7.4f) stats(N r2_a aic bic rss) star

*Formas funcionales
reg lnr6 reduca 
reg lnr6 lnreduca 
reg r6 lnreduca 

*Prueba de Heterocedasticidad grafica
reg r6 reduca
predict mu_hat , residual 
br r6 mu_hat
sum mu_hat
kdensity mu_hat
graph export "${Out}/f3.png",replace

graph box mu_hat
graph export "${Out}/f4.png",replace
*/

*Modelo Multivariado
*-----------------------------------
*Analisis exploratorio
glo Xs "r6 reduca rmujer redad redadsq rpareja"
glo Zs "reduca rmujer redad redadsq rpareja"
sum $Xs

	eststo clear
	estpost tabstat $Xs , s(n mean p50 min max sd) col(stat) 

	esttab using "${Tablas}\T_1_stata.tex", ///
	c("count(label(Personas)) mean(label(Promedio) fmt(%10.2fc)) p50(label(Mediana) fmt(%10.2fc) ) min(label(Min.) fmt(%10.2fc)) max(label(Max.) fmt(%10.2fc)) sd(label(Std) fmt(%10.0fc))") ///
	label nomtitles nodepvars noobs nonumbers booktabs prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T1} Estadisticas descriptivas }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace	


*Pregunta 1a)
reg lnr6 reduca
reg lnr6 $Zs

	eststo clear
	eststo: reg lnr6 reduca 
	eststo: reg lnr6 $Zs 

	esttab using "${Tablas}\T_3_stata.tex",  label booktabs b(3) se(2) nonumber star(* 0.10 ** 0.05 *** 0.01) varlabels(`e(labels)') mtitles("(1)" "(2)") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Modelo Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: EnAHO 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

*Pregunta 1b) Estimacion de errores del modelo		
*--------------------------------------------------------------
reg lnr6 $Zs
*Prediciones de los errores del modelo
predict uhat,resid

*Bosquejo de la densidad kernel de las estimaciones
*del error y tets para nomalidad de la distribucion de errors
kdensity uhat
sktest uhat, noadj 
graph export "${Out}/t1.png", replace

*Grafico de caja
graph box uhat
graph export "${Out}/t2.png", replace

*Pregunta 1c) prueba de errores de heterocedasticidad	
*--------------------------------------------------------------
*Test de Koenker para heterocedasticidad
hettest $Zs, iid

*Test manual de heterocedasticidad
gen uhatsq=uhat^2
label var uhatsq "$\mu^{2}$"

reg uhatsq $Zs
scalar r2 = e(r2)
scalar sample = e(N)
scalar lm_het = r2*sample
display lm_het

	eststo clear
	eststo: reg uhatsq $Zs

	esttab using "${Tablas}\T_4_stata.tex",  label booktabs b(3) se(2) nonumber star(* 0.10 ** 0.05 *** 0.01) varlabels(`e(labels)') mtitles("$\varepsilon^{2}_{i}$" ) prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Modelo Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: EnAHO 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

*Pregunta 2
*-------------------------------------------------------------
*Pregunta 2a) prediccion de los errores
reg lnr6 reduca        ,r
reg lnr6 reduca rmujer ,r
reg lnr6 $Zs ,r

	eststo clear
	eststo: reg lnr6 reduca        ,r
	eststo: reg lnr6 reduca rmujer       ,r
	eststo: reg lnr6 $Zs       ,r

	esttab using "${Tablas}\T_4a_stata.tex",  label booktabs b(3) se(2) nonumber wide star(* 0.10 ** 0.05 *** 0.01) varlabels(`e(labels)') mtitles("$\ln wage$" "$\ln wage$" "$\ln wage$" ) prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T4} Modelo Lineal robusto }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: EnAHO 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
		
*Pregunta 2b) prediccion de los errores
*Test de Wald: edad y edad cuadrado

	eststo clear
	eststo: reg lnr6 reduca        ,r
	eststo: reg lnr6 reduca rmujer       ,r
	eststo: reg lnr6 $Zs       ,r
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_5_stata.tex",  label booktabs  b(2) se(2) nonumber keep(reduca rmujer) mtitles("(1)" "(2)" "(3)")  star(* 0.10 ** 0.05 *** 0.01) /*
	*/  varlabels(`e(labels)') /*
	*/ stats(N r2_a Fixed1, layout(@) fmt(a3 a3  a2 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace


reg lnr6 reduca redad redadsq rpareja rmujer , r
*Extraer matrices
matrix b=e(b) 
matrix list b
matrix vb=e(V) 
matrix list vb

matrix bi=b[1,4..5] 
matrix vi=vb[4..5,4..5]
matrix w_test=bi*inv(vi)*bi'
matrix list w_test

test rmujer rpareja
		
* Prgunta 3
*-------------------------------------------
		
reg lnr6 $Zs , r

scalar edad_optima = _b[redad]/(-2*_b[redadsq])
display edad_optima

*scalar beta3_sq = _b[redad]/(2*(_b[redadsq])^2)

*display ((-22.5468^2)*0.0000001948) + ((69051.769^2)*0.00000000003635)+(2*(-22.5468)*(69051.769)*(-0.00000000255))

*display (27.670369-50)/(sqrt(0.18116328))

reg lnr6 $Zs

scalar beta3 = _b[redad]/(-2*(_b[redadsq]^2))
scalar beta2 = 1/(-2*_b[redad])

scalar beta2_sq = beta2^2
scalar beta3_sq = beta3^2

*Extraer matrices
matrix b=e(b) 
matrix list b
matrix vb=e(V) 
matrix list vb

matrix vage=vb[3..4,3..4]
matrix list vage

* Metodo delta
scalar var_beta2 = vage[1,1]
scalar var_beta3 = vage[2,2]
scalar cov_beta2_beta3 = vage[2,1]

scalar delta = (beta2_sq*var_beta2) + (beta3_sq*var_beta3) + (beta2*beta3*cov_beta2_beta3)
display delta

nlcom - _b[redad]/(2*_b[redadsq]) - 50		
gen lnr6_predicted=_b[redad]*redad +_b[redadsq]*redadsq
scatter lnr6_predicted redad		
graph export "${Imagen}/t3.png", replace	

reg lnr6 $Zs, r
test rmujer 
test rmujer rpareja


	eststo clear
	eststo: reg lnr6 reduca        ,r
	eststo: reg lnr6 reduca rmujer       ,r
	eststo: reg lnr6 $Zs       ,r
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_6_stata.tex",  label booktabs  b(2) se(2) nonumber keep(reduca ) mtitles("\ln wage" "\ln wage" "\ln wage")  star(* 0.10 ** 0.05 *** 0.01) /*
	*/  varlabels(`e(labels)') /*
	*/ stats(N r2_a Fixed1, layout(@) fmt(a3 a3  a2 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
		
		
		
reg lnr6 $Zs , r


*Pregunta 4
*--------------------------------------------------------------
*Rgresion por separado
reg lnr6 $Zs
reg lnr6 reduca  redad redadsq rpareja if rmujer==1 
reg lnr6 reduca  redad redadsq rpareja if rmujer==0 


*OLS INTERACCIONES	
g i1=rmujer*reduca
g i2=rmujer*redad
g i3=rmujer*redadsq
g i4=rmujer*rpareja

label var i1 "educa x mujer"
reg lnr6 rmujer reduca redad redadsq rpareja i1 i2 i3 i4

test i1 
test i1 i2
test i1 i2 i3 i4

reg lnr6 rmujer reduca redad redadsq rpareja i1 i2 i3 i4
reg lnr6 rmujer reduca redad redadsq rpareja
reg lnr6 reduca redad redadsq rpareja if rmujer==1
reg lnr6 reduca redad redadsq rpareja if rmujer==0

* Output latex
	eststo clear
	eststo: reg lnr6 rmujer reduca redad redadsq rpareja i1 i2 i3 i4, r
	estadd local Fixed1 "$\surd$",replace
	eststo: reg lnr6 rmujer reduca redad redadsq rpareja
	estadd local Fixed1 "$\surd$",replace
	eststo: reg lnr6 reduca redad redadsq rpareja if rmujer==1
	estadd local Fixed1 "$\surd$",replace
	eststo: reg lnr6 reduca redad redadsq rpareja if rmujer==0
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_7_stata.tex",  label booktabs  b(2) se(2) nonumber keep(reduca i1) mtitles("Interaccion" "(1)" "mujer" "hombre")  star(* 0.10 ** 0.05 *** 0.01) /*
	*/  varlabels(`e(labels)') /*
	*/ stats(N r2_a Fixed1, layout(@) fmt(a3 a3 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	