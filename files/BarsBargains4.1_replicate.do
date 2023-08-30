

/*	**********************************************************************		*/
/*	File Name: BarsBargains4.1_replicate.do										*/
/*	Date:	February 2018														*/
/*	Authors: Geoff Dancy 														*/
/*	Purpose: This file runs analyses using the most updated version of the  	*/
/*	TJRC data (with controls). It produces results that are presented 			*/
/* 	in the ISQ paper "Bars and Bargains", usable cross-national time series	    */
/*	dataset for statistical analysis.											*/
/*	Input File #1: barsbargains_fig1.dta										*/
/*  To Operate: Change file directory to whichever folder you use to  save 		*/
/*	the .dta files 																*/
/*	Version: Stata 11 or above													*/
	**********************************************************************		*/


cd "/Users/geoff/Dropbox/NSF Uber Analysis/ISQFinalFiles/"
			
		
	***Descriptives
	**************************************************************************
	
			***Figure 1
				use "barsbargains_fig1.dta", clear
				set scheme s1mono
				
				*Alter axes and labels using graph editor
				twoway (line famsum year, lcolor(gs8) c(l) yaxis(1)) (scatter trtsum year, msymbol(none) lpattern(dash) lcolor(black) c(l) yaxis(2))
			
			
			***Country stats
			
				use "barsbargains.dta", clear
				collapse (max) tj trt1 ambin famsum, by(ccode)
				
				*Footnote 8 -- 34 countries with amnesties, 13 passed more than one
				tab famsum
				
				*Intro -- Over 70 countries with TJ
				collapse (max) tj, by(ccode)
				tab tj

			
			***Footnote 9 -- How many overlapping TJ and Post-Conflict Justice cases? 
			
				use "barsbargains.dta", clear
				collapse (max) nsfupcsample ccode, by(dtr_id)
				tab nsfupcsample
			
			
			***Sample Stats
			
				use "barsbargains.dta", clear
				
				gen trtoram=0
					replace trtoram=1 if (trtst==1 | ambin==1)
				
				*751 of 1834 observations stat in Section 4.1
				tab trtoram	
				
			
			***Regime Stats in Section 4.2

				collapse (sum) trtpros (max) trt1 ambin famsum ccode (min) year, by(dtr_id)
				
				*66 regimes with prosecutions
				tab trt1
				
				*540 prosecutions
				gen sumtrt=sum(trtpros)
				
				*41 regimes with amnesties
				tab ambin
					
				gen tj=0
				replace tj=1 if (trt1==1 | ambin==1)
				
				*28 regimes with both
				gen both=0
				replace both=1 if (trt1==1 & ambin==1)
				
				*32 regimes without TJ
				tabulate tj	
			
			
			***Data on sequencing
			
				use "barsbargains.dta", clear
				*276 prosecutions follow amnesties
				collapse (sum) trtpros, by(ambin)
			
				use "barsbargains.dta", clear
				*37 amnesties follow a trial
				collapse (sum) amnesty_fit, by(trt1)
			

			
		
		
	****TJ Outcomes
	****************************************************************************************
		
		use "barsbargains.dta", clear
	
			sort dtr_id year
			xtset dtr_id year
		
			***Short term
									
				*Main Models
				xtnbreg totosv trtpros_l1 fitam_l1 upcwar polity2 poplog loggdp, fe
					estimates store m1
					
					gen osvbin=0
					replace osvbin=1 if totosv>=1 & totosv<.
					
				xtlogit osvbin trtpros_l1 fitam_l1 polity2 poplog loggdp if upcwar==1, fe
					estimates store m2
				
				xtlogit totosv trtpros_l1 fitam_l1 polity2 poplog loggdp if upcwar==1, fe
					estimates store m2
					
				
				*First Findings Table
					
				esttab m1 m2 using T1.tex, replace ///
				label ///
				cells(b(fmt(a3) star) se(par fmt(a3))) ///
				se scalars(N N_g r2) ///
				star(* 0.10 ** 0.05 *** 0.01) ///
				title(Short-term Effects of TJ on Atrocity) /// 
				note(***p\sym{<}.01  **p\sym{<}.05  *p\sym{<}.10. All covariates lagged one year. One-tailed tests reported.)	///
				nonumbers mtitles(m1 m2)
		
	
		
			***Long Term w/ CEM
			
			*PHYSINT
				
				*CEM
				
				imb prehr10 rtper break ji_keith, treatment(trtst)
				
				gen prehrcat=0
				replace prehrcat=1 if prehr10<=-1.29
				replace prehrcat=2 if (prehr10>-1.29 & prehr10<=-.745)
				replace prehrcat=3 if (prehr10>-.745 & prehr10<=.006)
				replace prehrcat=4 if prehr10>.006
				
				gen rtpercat=0
				replace rtpercat=1 if prehr10<=.42
				replace rtpercat=2 if (prehr10>.42 & prehr10<=.62)
				replace rtpercat=2 if (prehr10>.62 & prehr10<=.92)
				replace rtpercat=1 if prehr10>.92
				
				
				cem prehrcat(#0) rtpercat(#0) break(#0) ji_keith(#0), treatment(trtst) showbreaks
					
				
				*no matching
				reg physint physint_l1 trtsum_l1 famsum_l1 i.dtr_id, vce(robust)
				
				
				*m3
				reg physint physint_l1 trtsum_l1 famsum_l1 i.dtr_id [iw=cem_weights], vce(robust)
					
					estimates store m3
				
				*m4
				reg physint physint_l1 trtsum_l1 famsum_l1  ///
					prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					psacalc delta trtsum_l1
					psacalc delta trtsum_l1, rmax(.8)
					
					psacalc beta trtsum_l1
					psacalc beta trtsum_l1, rmax(.8) delta(1)
			
					estimates store m4
				
				*m5
				reg physint physint_l1 trtsum_l1 famsum_l1 trt_amsum  ///
					prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					estimates store m5
					
				*m6	
				reg physint physint_l1 trtgsum_l1 famsum_l1 ///
					prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					estimates store m6
					
					reg physint physint_l1 i.trtgsum_l1 famsum_l1   ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					margins trtgsum_l1
					marginsplot, yline(4.58)
					
				*m7	
				reg physint physint_l1 trtgsum_l1 famsum_l1 trtg_amsum  ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					estimates store m7
					
				
					
			*EMPINX
				
				
				imb prehrcat rtpercat break upcwar,  treatment(ambin)
			
				cem prehrcat(#0) rtpercat(#0) break(#0) upcwar(#0), treatment(ambin) showbreaks
					

				reg empinx empinx_l1 trtsum_l1 famsum_l1 i.dtr_id, vce(robust)
				
				
				*m8
				reg empinx empinx_l1 trtsum_l1 famsum_l1 i.dtr_id  [iw=cem_weights], vce(robust)
					
					estimates store m8
				
				
				*m9
				reg empinx empinx_l1 trtsum_l1 famsum_l1  ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
					
					estimates store m9
					
					psacalc beta famsum_l1
					psacalc beta famsum_l1, rmax(.9) delta(1)
					psacalc delta famsum_l1
					psacalc delta famsum_l1, rmax(.87)
					
	
				
				*m10
				reg empinx empinx_l1 trtsum_l1 famsum_l1 trt_amsum  ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)

					
					estimates store m10
				
				*m11
				reg empinx empinx_l1 trtgsum_l1 famsum_l1 /// ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)
	
					
					estimates store m11
					
					reg empinx empinx_l1 i.trtgsum_l1 famsum_l1   ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)

					
					margins trtgsum_l1
					marginsplot, yline(4.58)
				
				*m12
				reg empinx empinx_l1 trtgsum_l1 famsum_l1 trtg_amsum  ///
					prehr10 rtper break upcwar ji_keith dtr_t2 polity2 loggdp poplog i.dtr_id [iw=cem_weights], vce(robust)

					
					estimates store m12
					
				esttab m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 using Tbig.tex, replace ///
				label ///
				cells(b(fmt(a3) star) se(par fmt(a3))) ///
				se scalars(N N_g r2) ///
				star(* 0.10 ** 0.05 *** 0.01) ///
				title(Long-term Effects of TJ on Physical Integrity) /// 
				note(***p\sym{<}.01  **p\sym{<}.05  *p\sym{<}.10. All variables lagged one year.)	///
				nonumbers mtitles(m3 m4 m5 m6 m7 m8 m9 m10 m11 m12)
			
			
			
		***Figures
		*************************************************************************
			
			
				*Coefficients plot
				
					gen trtsum10=trtsum_l1/10
					label variable trtsum10 "Prosecutions"
					
					gen trtgsum10=trtgsum_l1/10
					label variable trtgsum10 "Guilty Verdicts"
				
					cem prehrcat(#0) rtpercat(#0) break(#0) ji_keith(#0), treatment(trtst) showbreaks
					reg physint physint_l1 trtsum10 famsum_l1  ///
						prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog  i.dtr_id [iw=cem_weights], vce(robust)
						estimates store m4b
						
					reg physint physint_l1 trtgsum10 famsum_l1 ///
						prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog  i.dtr_id [iw=cem_weights], vce(robust)	
						estimates store m6b
					
					cem prehrcat(#0) rtpercat(#0) break(#0) upcwar(#0), treatment(ambin) showbreaks
					reg empinx empinx_l1 trtsum10 famsum_l1   ///
						prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog  i.dtr_id [iw=cem_weights], vce(robust)
						estimates store m9b
						
					reg empinx empinx_l1 trtgsum10 famsum_l1 /// ///
					prehr10 rtper break ji_keith upcwar dtr_t2 polity2 loggdp poplog  i.dtr_id [iw=cem_weights], vce(robust)
						estimates store m11b
				
					coefplot m4b m6b, bylabel(PHYSINT) || m9b m11b, bylabel(EMPINX) ///
					|| , keep (trtsum10 trtgsum10 famsum_l1) ///
					order (trtsum10 trtgsum10 famsum_l1) xline(0) levels(95)  legend(off)
					
					
					
					
		***Appendix		
		*************************************************************************
		
			*Description of Variables
			
			estpost summarize physint empinx totosv  ///
				trtpros_l1 fitam_l1 trtsum_l1 trtgsum_l1  ///
				famsum_l1 trt_amsum trtg_amsum ///
				prehr10 rtper break upcwar lsji ji_keith dtr_t2 polity2 loggdp poplog ///
				if (year>=1980 & year<=2010)

				esttab using TableA1.tex, replace label cells("count mean sd min max") 

					
			*Visualize Interactions
			
				cem prehrcat(#0) rtpercat(#0) break(#0) ji_keith(#0), treatment(trtst) showbreaks
				
				*m5
				reg physint physint_l1 c.trtsum_l1##c.famsum_l1 ///
				ji_keith dtr_t2 polity2 loggdp poplog upcwar i.dtr_id [iw=cem_weights], vce(robust)
					
					margins, dydx(famsum_l1) at(trtsum_l1=(0(10)60))
						
					marginsplot, recast(line) recastci(rline) yline(0) ylabel(-1(.5)1) ///
					title(Average Marginal Effects of Amnesties)
							
					graph play marg.grec
					graph save pix1.gph, replace 
		
		
				*m7
				reg physint physint_l1 c.trtgsum_l1##c.famsum_l1 ///
					ji_keith dtr_t2 polity2 loggdp poplog upcwar i.dtr_id [iw=cem_weights], vce(robust)	
					
					margins, dydx(famsum_l1) at(trtgsum_l1=(0(10)60))
						
					marginsplot, recast(line) recastci(rline) yline(0) ylabel(-1(.5)1) ///
					title(Average Marginal Effects of Amnesties)
							
					graph play marg.grec
					graph save pix2.gph, replace 

				
				
				cem prehrcat(#0) rtpercat(#0) break(#0) upcwar(#0), treatment(ambin) showbreaks
	
				*m10
				reg empinx empinx_l1 c.trtsum_l1##c.famsum_l1 ///
				ji_keith dtr_t2 polity2 loggdp poplog upcwar i.dtr_id [iw=cem_weights], vce(robust)
					
					margins, dydx(famsum_l1) at(trtsum_l1=(0(10)60))
						
					marginsplot, recast(line) recastci(rline) yline(0) ylabel(-1(.5)1) ///
					title(Average Marginal Effects of Amnesties)
							
					graph play marg.grec
					graph save pix3.gph, replace 
		
		
				*m12
				reg empinx empinx_l1 c.trtgsum_l1##c.famsum_l1 ///
					ji_keith dtr_t2 polity2 loggdp poplog upcwar i.dtr_id [iw=cem_weights], vce(robust)	
					
					margins, dydx(famsum_l1) at(trtgsum_l1=(0(10)60))
						
					marginsplot, recast(line) recastci(rline) yline(0) ylabel(-1(.5)1) ///
					title(Average Marginal Effects of Amnesties)
							
					graph play marg.grec
					graph save pix4.gph, replace 					
