



/** Programme parameters. **/

 global working = "~/Dropbox/aspirations_small_folder/results/graph_GN/"

/** Prep data. **/
* This csv file should contain the point estimates and standard errors for all groups. In addition, it can also contain statistics which define where the difference between two study groups (e.g. treatment and placebo) is significant (jointlysignificant). 
 insheet using "${working}results_inuse_GN_lr test.csv", comma clear // 
 * Make a matrix using the statistics to input into the coefplot command. 
 mkmat  point_treat se_treat point_plac se_plac point_contr se_contr sign_treat sign_plac sign_contr jointlysignificant jointlysignificant2, rownames(outcome) mat(lr)
 
 matrix  lr_inv =(lr)'
 mat colnames lr_inv = "Aspirations Index" "Expectations Index" "Internal Locus of Control" "External Locus of Control" "Schooling Expenditure" "Children in School (6-15 years)" "Daily Minutes Working"  "Agricultural Investment"  "Revenue"  "Asset Aggregate"  "Subjective Wellbeing"
 * "Total Savings" "Consumption Aggregate" "Food Insecurity Index"

 * The graph is generated with 90% confidence intervals, and 3 study groups (all estimates are relative to the control group). The display in this graph does not change when a coefficient is significant vs when it is not. No sign is displayed to indicate whether the coefficient for different treatment groups are significantly different from each other. 
 coefplot (matrix(lr_inv[1,]), se(lr_inv[2,]) mcolor(blue) msize(small) label(Treatment )) ///
 (matrix(lr_inv[3,]), se(lr_inv[4,]) mcolor(red) msize(small) label(Placebo )) ///
 (matrix(lr_inv[5,]), se(lr_inv[6,]) mcolor(green) msize(small) label(Within-village Control)), ///
 xline(0)  levels(90)  lcolor(*.5 *1)

 
graph export "${working}coefplot_Dec17.eps", replace

 * The graph is generated without confidence intervals, and 3 study groups (all estimates are relative to the control group. The display in this graph changes (shapes become filled and unfilled) when a coefficient is significant vs when it is not. In addition, a sign is displayed to indicate whether the coefficient for different treatment groups are significantly different from each other. 

/* Graph for T v P v WC - unstar to use */
graph twoway scatter order2 point_plac if sign_plac==0, mcolor(red)  msymbol(Oh) msize(large)   ///
					|| scatter order2 point_plac if sign_plac==1, mcolor(red)  msymbol(O)  msize(large)  ///
					|| scatter order2 point_treat  if sign_treat  ==0, mcolor(blue) msymbol(Sh) msize(large)   ///
					|| scatter order2 point_treat  if sign_treat  ==1, mcolor(blue) msymbol(S)  msize(large)   ///
					|| scatter order2 jointsig, mcolor(purple) msymbol(X)   ///
					|| scatter order2 point_contr  if sign_contr  ==0, mcolor(green) msymbol(Th) msize(large)   ///
					|| scatter order2 point_contr  if sign_contr  ==1, mcolor(green) msymbol(T)  msize(large)   ///
					|| scatter order2 jointsig2, mcolor(black) msymbol(+)   ///
	ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13, valuelabel angle(horizontal)) ytitle("")   ///
	xtitle("") xline(0, lcolor(black)) xscale(range(-0.25 0.5)) xlabel(-0.25(0.25)0.5)   ///
	graphregion(color(white))   ///
	scale(0.9) ///
	legend( rows(5) order(2 4 7 5 8) lab(2 "Placebo") lab(4 "Treated") lab(7 "Within-Control      Filled shapes denote significance at 10% level") lab(5 "Significant difference between treatment and placebo at 10%") lab(8 "Significant difference between treatment and within-control at 10%") size(*0.75) )
	graph export "${working}lr_3Nov17_all_nofoodsec.eps", replace
	
