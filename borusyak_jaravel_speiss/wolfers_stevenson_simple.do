ssc install did_imputation
ssc install event_plot
ssc install reghdfe


// Going to use the same Wolfers and Stevenson (2006) example we have been

use http://pped.org/bacon_example.dta

// standard twfe to compare with
gen K = year - _nfd

forvalues l = 0/14 {
	gen L`l'_nfd = K == `l'
}
gen L15_nfd = K >= 20

forvalues l = 0/7 {
	gen F`l'_nfd = K == `l'
}
gen F8_nfd = K <= -8

reghdfe asmrs o.F1_nfd o.F2_nfd F3_nfd-F8_nfd L*_nfd, a(stfips year) cluster(stfips)

estimates store did_base

// new estimator
did_imputation asmrs stfips year _nfd, horizons(0/15) pretrend(8)
estimates store did_imp

// event study plot
event_plot did_imp did_base, stub_lag(tau# L#_nfd) stub_lead(pre# F#_nfd) together ///
plottype(scatter) default_look noautolegend ///
                graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-8(1)15) legend(label (3 "OLS") label (1 "BJS")))


