*Stata do-file: This is the do-file preparing the replication (using the R scripts provided) of the analyses reported in:

*Winzen, Thomas. 2023. "How Backsliding Governments Keep the European Union Hospitable for Autocracy: Evidence from Intergovernmental Negotiations." The Review of International Organizations.

*Please consult the instructions on the replication materials on the required packages and further information.
stop
clear

*Set your working directory
cd ""



********************************************************************************
**# Data for the analysis in R START
********************************************************************************

use "analysis", clear

export delimited using "analysisR", replace

drop if yintro<=2000
export delimited using "analysisRwaves2and3", replace

********************************************************************************
**# Data for the analysis in R END
********************************************************************************




********************************************************************************
**# Comparison with data coded by Wratil
* See section: "First analysis: Backsliding governments' positions" START
********************************************************************************
use "analysis", clear

*Correlation of .73 reported in the article
corr proeu int_position
corr proeubin int_binary


*Check all discrepancies (1=retain coding, .5=retain coding, but debatable)
gen diff=1 if proeubin!=int_binary & proeubin!=. & int_binary!=.
order ctr antieu proeu int_position proeubin int_binary diff position 
sort isnrnmc ctr

gen check=.
order check
replace check=1 if isnrnmc==18 & ctr=="at"
replace check=1 if isnrnmc==52 & ctr=="at"
replace check=1 if isnrnmc==73 & ctr=="at"
replace check=.5 if isnrnmc==74 & ctr=="at"
replace check=1 if isnrnmc==215 & ctr=="at"
replace check=.5 if isnrnmc==241 & ctr=="at"
replace check=1 if isnrnmc==247 & ctr=="at"
replace check=1 if isnrnmc==254 & ctr=="at"
replace check=1 if isnrnmc==255 & ctr=="at"
replace check=1 if isnrnmc==329 & ctr=="at"

replace check=1 if isnrnmc==39 & ctr=="fi"
replace check=1 if isnrnmc==72 & ctr=="dk"
replace check=.5 if isnrnmc==92 & ctr=="dk"
replace check=1 if isnrnmc==96 & ctr=="be"
replace check=1 if isnrnmc==133 & ctr=="be"
replace check=1 if isnrnmc==150 & ctr=="es"
replace check=1 if isnrnmc==154 & ctr=="nl"
replace check=1 if isnrnmc==185 & ctr=="de"
replace check=1 if isnrnmc==305 & ctr=="nl"
replace check=1 if isnrnmc==313 & ctr=="de"
replace check=1 if isnrnmc==320 & ctr=="be"
replace check=1 if isnrnmc==325 & ctr=="cy"
replace check=1 if isnrnmc==330 & ctr=="be"


********************************************************************************
* Comparison with data coded by Wratil
* See section: "First analysis: Backsliding governments' positions" END
********************************************************************************





********************************************************************************
**# Bimodality of the data discussed in section:
* "First analysis: Backsliding governments' positions" START
********************************************************************************
use "analysis", clear
histogram proeu
histogram position
tab proeu
tab proeubin

********************************************************************************
**# Bimodality of the data discussed in section:
* "First analysis: Backsliding governments' positions" END
********************************************************************************





********************************************************************************
**# List of DEU issues coded as backsliding-inhibiting START
* See Appendix 1
********************************************************************************
use "analysis", clear

keep prnrnmc isnrnmc dintro mintro yintro sensitive sensitivearea
collapse (mean) sensitive sensitivearea yintro mintro dintro, by(prnrnmc isnrnmc)
label values sensitivearea sens_short
keep if sensitivearea!=0

********************************************************************************
**# List of DEU issues coded as backsliding-inhibiting END
* See Appendix 1
********************************************************************************








