********************************************************************************
*** INSTALL NECESSARY MODULES **************************************************
********************************************************************************
*ssc install distinct

* CHAPTER 1: ALLOCATION

********************************************************************************
*** DEFINE GLOBALS *************************************************************
********************************************************************************

global godad "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\GODAD Final Data"
global godad_pre "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\GODAD Workshop Pre-releases" // where the data is stored
global honig "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\PPD 2.1 April 1 2022" // folder to Dan Honig's PPD data set
global riomarkers "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\RioMarkers data" // folder storing rio marker project level data\RioMarkers
global unsc "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\UNSC membership" // folder storing the UNSC membership data set
global plad "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\PLAD"
global latex "C:\Users\marlo\Documents\PhDEQUALFIN\Latex\Research Proposal" // folder for outputs and graphs
global alloc "C:\Users\marlo\Documents\PhDEQUALFIN\Chapter_1_Allocation" // folder for CHAPTER 1: ALLOCATION
global allocoutput "C:\Users\marlo\Documents\PhDEQUALFIN\Chapter_1_Allocation\Allocation-Outputs" // folder storing interesting outputs




********************************************************************************
*** LOAD AND PREPARE RIO MARKERS DATA ******************************************
********************************************************************************

* Import and save the individual Rio Marker Data sets for period 2002 - 2023
foreach period in 2022-23 2018-21 2014-17 2010-13 2006-09 2002-05 {
    
    clear
    * Import the data file for this period
    import delimited "C:\Users\marlo\Documents\PhDEQUALFIN\RioMarkers data\RioMarkers `period' data.txt"

    * Extract the first and last years without the dash
    local start = substr("`period'", 1, 4)
    local end   = substr("`period'", 6, 2)

    * Save the raw imported data
    save "C:\Users\marlo\Documents\PhDEQUALFIN\RioMarkers data\riomarkers`start'`end'", replace

}

use "$riomarkers/riomarkers202223", clear // Append the individual data files

foreach period in 2018-21 2014-17 2010-13 2006-09 2002-05 {
	
	* Extract the first and last years without the dash
    local start = substr("`period'", 1, 4)
    local end   = substr("`period'", 6, 2)
	
	append using "$riomarkers/riomarkers`start'`end'.dta"

}

save "$riomarkers/riomarkers200223", replace // Save the full data set
*** Full Rio Marker data set created and saved *********************************

use "$riomarkers/riomarkers200223", clear // collapse by projecttitle, keep Rio Marker only
collapse (max) score environment biodiversity climatemitigation climateadaptation desertification, by(projecttitle)
save "$riomarkers/riomarkers200223_aggreg_projtitle", replace // Save aggregated data set
*** Aggregated Rio Marker data set created and saved ***************************


* Prepare the Rio Marker data set for merge with GODAD 
use "$riomarkers/riomarkers200223_aggreg_projtitle", clear
gen len = length(projecttitle) // inspect length of variable names (necessary for merge)
summarize len // maximum length is 550
local maxlenrio = r(max)
gen str`maxlenrio' projecttitle2 = projecttitle
drop projecttitle
drop len
rename projecttitle2 projecttitle

* projecttitle_lower creates ~13,000 projects that have duplicate observations -> collapse
gen projecttitle_lower = lower(projecttitle) // change projecttitle variable to lower cases
duplicates report projecttitle_lower

/*

--------------------------------------
   Copies | Observations       Surplus
----------+---------------------------
        1 |       715608             0
        2 |        23868         11934
        3 |         1605          1070
        4 |          252           189
        5 |           40            32
        6 |            6             5
--------------------------------------

*/

foreach v of varlist score environment biodiversity climatemitigation climateadaptation desertification {
	bys projecttitle_lower: egen min_`v' = min(`v')
	bys projecttitle_lower: egen max_`v' = max(`v')
	replace `v' = min_`v'
}
collapse score environment biodiversity climatemitigation climateadaptation desertification, by(projecttitle_lower)

save "$riomarkers/riomarkers200223_aggreg_projtitle_merge", replace


********************************************************************************
*** PREPARE THE GODAD DATA *****************************************************
********************************************************************************

* generate sector code variable (alternative for Rio Markers)
use "$godad/GODAD_projectlevel", clear // load final GODAD data
gen env_crs = regexm(sector_codes, "(^|\\|)410(\\||$)") // create an indicator variable showing 1 if the string variable 
														// sector_codes contains "410" for General Environmental Protection
														
* Prepare GODAD data set for merge with Rio Marker data set 
gen len = length(title) // inspect length of variable names (necessary for merge)
summarize len // maximum length is 
local maxlengodad = r(max)
gen str`maxlengodad' projecttitle = title
drop title
drop len

gen projecttitle_lower = lower(projecttitle) // change projecttitle variable to lower cases

* focus on European Donors and US first
drop if donor == "World Bank" | ///
		donor == "China" | ///
		donor == "India"
		
save "$godad/GODAD_projectlevel_eu_us", replace
*** European GODAD data set created and saved ***************************
		
		
********************************************************************************
*** MERGE GODAD WITH RIO MARKER DATA *******************************************
********************************************************************************

use "$godad/GODAD_projectlevel_eu_us", clear
merge m:1 projecttitle_lower using "$riomarkers\riomarkers200223_aggreg_projtitle_merge"
drop if _merge == 2
save "$alloc/godad_rio_raw", replace

drop if _merge != 3
drop _merge
save "$alloc/godad_rio_w99", replace

tab score // inspect information on Rio Markers
drop if score == 99
save "$alloc/godad_rio", replace
*** GODAD + RIO MARKER data set created and saved ******************************
/*


    Result                      Number of obs
    -----------------------------------------
    Not matched                       767,105
        from master                   121,912  (_merge==1)
        from using                    645,193  (_merge==2)

    Matched                           225,283  (_merge==3)
    -----------------------------------------


     (mean) |
      score |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     65,629       29.13       29.13
          1 |     24,895       11.05       40.18
          2 |     19,421        8.62       48.80
         99 |    115,338       51.20      100.00
------------+-----------------------------------
      Total |    225,283      100.00

	
*/

********************************************************************************
*** PREPARE GODAD AND RIO FOR PYTHON MERGE *************************************
********************************************************************************

use "$riomarkers/riomarkers200223", clear //
keep year projecttitle donornamee recipientnamee sector score environment biodiversity climatemitigation climateadaptation desertification
save "$riomarkers/riomarkers200223_redux", replace

********************************************************************************
*** TAKE THE PYTHON MERGE + AI IMPUTATION AND MERGE TO GODAD_RIO DATA SET ******
********************************************************************************

use "$alloc/godad_rio_raw", clear

foreach v of varlist score environment biodiversity climatemitigation climateadaptation desertification {
	bys project_id: egen min_`v' = min(`v')
	bys project_id: egen max_`v' = max(`v')
	replace `v' = min_`v'
}

collapse (max) score environment biodiversity climatemitigation climateadaptation desertification, by(project_id)

save "$alloc/godad_rio_raw_unique", replace

* import and prepare the AI title merge
import delimited "$alloc\godad_rio_aimerge.csv", clear // import the AI title merge
save "$alloc\godad_rio_aimerge.dta", replace

* collapse the godad_rio_aimerge dataset by project_id
use "$alloc\godad_rio_aimerge.dta", clear

foreach rio in score environment biodiversity climatemitigation climateadaptation desertification {
	gen `rio'_original = `rio'
	* handle differing information on Rio Markers by taking minimum of those variables
	egen `rio'_min = rowmin(`rio' `rio'2 `rio'3)
	replace `rio' = `rio'_min
	drop `rio'_min `rio'2 `rio'3

	sum `rio'_original
	sum `rio'
	
	drop `rio'_original
}

foreach v of varlist score environment biodiversity climatemitigation climateadaptation desertification {
	bys project_id: egen min_`v' = min(`v')
	bys project_id: egen max_`v' = max(`v')
	replace `v' = min_`v'
}

bys project_id: egen min_similarity = min(similarity)
bys project_id: egen max_similarity = max(similarity)
gen diff_similarity = 0
replace diff_similarity = 1 if min_similarity != max_similarity

gen diff_similarity_num = 0
replace diff_similarity_num = max_similarity - min_similarity if min_similarity != max_similarity

sum diff_similarity_num if min_similarity != max_similarity
hist diff_similarity_num if min_similarity != max_similarity // overwhelming majority of similarity values are very similar

collapse (max) score environment biodiversity climatemitigation climateadaptation desertification (mean) similarity (firstnm) projecttitle_rio, by(project_id)

duplicates report project_id // only unique project_id values! 

save "$alloc\godad_rio_aimerge_unique.dta", replace

* import and prepare the AI imputations
import delimited "$alloc\godad_rio_predictions.csv", clear // import the AI imputations
save "$alloc\godad_rio_predictions.dta", replace

use "$alloc\godad_rio_predictions.dta", clear

* which rio markers to collapse? -> collapse all 3 -> but how to include the pred_conf information from the AI imputation data set?
* List of Rio Marker variables
local markers environment biodiversity climatemitigation climateadaptation desertification

* Create variables to hold the minimum and corresponding confidence for each marker
foreach marker of local markers {
    * Find minimum prediction for each project_id
    bysort project_id: egen `marker'_pred_min = min(`marker'_pred)
    
    * Create a variable to hold the confidence when prediction equals minimum
    gen `marker'_pred_conf_min = `marker'_pred_conf if `marker'_pred == `marker'_pred_min
    
    * Fill in the confidence value across all observations in the group
    bysort project_id (`marker'_pred_conf_min): replace `marker'_pred_conf_min = `marker'_pred_conf_min[1]
}

* Collapse to project level, keeping the minimum prediction and corresponding confidence
collapse (first) *_pred_min *_pred_conf_min, by(project_id)

* Rename variables back to original names
foreach marker of local markers {
    rename `marker'_pred_min `marker'_pred
    rename `marker'_pred_conf_min `marker'_pred_conf
}

save "$alloc\godad_rio_predictions_unique.dta", replace

* perform the merge

use "$alloc/godad_rio_raw", clear // contains Rio Markers from the initial plain title merge
drop _merge score environment biodiversity climatemitigation climateadaptation desertification

merge m:1 project_id using "$alloc/godad_rio_raw_unique.dta"
drop _merge

/*

    Result                      Number of obs
    -----------------------------------------
    Not matched                             0
    Matched                           347,195  (_merge==3)
    -----------------------------------------

*/

foreach v of varlist score environment biodiversity climatemitigation climateadaptation desertification {
	rename `v' `v'_title
	label variable `v'_title "This is the Rio Marker from the merge based on plain title_lower"
}

merge m:1 project_id using "$alloc\godad_rio_aimerge_unique.dta", keepusing(score environment biodiversity climatemitigation climateadaptation desertification similarity projecttitle_rio) // adds Rio Markers from the AI title merge
drop _merge

/*

    Result                      Number of obs
    -----------------------------------------
    Not matched                       115,752
        from master                   115,752  (_merge==1)
        from using                          0  (_merge==2)

    Matched                           231,443  (_merge==3)
    -----------------------------------------

*/

foreach v of varlist score environment biodiversity climatemitigation climateadaptation desertification similarity projecttitle_rio{
	rename `v' `v'_aititle
	label variable `v'_aititle "This is the Rio Marker from the AI title merge"
}


merge m:1 project_id using "$alloc\godad_rio_predictions_unique.dta"
drop _merge
/*

    Result                      Number of obs
    -----------------------------------------
    Not matched                            89
        from master                        58  (_merge==1)
        from using                         31  (_merge==2)

    Matched                           347,137  (_merge==3)
    -----------------------------------------

*/

foreach v in environment_pred biodiversity_pred climatemitigation_pred climateadaptation_pred desertification_pred {
    local newname = subinstr("`v'", "_pred", "_aiimput", .)
    rename `v' `newname'
    label variable `newname' "This is the Rio Marker from the AI Imputation"
}

foreach v in environment_pred_conf biodiversity_pred_conf climatemitigation_pred_conf climateadaptation_pred_conf desertification_pred_conf {
    local newname = subinstr("`v'", "_pred_conf", "_aiimput_conf", .)
    rename `v' `newname'
    label variable `newname' "This is the Confidence for the Rio Marker from the AI Imputation"
}

save "$alloc/godad_riomarkers.dta", replace


********************************************************************************
*** TURN GODAD INTO PANEL DATA STRUCTURE ***************************************
********************************************************************************

* use data set from Python (spatial join of world map, ethnicity and godad) 
import delimited "$alloc/godad_riomarkers_ethnicity.csv", clear
save "$alloc/godad_riomarkers_ethnicity.dta", replace

* first, create a dataset with unique project_id, aggregating by iso_code and paymentyear
use "$alloc/godad_riomarkers_ethnicity.dta", clear

collapse (sum) disb (firstnm) donor name_0 name_1 gid_0 projecttitle description score_title environment_title biodiversity_title climatemitigation_title climateadaptation_title desertification_title score_aititle environment_aititle biodiversity_aititle climatemitigation_aititle climateadaptation_aititle desertification_aititle similarity_aititle projecttitle_rio_aititle environment_aiimput biodiversity_aiimput climatemitigation_aiimput climateadaptation_aiimput desertification_aiimput environment_aiimput_conf biodiversity_aiimput_conf climatemitigation_aiimput_conf climateadaptation_aiimput_conf desertification_aiimput_conf dominant_ethnic_group, by(iso_code paymentyear project_id)

save "$alloc/godad_riomarkers_ethnicity_uniqueproj.dta", replace

use "$alloc/godad_riomarkers_ethnicity_uniqueproj.dta", clear
*** Unique GODAD projects with ethnicity saved *********************************

* create dummies to indicate climate projects for later collapse
foreach v of varlist ///
score_title environment_title biodiversity_title climatemitigation_title climateadaptation_title desertification_title ///
score_aititle environment_aititle biodiversity_aititle climatemitigation_aititle climateadaptation_aititle desertification_aititle ///
environment_aiimput biodiversity_aiimput climatemitigation_aiimput climateadaptation_aiimput desertification_aiimput {
	gen d_`v' = .
	replace d_`v' = 0 if `v' == 0
	replace d_`v' = 1 if `v' == 1 | `v' == 2
}

* create different disbursement variables based on the type of merge
foreach v of varlist ///
score_title environment_title biodiversity_title climatemitigation_title climateadaptation_title desertification_title ///
score_aititle environment_aititle biodiversity_aititle climatemitigation_aititle climateadaptation_aititle desertification_aititle ///
environment_aiimput biodiversity_aiimput climatemitigation_aiimput climateadaptation_aiimput desertification_aiimput {
	gen dis_`v' = .
	replace dis_`v' = 0 if `v' == 0 // zero disbursements if project is not climate related
	replace dis_`v' = disb if `v' == 1 | `v' == 2 // take disbursements if project is climate related
}

* Aggregate disbursements and count unique projects
collapse (sum) disb d_* dis_* (first) name_0 gid_0 name_1 dominant_ethnic_group, by(iso_code paymentyear)

save "$alloc/godad_riomarkers_ethnicity_isocode.dta", replace
use "$alloc/godad_riomarkers_ethnicity_isocode.dta", clear

* Create a balanced panel
* First, identify the full range of years
summarize paymentyear
local min_year = r(min)
local max_year = r(max)

* Get list of all unique iso_code values
preserve
keep iso_code
duplicates drop
tempfile iso_list
save `iso_list'
restore

* Create complete year range
preserve
clear
set obs `=`max_year' - `min_year' + 1'
gen paymentyear = `min_year' + _n - 1
tempfile year_list
save `year_list'
restore

* Cross-join to create all combinations
use `iso_list', clear
cross using `year_list'
tempfile balanced_frame
save `balanced_frame'

* Merge with actual data
merge 1:1 iso_code paymentyear using "$alloc/godad_riomarkers_ethnicity_isocode.dta"
*erase "$alloc/godad_riomarkers_ethnicity_isocode.dta"
drop _merge
order name_0 gid_0 name_1 iso_code paymentyear disb
sort iso_code paymentyear

* Fill missing geographic identifiers within each iso_code panel
* Carry forward non-missing values within each iso_code group
foreach v of varlist gid_0 name_0 name_1 dominant_ethnic_group {
    bysort iso_code (paymentyear): replace `v' = `v'[_n-1] if missing(`v') & _n > 1
}
* Carry backward if still missing (for iso_codes with no early data)
foreach v of varlist gid_0 name_0 name_1 dominant_ethnic_group {
    gsort iso_code -paymentyear
    by iso_code: replace `v' = `v'[_n-1] if missing(`v') & _n > 1
    sort iso_code paymentyear
}

* data cleaning: Some name_0 don't correspond to the gid_0 codes
replace name_0 = "Namibia" if gid_0 == "NAM"
replace name_0 = "Bosnia and Herzegovina" if gid_0 == "BIH"
replace name_0 = "Bolivia" if gid_0 == "BOL"
replace gid_0 = "SSD" if name_0 == "South Sudan" 
replace name_0 = "Kyrgyzstan" if gid_0 == "KGZ"
replace name_0 = "Somalia" if gid_0 == "SOM"
replace name_0 = "Cyprus" if gid_0 == "CYP"
replace gid_0 = "SRB" if iso_code == "RSKM" & paymentyear <= 2008
replace name_0 = "Serbia" if iso_code == "RSKM" & paymentyear <= 2008 
replace gid_0 = "XKX" if iso_code == "RSKM" & paymentyear > 2008
replace name_0 = "Kosovo" if gid_0 == "XKX"
replace name_0 = "China" if gid_0 == "CHN" // Hong Kong is part of China when it comes to UNSC membership

/*
* data cleaning: some gid_0's have missing name_0: CHN, CYP, KIR, MHL, SRB, XKX
br if gid_0 == "CYP"
br if gid_0 == "KIR"
br if iso_code == "RSKM"
br if gid_0 == "MHL"
br if gid_0 == "XKX"
br if gid_0 == "CHN"
*/
* Declare as panel data
encode iso_code, gen(iso_code_num)
xtset iso_code_num paymentyear

* Verify it's balanced
xtdescribe

/* DIAGNOSTICS

* Create a lookup of name_0 → gid_0
preserve
keep name_0 gid_0
duplicates drop
bysort name_0: assert _N == 1  // fails if inconsistency exists
restore

* Similarly check gid_0 → name_0
preserve
keep gid_0 name_0
duplicates drop
bysort gid_0: assert _N == 1
restore

* See which name_0 have multiple gid_0
preserve
keep name_0 gid_0
duplicates drop
bysort name_0: gen count = _N
list name_0 gid_0 if count > 1, sepby(name_0) clean
restore

* See which gid_0 have multiple iso_code
preserve
keep name_1 iso_code
duplicates drop
bysort name_1: gen count = _N
list name_1 iso_code if count > 1, sepby(name_1) clean
restore

*/ * DIAGNOSTICS END

save "$alloc/godad_riomarkers_ethnicity_panel.dta", replace
*** GODAD panel including ethnicity saved **************************************

* confirm that each country only has one gid_0 code
use "$alloc/godad_riomarkers_ethnicity_panel.dta", clear
collapse (first) iso_code, by(name_0 gid_0)
bysort name_0: assert _N == 1
bysort name_0: gen byte indic = (_N > 1)

********************************************************************************
*** MERGE UNSC MEMBERSHIP INFORMATION TO GODAD_RIOMARKERS DATA SET *************
********************************************************************************

use "$alloc/godad_riomarkers_ethnicity_panel.dta", clear

tab gid_0 if name_0 == "Ethiopia" | ///
			name_0 == "Cyprus" | ///
			name_0 == "Micronesia" | ///
			name_0 == "Yemen"
			
foreach c in "Croatia" "Slovenia" "Serbia" "Bosnia and Herzegovina" "Montenegro" "North Macedonia" {
tab paymentyear if name_0 == "`c'"
}

// all years in GODAD are after 1989

import excel "$unsc/UNSCdata.xls", sheet("data") firstrow clear
drop if code == "." // drop states like Czechoslovakia, Yugoslavia,...
replace code = "SRB" if code == "YUG" // Serbia continues to be represented at the UNSC under the name of "Republic of Yugoslavia"

// create entries for the states of Montenegro, Tuvalu, South Sudan and Democratic Republic of the Congo who have never been temporary UNSC members in the sample period (Democratic Republic of the Congo becomes non-permanent member from 2026 onwards)
expand 2 if code == "ISL", generate(is_duplicate)
replace code = "MNE" if is_duplicate == 1
replace aclpname = "Montenegro" if is_duplicate == 1
drop is_duplicate

expand 2 if code == "ISL", generate(is_duplicate)
replace code = "TUV" if is_duplicate == 1
replace aclpname = "Tuvalu" if is_duplicate == 1
drop is_duplicate

expand 2 if code == "ISL", generate(is_duplicate)
replace code = "SSD" if is_duplicate == 1
replace aclpname = "South Sudan" if is_duplicate == 1
drop is_duplicate

expand 2 if code == "ISL", generate(is_duplicate)
replace code = "COD" if is_duplicate == 1
replace aclpname = "Democratic Republic of the Congo" if is_duplicate == 1
drop is_duplicate

// Cook Islands share Head of State, Foreign Affairs and Defence with New Zealand
expand 2 if code == "NZL", generate(is_duplicate)
replace code = "COK" if is_duplicate == 1
replace aclpname = "Cook Islands" if is_duplicate == 1
drop is_duplicate

duplicates tag code year, generate(dup_code_year)
tab dup_code_year // no duplicates anymore
rename code gid_0

// examine cases when UNSC == "."
tab unsc // 2,585 cases with UNSC == "."
tab year if unsc == "." & year >= 1989 // only a handful of cases after 1990
forval y = 1989/2020 {
	display(`y')
	tab aclpname if unsc == "." & year == `y' 
} 
count if unsc == "." & year >= 1989 & year <= 2020 // 94 cases have missing entries due to non-membership in UN during GODAD years 


save "$unsc/UNSCdata.dta", replace

use "$alloc/godad_riomarkers_ethnicity_panel.dta", clear

drop if paymentyear > 2020 // no UNSC information after 2020

drop if gid_0 == "CHN" // China is permanent member of UNSC
drop if gid_0 == "AIA" // Anguilla is a UK overseas territory, UK is permanent member -> drop 
drop if gid_0 == "MSR" // Montserrat is a UK overseas territory, UK is permanent member -> drop 
drop if gid_0 == "SHN" // Saint Helena is a UK overseas territory, UK is permanent member -> drop 
drop if gid_0 == "TCA" // Turks and Caicos Islands is a UK overseas territory, UK is permanent member -> drop
drop if gid_0 == "VGB" // British Virgin Islands is a UK overseas territory, UK is permanent member -> drop 
drop if gid_0 == "MYT" // Mayotte is a French overseas territory, France is permanent member -> drop
drop if gid_0 == "PYF" // French Polynesia is a French overseas territory, France is permanent member -> drop
drop if gid_0 == "NCL" // New Caledonia is kind of a French overseas territory, France is permanent member -> drop
drop if gid_0 == "WLF" // Wallis and Futuna is a French overseas territory, France is permanent member -> drop
drop if gid_0 == "PSE" // Palestine not a UNSC member -> drop
drop if gid_0 == "XKX" // Kosovo not a UNSC member -> drop
drop if gid_0 == "SSN" // South Sudan as a very young state not represented at the UNSC
drop if gid_0 == "TLS" // Timor-Leste not a potential member of the UNSC

rename paymentyear year
merge m:1 gid_0 year using "$unsc/UNSCdata.dta", keepus(unsc)
rename year paymentyear

/*

remaining 27 observations that did not merge from GODAD are observations with few information, all funded by France -> drop


    Result                      Number of obs
    -----------------------------------------
    Not matched                         8,478
        from master                         0  (_merge==1)
        from using                      8,478  (_merge==2)

    Matched                            71,995  (_merge==3)
    -----------------------------------------


*/

drop if _merge != 3
drop _merge

save "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", replace

use "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", clear
tab unsc
tab paymentyear if unsc == "."
forval y = 1989/2020 {
	display(`y')
	tab name_0 if unsc == "." & paymentyear == `y' 
} 
// cases with missing entries are the countries that had not yet joined the UN,
// magnified by the number of regions in the respective year

********************************************************************************
*** CREATE COUNTRY CODE DATA SET BASED ON ALPHA 3 DIGIT CODES ******************
********************************************************************************
/*
use "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", clear
collapse (first) iso_code, by(name_0 gid_0)
drop if name_0 == ""
bysort name_0: gen byte indic = (_N > 1)
drop if gid_0 == "NAM" & name_0 == "Angola" | ///
		gid_0 == "BIH" & name_0 == "Serbia" | ///
		gid_0 == "BOL" & name_0 == "Argentina" | ///
		gid_0 == "KEN" & name_0 == "South Sudan" | ///
		gid_0 == "KGZ" & name_0 == "Uzbekistan"

bysort name_0: assert _N == 1
rename gid_0 ccode
drop indic
drop iso_code

save "$alloc/godad_country_codes.dta", replace
*** country codes saved ********************************************************

use "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", clear

br name_0 gid_0 paymentyear iso_code if iso_code == "NAKW"
br name_0 gid_0 paymentyear iso_code if iso_code == "BASRP"
br name_0 gid_0 paymentyear iso_code if iso_code == "BOT"
br name_0 gid_0 paymentyear iso_code if name_0 == "South Sudan"
br name_0 gid_0 paymentyear iso_code if iso_code == "SSEE"
br name_0 gid_0 paymentyear iso_code if iso_code == "KGO"

collapse (first) iso_code, by(name_0 gid_0 paymentyear)

br if gid_0 == "NAM"
br if name_0 == "Angola"

br if name_0 == "Serbia"
br if gid_0 == "BIH"

br if name_0 == "Argentina"
br if gid_0 == "BOL"

br name_0 gid_0 paymentyear iso_code if name_0 == "South Sudan"
br if gid_0 == "KEN"

br if name_0 == "Uzbekistan"
br if gid_0 == "KGZ"

bysort gid_0: tab paymentyear if name_0 == "Angola"  

*/
********************************************************************************
*** MERGE POLITICAL LEADERS INFORMATION TO GODAD_RIOMARKERS DATA SET ***********
********************************************************************************

use "$alloc/PLAD_ISO.dta", clear

rename gid_0 birthplace_gid_0
gen gid_0 = birthplace_gid_0

replace gid_0 = "DZA" if idacr == "ALG" // Algeria
replace gid_0 = "ARM" if idacr == "ARM" // Armenia
replace gid_0 = "AUS" if idacr == "AUL" // Australia
replace gid_0 = "BGD" if idacr == "BNG" // Bangladesh
replace gid_0 = "BRB" if idacr == "BAR" // Barbados
replace gid_0 = "BEL" if idacr == "BEL" // Belgium
replace gid_0 = "BEN" if idacr == "BEN" // Benin
replace gid_0 = "BTN" if idacr == "BHU" // Bhutan had missing values for some observations
replace gid_0 = "BIH" if idacr == "BOS" // Bosnia
replace gid_0 = "BWA" if idacr == "BOT" // Botswana
replace gid_0 = "BGR" if idacr == "BUL" // Bulgaria
replace gid_0 = "CAF" if idacr == "CEN" // Central African Republic
replace gid_0 = "COM" if idacr == "COM" // Comoros
replace gid_0 = "CRI" if idacr == "COS" // Costa Rica
replace gid_0 = "CYP" if idacr == "CYP" // Cyprus
replace gid_0 = "CZE" if idacr == "CZE" // Czech Republic
replace gid_0 = "CZE" if idacr == "CZR" // Czech Republic
replace gid_0 = "DJI" if idacr == "DJI" // Djibouti
replace gid_0 = "ECU" if idacr == "ECU" // Ecuador
replace gid_0 = "FIN" if idacr == "FIN" // Finland
replace gid_0 = "GAB" if idacr == "GAB" // Gabon
replace gid_0 = "GEO" if idacr == "GRG" // Georgia
replace gid_0 = "GRC" if idacr == "GRC" // Greece
replace gid_0 = "GUY" if idacr == "GUY" // Guyana
replace gid_0 = "HTI" if idacr == "HAI" // Haiti had missing values for some observations
replace gid_0 = "HND" if idacr == "HON" // Honduras
replace gid_0 = "IND" if idacr == "IND" // India
replace gid_0 = "IRQ" if idacr == "IRQ" // Iraq
replace gid_0 = "ISR" if idacr == "ISR" // Israel
replace gid_0 = "KWT" if idacr == "KUW" // Kuwait
replace gid_0 = "LAO" if idacr == "LAO" // Laos
replace gid_0 = "LVA" if idacr == "LAT" // Latvia
replace gid_0 = "MDV" if idacr == "MAD" // Maldives
replace gid_0 = "MKD" if idacr == "MAC" // Macedonia
replace gid_0 = "MDA" if idacr == "MLD" // Moldavia
replace gid_0 = "NPL" if idacr == "NEP" // Nepal
replace gid_0 = "PRK" if idacr == "PRK" // North Korea
replace gid_0 = "PAK" if idacr == "PAK" // Pakistan
replace gid_0 = "ROU" if idacr == "RUM" // Romania
replace gid_0 = "SRB" if idacr == "SER" // Serbia
replace gid_0 = "SRB" if idacr == "YUG" // attribute Yugoslavia to Serbia before 2003 (Montenegro not part of PLAD)
replace gid_0 = "SLB" if idacr == "SOL" // Solomon Islands had missing values for some observations
replace gid_0 = "SOM" if idacr == "SOM" // Somalia
replace gid_0 = "KOR" if idacr == "ROK" // South Korea
replace gid_0 = "TWN" if idacr == "TAW" // Taiwan
replace gid_0 = "THA" if idacr == "THI" // Thailand
replace gid_0 = "GBR" if idacr == "UKG" // UK
replace gid_0 = "ZMB" if idacr == "ZAM" // Zambia

replace country = "Iraq" if gid_0 == "IRQ" 
replace country = "Czech Republic" if gid_0 == "CZE"

replace birthplace_gid_0 = "MDV" if idacr == "MAD" // birthplace_gid_0 was empty because gid_0 was empty, but data geolocates birthplaces of all leaders in Maldives

drop if idacr == "GDR" // delete two entries on German Democratic Republic

assert !missing(startyear) & !missing(endyear) // sanity check
quietly assert startyear <= endyear

gen long id_leader = _n // create an id
gen long nyrs = endyear - startyear + 1 // compute number of years to expand

expand nyrs // expand to one row per year in the inclusive interval [startyear,endyear]
by id_leader, sort: gen long paymentyear = startyear + (_n - 1) // creates the year panel variable

order gid_0 paymentyear leader
sort gid_0 paymentyear leader

gsort idacr paymentyear  -startyear -endyear // if two spells share the same startyear, the one with the later endyear appears first
by idacr paymentyear : keep if _n == 1 // Keep the first observation per country-year

drop if gid_0 == "SRB" & idacr == "YUG" & paymentyear == 2006 & leader == "Svetozar Marovic" // drop first leader in year 2006 for Yugoslavia

by idacr paymentyear : assert _N == 1 // confirm uniqueness

// the gid_0 variable shows the birthplace of the leaders, which may be born in another
// country. Create a variable showing the gid_0 code of the ruled country itself

/*
* DIAGNOSTICS
collapse (firstnm) idacr, by(country gid_0)
bysort country: gen byte indic = (_N > 1) 
tab country if indic == 1
* br if indic == 1
br if indic != 1

br if name_0 == "Bhutan"
br if name_0 == "Haiti"
br if name_0 == "Solomon Islands"

* DIAGNOSTICS END
*/



order gid_0 country idacr
rename country name_0
rename ISO_CODE birthplace_iso

* some countries have more than 1 gid_0 code. E.g., Algeria has country code MAR (Morocco) for Bouteflika's term -> gid_0 indicates the country in which the leader is born, not the country he governs. Need to map idacr onto alpha 3 country codes

/* diagnostics

br if country == "Algeria"
br if idacr == "ALG"

tab gid_0 if name_0 == "Czech Republic"
tab gid_0 if name_0 == "Czech Republik"


br gid_0 name_0 idacr paymentyear birthplace_iso if name_0 == "Czech Republik" | name_0 == "Czech Republic"
sort

count if idacr != gid_0 // 4,112 cases where idacr and gid_0 differ
tab country if idacr != gid_0 
count if gid_0 == "."
tab country if gid_0 == "."

*drop gid_0
*rename idacr gid_0


sort idacr paymentyear 

br country idacr gid_0 leader paymentyear 



duplicates report gid_0 paymentyear
duplicates tag gid_0 paymentyear, gen(dup)
br if dup == 1

 * diagnostics end */

save "$plad/PLAD_panel.dta", replace
*** PLAD panel saved ***********************************************************

* merge PLAD to GODAD
use "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", clear

merge m:1 gid_0 paymentyear using "$plad/PLAD_panel.dta", keepus(leader birthplace_iso ethnicity_geoepr1 ethnicity_geoepr2)
drop if _merge != 3
drop _merge

/*

    -----------------------------------------
    Not matched                         6,640
        from master                     3,530  (_merge==1)
        from using                      3,110  (_merge==2)

    Matched                            68,464  (_merge==3)
    -----------------------------------------

*/

/* DIAGNOSTICS

tab name_0 if _merge == 1, sort // Montenegro, Somalia
// Montenegro not part of initial PLAD
// Somalia did really not have any government btw 1992 and 2011
// Samoa not contained in PLAD
// São Tomé and Príncipe not contained in PLAD
// Dominica not part of PLAD
// South Sudan has missing values corresponding to its state foundation only in 2011
// Maldives had wrong gid_0 code -> now corrected
// North Macedonia is missing entries in 1989 and 1990, corresponding to its state formation in 1991
// Serbia formed a union with Macedonia until 2006
/// Serbia had missing entries -> took the ones for Yugoslavia
// the rest seems to be small island states without information on leaders in PLAD or years around 1990


local c Eritrea 
tab paymentyear if _merge == 1 & name_0 == "`c'"
br gid_0 name_0 paymentyear iso_code leader birthplace_iso if name_0 == "`c'" 
use "$plad/PLAD_panel.dta", clear
br if name_0 == "`c'"
use "$alloc/PLAD_ISO.dta", clear
br if country == "`c'"
br if country == "Yugoslavia"
tab paymentyear if _merge == 1 & name_0 == "Serbia"
tab paymentyear if _merge == 1 & name_0 == "Philippines"


 DIAGNOSTICS END 

foreach c in Algeria Montenegro Armenia {
	display("`c'")
	tab paymentyear if name_0 == "`c'" & _merge == 1
}

// Algeria only contains leader information until 1999
// Montenegro and Armenia don't merge for any year

tab paymentyear if _merge == 1 // more non-merges in early years, but also >100 in later years


use "$plad/PLAD_panel.dta", clear

foreach c in Algeria Montenegro Armenia {
	display("`c'")
	tab paymentyear if country == "`c'"
}
// Algeria has gap in years btw. 1999 and 2019
// Montenegro doesn't have any observations
// Armenia has observations for almost every year

use "C:\Users\marlo\Documents\PhDEQUALFIN\Datasets\Berlin et al (2023) - Replication Data\TradingFavors.dta", clear


* DIAGNOSTICS
use "$alloc/godad_riomarkers_ethnicity_panel_unsc.dta", clear

preserve
collapse (first) iso_code, by(name_0 gid_0)
*quietly by name_0: assert _N == 1 // 6 countries have different gid_0 codes
sort name_0 gid_0
by name_0: gen nobs = _N
list name_0 gid_0 if nobs>1, sepby(name_0)
restore
// Angola, Argentina, Serbia, South Sudan, and Uzbekistan have different gid_0's

* inspect years in which the gid_0's differ
collapse (first) iso_code, by(name_0 gid_0 paymentyear)

keep if name_0 == "Angola" | ///
		name_0 == "Namibia"


keep if name_0 == "Angola" | ///
		name_0 == "Argentina" | ///
		name_0 == "Serbia" | ///
		name_0 == "South Sudan" | ///
		name_0 == "Uzbekistan"

// countries may have different gid_0's, but okay to join by gid_0: if the region belonged to another country, it should have received more money because the respective country's leader was born in that region. When the region belongs to another country thereafter, it will receive more money if that country's leader is born in that region.
		
use "$plad/PLAD_panel.dta", clear

collapse (first) leader, by(country gid_0)
by country: assert _N == 1 // gid_0 code is unique within country

 

* DIAGNOSTICS END */

save "$alloc/godad_riomarkers_ethnicity_panel_unsc_plad.dta", replace

********************************************************************************
*** FINAL MODIFICATIONS ON MERGED GODAD ****************************************
********************************************************************************

use "$alloc/godad_riomarkers_ethnicity_panel_unsc_plad.dta", clear

* generate dummy indicating whether region == leader's birthplace in that year
codebook iso_code, detail // no missing values
codebook birthplace_iso 
br if birthplace_iso == ""
tab name_0 if birthplace_iso == ""

levelsof name_0 if birthplace_iso == "", local(missingcountries)
// 5 developing countries `"Bhutan"' `"Comoros"' `"Guinea-Bissau"' `"Philippines"' `"Solomon Islands"'

foreach c of local missingcountries {
    display `"`c'"'
    tab paymentyear if name_0 == `"`c'"' & birthplace_iso == ""
} // years look weird, all 5 countries do contain some info on leader's birthplace but some missing years
// Bhutan has information on missing years in PLAD, but no geo-information on respective leaders
// Comoros has information on missing years in PLAD, but no geo-information on respective leaders
// Guinea_Bissau is weird, most leaders have geo-information but few have mapped onto isocodes
// Philippines is weird, all leaders have geo-information but few have mapped onto isocodes

gen ident_region_leaderbirth = .
replace ident_region_leaderbirth = 0 if iso_code != birthplace_iso & birthplace_iso != "" // only denote different region if there's info on leaders' birthplace iso code
replace ident_region_leaderbirth = 1 if iso_code == birthplace_iso 

* generate dummy indicating whether region's dominant ethnicity == leader's ethnicity 1 or 2 in that year
codebook dominant_ethnic_group, detail
tab name_0 if dominant_ethnic_group == ""
levelsof name_0 if dominant_ethnic_group == "", local(missingethn)
// following countries have missing values on dominant ethnic group: `"Barbados"' `"Burkina Faso"' `"Cape Verde"' `"Colombia"' `"Egypt"' `"Haiti"' `"Iran"' `"Jamaica"' `"Maldives"' `"Malta"' `"Mozambique"' `"North Korea"' `"Oman"' `"Solomon Islands"' `"Tunisia"' `"Uganda"' `"Yemen"'
// Barbados not in geoEPR
// Burkina Faso not in geoEPR
// Cape Verde not in geoEPR
// Colombia is in geoEPR but covers very few regions
// Egypt is in geoEPR but covers very few regions
// -> missing values probably make sense as there is no data in geoEPR
	
gen byte ident_region_leaderethn = .
replace ident_region_leaderethn = 1 if ///
    (dominant_ethnic_group == ethnicity_geoepr1 | dominant_ethnic_group == ethnicity_geoepr2) & ///
    dominant_ethnic_group != "" & (ethnicity_geoepr1 != "" | ethnicity_geoepr2 != "")

replace ident_region_leaderethn = 0 if ///
    dominant_ethnic_group != "" & (ethnicity_geoepr1 != "" | ethnicity_geoepr2 != "") & ///
    missing(ident_region_leaderethn)
	

drop iso_code_num	
destring unsc, replace // unsc is string, turn into numeric

foreach f in gid_0 iso_code { // turn gid_0 and iso_code into factor variables to use them in regression
	encode `f', gen(`f'_f)
	drop `f'
	rename `f'_f `f'
}

* disbursement variable has negative values -> CHECK WHY THEY ARE NEGATIVE!!!
count if disb < 0
br if disb < 0
replace disb = 0 if disb < 0 

save "$alloc/godad_riomarkers_ethnicity_panel_unsc_plad_finvar.dta", replace
 
********************************************************************************
*** INSPECT THE GODAD_RIOMARKERS DATA SET **************************************
********************************************************************************
/*
foreach v of varlist climatemitigation_title climatemitigation_aititle climatemitigation_aiimput climateadaptation_title climateadaptation_aititle climateadaptation_aiimput biodiversity_title biodiversity_aititle biodiversity_aiimput environment_title environment_aititle environment_aiimput {
	codebook `v', detail
}

// aititle climatemitigation has 1200 fewer missing values than plain title climatemitigation, but aititle climateadaptation has 4200 *more* missing values than plain title climateadaptation, aititle biodiversity also only improves plain title merge by 1500 missing values, ai title environment only improves by 3300 & throughout all variables, aititle records significicantly fewer values with 2 and 1, so the improvement, if any, stems from much more observations with Rio Marker 0 
*/

********************************************************************************
*** REPLICATION OF BERLIN ET AL. (2023) ****************************************
********************************************************************************

*ssc install xtpqml

use "$alloc/godad_riomarkers_ethnicity_panel_unsc_plad_finvar.dta", clear

eststo clear
foreach var in unsc ident_region_leaderbirth ident_region_leaderethn {

xi: qui eststo: xtpqml disb `var'  i.paymentyear, irr fe i(gid_0)
xi: qui eststo: xtpqml disb `var'  i.paymentyear, irr fe i(iso_code)

}

esttab  using mainC.tex, eform nocon title(Commitments - main effects\label{mainC}) ///
mtitles("UNSC" "UNSC" "Birth Region" "Birth Region" "Coethnic Region" "Coethnic Region" )  ///
replace drop(_I*) cells(b(fmt(3)) p(fmt(3) par([ ]))) scalars("N_g Groups")  ///
ren(birthregionD MainEffect unsc MainEffect  coethnicshare MainEffect ///
    incumbentregD MainEffect)  nogap compress obslast nonotes


* Original code

************************************** Table 1 *******************************************
eststo clear
foreach var in unsc birthregionD incumbentregD {
xi: qui eststo: xtpqml wb_start `var'  i.year, irr fe i(countrycode)
xi: qui eststo: xtpqml wb_start `var'  i.year, irr fe i(geoname_id)
}

esttab  using lin.tex, eform nocon title(Number of projects - main effects\label{main}) ///
mtitles("UNSC" "UNSC" "Birth Region" "Birth Region" "Coethnic Region" "Coethnic Region" ) ///
replace drop(_I*) cells(b(fmt(3)) p(fmt(3) par([ ]))) scalars("N_g Groups")  ///
ren(birthregionD MainEffect  unsc MainEffect  coethnicshare MainEffect ///
    incumbentregD MainEffect) nogap compress obslast nonotes
	
eststo clear
foreach var in unsc birthregionD incumbentregD {

xi: qui eststo: xtpqml wb_commit `var'  i.year, irr fe i(countrycode)
xi: qui eststo: xtpqml wb_commit `var'  i.year, irr fe i(geoname_id)

}

esttab  using mainC.tex, eform nocon title(Commitments - main effects\label{mainC}) ///
mtitles("UNSC" "UNSC" "Birth Region" "Birth Region" "Coethnic Region" "Coethnic Region" )  ///
replace drop(_I*) cells(b(fmt(3)) p(fmt(3) par([ ]))) scalars("N_g Groups")  ///
ren(birthregionD MainEffect unsc MainEffect  coethnicshare MainEffect ///
    incumbentregD MainEffect)  nogap compress obslast nonotes

************************************** Table 2 *******************************************

eststo clear
foreach var in birthregionD incumbentregD  {

gen interaction=`var'*unsc
xi: qui eststo: xtpqml wb_start unsc `var' interaction i.year, irr fe i(geoname_id)
xi: qui eststo: xtpqml wb_commit unsc `var' interaction i.year, irr fe i(geoname_id)

drop interaction
}

esttab using interC.tex, eform nocon title(UNSC membership and political connections - Interaction effects\label{inter}) ///
mtitles("Birth Region" "Coethnic Region" "Coethnic Share" ) scalars("N_g Regions") ///
replace drop(_I*) cells(b(fmt(3)) p(fmt(3) par([ ]))) nogap compress ///
ren(birthregionD BirthRegion interaction UNSCInteraction ///
    unsc UNSC  incumbentregD CoethnicRegion) obslast nonotes ///
    order(UNSC BirthRegion CoethnicRegion) ///
    addnotes("The table reports Incidence Rate Ratios from FE Poisson regressions. The dependent" ///
        "variable is USD commitments to projects started in each region and year. All regressions" ///
        "include region and year fixed effects. Standard errors are cluster-robust at the country level." ///
        "P-values under the coefficients.")

********************************************************************************
*** DESCRIPTIVE STATISTICS *****************************************************
********************************************************************************


* Plot the share of Rio Marker information in total projects across years by donor
********************************************************************************

* --- file paths ---
local godad_rio "$alloc/godad_riomarkers.dta"
local out_shares "$alloc/rio_marker_shares_by_donor_year.dta"

use "`godad_rio'", clear

* Ensure paymentyear is numeric
capture confirm numeric variable paymentyear
if _rc {
    destring paymentyear, replace force
}

* Clean donor name
gen strL donor_clean = lower(strtrim(donor))
gen str50 donor_label = donor_clean

* Create indicator variables for non-missing Rio Markers
foreach marker in climatemitigation biodiversity environment climateadaptation {
    gen byte has_`marker'_title    = !missing(`marker'_title)
    gen byte has_`marker'_aititle  = !missing(`marker'_aititle)
    gen byte has_`marker'_aiimput  = !missing(`marker'_aiimput)
}

collapse (count) n_projects = has_climatemitigation_title ///
    (sum) n_clim_title    = has_climatemitigation_title ///
    (sum) n_clim_aititle  = has_climatemitigation_aititle ///
    (sum) n_clim_aiimput  = has_climatemitigation_aiimput ///
    (sum) n_bio_title     = has_biodiversity_title ///
    (sum) n_bio_aititle   = has_biodiversity_aititle ///
    (sum) n_bio_aiimput   = has_biodiversity_aiimput ///
    (sum) n_env_title     = has_environment_title ///
    (sum) n_env_aititle   = has_environment_aititle ///
    (sum) n_env_aiimput   = has_environment_aiimput ///
    (sum) n_adapt_title   = has_climateadaptation_title ///
    (sum) n_adapt_aititle = has_climateadaptation_aititle ///
    (sum) n_adapt_aiimput = has_climateadaptation_aiimput, ///
    by(donor_label paymentyear)

* Compute shares (percentages)
foreach marker in clim bio env adapt {
    gen double share_`marker'_title_pct    = 100 * n_`marker'_title / n_projects if n_projects > 0
    gen double share_`marker'_aititle_pct  = 100 * n_`marker'_aititle / n_projects if n_projects > 0
    gen double share_`marker'_aiimput_pct  = 100 * n_`marker'_aiimput / n_projects if n_projects > 0
    format share_`marker'_title_pct    %5.1f
    format share_`marker'_aititle_pct  %5.1f
    format share_`marker'_aiimput_pct  %5.1f
}

save "`out_shares'", replace

* Visualization: Three lines per donor panel for each marker group
use "`out_shares'", clear

levelsof donor_label, local(all_donors)
local nd = wordcount("`all_donors'")
local cols = 4
local rows = (`nd' + `cols' - 1) / `cols'

* Climatemitigation
graph twoway ///
    (line share_clim_title_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(blue)) ///
    (line share_clim_aititle_pct paymentyear, sort lwidth(medthick) lpattern(dash) lcolor(red)) ///
    (line share_clim_aiimput_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(green)), ///
    by(donor_label, cols(`cols') note("") ///
        title("Share of Climatemitigation Rio Markers by Donor × Year (%)") ///
        legend(position(6))) ///
    ytitle("Share with Rio markers (%)") xtitle("Payment year") ///
    ylabel(0(20)100, format(%3.0f)) yscale(range(0 100)) ///
    legend(order(1 "Title-based" 2 "AI Title-based" 3 "AI Imputed") ///
           position(6) ring(0) cols(3) size(small))
graph export "$allocoutput/climatemitigation_shares_by_donor.png", replace width(3000)

* Biodiversity
graph twoway ///
    (line share_bio_title_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(blue)) ///
    (line share_bio_aititle_pct paymentyear, sort lwidth(medthick) lpattern(dash) lcolor(red)) ///
    (line share_bio_aiimput_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(green)), ///
    by(donor_label, cols(`cols') note("") ///
        title("Share of Biodiversity Rio Markers by Donor × Year (%)") ///
        legend(position(6))) ///
    ytitle("Share with Rio markers (%)") xtitle("Payment year") ///
    ylabel(0(20)100, format(%3.0f)) yscale(range(0 100)) ///
    legend(order(1 "Title-based" 2 "AI Title-based" 3 "AI Imputed") ///
           position(6) ring(0) cols(3) size(small))
graph export "$allocoutput/biodiversity_shares_by_donor.png", replace width(3000)

* Environment
graph twoway ///
    (line share_env_title_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(blue)) ///
    (line share_env_aititle_pct paymentyear, sort lwidth(medthick) lpattern(dash) lcolor(red)) ///
    (line share_env_aiimput_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(green)), ///
    by(donor_label, cols(`cols') note("") ///
        title("Share of Environment Rio Markers by Donor × Year (%)") ///
        legend(position(6))) ///
    ytitle("Share with Rio markers (%)") xtitle("Payment year") ///
    ylabel(0(20)100, format(%3.0f)) yscale(range(0 100)) ///
    legend(order(1 "Title-based" 2 "AI Title-based" 3 "AI Imputed") ///
           position(6) ring(0) cols(3) size(small))
graph export "$allocoutput/environment_shares_by_donor.png", replace width(3000)

* Climate Adaptation
graph twoway ///
    (line share_adapt_title_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(blue)) ///
    (line share_adapt_aititle_pct paymentyear, sort lwidth(medthick) lpattern(dash) lcolor(red)) ///
    (line share_adapt_aiimput_pct paymentyear, sort lwidth(medthick) lpattern(solid) lcolor(green)), ///
    by(donor_label, cols(`cols') note("") ///
        title("Share of Climate Adaptation Rio Markers by Donor × Year (%)") ///
        legend(position(6))) ///
    ytitle("Share with Rio markers (%)") xtitle("Payment year") ///
    ylabel(0(20)100, format(%3.0f)) yscale(range(0 100)) ///
    legend(order(1 "Title-based" 2 "AI Title-based" 3 "AI Imputed") ///
           position(6) ring(0) cols(3) size(small))
graph export "$allocoutput/climateadaptation_shares_by_donor.png", replace width(3000)



* Which Rio-Marker projects were successfully merged to GODAD?
**************************************************************

/*

       donor_clean   n_unique_godad   n_unique_title   share_title   n_unique_aititle   n_unique_aiimput  
             spain            31905            13041          0.41              14763              31890  
           germany            20282             8257          0.41               9277              20282  
     united states            16125             7898          0.49               8475              16125  
             italy            14083             6980          0.50               7293              14083  
            norway             6351             2877          0.45               2745               6351  
    united kingdom             5930             2611          0.44               2350               5930  
           ireland             4270             1945          0.46               1794               4270  
           austria             3986             1758          0.44               1815               3986  
       switzerland             4725             1600          0.34               2387               4725  
           finland             2850             1405          0.49               1318               2850  
            sweden             4275             1161          0.27               1278               4275  
           belgium             6868             1104          0.16               2941               6868  
            france            10338              866          0.08               2868              10338  
       netherlands             3325              800          0.24                793               3325  
          portugal             1211              398          0.33                352               1211  
           denmark             1561              322          0.21                327               1561  
        luxembourg             1271              185          0.15                417               1271  
           iceland              160              131          0.82                134                160  
            greece              381               33          0.09                 18                381  

*/


use "$alloc/godad_riomarkers.dta", clear

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donor))
* Remove missing or blank donors
drop if missing(donor_clean) | donor_clean == ""

* 1) Unique projects in GODAD by donor
egen tag_godad = tag(donor_clean projecttitle)
bysort donor_clean: egen n_unique_godad = total(tag_godad)

* 2) Unique projects with climateadaptation_title info
egen tag_title = tag(donor_clean projecttitle) if !missing(climateadaptation_title)
bysort donor_clean: egen n_unique_title = total(tag_title)

* 4) Unique projects with climateadaptation_aititle info
egen tag_aititle = tag(donor_clean projecttitle) if !missing(climateadaptation_aititle)
bysort donor_clean: egen n_unique_aititle = total(tag_aititle)

* 5) Unique projects with climateadaptation_aiimput info
egen tag_aiimput = tag(donor_clean projecttitle) if !missing(climateadaptation_aiimput)
bysort donor_clean: egen n_unique_aiimput = total(tag_aiimput)

* Keep one row per donor/projecttitle for deduplication
bysort donor_clean (projecttitle): keep if _n == 1

* Keep only donor and the unique counts
keep donor_clean n_unique_godad n_unique_title n_unique_aititle n_unique_aiimput

* Remove duplicates by donor
bysort donor_clean: keep if _n == 1

* 3) Share of unique projects with climateadaptation_title info
gen share_title = n_unique_title / n_unique_godad if n_unique_godad > 0
format share_title %6.2f

* Optional: sort and list
gsort -n_unique_title
list donor_clean n_unique_godad n_unique_title share_title n_unique_aititle n_unique_aiimput, noobs clean abbrev(32)


* shares of financial flow (flow_class)
***************************************

/*
 Disbursements in Mio.

       donor_clean   total_disb_godad   adapt_disb_title   share_disb_title   adapt_disb_aititle   adapt_disb_aiimput  
     united states          47,807.13          27,732.68               0.58            24,166.64            47,807.13  
           germany          35,646.17          15,955.75               0.45            14,192.23            35,646.17  
    united kingdom          22,042.45           8,159.82               0.37             9,032.36            22,042.45  
            france          36,055.89           7,171.40               0.20             9,925.50            36,055.89  
            norway          10,373.40           6,143.24               0.59             5,267.29            10,370.81  
            sweden           8,469.64           4,493.16               0.53             4,748.74             8,469.64  
             spain          10,120.76           3,212.47               0.32             3,540.28            10,103.13  
       switzerland           8,199.28           2,931.20               0.36             3,962.28             8,199.28  
             italy           5,278.62           2,094.59               0.40             2,061.78             5,278.62  
           belgium          10,584.66           1,678.62               0.16             4,626.43            10,584.66  
       netherlands           3,733.88           1,519.53               0.41             1,314.86             3,733.88  
           finland           2,993.06           1,427.51               0.48             1,250.38             2,993.06  
           denmark           2,802.32             950.67               0.34               882.89             2,802.32  
           ireland             881.04             630.05               0.72               589.89               881.04  
          portugal           1,136.05             400.03               0.35               287.56             1,136.05  
           austria             955.52             359.26               0.38               379.47               955.52  
        luxembourg             837.55             345.27               0.41               216.43               837.55  
           iceland             188.28             159.54               0.85               168.19               188.28  
            greece             154.84              39.77               0.26                37.21               154.84  



*/

use "$alloc/godad_riomarkers.dta", clear

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donor))
drop if missing(donor_clean) | donor_clean == ""

* 1) Total disbursement in GODAD by donor
bysort donor_clean: egen total_disb_godad = total(disb)

* 2) Total disbursement with climateadaptation_title info
gen disb_title = disb if !missing(climateadaptation_title)
bysort donor_clean: egen adapt_disb_title = total(disb_title)

* 3) Total disbursement with climateadaptation_aititle info
gen disb_aititle = disb if !missing(climateadaptation_aititle)
bysort donor_clean: egen adapt_disb_aititle = total(disb_aititle)

* 4) Total disbursement with climateadaptation_aiimput info
gen disb_aiimput = disb if !missing(climateadaptation_aiimput)
bysort donor_clean: egen adapt_disb_aiimput = total(disb_aiimput)

* Keep only one row per donor
bysort donor_clean: keep if _n == 1

* Rescale to millions
replace total_disb_godad   = total_disb_godad   / 1e6
replace adapt_disb_title   = adapt_disb_title   / 1e6
replace adapt_disb_aititle = adapt_disb_aititle / 1e6
replace adapt_disb_aiimput = adapt_disb_aiimput / 1e6

* 5) Share of disbursement with climateadaptation_title info
gen share_disb_title = adapt_disb_title / total_disb_godad if total_disb_godad > 0
format total_disb_godad  %12.2fc
format adapt_disb_title  %12.2fc
format adapt_disb_aititle  %12.2fc
format adapt_disb_aiimput  %12.2fc
format share_disb_title %6.2f

* Optional: sort and list
gsort -adapt_disb_title
list donor_clean total_disb_godad adapt_disb_title share_disb_title adapt_disb_aititle adapt_disb_aiimput, noobs clean abbrev(32)


* How much did donors spend on climate finance?
***********************************************

* for climate adaptation: number of projects

/*

       donor_clean   n_unique_godad   n_unique_title12   share_title12   n_unique_aititle12   n_unique_aiimput12  
           germany            42017               6804        .1619345                 6098                15172  
             spain            40516               4118        .1016389                 3012                 9246  
     united states            37202               1188        .0319338                  349                 1647  
           finland             5439               1046        .1923148                  271                  892  
             italy            17026                878        .0515682                  725                 2385  
    united kingdom             8079                678        .0839213                  486                 1416  
            norway             8814                659        .0747674                  189                  829  
       switzerland             9441                574        .0607986                  499                 1517  
        luxembourg             2731                478        .1750275                   54                  654  
           ireland             6138                462        .0752688                  353                 1804  
           belgium             9045                435        .0480929                 1135                 2655  
            france            15852                379        .0239087                  459                 3701  
            sweden             6432                373        .0579913                  373                  922  
           austria             5374                212        .0394492                  218                  733  
       netherlands             4059                109        .0268539                   75                  460  
           denmark             2026                 81        .0399803                   72                  340  
           iceland              183                 41        .2240437                   41                   46  
          portugal             1696                 38        .0224057                   49                  161  
            greece              413                  8        .0193705                    1                   19  

*/

use "$alloc/godad_riomarkers.dta", clear

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donor))
drop if missing(donor_clean) | donor_clean == ""

* 1) Unique projects in GODAD by donor (using project_id)
egen tag_godad = tag(donor_clean project_id)
bysort donor_clean: egen n_unique_godad = total(tag_godad)

* 2) Unique projects with climateadaptation_title == 1 or 2
egen tag_title12 = tag(donor_clean project_id) if inlist(climateadaptation_title, 1, 2)
bysort donor_clean: egen n_unique_title12 = total(tag_title12)

* 3) Share of climate adaptation projects in total godad projects
gen share_title12 = n_unique_title12 / n_unique_godad if n_unique_godad > 0

* 4) Unique projects with climateadaptation_aititle == 1 or 2
egen tag_aititle12 = tag(donor_clean project_id) if inlist(climateadaptation_aititle, 1, 2)
bysort donor_clean: egen n_unique_aititle12 = total(tag_aititle12)

* 5) Unique projects with climateadaptation_aiimput == 1 or 2
egen tag_aiimput12 = tag(donor_clean project_id) if inlist(climateadaptation_aiimput, 1, 2)
bysort donor_clean: egen n_unique_aiimput12 = total(tag_aiimput12)

* Keep one row per donor/project_id for deduplication
bysort donor_clean (project_id): keep if _n == 1

* Keep only donor and the unique counts
keep donor_clean n_unique_godad n_unique_title12 share_title12 n_unique_aititle12 n_unique_aiimput12

* Remove duplicates by donor
bysort donor_clean: keep if _n == 1

* Optional: sort and list
gsort -n_unique_title12
list donor_clean n_unique_godad n_unique_title12 share_title12 n_unique_aititle12 n_unique_aiimput12, noobs clean abbrev(32)

* for climate adaptation: funds disbursed (in Mio.)

/*

       donor_clean   total_disb_godad   total_disb_title12   share_disb_title12   total_disb_aititle12   total_disb_aiimput12  
           germany          35,646.17             3,256.22                 0.09               3,413.26               9,620.64  
            france          36,055.89             1,483.13                 0.04               2,646.95              11,703.09  
    united kingdom          22,042.45             1,364.93                 0.06               1,100.62               4,938.52  
     united states          47,807.13             1,206.75                 0.03                 584.92               2,155.26  
            sweden           8,469.64             1,183.06                 0.14               1,347.21               1,907.38  
             spain          10,120.76               785.98                 0.08                 701.84               2,422.41  
       switzerland           8,199.28               675.21                 0.08               1,645.70               2,573.27  
           belgium          10,584.66               423.14                 0.04               1,146.45               3,110.26  
             italy           5,278.62               414.24                 0.08                 361.30                 986.87  
            norway          10,373.40               409.76                 0.04                 356.81               1,398.22  
           finland           2,993.06               360.49                 0.12                 158.82               1,264.69  
       netherlands           3,733.88               317.97                 0.09                 306.46                 637.85  
           ireland             881.04               263.34                 0.30                 242.34                 374.46  
           denmark           2,802.32               183.32                 0.07                 162.05                 930.89  
        luxembourg             837.55               109.06                 0.13                  18.64                 196.36  
           austria             955.52                83.06                 0.09                  88.11                 272.07  
           iceland             188.28                23.70                 0.13                  27.26                  45.68  
          portugal           1,136.05                11.31                 0.01                  12.14                  31.30  
            greece             154.84                 0.98                 0.01                   0.00                   2.54  

*/

use "$alloc/godad_riomarkers.dta", clear

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donor))
drop if missing(donor_clean) | donor_clean == ""

* 1) Total disbursement in GODAD by donor
bysort donor_clean: egen total_disb_godad = total(disb)

* 2) Total disbursement with climateadaptation_title == 1 or 2
gen disb_title12 = disb if inlist(climateadaptation_title, 1, 2)
bysort donor_clean: egen total_disb_title12 = total(disb_title12)

* 3) Share of climate adaptation disbursement in total godad disbursement
gen share_disb_title12 = total_disb_title12 / total_disb_godad if total_disb_godad > 0

* 4) Total disbursement with climateadaptation_aititle == 1 or 2
gen disb_aititle12 = disb if inlist(climateadaptation_aititle, 1, 2)
bysort donor_clean: egen total_disb_aititle12 = total(disb_aititle12)

* 5) Total disbursement with climateadaptation_aiimput == 1 or 2
gen disb_aiimput12 = disb if inlist(climateadaptation_aiimput, 1, 2)
bysort donor_clean: egen total_disb_aiimput12 = total(disb_aiimput12)

* Keep only one row per donor
bysort donor_clean: keep if _n == 1

* Optional: rescale to millions
replace total_disb_godad     = total_disb_godad     / 1e6
replace total_disb_title12   = total_disb_title12   / 1e6
replace total_disb_aititle12 = total_disb_aititle12 / 1e6
replace total_disb_aiimput12 = total_disb_aiimput12 / 1e6

* Format for display
format total_disb_godad     %12.2fc
format total_disb_title12   %12.2fc
format total_disb_aititle12 %12.2fc
format total_disb_aiimput12 %12.2fc
format share_disb_title12   %6.2f

* Optional: sort and list
gsort -total_disb_title12
list donor_clean total_disb_godad total_disb_title12 share_disb_title12 total_disb_aititle12 total_disb_aiimput12, noobs clean abbrev(32)

* Disbursements on Climate Adaptation across years

use "$alloc/godad_riomarkers.dta", clear

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donor))
drop if missing(donor_clean) | donor_clean == ""

* Aggregate disbursements by donor and year for climateadaptation_title (not missing)
gen disb_title12 = disb if inlist(climateadaptation_title, 1, 2)
bysort donor_clean paymentyear: egen disb_title_year = total(disb_title12)

* Aggregate disbursements by donor and year for climateadaptation_aiimput == 1 or 2
gen disb_aiimput12 = disb if inlist(climateadaptation_aiimput, 1, 2)
bysort donor_clean paymentyear: egen disb_aiimput_year = total(disb_aiimput12)

* Keep only one row per donor/year for plotting
bysort donor_clean paymentyear: keep if _n == 1

* Rescale to millions
replace disb_title_year = disb_title_year / 1e6
replace disb_aiimput_year = disb_aiimput_year / 1e6
format disb_title_year %12.2fc
format disb_aiimput_year %12.2fc

* Create panel graph: one panel per donor, two lines per panel
levelsof donor_clean, local(donors)
local nd = wordcount("`donors'")
local cols = 4
local rows = (`nd' + `cols' - 1) / `cols'

graph twoway ///
    (line disb_title_year paymentyear, sort lwidth(medthick) lcolor(blue)) ///
    (line disb_aiimput_year paymentyear, sort lwidth(medthick) lcolor(green)), ///
    by(donor_clean, cols(`cols') note("") ///
        title("Disbursements for Climate Adaptation Projects by Donor × Year (millions)") ///
        legend(order(1 "Title-based" 2 "AI Imputed") position(6) ring(0) cols(1) size(small)) ) ///
    ytitle("Disbursement (millions)") xtitle("Year") ///
    ylabel(, format(%8.0f))

* Export graph
graph export "$allocoutput/disb_climateadaptation_by_donor_panel.png", replace width(3000)

* Compare numbers with magnitudes in Rio data
*********************************************

/*
in $ Mio.

        donor_clean   total_comm_rio   total_comm_12   share_comm_12  
      united states          4289850        93865.73        .0218809  
    eu institutions          2358384        245284.6        .1040054  
            germany          2125220        261093.1        .1228547  
              japan          1519625        194633.4        .1280799  
             france          1176872        148645.8        .1263058  
     united kingdom         822353.9        57933.97        .0704489  
        netherlands         587517.1        71120.95        .1210534  
             canada         452282.2        28457.89        .0629206  
             norway         375824.1           17284        .0459896  
          australia         322650.9        38797.93        .1202474  
             sweden         320624.7        35845.22        .1117981  
        switzerland         303538.9        26811.79        .0883307  
              spain         240813.3        11868.15        .0492836  
              italy         228515.2        12786.68        .0559555  
              korea         218007.6         35752.1        .1639948  
            denmark         190442.7        19092.93        .1002555  
            belgium         187644.4        16183.45        .0862453  
            austria         96675.73        3669.348        .0379552  
            finland          90486.1        8285.927        .0915713  
            ireland            80290        6340.053        .0789644  
        new zealand         48042.86        5421.991        .1128574  
         luxembourg         36182.06        1880.961         .051986  
           portugal         35832.09        213.7305        .0059648  
             poland         35170.07        628.2283        .0178626  
             greece         16350.87        32.99482        .0020179  
            czechia         12261.11        373.1153        .0304308  
            hungary         8211.628        275.5668        .0335581  
            iceland          3474.17        789.9802        .2273868  
           slovenia         2729.529        154.7701        .0567021  
            estonia         2294.771        21.72528        .0094673  
          lithuania         2009.906        39.23008        .0195184  
    slovak republic         1869.655        38.91916        .0208162  
             latvia         1071.512          2.1384        .0019957  


*/

use "$riomarkers/riomarkers200223", clear

* climate adaptation by donor by year

* Clean donor name for consistency
gen strL donor_clean = lower(strtrim(donornamee))
drop if missing(donor_clean) | donor_clean == ""

* 1) Total disbursement in GODAD by donor
bysort donor_clean: egen total_comm_rio = total(usd_commitment_defl)

* 2) Total disbursement with climateadaptation_title == 1 or 2
gen comm_12 = usd_commitment_defl if inlist(climateadaptation, 1, 2)
bysort donor_clean: egen total_comm_12 = total(comm_12)

* 3) Share of climate adaptation disbursement in total godad disbursement
gen share_comm_12 = total_comm_12 / total_comm_rio if total_comm_rio > 0

* Keep only one row per donor
bysort donor_clean: keep if _n == 1


* Format for display
format total_comm_rio     %12.2fc
format total_comm_12   %12.2fc
format share_comm_12   %6.2f


* Optional: sort and list
gsort -total_comm_rio
list donor_clean total_comm_rio total_comm_12 share_comm_12, noobs clean abbrev(32)



* How well does CRS Code 410 recover environment projects? A: not very well.
tab env_crs
tab score

/*

. tab env_crs

    env_crs |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |    103,506       95.89       95.89
          1 |      4,433        4.11      100.00
------------+-----------------------------------
      Total |    107,939      100.00

. tab score

(max) score |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     64,100       59.39       59.39
          1 |     24,525       22.72       82.11
          2 |     19,314       17.89      100.00
------------+-----------------------------------
      Total |    107,939      100.00

*/




* inspect which project titles merged, especially for lower similarity ~ 0,8
use "$alloc\godad_rio_aimerge", clear
hist similarity // similarity is in the interval [0.8, 1] most are at 1, only very few in tails

forvalues i = 80/100 { 
    local lower = `i'/100
    local upper = (`i'+1)/100
    count if similarity >= `lower' & similarity < `upper'
    di "Interval [`lower', `upper'): " r(N)
}

// each interval has more than 1000 observations, Interval [.99, 1): 51040, Interval [1, 1.01): 135587
// -> the AI merge does improve the merge by a magnitude of 15,000 observations

list projecttitle projecttitle_rio if similarity >=0.8 & similarity <=0.81 // some merged project titles appear to belong to the same project but slightly different disbursements within the same project
list projecttitle projecttitle_rio if similarity >=0.85 & similarity <=0.86 // sometimes different phases of projects -> should not change the overall rating
list projecttitle projecttitle_rio if similarity >=1 & similarity <=1.01 // these are literally the same project titles






