* calculate Gini for CHN using the synthetic distribution
version 16

cap frame drop gini
frame create gini  year double(gini)

local chn_files: dir "." files "CHN*.dta", respectcase

qui foreach f of loca chn_files {
	
	use "`f'", clear	
	if regexm("`f'", "_([0-9]+)_") local year = regexs(1)
	
	fastgini welfare_ppp [w = weight]
	
	frame post gini (`year') ( r(gini))
	
}

frame gini: list



