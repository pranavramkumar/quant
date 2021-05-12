//Initial steps
	cd "C:\Users\Pranav Ramkumar\Desktop\Quant Series\Q4\Data collection\36 cities"
	import delimited "36cities_dataset.csv"
	log using 36cities_log, replace
	
//Install the estout package to be able to store regression results
	ssc install reghdfe
	ssc install ftools
	ssc install estout, replace

//Acknowledge biases
   //deaths - U replaced with 0s
   //some averagelst and averagendvis are unavailable and hence replaced
   //53rd week of 2010, 2011, 2012, 2013, 2015 to be removed?

//Initial steps   
	label define citylabels 2 "Albuquerque" 4 "Atlanta" 5 "Austin" 3 "Baltimore" 7	"Boise City" 6 "Boston" 8 "Charlotte" 9 "Chicago" 10 "Cleveland" 11 "Denver" 12 "Des Moines" 13 "Detroit" 16 "Hartford" 37 "Honolulu" 14 "Jersey City" 15 "Kansas City" 17 "Las Vegas" 18 "Little Rock" 20 "Los Angeles" 22 "Memphis" 21 "Miami" 19 "Minneapolis" 25	"Montgomery" 26	"New Orleans" 24 "New York" 30 "Philadelphia" 27 "Phoenix" 29 "Portland" 28 "Providence" 35 "Richmond" 33 "Salt Lake City" 32 "San Diego" 34 "San Francisco" 31 "Seattle" 36 "Tulsa" 23	"Washington"
	label values cityindex citylabels
  
//Run some descriptive and summary statistics
	des mmwrweek
	codebook cityindex
	sum deaths_pneumonia_and_influenza
	sum deaths_pneumonia_and_influenza, detail
	tabulate cityindex, summarize(deaths_pneumonia_and_influenza) means standard
	tabulate cityindex, summarize(averagelst_degc) means standard
	tabulate cityindex, summarize(average_ndvi) means standard
	tabulate year, summarize(deaths_pneumonia_and_influenza) means standard

//Run some Naive OLS Regressions
	reg deaths_pneumonia_and_influenza averagelst_degc
	//eststo Col1
	reg deaths_pneumonia_and_influenza averagelst_degc average_ndvi
	//eststo Col2
	reg deaths_pneumonia_and_influenza averagelst_degc average_ndvi i.year
	//eststo Col3
	reg deaths_pneumonia_and_influenza averagelst_degc average_ndvi i.cityindex
	//eststo Col4
	reg deaths_pneumonia_and_influenza averagelst_degc average_ndvi i.year i.cityindex
	//eststo Col5

//log-linear specifications?
	
//Run some Fixed effects regressions
	//areg
	areg deaths_pneumonia_and_influenza averagelst_degc average_ndvi, absorb(city)
	//eststo Col6
	
	//xtreg
	//xtset city year mmwrweek
	//xtreg deaths_pneumonia_and_influenza averagelst_degc average_ndvi, fe
	//eststo Col7
	//store and run Hausman test on whether better to use fixed effects or random effects
	
	//reghdfe
	reghdfe deaths_pneumonia_and_influenza averagelst_degc average_ndvi, absorb(year, mmwrweek, city) vce(robust)
	//eststo Col8
	reghdfe deaths_pneumonia_and_influenza averagelst_degc average_ndvi, absorb(year, mmwrweek, city) vce(cluster cityindex)
	//eststo Col9

//Output the regressions into an rtf table
	esttab Col1 Col3 Col4 Col5 Col6 Col7 Col8 Col9 using "36cities.rtf", label mtitles ("(OLS1)" "(OLS2)" "(YEAR)" "(CITY)" "(YEAR&CITY)" "(AREG)" "(XTREG)" "(REGHDFE1)" "(REGHDFE2)") title(Regression_table_36Cities)

//Ponder about the biases further

//Closing steps
log close


