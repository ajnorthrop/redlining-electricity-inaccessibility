# redlining-electricity-inaccessibility
A place to share code for _Electricity inaccessibility across historically redlined areas and present-day areas in New York City_

**1.	Base data**
   
	a. NYC HOLC Data [Aggregates NYC HOLC Maps]

	b. CT ICE Calculations [Loads and calculates ICE Metrics for NYC, 2020]
	
**2.	Data Imputation**

	a. SAIFI Data Imputation

	b. Energy Usage Imputation
 	
**3.	Data Processing**
   
	a. NYSPO -> SAIFI Calculations [Calculates SAIFI for NYC POLs 2017-2019]

 	b. Census data -> [Households to HOLC boundaries]

	c. 311 Calls -> Aggregation to HOLC [Calculates outage counts for NYC HOLCs 2017-2019]
	
    	i.	311 Calls -> Aggregation to HOLC (All)
		
    	ii.	311 Calls -> Aggregation to HOLC (Seasonal)
		
	d. 311 Calls -> Aggregation to CT [Calculate outage for NYC CTs 2017-2019]
   
	e. Energy use [Interpolates Total MWH and Households for Average MWH/Household]
	
    	i.	ZCTA -> HOLC (All)
		
    	ii.	ZCTA -> HOLC (Seasonal)
		
    	iii.	ZCTA -> CT (All)
		
    	iv.	ZCTA -> CT (Seasonal)
		
	f. NYSPO [Interpolates SAIFI to HOLCs and CTs]
	
    	i.	POL -> HOLC (All)
		
    	ii.	POL -> HOLC (Seasonal)
		
    	iii.	POL -> CT (All)
		
    	iv.	POL -> CT (Seasonal)
		
**4.	Applying Filters**
   
	a. HOLC
	
    	i.	SAIFI Filter: >30 customers 
		
    	ii.	311 Filter: > 30 households 
		
    	iii.	Energy Use Filter: > 30 residences
		
	b. CT
	
    	i.	SAIFI Filter: >30 customers 
		
    	ii.	311 Filter: > 30 households 
		
    	iii.	Energy Use Filter: > 30 residences
		
**5.	Analyses**
    
	a. HOLC
	
    	i.	SAIFI Analysis 
		
    	ii.	311 Analysis 
		
    	iii.	Energy Use Analysis 
		
	b. CT
	
    	i.	SAIFI Analysis 
		
    	ii.	311 Analysis 
		
    	iii.	Energy Use Analysis 
		
	c. Correlation
	
    	i.	HOLC 
		
    	ii.	CT 
		
	d. Seasonal
	
    	i.	HOLC 
		
    	ii.	CT
		
**6.	Additional Analyses**
   
	a. ZCTA and Income 
	
	b. TBD
