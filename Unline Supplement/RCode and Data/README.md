CSV files of Green Sturgeon adult spawner counts in the Sacramento River, California, USA,
with numeric column and row names.  Year data collected is in document title.
See publication DOI:XXX??? for in-depth details.

Count****.csv Details
Data is formatted to be imported into R using code archived at github site www.XXXXX
and at Zenodo associated with this Dryad data archive.

Top row is column headers.  Initial column header is blank for row names
First column is site numbers with "1" being our most southerly site and increasing north.  Site numbers are deliberately NOT geolocated in the paper to avoid locating aggregations of an endangered species often targeted for poaching.  Site numbers are consistent across years.
Rows are sites and columns are replicate transects at each site within a day.
Data are counts of animals identified on side scan sonar data for a single transect.

UpDownStream.txt Details
Direction of transect relative to river flow (upstream or Downstream)
Data is for 2022 only and table dimensions are the same as Count2022.csv
Row 1 is Column labels
Column 1 is Relative site locations, 1 being most southerly and higher being more northerly.
1 indicates downsteam
0 indicates upstream

R code
R code for all N-mixture models, descriptive statistics, Figures, and "unmarked" package analyses.
Placing all data and code files associated with this archive in the same folder, then opening the
Publication.Rproj file should allow all *.R code to run completely.