# About this story

Data and code behind [Shot by cops and forgotten](https://news.vice.com/story/shot-by-cops/), a VICE News investigation into police shootings in the 50 largest local police departments between 2010-2016.

# Data and code

There are seven files in this repository:
- `incident_data.csv` - clean data for every police shooting **incident**.
- `subject_data.csv` - clean data for every police shooting **subject**.
- `census_pop_2013.csv` - population data from the [2013 American Community Survey (ACS)](https://www.census.gov/programs-surveys/acs/).
- `initial_data_processing.R` - code to read in, clean and process the incident based data.
- `building_subject_database.R` - code to build the subject database from the incidents.
- `final_analysis.R` - code to generate the facts and figures in the article.
- `graphics.R` - code to build the graphics.

# Credits:

Data analysis and graphics: Rob Arthur and Allison McCann.

Reporting: Rob Arthur, Taylor Dolven, Keegan Hamilton, Allison McCann,
and Carter Sherman.

# License:

This code and data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/).

Questions/comments/corrections: [email Allison McCann](mailto:allison.mccann@vice.com).




