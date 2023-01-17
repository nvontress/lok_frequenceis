# lok_frequencies
Code used for master's research at NCSU

#### Running the code
1. __ci_cyano_processing.R__: This script is the first step of the code, used to scale the digital numbers of the Cyan data product to CI<sub>cyano</sub>. This code also saves the environment so the terra objects can be accessed in later steps. Data inputs include CI<sub>cyano</sub> imagery from https://oceancolor.gsfc.nasa.gov/projects/cyan/ and Lake Okeechobee shape file.
2. __roi_analysis_ci_cyano.R__: Step 2 of workflow, used to calculate the ROI in m. Data required include ci_cyano_processing_workspace.Rdata (for projection info and lake boundary), the DBHYDRO station shapefile (for S308's coordinates), and the NWIS S-308 flow data (NWISdata/20160501-20210430_s308.csv).
3. __lake_station_selection.R__: Step 3 of workflow, used to filter Lake Okeechobee chl-_a_ sample locations based on data availability during the study period and site location wrt the land QA filter (https://www.sciencedirect.com/science/article/pii/S2352340919311813). Data inputs include the ci_cyano_processing_workspace.Rdata (for projection info and lake boundary), the DBHYDRO station shapefile (for S308's coordinates), and the littoral zone shapefile. This script also has code to create lake quadrants, which are saved to the work space.
4. __chla_mod_ci_cyano.R__: Step 4 of workflow, used to calculate MAE<sub>log</sub> and bias<sub>log</sub> between chlorophyll-_a_ _in situ_ samples and chlorophyll-_a_ estimates from CI<sub>cyano</sub> imagery. Also used to produce Figure 5 in manuscript (figure number is subject to change).
5. __olci_scihub_downlaod.R__: Step 5 of workflow, used to queue S3A and S3B flyover dates for Lake Okeechobee. A Copernicus SciHub login is required to run this code.
