COMPASS-FME Level 1 data
Version: [VERSION]
Date: [DATESTAMP]
Observations: [OBSERVATIONS]
Git commit: [GIT_COMMIT]

DESCRIPTION
—----------------------------------
Level 1 (L1) data are close to raw, but are units-transformed and have
out-of-instrument-bounds and out-of-service flags added. Duplicates and
missing data are removed but otherwise these data are not filtered, and
have not been subject to any additional algorithmic or human QA/QC. Any
scientific analyses of L1 data should be performed with care.

CONTACT
—----------------------------------
Project: https://compass.pnnl.gov
Data lead: Stephanie Pennington, stephanie.pennington@pnnl.gov

HOW TO CITE THESE DATA
—----------------------------------
Pennington, Bittencourt Peixoto, Bond-Lamberty, Cheng, LaGorga,
Machado-Silva, Peresta, Phillips, Regier, Rich, Sandoval, Stearns, Ward,
Wilson, Weintraub, Megonigal, and Bailey (2024). COMPASS-FME Level 1
Sensor Data (version [VERSION] released [DATESTAMP]), downloaded
YYYY-MM-DD, https://compass.pnnl.gov.

DATA STRUCTURE
—----------------------------------
Data are organized into {SITE_YEAR} folders, with up to 12 monthly CSV
files in each folder for each plot at the site. Sites include CRC (Crane
Creek), GCW (GCReW), GWI (Goodiwn Island), MSM (Moneystump Marsh), OWC
(Old Woman Creek), PTR (Portage River), SWH (Sweet Hall Marsh), and TMP
(TEMPEST experiment). See site-specific metadata files in each folder.

CHANGELOG
—----------------------------------
Version 1-0 released 2024-05-15
* Covers late 2019 through April 2024 for TEMPEST and all synoptic sites
* Restructured for ease of use, with metadata (location, sensor ID, etc) in separate columns
* SWH plot naming reworked for new upland plot; mirroring TMP C to GCW UP
* GCReW weather station variables are now mirrored to GCW-W 
* Many fixes to variable units and bounds
* Out-of-service is valid for AquaTROLL and EXO

Version 0-9 released 2024-01-22
* Preliminary release covering all synoptic site and TEMPEST data collected to date
* Units and bounds (and thus OOB flags) are missing for some ClimaVue, AquaTROLL, and Sonde variables
* Some research_name assignments for may incorrect for ClimaVue
* Out-of-service is working only for AquaTROLL
* No TEMPEST 1- or 5-minute data included
