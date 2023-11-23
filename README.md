# TIPZOO-R
R scripts used in TIPZOO (a Touchscreen Interface for Palaeolithic Zooarchaeology)
More info at https://tipzoo.cnrs.fr/

To facilitate the use of these scripts:
	1) set the TIPZOO-R/ folder as your working directory in Rstudio.
	2) place all the data exported by TIPZOO-FMP in the « for_import » sub-folder
	3) Launch the following scripts depending on the analyses you want to carry out:
	- SPECIES_script.R for reporting species list
	- TAPHO_script.R for analyses of bone surface modifications
	- CUT_script.R for reporting cut-mark data
	- NDE_script.R for analyses of skeletal-part representation

You should not edit the TIPZOO.R file in the source/ folder.

Summary tables (Excel files) are automatically created in the « export » sub-folder.

If you want to add new reference datasets (e.g. other species), you can use the files in the « ref_data » sub-folder as examples. A ref_data_README file in this sub-folder provides information about the reference datasets included in TIPZOO-R, including the names of the people that generously contributed data.

For any questions: emmanuel.discamps@cnrs.fr
