# msu-r-aci-fit
Fit A/Ci curves with simple R scripts.

## Install
Automatic installation is easiest with devtools. Install devtools by running
<pre>install.packages("devtools")</pre>
Then install this package by running
<pre>devtools::install_github("poales/msuRACiFit")</pre>
Installation with devtools is **highly** recommended because it will automatically download the remote dependencies of this package.

## Usage
There are two main ways to use this package. You can fit curves from the terminal, or fit curves using the shiny app.
### Shiny app
The shiny app is run by executing
<pre>msuRACiFit::genApp()</pre>
and is designed to mimic the feel of existing excel fitting tools while being more overall flexible - in particular, the shiny app deals much better with large datasets.
#### Shiny app notes
1. Load in your data file using the load data menu. This can load multiple types of data:
	1. Rectangular data in csv or xlsx format. The data must have headers.
	2. LI-COR data files. The data will be read in by remote dependency poales/readLicorData. This works best with the extensionless text file (tab-delimited) produced by the LI-COR. It will also work with the excel files produced by the LI-COR.
	- **IF YOU USE XLSX FILES**: R is incapable of calculating formulae in excel data files before or while reading them in. The calculated values must be cached. To achieve this, open the excel files in excel, save the file, then close it.
2. Select the data columns. By default the program will try to find data with headers "A" or "Phot" and "Ci" or "pCi". If the program does not automatically find these columns, you can specify them in the dropdown menu.
3. Enter the appropriate temperature, pressure, and gamma\*. The default gamma\* is for tobacco at 25 degrees C with an oxygen concentration of 21%.
4. The fitting functions are good at finding local minima, and not very good at finding the global minimum. This is true of virtually every nonlinear fitting program. This means that to get a good fit, you must have the starting values relatively close to the "correct" values. To help achieve this goal, press the "Generate guess" button, which uses a couple of heuristics to make a guess of the correct values. Only after you generate guesses should you fit the data using the "Fit" button. If the fit looks qualitatively wrong, you should try adjusting the starting parameters before fitting again.
5. Save your data. The "write" button will allow you to save a 1-row tidy data csv containing the parameters, the sum of squares residual, and the number of points. To save the graph, hover over the graph and hit the save button (camera). To save the fitting table, click the "save table" button.

### Terminal
The main fitting function is fitACi. This accepts a tibble which contains the relevant columns for assimilation and Ci. The program by default looks for assimilation in the "A" column and Ci in either "pCi" or "Ci" columns, but these can be changed with arguments <code>name_assimilation</code> and <code>name_ci</code>. Fitting is done using minpack.lm, and can be controlled in various ways. Upper and lower bounds can be set with <code>bound_h</code> and <code>bound_l</code>, and values can be forced with <code>forceValues</code>. By default, the program will generate guesses for the initial values of the parameters, to guide the fitting program into the correct local minimum, and reduce misfitting.

After fitting is done, the parameters can be passed on to <code>reconstituteGraph</code> and <code>reconstituteTable</code> to visualize and see greater detail on the quality of the fit, including which data points are fit to which limitation on photosynthesis.

If you wish to see everything all at once, the function <code>fitComplete</code> will automatically generate the table and the graph for the fit.
