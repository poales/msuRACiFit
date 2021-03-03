# msu-r-aci-fit
Fit A/Ci curves with simple R scripts.

## Install
Automatic installation is easiest with devtools. Install devtools by running
<pre>install.packages("devtools")</pre>
Then install this package by running
<pre>devtools::install_github("poales/msuRACiFit")</pre>

## Usage
There are two main ways to use this package. You can fit curves from the terminal, or fit curves using the shiny app.
#### Shiny app
The shiny app is run by executing
<pre>msuRACiFit::genApp()</pre>
and is designed to mimic the feel of existing excel fitting tools while being more overall flexible - in particular, the shiny app deals much better with large datasets.
#### Terminal
The main fitting function is fitACi. This accepts a tibble which contains the relevant columns for assimilation and Ci. The program by default looks for assimilation in the "A" column and Ci in either "pCi" or "Ci" columns, but these can be changed with arguments <pre>name_assimilation</pre> and <pre>name_ci</pre>. Fitting is done using minpack.lm, and can be controlled in various ways. Upper and lower bounds can be set with <pre>bound_h</pre> and <pre>bound_l</pre>, and values can be forced with <pre>forceValues</pre>. By default, the program will generate guesses for the initial values of the parameters, to guide the fitting program into the correct local minimum, and reduce misfitting.

After fitting is done, the parameters can be passed on to reconstituteGraph and reconstituteTable to visualize and see greater detail on the quality of the fit, including which data points are fit to which limitation on photosynthesis.

If you wish to see everything all at once, the function <pre>fitComplete</pre> will automatically generate the table and the graph for the fit.
