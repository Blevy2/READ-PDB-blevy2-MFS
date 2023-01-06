# READ-PDB-blevy2-toy


#Descriptions of new and edited scripts

run_sim has been edited to work for our purposes. This includes eliminating fishing, allowing for a sequence of move_cov matrices to be used, implimenting new movement proecedure, 
fixing the recruitment/spawning processes (overlapping weeks leads to 4 pulses of correct magnitude, allows for different weeks between species, use previous year population to determine recruitment size)
removing the scientific survey

BENS_init_survey.R is and edit of init_survey.R, which defines things relevant to the survey and simulation including number of stations, days_fished, and the log.mat (log matrix) which records survey values
My edit added the "random_station" survey option which reads in strata_coords (from hab$strata), strata_num (from hab$stratas), years_cut (number of extra years in each simulation to ignore)
This code generates random cells from each strata and stores them in log.mat. log.mat is therefore important as it guides the survey since it contains the cells to draw from
Instead of running this before a simulation it is called in run_survey_Bens_NEW prior to implimenting the post hoc survey


init_movCov_Bens  (original, 2, 3 and 4) each create temperature covariate matrices that extend longer than 1 year using MixFishSim builtin functions
I moved away from these and used movecov_longer_time_Bens (see below)


init_movCov_Bens edits init_movCov in 2 ways
	1) changes trend from decreasing temperature over the course of a year to increasing temp
	2) each year it extends the temp further
This one is not ideal because it starts at the same temp each year and extends further


init_movCov_Bens2 edits init_movCov by starting at a higher temp point each year but following the same trend
problem with this trend is that it just increases temp over the course of a year and then jumps back to a lower value


init_movCov_Bens3 is an attempt at an oscillating relationship rather than just increasing over the course of a year and then jumping back to a lower value

init_movCov_Bens4 is same as init_moveCov except only creates first matrix. Then this matrix is adjusted, sine is applied, second half stretched, and finally copied 52 times

movCov_longer_time is another attempt to create temperature covariate matrices that extend longer than 1 year using MixFishSim builtin functions
I moved away from these and used movecov_longer_time_Bens (see below)


movCov_longer_time_Bens builds temperature gradient from scratch by transitioning from min value in upper right cornre to max value in lower left corner. This temp gradient
is then hit with a sine function that gradually increases over time (or is repeated 20 times) to create 20 year datasets. 
We then decided to run 22 years and throw the first 2 out which required 2 more years to be copied in the beggining using add_2_movecov_yrs.R

add_2_movecov_yrs.R is used to add 2 copies of the first year in the begginign of movcov matrix to account for running 22 years

movCov_FVCOM_Data does the same thing as movCov_longer_time_Bens, except it uses the temperature data extracted from FVCOM. Thus this code first plots various temp
data before choosing a specific year to use to extend to 20 years.


FVCOM_extract.R and its various iterations are the code from Robyn to extract depth, temperature, and salinity data.
FVCOM_extract is the closest to the original code from Robyn
FVCOM_extract_Bens is my first iteration extracting for all dates
FVCOM_extract_Bens_JustSurveyDates focuses on just extracting the temperature data for only days in which we have survey data
FVCOM_extract_Bens_TEmpTimeseries focuses on just extracting the temperature data over 11 years


init_pop_Bens same as init_pop except I also record the lambda parameter for use in creating movement probabilities, pass nonzero habitat values for new movement scripts, call new movement script to set initial population


init_sim_Bens creates different week.breaks (called week.breaks.all) that will run from week 1 to week n where n is total week in simulation
week.breaks.all will be used to call the temperature covariate matrix for each week in the simulation (rather than looping back each year)

init_sim_Bens_nofish is the same as init_sim_Bens except it also creates a day.seq and day.breaks that allows us to intialize a fleet where no fishing is taking place (ghost fleet fishes 1 day per week)

Bens_plot_spatiotemp_hab takes plot_spatiotemp_hab and allows only certain weeks to be plotted, as specified by plot_wk
also plots the first week of each month over entire simulation. Result has same week plotted for each year so that each page has 5X4 grid 

Bens_plot_spatiotemp_hab_justtemp takes plot_spatiotemp_hab and plots only the temperature gradient 
as well as temp gradient + species temp preferences. Also allows only certain weeks to be plotted, as specified by 'plot_wk' and to specify color gradient (breaks) using 'colrange'

Bens_plot_pop_spatiotemp.R uses plot_spatiotemp_hab to instead plot the spatial population in a given simulation. Options to plot_weekly or plot_monthly, plot_week lets us plot specific weeks, ColBreaks allows user defined color breaks

BENS_plot_habitat.R is an edit of plot_habitat that includes the values of nu, var, scale and Aniso (RandomFields inputs to create habitat) on the plots. I used this while creating generic habitats 

go_fish_Bens eliminates most of the script to avoid decision about where to fish and instead chooses a random location for the single vessel. At the bottom the catchability value is replaced with a 0

go_fish.R and go_fish_orign.R are both the same as the original go_fish script. I played around with some changes in these before reverting them backk and creating go_fish_Bens.R to use

Analyze_Survey.R is a script that reads in stratified mean output csv files and plots them in different groupings. Assumes that survey and resulting stratified mean calculations have been completed

Anisotropy.R is a script creatd by Chris Legault that explores the RandomFields RMsimulate and RMmatern functions that are used to created generic habitats. Ben edited the bottom of te code to try
additional combinations of parameters


Environmental_Interp_Kern_Dens and similar files are for creating fish habitat by relating covariates (depth and median sediment)

Environmental_Interp_Kern_Dens is original file that contains the example from the website before I recreate the example with my own data. I used this file to flesh out the general process including extracting 
results from the envi package, rescaling from [-1,1] to [0,1] and saving results as a matrix as well as a raster.

Environmental_Interp_Kern_Dens_Cod, _Haddock, and _YellowTailFlounder apply the same process from Environmental_Interp_Kern_Dens to each species to create species-specific habitats (edit these codes for future use)

lrren_Bens is my version of envi::lrren used inside the Environmental_Interp_Kern_Dens codes. I originally make this to save output to extract for habitats before realizing that what I wanted was from envi::plot_predict (see below)

plot_predict_BENS is my version of envi::plot_predict used inside the Environmental_Interp_Kern_Dens codes. I made this to save output for habitats. The output is saved using my version and transformed to fall between [0,1] inside
the Environmental_Interp_Kern_Dens codes

Fish_life.R is a script that uses the package FishLife to extract key parameters for fish species such as growth K, natural mortality M, and other parameters that can be used to calculat beverton holt recruitment params



GB_habitat_create reads in rasters created in Environmental_interp_kern_dens to create the hab object used in simulation. hab is 4 components: hab (spatial habitat for each species), spawn_hab (spawning habitat for each species), 
strata (numbers for each strata), and stratas (strata numbers marked spatially in matrix).
This code also makes a number of plots at the end: temperature over time, species=specific temperature in constant and increasing scenarios, species-specific habitat + temp preferences for constant and increasing temps, 
also plots the true model output, temperature, and habitat in survey weeks with the survey point locations on top of them,
also plots the habitat in a 3 pane plot for publication
also plots population line plots used in publications
also plots final habitat used in poublication
also plots temperature oscilations .

Generic_Species_Profile.R is a bare bones version of Generic_Species.Rmd that is stripped down to the essential commands so it can be used for profiling. This was created early on to profile the code.


pos2coord.R is a script to take the position in a vector and translate it to the entries in matrix. This was used in init_sim_Bens but I ended up coding the action directly into init_sim_Bens rather than leaving a seperate function

Plotting.R is a script I used early on to practice/learn about plotting objects in R. It does not contain anything that I continued to use

rand_samp.R is a script Chris Legault created to test the theory that having a large number of zeros in a sample can create large confidence intervals. This script shows that is the case.

Strat_Rand_Samp is a script I created very early on to practice generating a random sample. It does not have further use.


reprex_generic_species.R is  script I created to try and recreate the C++ issus I was having (ie, reproducible example). This script eventually become the basis for rewriting the C++ code in the package

Run_survey_BENS was created to run the scientific bottom trawl survey after a simulation has run (ie, assumes object "result" has been created that has all the different simulation iterations). The survey in this script was not was
Chris has in mind so I created a new version (below)

Run_survey_BENS_NEW is the same as above except it applies noise in the correct locations to generate the correct number of samples and also allows for different numbers of different sized strata. 
This code should be used instead of the older version

stock-recruit.R was created by Chris Legault to show Ben the different forms of the Beverton-Holt stock-recruitement curves 
and to illustrate how spawners per recruit and a stock recruitment curve can be used to calculate an equilibrium value


Strat_Rand_Samp.R is a script I used to test different sampling methods as I was working on a stratified random sampling procedure. There is nothing very useful in here. The procedure I created is in init_sim_Bens_NoFish.R

TempTolerance.R is a script used to analyze temperature tends in bottom trawl data in order to determine mean/variance values for temperature tolerances for yellowtail flounder, cod, and haddock on george's bank
The script reads in bottom trawl data, evens out the samples between season, and then tries to fit the samples to data. We realized this was not a good approach and then decided to choose values by which the data would fall
beneath the normal curve. THis is the final approach we took, which is at the bottom of the code

Test_from_andy_beet.R was sent to me by Andy Beet when I was having problems with R crashing. I believe it carries out some intense computations to see whether my system could handel it. I used this code once.

testpush.R is a script created by Chris Legault to generate a github token that can then be used to set a personal access token. 
Doing both of these things allows me to push files through on NOAA servers, but this has to be repeated each new session.

model_based_estimation.R was created by Chris Legault to illustrate a basic model-based estimation for survey data

calc_srs_index_survey_BENS.R and fn_src_survey_BENS.R work in coordination to receive survey data and calculate stratified means. These were adapted from code send by Liz Brooks. 
calc_srs_index_survey_BENS.R is the main function that reads in survey data, defines strata & areas, defines species names, and sends them to fn_src_survey_BENS.R 
fn_src_survey_BENS.R receives the information and carries out the calculation in a series of chunks that manipulate the inputted table.
fn_src_survey_BENS.R outputs information that is transformed once more by calc_srs_index_survey_BENS.R into the final desired values.

param_calc.R is a script that calculates various input parameters. Individual calculations explained below
First, calulates average weight of recruits and prerecruits by reading in weight info (avg weight by age and avg number by age) for each species to calculate weighted mean of adults. This value is used to calculate phi0 and in delay_diff for MFS
First B, calculates weighted average fishing mortality for each species
Second,simple scripts to calculate phi0 and R0 for each species 
Third, simple script to define phi0 and R0 for each species to calculate alpha and beta parameters for Beverton Holt stock recruitment fucntion

create_spawn_hab_Bens.R is an edit of create_spawn_hab to define the spawning habitat. It is the same as create_spawn_hab.R except it receives spawning
gorund coordinates and sends them to define_spawn_Bens.R below, which will increase habitat values in spawning grounds

define_spawn_Bens.R is an edit of define_spawn. This function is called inside create_spawn_hab_Bens.R. Previously, define_spawn would read in [x1,x2,y1,y2] defining rectangular spawning grounds. This new code reads in coordinates of all
spawning locations and then does a for-loop to increase habitat values by mult in each of those locations.

Movement in the model was edited in the following ways. Changes implimented in Movement.cpp, a C++ extension to this package. TO get this to work one must a) rebuild the package b) use rcpp::sourcercpp to source the two .cpp files c) compile attributes
1) Instead of redistributing the (possibly zero) value of all habitat cells to all other habitat cells (many of which are zero), we instead send in a list of nonzero indices and only redistribute nonzero cells to nonzero locations in the domain.
2) After multiplying the temperature matrix with the habitat matrix for the given weeks, run_sim used to normalize the this matrix each time step before sending it to movement. 
instead, I send the nonzero values of this non-normalized matrix into C++, find its sum, and normalize each cell value as the movement is carried out

plot_pop_summary.R is an original file that was edited to better plot the output from simulations. This file will plot total biomass and recruitment for each species over the duraction of the simulation.
it will plot these on the same plot as well as on individual plots. It has "daily" and "annual" options and the "daily" produces an error but I use the "annual". I should fix this sometime

RcppExports.R and RcppExports_Bens.R were files I was trying to edit when I was working on changin the C++ movement scripts. It turns out that this code links R to the C++ code and that it should not be edited.
Instead, it is automatically updated when C++ changes are implimented using the build() command.

BENS_create_hab.R is an edit of create_hab.R, which is used to generate a generic habitat with the RandomFields package. This is only used to defined a generic habitat. 
My edit reads in the corners of retangular stratas defined by [x1,x2,y1,y2] and creates a matrix that will have the strata number in each strata cell.
As a result we create hab$strata and hab$stratas, which is used in Run_survey and BENS_init_survey.R to generate random survey locations

param_estimate.R is used to fit recruitment parameters for desired scenario (increasing, decreasing, constant populations). This is the main script that sets up the simulations and 
impliments the optimizations scheme by calling the below codes.

run_sim_param_estimate.R is run_sim with everything removed except for recruitment and population dynamics. This code is called by param_estimate.R to fit recruitment parameters for desired scenario (increasing, decreasing, constant populations)

init_pop_Bens_param_estimate.R is an edit of init_pop_Bens used to initialize the population during parameter estimation. The main difference here is instead of creating an initial spatial population, the initial pop is a scalar value

stratified_mean_limited.R is a script that calculates the stratified mean from survey values for each individual simulation (rather than summarizing all into single value). Then it calculates the error between the seasonal estimates and the
true seasonal values from the model. Finally the script will plot each individual estimate with each simulation and save as a 100 page pdf and also saves error values from each to create a scatter plot. 
If we want to compare scatterplots for included all strata in mean calcs vs not including all, we need to run the script twice. FIrst time to create gg object 'cc', second time to add cc to second scatterplot

stratified_mean_limited_VAST.R
stratified_mean_limited_VAST_Multiple_Surveys.R
VAST_ModelSelection_Error_YT.R
VAST_ModelSelection_Error_All.R
VAST_ModelSelection_Error_Comparison_All.R
VAST_ModelSelection_Error_All_withCovs.R
VAST_ModelSelection_Error_All_withCovs_ForPaper.R

The above 8 .R files are all similar in that they compare the error between index calcluations and the true value, and plot the output. They differ in that they are successive iterations of each other. They start by just comparing
stratified mean and then we add VAST, we add noise, we add multiple species etc. The final version is the bottom one with the _ForPaper designation which will specify things at the top, read in and pull out the needed simulation info,
calculate the stratified mean given the settings (strata, noise, species etc), find the error between the vast/stratified mean and true model values, and plot everything available for all species and scenarios in a pdf



Percent_area_occupied.R is a script used to calculate the percent of the population that is in each strata and plot them over time. This can be used to see species shift as temp shifts

movement_check.R is a script that can be used to compare spatial model results with observation data from surveys. Use this to set movement values like lambda

run_on_ada.R is a bare bones version of GB_3species used to run on Steven Fiedlers computer named ada

compile_results.R is a script used to take individual simulation runs that would come from Steven Fiedlers linux computer and put them into an object called result to match the output from NOAA servers so that I was able to run the survey on them

x_y_to_lat_lon.R is a test script I used to practice reading in x,y locations from a survey and converting them into lat lon coordinates by relating them to the related habitat raster


strata_area_cell_size.R is a file that compares cell sizes given to me by David Chevrier with those given to me by Liz Brooks to try and find "true" cell sizes on George's Bank

Visualize_covariate_response.R is a script where I was testing different ways to visualze covariate responses from VAST models. I ultimately went with the one suggested in the VAST wiki which can be see in my VAST model scripts

