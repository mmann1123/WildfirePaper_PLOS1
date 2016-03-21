

# This script aims to orgainize all input data and run fire analysis in one shot.... lets see how that goes
# use G:\\Faculty\\Mann\\SHARE\\FIRE\\PYTHON SCRIPTS\\AGGREGATESCRIPT2 to build decadal summaries of data

rm(list = ls())
# this rdata has an example of the stacked outputs used in my regression etc
# this should allow you to estimate the model and create figures. 
# Also look at how each periods data is organized
load("C:\\Users\\mmann\\Google Drive\\Wildfire_Share\\StackOutputs\\firedata_allfire3i.RData")  


install.packages('rgdal')
library(rgdal)
install.packages('car')
library(car)
install.packages('raster')
library(raster)
install.packages('snow')
library(snow)
install.packages('dismo')
library(dismo)
install.packages('pscl')
library(pscl)
install.packages('maptools')
library(maptools)
install.packages('fields')
library(fields)
install.packages('gridExtra')
library(gridExtra)
install.packages('ggplot2')
library(ggplot2)
library(lattice)
install.packages('snowfall')
library(snowfall)
source("G:\\Faculty\\Mann\\Share\\Scripts\\ten_sample.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\ten_samplev2.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\ten_samplev3.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\ten_samplev4.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\outofsamplehist2.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\outofsamplehist4.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\cfac.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\fire_plotter.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\py.rasterize2aggregate.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\py.rasterize.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\py.resample.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\py.EucDistance.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\py.ProjectRaster_management.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\nonlinear_test_zeroinfl2.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\multi_nonlinear_test_zeroinfl.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\plot_forecasts.R")
source("G:\\Faculty\\Mann\\Share\\Scripts\\plot_nonlinear_zeroinfl.R")
source('G:\\Faculty\\Mann\\Share\\Scripts\\predict.zeroinfl.se.R')
source('G:\\Faculty\\Mann\\Share\\Scripts\\ternary.R')

beginCluster(type="SOCK")
#############################################################################
#############################################################################
#############################################################################
#############################################################################
trim_white_space <- function (x) gsub("^\\s+|\\s+$", "", x)


multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
  if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
  return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
}


"%w/o%" <- function(x, y) x[!x %in% y]

 







  #############################################################################
  # Bring data into raster stacks and pull to points
  #############################################################################
  
  # These are used to iterate through all periods, and to create unified naming convensions

  decade_group_names = c(  '1951_1975', '1976_2000', '2001_2025', '2026_2050') # keep in chronological order & must be mutually exclusive and seperated by "_" 1971_2000
                   #   c('1925_1949','1950_1974','1975_1999','2000_2024','2025_2050')    
  
  alt_decade_group_names = c('5175','7600','0125','2650')
  
  envirn_variables_of_interest = c('aet','cwd','pet','ppt','tmn','tmx')  #'Ammppt','Apmppt',  names of form  "ppt1941_1970_ave"  "aet","cwd","pet","ppt","tmx","tmn",'ave_ppt_tmn','ave_ppt_tmx','mmcwd','mpcwd','pmcwd','mmppt','mpppt','pmppt','Ammpt','Apmppt'
  
  envirn_indicators_of_interest = c("Aave","Asd","Amm",'Amp','Apm')  # 'Acov','Amax','Amin','Arng',"Asum",abreviated names of variable descriptions used... for instance "ave" for mean values
  
  # these variables are included in all time periods (time invariant)
  universal_variables = c("G:\\Faculty\\Mann\\Other\\Fire\\elev_proj.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\FVeg.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Jepson.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Slope_proj.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\North_Southa_proj.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\OceanDistEuc.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\AllRoads.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Incorp.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\PrimSec.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Pp30k.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Pp20k.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Ppall.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Water.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\station_dist.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\camp_dist.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\air_dist.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\allstationdist.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\ExcldLand.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\NPark.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\PubLand.tif",
                          "G:\\Faculty\\Mann\\Other\\Fire\\PubLandDum.tif",
                          #"B:\\Aggregated1080\\Summary_Files\\OtherVariables\\OceanElevCost.tif",
                          #"B:\\Aggregated1080\\Summary_Files\\OtherVariables\\HydroReg.tif",
                          "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Lightning\\LightPredC2.tif",
                          "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Lightning\\LightPredD.tif",
                          "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Lightning\\LightMarc.tif ",
                          "G:\\Faculty\\Mann\\Other\\Fire\\Light5yrWIS.tif"
                          )
                          
  # these time varying variables are included in thier respective time period stacks 
  universal_variables_time_varying = c("G:\\Faculty\\Mann\\Other\\Fire\\den_5175_proj.tif",  
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_5175_proj.tif",                                
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_2650_proj.tif",
                                      
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax5x5_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax5x5_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax5x5_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax5x5_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax10x10_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax10x10_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax10x10_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax10x10_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax20x20_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax20x20_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax20x20_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax20x20_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_5175_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_7600_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_0125_proj.tif", 
                                       "G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_2650_proj.tif",
                                       
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden_0125_proj.tif", 
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden_2650_proj.tif",                               
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden3x3_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden5x5_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden10x10_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden3x3_2650_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden5x5_2650_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Rden10x10_2650_proj.tif",
#                                        
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden_0125_proj.tif", 
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden_2650_proj.tif",                                 
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden3x3_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden5x5_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden10x10_0125_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden3x3_2650_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden5x5_2650_proj.tif",
#                                        "G:\\Faculty\\Mann\\Other\\Fire\\Uden10x10_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax10x10_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax10x10_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax10x10_2650_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax10x10_2650_proj.tif",
                                       
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax20x20_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax20x20_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax20x20_2650_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax20x20_2650_proj.tif",
                                     
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax50x50_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax50x50_0125_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Udenmax50x50_2650_proj.tif",
                                       "G:\\Faculty\\Mann\\Other\\Fire\\Rdenmax50x50_2650_proj.tif"
                                       )
                                      # can be names of form  "nameALT_decade_group_names"  or nameDECADE_group_names"
   
  
  # location of all files not explicitly referenced above  IMPORTANT
  # all future climate data must be in one directory
  directories_inputs = c('G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080\\Summary_Files',
                         'G:\\Faculty\\Mann\\Other\\Future_Climate')
  
  # set output directory for period stacks
  directory_outputs = c("G:\\Faculty\\Mann\\Other\\Fire\\StackOutputs")
  
  
  # designate which periods are 'future scenarios' and model scenario abreviations
  fut_decade_group_names =  c('2001_2025','2026_2050')
  fut_alt_decade_group_names = c('0125','2650')
  model_abrev = c("G","P") # For GFDL and PCM
  scenario_abrev = c("A2" ) # IPCC scenario
  

 


  #############################################################################
  # create data stacks for all relevant periods
  stack_maker <-function(build_future,print_projections,print_extent){
      # searches all "directories_inputs" for variables of interest, universal, other, and time varying variables and puts them in 
    # and puts them in this respective decadal data.frames 
  
    # Check data inputs 
    if(length(multi_grep_character('/',universal_variables ))!=0){print('Remove all forward slashes from file paths, replace with double backslash')}
    if(length(multi_grep_character('/',universal_variables_time_varying ))!=0){print('Remove all forward slashes from file paths, replace with double backslash')}

    # HISTORIC 
    for(j in 1:length(directories_inputs)){
        setwd(directories_inputs[j])
        directory = dir()
        
        # get all possible combinations of decades, variables and indicators
        names_to_search = do.call(paste, c(expand.grid(do.call(paste, c(expand.grid(envirn_variables_of_interest,decade_group_names ),sep="")),  envirn_indicators_of_interest),sep="_"))  # creates all possible combinations of variable names
        # find all relevant file names
        #directory[multi_grep_character('pmcwd1925_1949.tif',directory )]
        
        found_names = directory[ unlist(lapply( 1:length(names_to_search), function(x) grep(names_to_search[x],directory ))) ]
        found_names = found_names[order(found_names)]  # put in alphabetical
        found_names = found_names[-c(grep('.xml',found_names),grep('.tfw',found_names),grep('.vat',found_names))]   # remove unneeded filetypes 
        found_names = unique(found_names)
        
        for(x in 1:length(decade_group_names)){
            if( length( found_names[grep(decade_group_names[x], found_names) ] )  !=0){  # if not empty
                found_universal_time_varying = unique(universal_variables_time_varying[c(grep(decade_group_names[x],universal_variables_time_varying),grep(decade_group_names[x],universal_variables_time_varying)) ] )# finds path for relevant decade
                alt_found_universal_time_varying = unique(universal_variables_time_varying[c(grep(decade_group_names[x],universal_variables_time_varying),grep(alt_decade_group_names[x],universal_variables_time_varying)) ]) # finds path for relevant decade
                all_found_universal_time_varying = unique(c(found_universal_time_varying,alt_found_universal_time_varying))
                
                if(print_projections==T){
                    for(rasters in c(found_names[grep(decade_group_names[x], found_names) ],all_found_universal_time_varying,universal_variables)){
                        # check to see which rasters have wrong projection
                        print(rasters)
                        print(proj4string(raster:::raster(rasters)))
                        
                    }            
                }
                if(print_extent==T){
                  for(rasters in c(found_names[grep(decade_group_names[x], found_names) ],all_found_universal_time_varying,universal_variables)){
                    # check to see which rasters have wrong projection
                    print(rasters)
                    print(raster:::extent(raster:::raster(rasters)))
                    
                  }            
                }
             
                # create stack of relevant data for the HISTORICAL
                # order is universal_variables_time_varying, envirn_variables_of_interest by indicators_of_interest, universal_variables
                assign( paste("alldata",decade_group_names[x],sep="") , raster::stack( c(  found_names[grep(decade_group_names[x], found_names) ] ,  all_found_universal_time_varying ,universal_variables)    ) , envir=.GlobalEnv  )
                #projection(get(paste("alldata",decade_group_names[x],sep=""), envir=.GlobalEnv))=  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
            }
        }
    }    
    remove(j,x)
 
    ####  FUTURE SCENARIOS 
    
    # seperate out models and scenarios from future data stacks
    if(build_future ==T){
        model_scenario = do.call(paste, c(expand.grid(model_abrev,scenario_abrev ),sep=""))
        future_stack_names = paste("alldata",fut_decade_group_names,sep="")
        for (i in 1:length(future_stack_names)){
            for(j in 1:length(model_scenario)){
                # find location of universal variables in future stack
                const_names =  names(get(paste("alldata",decade_group_names[2],sep="")))[names(get(paste("alldata",decade_group_names[1],sep=""))) %in% names(get(paste("alldata",decade_group_names[2],sep="")))] # retrieves constant names from historical data
                const_loc   = grep("TRUE", names(get(future_stack_names[i])) %in% const_names) # find universal variables in future stack names
                if(length(const_names) != length(const_loc)){paste('not all layers found for', future_stack_names[i])}
                # find location of time varying universal variables
                found_universal_time_varying = universal_variables_time_varying[c(grep(fut_decade_group_names[i],universal_variables_time_varying),grep(fut_alt_decade_group_names[i],universal_variables_time_varying)) ]  
                
                # parse out the variable names from the universal_variables_time_varying & get location in model scenario of interest 
                nameholder = lapply(1:length(strsplit(found_universal_time_varying, "\\.")),function(x) strsplit(found_universal_time_varying, "\\.")[[x]][1]    ) 
                nameholder = lapply(1:length(nameholder), function(x)  strsplit(nameholder[[x]], "\\\\")   ) 
                nameholder = unlist(lapply(1:length(nameholder),function(x) nameholder[[x]][[1]][length(nameholder[[x]][[1]])]    )  )
                loc_universal_time_varying = grep( "TRUE" ,names(get(future_stack_names[i])) %in% nameholder  )
                        
                # reassign stack for each model scenario combination while keeping universal variables
                # variables to keep 
                variables = names(get(future_stack_names[i]))[ c( grep( model_scenario[j],names(get(future_stack_names[i]))),const_loc, loc_universal_time_varying) ]
                assign( paste(future_stack_names[i],model_scenario[j],sep="_"), raster::subset(get(future_stack_names[i]),  variables  ), envir=.GlobalEnv)
                #projection(get(paste(future_stack_names[i],model_scenario[j],sep="_"), envir=.GlobalEnv)) =  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
            }   
        }
        remove(i,j)
    }
}

  # RUN STACK MAKER
  stack_maker(build_future=T, print_projections=F,print_extent=F)
  # verify all elements
  names(alldata1951_1975) 
  names(alldata1976_2000)
  names(alldata2001_2025_GA2)
  names(alldata2026_2050_GA2)
  names(alldata2001_2025_PA2)
  names(alldata2026_2050_PA2)


#############################################################################
# rename variables in data stacks  
  remove_trailing = c("_HST1080","res1080","1080","4170","_proj")  # patterns to be removed from the back of variable names
  
  for(j in decade_group_names  ){
      # this loop removes decade and alt_decade and "remove_trailing" patterns from variable names
      # done seperately from stack_maker so that you can double check all names 
      stack_names = ls(pattern="alldata")  # get all stacks in workspace
      relevant_year_stacks = stack_names[grep(j,stack_names)]   # find relevant stacks for each decade
      print(paste("current stack:",relevant_year_stacks))
      remove_trailing = c(remove_trailing,"_")
      for(i in 1:length(relevant_year_stacks)){   # if climate scenarios present 
          model_scenario  = paste("_",do.call(paste, c(expand.grid(model_abrev,scenario_abrev ),sep="")),sep="")  #remove _modelScenario
          remove_trailing = c(remove_trailing,model_scenario)
          # remove relevant decade_group_names from name
          replacement_names = sub(names(get(relevant_year_stacks[i],envir=.GlobalEnv)), pattern=j, replacement="")
              for(x in 1:length(remove_trailing)){    
                  #remove remove_trailing patterns one by one 
                  replacement_names = sub(replacement_names, pattern=remove_trailing[x], replacement="")
              }
              for(x in 1:length(alt_decade_group_names)){    
                  #remove remove_trailing patterns one by one 
                  replacement_names = sub(replacement_names, pattern=alt_decade_group_names[x], replacement="")
              }
               
              #remove "_"
          replacement_names = sub(replacement_names, pattern="_", replacement="")
          stackholder = get(relevant_year_stacks[i],envir=.GlobalEnv)
          names(stackholder) = replacement_names   # reassign new names 
          assign(relevant_year_stacks[i], stackholder, envir=.GlobalEnv)
      }
      print(j)
      remove(x,i,j)
      do.call(remove, list(paste("alldata",fut_decade_group_names,sep="") )) # remove non.scenario future stacks  MUST RUN THIS
  }
  # following only relevant if future scenarios built
  
  names(alldata1951_1975) 
  names(alldata1976_2000)
  names(alldata2001_2025_GA2)
  names(alldata2026_2050_GA2)
   
  #save.image("G:/Faculty/Mann/FireWorkspace.RData")
  #load("G:/Faculty/Mann/FireWorkspace.RData")
 

#############################################################################
# de-mean variables assign name(var_a/mean(var_a)) = 'var_a2'
  
  for(j in decade_group_names  ){
      # this loop removes decade and alt_decade and "remove_trailing" patterns from variable names
      # done seperately from stack_maker so that you can double check all names 
      stack_names = ls(pattern="alldata")  # get all stacks in workspace
      relevant_year_stacks = stack_names[grep(j,stack_names)]   # find relevant stacks for each decade
      print(paste("current stack:",relevant_year_stacks))
      
      for(i in 1:length(relevant_year_stacks)){   # if climate scenarios present 
          stackholder = get(relevant_year_stacks[i],envir=.GlobalEnv)
          #get mean values of all rasters in stack 
          mean_vector = unlist(lapply(1:dim(stackholder)[3],function(x){return(   mean(values(stackholder[[x]]),na.rm=T)  )} ))
          # divide each raster by its mean 
          demean_stack = calc(stackholder, fun=function(x){x / mean_vector})
          old_names = names(stackholder)
          new_names = paste(old_names,'2',sep='')
          names(demean_stack) = new_names
          combine_stack = stack(stackholder,demean_stack)
          assign(relevant_year_stacks[i], combine_stack, envir=.GlobalEnv)
      }
      print(j)
      remove(x,i,j)
  }
  names(alldata1951_1975) 
  names(alldata1976_2000)
  names(alldata2001_2025_GA2)
  names(alldata2026_2050_GA2)
  names(alldata2001_2025_PA2)
  names(alldata2026_2050_PA2)  

 

#############################################################################
# If you want to include a one period lag then run this portion
# integrate past climate data into datastacks
#   for(j in  2:length(decade_group_names) ){   # starting with the second period loop through stacks 
#       print("J SHOULD START AT 2 ")
#       stack_names = ls(pattern="alldata")  # get all stacks in workspace
#       previous_year_stacks = stack_names[grep(decade_group_names[j-1],stack_names)]   # find relevant stacks from pervious period
#       relevant_year_stacks = stack_names[grep(decade_group_names[j],stack_names)]     # find relevant stacks for each decade
#       
#           if(length(relevant_year_stacks) == 1 ){  # avoid issue with multiple scenarios for current period
#               # for non-scenarios or if only one scenario 
#               # find all environmental variables in previous period stack
#               previous_env_loc =  unique(multi_grep_character(find=envirn_variables_of_interest, inthis=names(get(previous_year_stacks,envir=.GlobalEnv)) ))
#               # remove names that already have "_P" from the list otherwise creates multiple lags 
#               previous_env_loc_with_P =    unique( grep("_P",names(get(previous_year_stacks,envir=.GlobalEnv)))  )
#               previous_env_loc = previous_env_loc %w/o% previous_env_loc_with_P  # remove _P locations from previous_env_loc
#               # get the data we want from previous year 
#               pervious_relevant_data   = raster::subset( get(previous_year_stacks,envir=.GlobalEnv), previous_env_loc ) 
#               
#               past_current_stack = raster::stack(  get(relevant_year_stacks,envir=.GlobalEnv) , pervious_relevant_data  )
#               
#               names(past_current_stack) =c( names(get(relevant_year_stacks,envir=.GlobalEnv)), paste(names(pervious_relevant_data),"_P",sep=""))
#               assign(relevant_year_stacks,past_current_stack, envir=.GlobalEnv )
#           }
#           if(length(relevant_year_stacks) == (length(model_abrev) * length(scenario_abrev))& length(relevant_year_stacks) !=1 & length(previous_year_stacks) == 1 ){  # avoid issue with multiple scenarios for current period, also avoid if only one scenario
#               # if current period has scenarios and previous period does not 
#               for(h in 1:length(relevant_year_stacks)){
#                   # find all environmental variables in previous period stack
#                   previous_env_loc =  multi_grep_character(find=envirn_variables_of_interest, inthis=names(get(previous_year_stacks,envir=.GlobalEnv)) )
#                   # remove names that already have "_P" from the list otherwise creates multiple lags 
#                   previous_env_loc_with_P =    grep("_P",names(get(previous_year_stacks,envir=.GlobalEnv)))  
#                   previous_env_loc = previous_env_loc %w/o% previous_env_loc_with_P  # remove _P locations from previous_env_loc
#                   # get the data we want from previous year 
#                   pervious_relevant_data   = raster::subset( get(previous_year_stacks,envir=.GlobalEnv), previous_env_loc ) 
#                   
#                   past_current_stack = raster::stack(  get(relevant_year_stacks[h],envir=.GlobalEnv) , pervious_relevant_data  )
#                   
#                   names(past_current_stack) =c(names(get(relevant_year_stacks[h],envir=.GlobalEnv)), paste(names(pervious_relevant_data),"_P",sep=""))
#                   assign(relevant_year_stacks[h],past_current_stack, envir=.GlobalEnv )
#               }
#           }
#           if(length(relevant_year_stacks) == (length(model_abrev) * length(scenario_abrev))& length(relevant_year_stacks) !=1 & length(previous_year_stacks) == (length(model_abrev) * length(scenario_abrev)) ){   
#               # if current period has scenarios and previous period does too 
#               model_scenario = do.call(paste, c(expand.grid(model_abrev,scenario_abrev ),sep=""))
#               
#               for(h in  model_scenario ){
#                   previous_stack_scenario = previous_year_stacks[grep(h,previous_year_stacks)]
#                   current_stack_scenario = relevant_year_stacks[grep(h,relevant_year_stacks)]
#                   # find all environmental variables in previous period stack
#                   previous_env_loc =  multi_grep_character(find=envirn_variables_of_interest, inthis=names(get(previous_stack_scenario,envir=.GlobalEnv)) )
#                   # remove names that already have "_P" from the list otherwise creates multiple lags 
#                   previous_env_loc_with_P =    grep("_P",names(get(previous_stack_scenario,envir=.GlobalEnv)))  
#                   previous_env_loc = previous_env_loc %w/o% previous_env_loc_with_P  # remove _P locations from previous_env_loc
#                   # get the data we want from previous year 
#                   pervious_relevant_data   = raster::subset( get(previous_stack_scenario,envir=.GlobalEnv), previous_env_loc ) 
#                   
#                   past_current_stack = raster::stack(  get(current_stack_scenario,envir=.GlobalEnv) , pervious_relevant_data  )
#                   
#                   names(past_current_stack) =c(names(get(current_stack_scenario,envir=.GlobalEnv)), paste(names(pervious_relevant_data),"_P",sep=""))
#                   assign(current_stack_scenario,past_current_stack, envir=.GlobalEnv )
#               }
#       }  
#   }
#   names(alldata1951_1975) 
#   names(alldata1976_2000)
#   names(alldata2001_2025_GA2)
#   names(alldata2026_2050_GA2)
#   names(alldata2001_2025_PA2)
#   names(alldata2026_2050_PA2)
# 
#  

#################   DELETE ALL FILES FROM directory_outputs firsT !
 

print('DELETE ALL FILES FROM directory_outputs firsT !')
stacks = ls(pattern="alldata") # must do this first

proj = proj4string(readOGR("G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FirePerim11_1","FRAP_Fire111"))
fire_perm = readShapePoly("G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FirePerim11_1\\FRAP_Fire111.shp", proj4string=CRS(proj))
fire_perm = spTransform(fire_perm, CRS=CRS( proj4string(get(stacks[1])) ))

  ## test 
  sum(data1976_2000$fire_poly)
  
  rows = dim(raster(alldata1976_2000,1))[1]
  cols = dim(raster(alldata1976_2000,1))[2]
  extents  = extent( raster(alldata1976_2000,1))
  crss = crs(raster(alldata1976_2000,1))
  
  highres = raster(extents,nrows=rows*10, ncols=cols*10,crs=crss )
  pts=raster::rasterToPoints(raster::raster(highres ))
  pts<-pts[,c(1,2)]
  pts= as.data.frame(pts)
  
  coordinates(pts) = ~x+y
  proj4string(pts) = proj4string(highres)
  
  fire_perm_period = fire_perm[fire_perm$YEAR_ %in% seq(1975,2000),]
  firecounter = sapply(over(pts, geometry(fire_perm_period), returnList = TRUE), length)
  firecounter2 = firecounter/100
  
  fire <- rasterize(pts,raster(alldata1976_2000,1),firecounter2,fun=sum)
  
  windows()
  plot(fire, main="Actual Fire Count")
  sum(getValues(fire))
  #end test should be ~32,000km2  closest is 28380 for 7500
  
  
for( j in 1:length(stacks) ){  # for all stacks in workspace
    print(paste("working on stack ",stacks[j]))
    if(j==1){pts <- raster::rasterToPoints(raster::raster(get(ls(pattern="alldata")[1]),1) )           #just a layer to get point locations from
             pts<-pts[,c(1,2)]
             pts= as.data.frame(pts)
             
             coordinates(pts) = ~x+y
             proj4string(pts) = proj4string(get(stacks[j] ))
             }
    extractor =  as.data.frame(raster::extract(get(stacks[j] ),pts))
    extractor$x = coordinates(pts)[,'x']
    extractor$y = coordinates(pts)[,'y']
    
    # get future model scenario combinations
    hist_group_names = decade_group_names[-c(multi_grep_character(fut_decade_group_names,decade_group_names))]
    model_scen =  do.call(paste, c(expand.grid(model_abrev,scenario_abrev ),sep=""))
    futr_group_names = do.call(paste, c(expand.grid(fut_decade_group_names,model_scen ),sep="_"))
    hist_futr_group_names = c(hist_group_names,futr_group_names)
    
    print('         counting overlapping fire polygons for all points')
    # limit polyons to period of interest
    # any run if there is more than one fire in this period
    if(sum(fire_perm$YEAR_ %in% seq(as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][1]), as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][2])))!=0){
      fire_perm_period = fire_perm[fire_perm$YEAR_ %in% seq(as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][1]), as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][2])     ),]
      #fire_perm_period = fire_perm[fire_perm$YEAR_ %in% seq(as.numeric(strsplit(decade_group_names[j],'_')[[1]][1]),as.numeric(strsplit(decade_group_names[j],'_')[[1]][2])     ),]
      # sum up number of overlapping fire polygons 
      extractor$fire_poly = sapply(over(pts, geometry(fire_perm_period), returnList = TRUE), length)
    }
   
#    }
    print('         writing to csv') 
    write.csv(extractor, paste(directory_outputs,'\\',paste("pnt_",stacks[j],sep=""),".csv",sep='')   )
    assign( paste("pnt_",stacks[j],sep="") ,extractor)
    do.call(remove, list("extractor", stacks[j], paste("pnt_",stacks[j],sep="")))  # clear up some memory
} 
 #  remove(pts)



#############################################################################
# remove all non-flamable or missing data from datasets

# omit_variables_logic > use to make explicit variable omission for example "elevation >= 100000"
# na_omit_list >  list all variables that you want to omit nas from for example "slope" will remove all points with slope == NA
  omit_variables_logic = c( "FVeg!=9" , "FVeg!=1") #  , # remove water and farms INTERPRET LOGIC AS: KEEP ALL .....
  
   
  na_omit_list = c('LightMarc','NorthSoutha','FVeg','Slope','elev')   # variables where NAs are omited for all variables 
  replace_list = data.frame(name=c("den","den3x3","den5x5","den10x10","den20x20",'PubLand' ),replace=c(NA,NA,NA,NA,NA,NA),with=c(0,0,0,0,0,0), stringsAsFactors=F)  # replace value with ___?
                            
  input_pnts = list.files(paste(directory_outputs), pattern="pnt_alldata")
   
  for (i in 1:length(input_pnts)){
      # replace list notworking at removing nas from Den
      logic = paste("in_pnts$",omit_variables_logic,sep="",collapse=" & ")
      na_logic = paste("!is.na(in_pnts$",na_omit_list,")",sep="",collapse=" & ")
      in_pnts  = read.csv(paste(directory_outputs,'\\',  input_pnts[i] ,sep=''))    
      in_pnts  = in_pnts[eval(parse(text=logic)),]      
      in_pnts  = in_pnts[eval(parse(text=na_logic)),]   
      for(j in 1:dim(replace_list)[1]){
           # avoid if variables don't exist in all stack
          if(replace_list$name[j] %in%  names(in_pnts) ){    
              # treat differently if value replaced is NA   
              # test if is.na and has some values meeting that criteria
              if(is.na(replace_list$replace[j]) & is.integer(is.na( in_pnts[,replace_list$name[j]] ))==T ){ in_pnts[is.na( in_pnts[,replace_list$name[j]] ),replace_list$name[j]] =  replace_list$with[j]  }
              if(!is.na(replace_list$replace[j])& is.integer(is.na( in_pnts[,replace_list$name[j]] ))==T ){ in_pnts[in_pnts[,replace_list$name[j]]==replace_list$replace,replace_list$name[j]] = replace_list$with[j]}               
          }
          else{print(paste(replace_list$name[j], "does not exist in stack input_pnts", i,input_pnts[i]))}
      }
      write.csv(in_pnts, paste(directory_outputs,'\\',paste("na_",stacks[i],sep=""),".csv",sep='') )
      print(paste("done with ",i,"out of",length(input_pnts)))
   }

     
    
#############################################################################
# ADD URBAN VS RURAL SCENARIOS HERE... JUST DUPLICATE alldata forecasts and change density to U or R 

#############################################################################
# create k-fold samples 
 
input_pnts = list.files(directory_outputs, pattern="na_alldata")
for (i in 1:length(input_pnts)){
    in_pnts  = read.csv(paste(directory_outputs,'\\',  input_pnts[i] ,sep=''))
    set.seed(12)
   # k <- 50   #50 for samples of 2%    #trying a trade-off in sparse sampling, but still catching the 2+ values
    # k<-40 2.5% sample (dim(data1951_1975)[1]/40)/dim(data1951_1975)[1]
    k = 40
    group <- dismo::kfold(in_pnts,k)         
    in_pnts$group2 <- group
     print('one done')
    write.csv(in_pnts, paste(directory_outputs,'\\',paste("Kfold_",stacks[i],sep=""),".csv",sep='') )
}
remove(in_pnts,group)

     
 
#############################################################################
# prepare data for regressions historical 


##############################################################################
# get historical data assign as "datadecade_group_names" designate training and testing data
  historical_decade_group_names  =  decade_group_names  %w/o% fut_decade_group_names
  
  input_pnts = list.files(directory_outputs, pattern="Kfold_alldata")
  input_pnts = input_pnts[ multi_grep_character(find=historical_decade_group_names, inthis=input_pnts)] # restrict to historical
  print(paste("Make sure", historical_decade_group_names, "is in the same order as ",input_pnts,  "  if not reorder so they match"))   
  for (i in 1:length(input_pnts)){
      in_pnts  = read.csv(paste(directory_outputs,'\\',  input_pnts[i] ,sep=''))
      in_pnts$train = in_pnts$group2==1
      in_pnts$test  = in_pnts$group2!=1
      assign(paste("data",historical_decade_group_names[i],sep=""), in_pnts)
  }
  remove(input_pnts)
  
  # create dataframes for future datasets
  input_pnts = list.files(directory_outputs, pattern="Kfold_alldata")
  input_pnts = input_pnts[ multi_grep_character(find=fut_decade_group_names , inthis=input_pnts)] # restrict to historical
   
  for (i in 1:length(input_pnts)){
    in_pnts  = read.csv(paste(directory_outputs,'\\',  input_pnts[i] ,sep=''))
    name = strsplit( strsplit(input_pnts[i],'_all')[[1]][2],'.csv' )[[1]][1]
    print(name)
    assign(name, in_pnts)
  }
  remove(input_pnts)

#################################################
#  load processed data here
# save.image("G:\\Faculty\



 







  windows()
plot(raster(alldata2026_2050_PA2,'cwdAave')-raster(alldata1976_2000,'cwdAave'), main='PA2 cwd change 00-50')
windows()
plot(raster(alldata2026_2050_GA2,'cwdAave')-raster(alldata1976_2000,'cwdAave'), main='GA2 cwd change 00-50')




 
#######################
# plot all variables
for(i in seq(1,dim(alldata2026_2050_PA2)[3], 6)){
  windows(width=30,height=20)
  plot(alldata2026_2050_PA2[[i: min((i+5),dim(alldata2026_2050_PA2)[3])  ]])
}


########################
# plot variables of interest over time

# for deserts
aet=data.frame(FVeg=NULL,aetAave=NULL, Model=NULL)
for( i in c('data1951_1975','data1976_2000','data2001_2025_GA2','data2001_2025_PA2','data2026_2050_GA2','data2026_2050_PA2')){
  inner = get(paste(i))[ ,c('FVeg','aetAave')]
  inner = inner[inner$FVeg==4,]
  aet = rbind(aet,cbind(inner,paste(i)))             
}
names(aet)=c('FVeg','aetAave','Model')
windows()
ggplot(aet,aes(x=aetAave,fill=Model,group=Model))+geom_density(alpha=.2)
  g
windows()
ggplot(aet,aes(x=aetAave,fill=Model,group=Model))+geom_histogram(position="dodge")+coord_cartesian(xlim = c(5, 25))+ggtitle('AET')+scale_fill_manual(values=c('#D01C8B','#F1B6DA','#DFC27D','#A6611A','#80CDC1','#018571')) 

library(reshape)
windows()
ggplot(melt(aet[,c('aetAave','Model')] ) )+geom_boxplot(aes(y=value,x=Model,fill=Model ))
+coord_cartesian(xlim = c(5, 25))+ggtitle('AET')+scale_fill_manual(values=c('#D01C8B','#F1B6DA','#DFC27D','#A6611A','#80CDC1','#018571')) 

# for chaparrel aet
aet=data.frame(FVeg=NULL,aetAave=NULL, Model=NULL)
for( i in c('data1951_1975','data1976_2000','data2001_2025_GA2','data2001_2025_PA2','data2026_2050_GA2','data2026_2050_PA2')){
  inner = get(paste(i))[ ,c('FVeg','aetAave')]
  inner = inner[inner$FVeg==7,]
  aet = rbind(aet,cbind(inner,paste(i)))             
}
names(aet)=c('FVeg','aetAave','Model')
 
library(reshape)
windows()
ggplot(melt(aet[,c('aetAave','Model')] ) )+geom_boxplot(aes(y=value,x=Model,fill=Model ))+ggtitle('Chapparal AET')+scale_fill_manual(values=c('#D01C8B','#F1B6DA','#DFC27D','#A6611A','#80CDC1','#018571')) 
windows()
ggplot(aet,aes(x=aetAave,fill=Model,group=Model))+geom_histogram(position="dodge")+coord_cartesian(xlim = c(5, 45))+ggtitle('AET')+scale_fill_manual(values=c('#D01C8B','#F1B6DA','#DFC27D','#A6611A','#80CDC1','#018571')) 

# chaparrel cwd
cwd=data.frame(FVeg=NULL,cwdAave=NULL, Model=NULL)
for( i in c('data1951_1975','data1976_2000','data2001_2025_GA2','data2001_2025_PA2','data2026_2050_GA2','data2026_2050_PA2')){
  inner = get(paste(i))[ ,c('FVeg','cwdAave')]
  inner = inner[inner$FVeg==7,]
  cwd = rbind(cwd,cbind(inner,paste(i)))             
}
names(cwd)=c('FVeg','cwdAave','Model')

library(reshape)
windows()
ggplot(melt(cwd[,c('cwdAave','Model')] ) )+geom_boxplot(aes(y=value,x=Model,fill=Model ))+ggtitle('Chapparal CWD')+scale_fill_manual(values=c('#D01C8B','#F1B6DA','#DFC27D','#A6611A','#80CDC1','#018571')) 



########################
#create 3d plot of resource limitations
  library(classInt) 
  resources = raster(alldata1976_2000,'aetAave')
  break_levels =classIntervals(getValues(raster(alldata1976_2000,'aetAave')), n=3, style="equal")
  breaks = break_levels$brks
  resources  = cut(resources ,breaks)
  windows()
  plot(resources)
  
  conditions = raster(alldata1976_2000,'cwdAave')
  break_levels =classIntervals(getValues(raster(alldata1976_2000,'cwdAave')), n=3, style="equal")
  breaks = break_levels$brks
  conditions = cut(conditions ,breaks)
  plot(conditions)
  
  ignition = raster(alldata1976_2000,'Ppall')
  break_levels =classIntervals(getValues(raster(alldata1976_2000,'Ppall')), n=3, style="equal")
  breaks = break_levels$brks
  ignition = cut(ignition ,breaks)
  plot(ignition )
  
  values_resources = values(resources)
  values_conditions = values(conditions)
  values_ignition = values(ignition)
  values_all = paste(values_resources,values_conditions,values_ignition,sep='')
  head(cbind(values_resources,values_conditions,values_ignition,values_all))
  values_all[values_all=='NANANA'|values_all=="NA11"|values_all== "1NA2"]=NA
  values_all = as.numeric(values_all)
  unique(values_all)
  
  codes_raster = resources
  values(codes_raster) = values_all
  plot( codes_raster)
  
  data = data.frame(x = coordinates(codes_raster)[,'x'],y = coordinates(codes_raster)[,'y'],codes=values_all)
  
  # resource conditions ignitions 
  data2= na.omit(data)
  data2$alpha2 =  1-as.numeric(substring(data2[,3],3,3))/3 
  windows()
  ggplot(data=data2) + geom_tile(aes(x, y, fill=factor(codes), alpha =alpha2  ))  
  # ? use alpha for 3rd dimention only?  
  
  data3 =data2
  data3$Ignition =  1-as.numeric(substring(data3[,3],3,3))/3  
  data3$Ignition = factor(data3$Ignition,labels=c('Low','Med','High'))
  data3$ResourceConditions = as.numeric(substring(data3$codes,1,2))
  data3$ResourceConditions = factor(data3$ResourceConditions,levels=c(11,12,13,21,22,23,31,32),labels=c('LL','LM','LH','ML','MM','MH','HL','HM') )
  windows()
  ggplot(data=data3) + geom_tile(aes(x, y, fill=ResourceConditions, alpha =  factor(Ignition)   )) +xlab('')+ylab('') + scale_colour_brewer(palette="Set1")
  
  windows()
  ggplot(data=data3) + geom_tile(aes(x, y, fill=ResourceConditions, alpha =  factor(Ignition)   )) +xlab('')+ylab('') +
  scale_fill_manual(values = c('#FFFFFF','#FF9999','#FF3333','#9999FF','#FF33FF','#FF0066','#3333FF','#6600FF'))
  
  windows()
  ggplot(data=data3) + geom_tile(aes(x, y, fill=ResourceConditions, alpha =  factor(Ignition)   )) +xlab('')+ylab('') + scale_colour_brewer(palette="Set1")+ theme(legend.position="none")



  #overlays
  unique(data3$ResourceConditions)
  data3$Resource  = substring( as.character(data3$ResourceConditions),1,1)
  data3$Resource  = factor(data3$Resource,levels=c('H','M','L'),labels=c('H','M','L'),order=T)

  data3$Condition = substring( as.character(data3$ResourceConditions),2,2)
  data3$Condition  = factor(data3$Condition,levels=c('H','M','L'),labels=c('H','M','L'),order=T)
   
  resourcecolor <- c('#E34A33','#FDBB84',"#FEE8C8")
  conditioncolor <- c('#3182BD','#9ECAE1',"#DEEBF7")

  windows()
 ggplot(data3,aes(x, y, fill=Resource, alpha =.1,group=Condition)) + geom_tile() +  scale_fill_manual(values = resourcecolor)
   a+  geom_tile(data=data3,aes(x, y, fill=Condition, alpha = 0))  +scale_colour_identity(conditioncolor)
  plot(a)
+  scale_fill_manual(values = conditioncolor)

  


#############################################################################
#############################################################################
# random set of code used before the regression 

# compare negative bin and poisson  http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
   nb1 <- glm.nb(fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)+PubLandDum  +denmax50x502+I(denmax50x502^2)+campdist2  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) ,   data = data1976_2000[data1976_2000$train==T,])
   ps1  = glm(fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)+PubLandDum  +denmax50x502+I(denmax50x502^2)+campdist2  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) , family = "poisson",   data = data1976_2000[data1976_2000$train==T,])
   pchisq(2 * (logLik(nb1) - logLik(ps1)), df = 1, lower.tail = FALSE)  #This strongly suggests the negative binomial model, estimating the dispersion parameter, is more appropriate than the Poisson model.

# demonstrate that zero-inflation improves performance
    nb <- glm.nb( fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)+PubLandDum  +denmax50x502+I(denmax50x502^2)+campdist2  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) ,   data = data1976_2000[data1976_2000$train==T,])
    m1 <- zeroinfl(fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)+PubLandDum  +denmax50x502+I(denmax50x502^2)+campdist2  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5),dist="negbin", data = data1976_2000[data1976_2000$train==T,])
    vuong(nb, m1) #zero-inflated model is superior to the standard Poisson model.  http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm

# compare to null model http://www.ats.ucla.edu/stat/r/dae/zinbreg.htm
   m0 <- update(m1, . ~ 1)
   pchisq(2 * (logLik(m1) - logLik(m0)), df = 3, lower.tail = FALSE)  # we can see that our overall model is statistically significant.
   

# summary stats
    print("Available Variables: ")
    names(data1976_2000)[order(names(data1976_2000) )]
    summary(data1976_2000)
    variables = paste(names(data1976_2000)[order(names(data1976_2000) )],collapse=' ')

## correlations
    library(Hmisc)
 
    testdata  = data1976_2000[sample(nrow(data1976_2000), 50000), ]
    testdata$Jepson = factor(testdata$Jepson,labels=c('Cascade','Central W','E of Sierra','C Valley','Modoc','Mojave D','NWest','Sierra','Sonoran D','SWest'))

    windows()
    ggplot( testdata , aes(x=aetAave,y=cwdAave))+geom_point(shape=1)
    ggplot( testdata , aes(x=cwdAave,y=aetAave))+stat_density2d(aes(colour = factor(Jepson),fill = ..level..,alpha = ..level..), geom="polygon")
    ggplot( testdata , aes(x=cwdAave,y=aetAave,z=fire_poly))+geom_density2d(aes(colour = factor(Jepson)), size=1)+guides(colour = guide_legend(override.aes = list(size=2)))

    # plot functional plant types 
    windows()
    ggplot( testdata , aes(x=aetAave,y=cwdAave, colour=factor(FVeg)))+geom_point(shape=1)
    ggplot( subset(testdata,FVeg!=10& FVeg!=8 & FVeg!=2) , aes(x=cwdAave,y=aetAave))+stat_density2d(aes(colour = factor(FVeg),alpha = ..level..), geom="polygon")

    fvege = raster(alldata1976_2000,'FVeg')
    val <- getValues(fvege)
    xy <- as.data.frame(coordinates(fvege))
    xy <- cbind(xy,val)
    windows()
    ggplot(na.omit(xy), aes(x=x, y=y, fill=factor(val)))+ geom_raster() +scale_fill_brewer(type='qual',palette=2)+xlab('')+ylab('')+ theme(legend.position = "none")

 
    

    rcorr(testdata$aetAave, testdata$cwdAave)

    correlations = rcorr(as.matrix(data1976_2000[data1976_2000$group2 %in% c(1:20),c("fire_poly", names(data1976_2000)[order(names(data1976_2000) )])]), type="pearson") # type can be pearson or spearman
    order = order(correlations$r[,'fire_poly'],decreasing=T)
    data.frame(cor=correlations$r[order,'fire_poly'], pval=correlations$P[order,'fire_poly'])
    
    data1976_2000$fire01 = data1976_2000$fire_poly>=1
    correlations01 = rcorr(as.matrix(data1976_2000[data1976_2000$group2 %in% c(1:20),c("fire01", names(data1976_2000)[order(names(data1976_2000) )])]), type="pearson") # type can be pearson or spearman
    order = order(correlations01$r[,'fire01'],decreasing=T)
    data.frame(cor=correlations01$r[order,'fire01'], pval=correlations01$P[order,'fire01'])
    
    
    dater = subset(data1976_2000,select=c('fire_poly','pptArng','pptAmax','pptAsd','pptAmax_P','pptArng_P','Slope','aetAave','aetAsum','aetAave_P'))
    dater = dater[sample(nrow(dater),10000,replace=F),]
    
    dater2 = subset(data1976_2000,select=c('fire_poly','OceanDistEuc','aetAcov','aetAcov_P','tmnArng','tmnAsd','tmnAsd_P','Pp30k','tmxArng','tmxAsd' ))
    dater2= dater2[sample(nrow(dater2),10000,replace=F),]
    
    panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y,use='complete.obs')
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt)#, cex = cex.cor * r)
    }
    windows()
    pairs(dater, lower.panel=panel.smooth, upper.panel=panel.cor)
    windows()
    pairs(dater2, lower.panel=panel.smooth, upper.panel=panel.cor)
    
    all_interesting = subset(data1976_2000,select=c('fire_poly','aetAave','aetAcov','aetAsd','cwdAsum','cwdAave','cwdAcov','cwdAsd','cwdArng','pptAave','pptAsum','den','Pp30k','PrimSec'))
    all_interesting = na.omit(all_interesting)
    R=cor(all_interesting)
    format(round(R, 2)) 
    
    windows()
    par(mfrow=c(2,3))
    plot(raster(alldata1976_2000,'aetAave'),main="aetave")
    plot(raster(alldata1976_2000,'aetAcov'),main="aetcov")
    plot(raster(alldata1976_2000,'aetAsd'),main="aetcov")
    plot(raster(alldata1976_2000,'cwdAave'),main="cwdave")
    plot(raster(alldata1976_2000,'cwdAcov'),main="cwdcov")
    plot(raster(alldata1976_2000,'cwdAsd'),main="cwdsd")
    
    windows() 
    par(mfrow=c(2,3))
    plot(raster(alldata1976_2000,'pmcwd'),main="plusminuscwd")
    plot(raster(alldata1976_2000,'mpcwd'),main="meanpluscwd")
    plot(raster(alldata1976_2000,'pptave'),main="pptave")
    plot(raster(alldata1976_2000,'tmnrng'),main="tmnrng")
    plot(raster(alldata1976_2000,'tmxrng'),main="tmxrng")
    plot(raster(alldata1976_2000,'petsd'),main="petsd")
    
    windows() 
    par(mfrow=c(2,3))
    plot(raster(alldata1976_2000,'aveppttmx'),main="aveppttmx")
    plot(raster(alldata1976_2000,'aveppttmn'),main="aveppttmn")
    plot(raster(alldata1976_2000,'pptmax'),main="pptmax")
    plot(raster(alldata1976_2000,'pptmax_P'),main="pptmax_P")
    plot(raster(alldata1976_2000,'aetave'),main="aetave")
    plot(raster(alldata1976_2000,'aetave_P'),main="aetave_P")
    
    library(rasterVis)
    windows()
    plot(is.na(raster(alldata1976_2000,'pptmax')), legend=F)
    plot(raster(alldata1976_2000,'Fire'))

    levelplot(raster(alldata1976_2000,'Fire'), add=t)
    library(fields)
    windows()
    a=raster(alldata1976_2000,'Fire')
    a[!is.na(raster(alldata1976_2000,'pptmax'))]=0
    plot(a,main="Fire")
    

#############################################################################
# run regressions historical 
# 
#  
# # step-wise selection  can help to narrow down selection 
#     library(MASS)
#     zero=glm( I(fire_poly>=1) ~  aetAave+aetAave_P+aetAsd+aetAsd_P+aetAsum+aetAsum_P+AllRoads+cwdAave+cwdAave_P+cwdAsd+cwdAsd_P+cwdAsum+cwdAsum_P+I(den+den^2)+den+elev+Incorp+NorthSoutha+NPark+OceanDistEuc+petAave+petAave_P+petAsd+petAsd_P+petAsum+petAsum_P+Pp20k+Pp30k+Ppall+pptAave+pptAave_P+pptAsd+pptAsd_P+pptAsum+pptAsum_P+PrimSec+PubLand+Slope+tmxAave+tmxAave_P+tmxAsd+tmxAsd_P+tmxAsum+tmxAsum_P+tmnAave+tmnAave_P+tmnAsd+tmnAsd_P+tmnAsum+tmnAsum_P+Water, 
#            data=data1976_2000[data1976_2000$train ==T,], family = "binomial" )
#     
#     zero2= glm( I(fire_poly>=1) ~ aetAave+aetAcov+aetAmax+aetAmin+aetArng+aetAsum+cwdAave+cwdAcov+cwdAmax+cwdAmin+
#           cwdArng+cwdAsum+petAave+petAcov+petAmax+petAmin+petArng+petAsum+pptAave+pptAcov+pptAmax+
#           pptAmin+pptArng+pptAsum+tmnAave+tmnAcov+tmnAmax+tmnAmin+tmnArng+tmnAsum+tmxAave+tmxAcov+tmxAmax+       
#           tmxAmin+tmxArng+tmxAsum+
#           den+I(den+I(den^2))+elev+Slope+NPark+PubLand+AllRoads+Incorp+PrimSec+Pp30k+Pp20k+        
#           Ppall+Water+OceanDistEuc+NorthSoutha+aetAave_P+aetAcov_P+aetAmax_P+aetAmin_P+
#           aetArng_P+aetAsum_P+cwdAave_P+cwdAcov_P+cwdAmax_P+cwdAmin_P+cwdArng_P+cwdAsum_P+
#           petAave_P+petAcov_P+petAmax_P+petAmin_P+petArng_P+petAsum_P+pptAave_P+pptAcov_P+pptAmax_P+pptAmin_P+pptArng_P+
#           pptAsum_P+tmxAave_P+tmxAcov_P+tmxAmax_P+
#           tmxAmin_P+tmxArng_P+tmxAsum_P+tmnAave_P+tmnAcov_P+tmnAmax_P+tmnAmin_P+tmnArng_P+tmnAsum_P,data=data1976_2000[data1976_2000$train ==T,], family = "binomial" )
#     
#     summary(zero)
#     zero <- stepAIC(zero,  direction="backward")
#     summary(zero)
#     
#     
#     zero2= glm( I(fire_poly>=1) ~ aetave+aetcov+aetmax+aetmin+aetrng+aetsum+cwdave+cwdcov+cwdmax+cwdmin+
#         cwdrng+cwdsum+petave+petcov+petmax+petmin+petsum+pptave+pptcov+pptmax+
#         pptmin+pptrng+pptsum+tmnave+tmncov+tmnmax+tmnmin+tmnsum+tmxave+tmxcov+tmxmax+
#         tmxmin+tmxrng+tmxsum+aveppttmn+aveppttmx+avetmppptmx+mmcwd+mpcwd+pmcwd+mmppt+mpppt+
#         pmppt+den+I(den+I(den^2))+Elev+Slope+NPark+PubLand+AllRoads+Incorp+PrimSec+Pp30k+Pp20k+
#         Ppall+Water+OceanElevCost+OceanDistEuc+NorthSouth+HydroReg+aetave_P+aetcov_P+aetmax_P+aetmin_P+
#         aetrng_P+aetsum_P+cwdave_P+cwdcov_P+cwdmax_P+cwdsum_P+mmcwd_P+mpcwd_P+pmcwd_P+
#         petave_P+petcov_P+petmax_P+petmin_P+petsum_P+pptave_P+pptcov_P+pptmax_P+pptmin_P+pptrng_P+    
#         pptsum_P+aveppttmn_P+aveppttmx_P+avetmppptmx_P+mmppt_P+mpppt_P+pmppt_P+tmxave_P+tmxcov_P+tmxmax_P+
#         tmxmin_P+tmxsum_P+tmnave_P+tmncov_P+tmnmax_P+tmnmin_P+tmnsum_P
#         ,data=data1976_2000[data1976_2000$train ==T,], family = "binomial" )
#     zero2 <- stepAIC(zero2,  direction="backward")
#     summary(zero2)
#     
#     
#     
#     count=glm( fire ~ aetave+aetave_P+aetseas+aetseas_P+aetsum+aetsum_P+allroads+cwdave+cwdave_P+cwdseas+cwdseas_P+cwdsum+cwdsum_P+I(Den+Den^2)+Den+Den10x10+I(Den10x10+Den10x10^2)+Den3x3+I(Den3x3+Den3x3^2)+Den5x5+I(Den5x5+Den5x5^2)+elev+Incorp+northsouth2+npark+OceanDistEuc+OceanElevCost+petave+petave_P+petsd+petseas_P+petsum+petsum_P+pp20k+pp30k+ppall+pptave+pptave_P+pptseas+pptseas_P+pptsum+pptsum_P+primsec+publicland+slope+tmaxave+tmaxave_P+tmaxseas+tmaxsd_P+tmxsum+tmxsum_P+tmnave+tmnave_P+tmnsd+tmnsd_P+tmnsum+tmnsum_P+water+x+y , 
#               data=data1971_2000[data1971_2000$train ==T,], family = "poisson" )
#     summary(count)
#     count <- stepAIC(count,  direction="backward")
#     summary(count)
#     
#     a= zeroinfl( fire ~  1  | Den10x10+I(Den10x10^2)+OceanElevCost+cwdseas , 
#              dist="poisson",data= data1971_2000[data1971_2000$train==T,])
#     summary(a)
#     AIC(a)
#     
#     b= zeroinfl( fire ~  Den10x10+I(Den10x10^2)+ slope+ cwdseas+ pptseas+pptave_P | 1  , 
#                  dist="poisson",data= data1971_2000[data1971_2000$train==T,])
#     summary(b)
#     AIC(b)

##################################################
# multiple run approach 
 
names(data1976_2000)
[1] "X"               "X.2"             "X.1"             "aetAave"         "aetAsd"          "cwdAave"        
[7] "cwdAmm"          "cwdAmp"          "cwdApm"          "cwdAsd"          "petAave"         "petAsd"         
[13] "pptAave"         "pptAmm"          "pptAmp"          "pptApm"          "pptAsd"          "tmnAave"        
[19] "tmnAsd"          "tmxAave"         "tmxAsd"          "den"             "den3x3"          "den5x5"         
[25] "den10x10"        "den20x20"        "denmax5x5"       "denmax10x10"     "denmax20x20"     "denmax50x50"    
[31] "elev"            "FVeg"            "Jepson"          "Slope"           "NorthSoutha"     "OceanDistEuc"   
[37] "AllRoads"        "Incorp"          "PrimSec"         "Pp30k"           "Pp20k"           "Ppall"          
[43] "Water"           "stationdist"     "campdist"        "airdist"         "allstationdist"  "ExcldLand"      
[49] "NPark"           "PubLand"         "PubLandDum"      "LightPredC2"     "LightPredD"      "LightMarc"      
[55] "Light5yrWIS"     "aetAave2"        "aetAsd2"         "cwdAave2"        "cwdAmm2"         "cwdAmp2"        
[61] "cwdApm2"         "cwdAsd2"         "petAave2"        "petAsd2"         "pptAave2"        "pptAmm2"        
[67] "pptAmp2"         "pptApm2"         "pptAsd2"         "tmnAave2"        "tmnAsd2"         "tmxAave2"       
[73] "tmxAsd2"         "den2"            "den3x32"         "den5x52"         "den10x102"       "den20x202"      
[79] "denmax5x52"      "denmax10x102"    "denmax20x202"    "denmax50x502"    "elev2"           "FVeg2"          
[85] "Jepson2"         "Slope2"          "NorthSoutha2"    "OceanDistEuc2"   "AllRoads2"       "Incorp2"        
[91] "PrimSec2"        "Pp30k2"          "Pp20k2"          "Ppall2"          "Water2"          "stationdist2"   
[97] "campdist2"       "airdist2"        "allstationdist2" "ExcldLand2"      "NPark2"          "PubLand2"       
[103] "PubLandDum2"     "LightPredC22"    "LightPredD2"     "LightMarc2"      "Light5yrWIS2"    "x"              
[109] "y"               "fire_poly"       "group2"          "train"           "test"        

# List of potential specification (yes tons of them... was more challenging than ussual)

#GOOD "mikenewe3 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2) +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2 +Light5yrWIS2 +log(aetAave2+.5)+log(aetAave2+.5):Light5yrWIS2" 
#"mikenewe2 = fire_poly ~cwdAave2+I(cwdAave2^2)+log(aetAave2+.5)+cwdAsd2  +Slope +PubLandDum +denmax20x202+I(denmax20x202^2) | Ppall2 +Light5yrWIS2 +log(aetAave2+.5)+log(aetAave2+.5):Light5yrWIS2" 
# almost works but cwdsd problem 
# "mikenewe2 = fire_poly ~cwdAave2+I(cwdAave2^2)+log(aetAave2+.5)  +Slope +PubLandDum +denmax50x502+I(denmax50x502^2)+cwdAsd2:log(aetAave2)  +cwdAsd2 | Ppall2 +Light5yrWIS2 +log(aetAave2+.5)+log(aetAave2+.5):Light5yrWIS2" 
# aet linear doesn't work - log(aet+.5) in count only works.  ALL log(+.5) works
# denmax50x50 significant but messes up prediction,  denmax50x502 doesn't work 
# pp30k or ppall(best) work not 20k                     
# sd is important but not significant.        

#"mikenewe = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)+  cwdAsd2+ +Slope +PubLandDum +denmax50x502+I(denmax50x502^2)  | Ppall2  +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "                                 
# sq station dist doesn't work "mikenewd8 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum +allstationdist2 +I(allstationdist2^2) | log(campdist2+1) + den  +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "  
# "mikenewd9 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum  | campdist2+I(campdist2^2)+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  " 
# FRI OK human effects odd all station dist not sig "mikenewd8 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum +log(allstationdist2+1)  | log(campdist2+1) + den+I(den^2)  +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "
# DONT USE FRI OK human effects odd  "mikenewd7 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum +allstationdist2+I(allstationdist2^2)+den10x10 | campdist2+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "                      
# keep public land as dummy "mikenewd6 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope  +log(PubLand2+1)   | campdist2+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "
# FRI OK allstation removes explainitory power "mikenewd5 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum | allstationdist2+campdist2+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "
# FRI VERY GOOD ALL GOOD DEN2 remove "mikenewd4 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum | campdist2+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "
#  adding den den^2 almost signif 20 times 
#  adding denixi and nonlinear doesn't add anything in count  
#  adding den3x3 to zero points to similar significance for 3x3 to den 
#  adding den3x3 den3x3^2 doesn't work    
#  adding den10x10 den10x10^2 in count almost works with log(den+.5) in zero

# FRI GOOD ALMOST ALL GOOD BETER LIGHTNIG "mikenewd3 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum+  |Pp30k2+campdist2+ den+I(den^2) +Light5yrWIS2 +aetAave2+aetAave2:Light5yrWIS2  "
# FRI GOOD ALL GOOD!  "mikenewd2 = fire_poly ~cwdAave2+I(cwdAave2^2)+ cwdAsd2+log(aetAave2)+cwdAsd2:log(aetAave2) +Slope +PubLandDum |Pp30k2+ den+I(den^2)   +LightMarc+aetAave2+aetAave2:LightMarc  "
# FRI bad pretty good also "mikenewc4 = fire_poly ~cwdAave2+ aetAave2+I(aetAave2^2)+I(aetAave2^3)+Slope +PubLandDum |log(aetAsd2)+  stationdist2 +airdist2+airdist2:stationdist2 +den+I(den^2)+I(den^3)+LightMarc+aetAave2+aetAave2:LightMarc "
# FRI GOOD pretty good prob with den^2 is + aet nonlinearr, 20 runs doesn't help "mikenewc4 = fire_poly ~cwdAave2+ aetAave2+I(aetAave2^2)+I(aetAave2^3)+Slope +PubLandDum |log(aetAsd2)+  stationdist2 +airdist2+airdist2:stationdist2 +den+I(den^2)+LightMarc+aetAave2+aetAave2:LightMarc "
# FRI BAD ok but aetsd issue den^2 is +, try log "mikenewc3 = fire_poly ~cwdAave2+ aetAave2+I(aetAave2^2)+I(aetAave2^3)+Slope +PubLandDum |aetAsd2+I(aetAsd2^2)+  stationdist2 +airdist2+airdist2:stationdist2 +den+I(den^2)+LightMarc+aetAave2+aetAave2:LightMarc  "
# FRI OK all else good but aet not signif "mikenewc2  = fire_poly ~cwdAave2 + aetAave2+I(aetAave2^2)+I(aetAave2^3)+Slope +PubLandDum |  stationdist2 +airdist2+airdist2:stationdist2 +den+I(den^2)+LightMarc+aetAave2+aetAave2:LightMarc  "
# FRI BAD not bad "mikenewf = fire_poly ~cwdAave2 + aetAave2+I(aetAave2^2)+I(aetAave2^3)+Slope +PubLandDum | den+I(den^2) +LightMarc+aetAave2+aetAave2:LightMarc " 
# FRI OVER Estimated den^2 is + "mikenewe = fire_poly ~cwdAave2+ cwdAave2:aetAave2+I(cwdAave2^2)+ cwdAsd2 +Slope +PubLandDum | stationdist2 +airdist2+den+I(den^2)   +LightMarc+aetAave2+aetAave2:LightMarc  " 
# FRI GOOD weird aet specification Den - ok "mikenewd = fire_poly ~cwdAave2+ cwdAave2:aetAave2+I(cwdAave2^2)+ cwdAsd2 +Slope +PubLandDum |Pp30k2+ den+I(den^2)   +LightMarc+aetAave2+aetAave2:LightMarc  "
# FRI BAD aet and pp30k not significant "mikenewc = fire_poly ~   cwdAave2 + aetAave2  +I(aetAave2^2)+I(aetAave2^3)  +Slope +PubLandDum | Pp30k2+I(Pp30k2^2)+den+I(den^2)   +LightMarc+aetAave2+aetAave2:LightMarc  "  #replace pp30k with fire station distance 
#  intersting  # FRI OK underestimates all sigf but rethink lightning interaction "mikenewb = fire_poly ~  cwdAave2 +I(cwdAave2^2)+(aetAave2)+I(aetAave2^2)+I(aetAave2^3)+Slope |aetAave2+aetAave2:LightMarc +den+I(den^2) +den10x10+I(den10x10^2)  "
# FRI BAD way underestimates all signif, rethink lightning? "mikenewa = fire_poly ~  cwdAave2 +I(cwdAave2^2)+(aetAave2)+I(aetAave2^2)+I(aetAave2^3)+Slope  |aetAave2+aetAave2:LightMarc+den+I(den^2)   "
#  NOT GOOD "mikenewa2 = fire_poly ~  cwdAave2 +I(cwdAave2^2)+(aetAave2)+I(aetAave2^2)+I(aetAave2^3)+Slope  |aetAave2+LightMarc+aetAave2:LightMarc+den+I(den^2)"  



################ START HERE FOR ESTIMATION  ##########################



zeroinflate_it <- function(equations_list,data_in_z,back_cast_data_z, distribution){
  # This function allow us to estiamte the zero infated NB multiple times and store the results
  split_eq = strsplit(equations_list[[1]],"=")
  equation_name <<- trim_white_space( split_eq[[1]][1] )
  equation = as.formula(split_eq[[1]][2])
  
  assign(equation_name, pscl::zeroinfl(  equation, dist=distribution,data= data1976_2000[data1976_2000$train==T,]) )
  #print(summary(get(equation_name)))
  AICER = AIC(get(equation_name))
  print(paste('AIC:',round(AICER,2)))
  
  # compare actual and predicted   (actual-predicted for most graphs)
  listed_out <<- ten_samplev4(equation_in=equation,graph_name='e3f',num_runs=59,data_in=data_in_z,back_cast_data=back_cast_data_z,distribution=distribution, pre_zip=pscl::zeroinfl(equation, dist=distribution,data=data_in_z[data_in_z$train==T,]),extraplots=F  )
  ten_sample_zip <<- listed_out[[1]]
  vcov_list <<- list(listed_out[[2]])
  zip_list <<-  listed_out[[3]]
  #assign( 'ten_sample_zip' ,ten_sample_zip, envir=.GlobalEnv)  #something going wrong here not always assigning
  return(list(ten_sample_zip,vcov_list))       
}
 
   # choose the specification we want to estimate
   equations_list = list( "model5 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)  +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) " 
                          #"model4 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) " 
                         #"model4test = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  + log(Slope2+.5)+den2  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) " 
                       # "model4test2 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  + log(Slope2+.5)+den2  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |   log(elev2+.5) " 
                        
                          # "model3 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2) +cwdAsd2 +log(Slope2+.5) +denmax50x502+I(denmax50x502^2) |  Ppall2+I(Ppall2^2)+ Light5yrWIS2+log(elev2+.5) " 
                          #"model2 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5) +denmax50x502+I(denmax50x502^2) |  Ppall2+I(Ppall2^2)+ Light5yrWIS2+log(elev2+.5) " 
                          #"model1 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  |  Light5yrWIS2+log(elev2+.5) " 
                          # "mikenewe9 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)   +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2) |  Ppall2+I(Ppall2^2) +log(elev2+.5)+log(Pp30k2+.5)   " 
                          # "mikenewe8 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)   |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5)  " ,
                          # "mikenewe7 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5) " 
                          # campdist linear works with denmax50x50
                          # removing elevation doesn't work 
                          # denmax50x502 works too , allstation NO, stationdist NO, allpp NO, campdist2 alomost
                          # removing lightning replace with linear incorp Pp30k2 works (but lightning better out of sample and in sample), log(Pp30k2+.5) is worse
                          # variance: DONT pptAmm pptAmp tmxAsd aetsd cwdAsd ALMOST log(tmxAsd2)
                           #"mikenewe6 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5)" 
                          # Best yet 
                          #"mikenewe5 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2 +Light5yrWIS2+log(elev2+.5)" 
                          # not bad but ligtning and ppall not working. 
                         #"mikenewe4 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2 +Light5yrWIS2  +log(aetAave2+.5) " 
                         # works very well
                          #"mikenewe4 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2 +Light5yrWIS2  +aetAave2+I(aetAave2^2)"
                         # performs well but need to find out how to test joint significance for aet + aet2 in zero component  
                       # "mikenewe3 = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2) +log(Slope+.5)+PubLandDum +denmax20x202+I(denmax20x202^2) |  Ppall2 +Light5yrWIS2 +log(aetAave2+.5)+log(aetAave2+.5):Light5yrWIS2"     
                       # but only works with 10 or 20 samples
      )

    # run zeroinflate and store results
    listed_out2 = lapply(1:length(equations_list),  function(x) zeroinflate_it(equations_list[[x]],data_in_z=data1976_2000,back_cast_data_z=data1951_1975,distribution='negbin') )
    multi_sample_zip = listed_out2[[1]][[1]]
    multi_sample_zip2 = listed_out2[[2]][[1]]
    vcov_list = listed_out2[[1]][[2]]
    AIC( multi_sample_zip)
    outofsamplehist4(equation_in=equations_list[[1]], zip= multi_sample_zip,data_in_z=data1976_2000, years=25)
  #  hitmiss(multi_sample_zip)
     
    
#a =pscl::zeroinfl(fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5), dist=distribution,data=data_in[data_in$group2!=1,])
 
    ##############################################################
    # Chi sqr test 
    # is the first equation statistically different from the second?   http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm   http://en.wikipedia.org/wiki/Likelihood-ratio_test
    # this is for nested model... ie. adding or removing some variables from another equation 
    #model1 =multi_sample_zip    
    #model2 =multi_sample_zip    
    degrees = (length(model2$coefficients$count)+length(model2$coefficients$zero)-2 ) - (length(model1$coefficients$count)+length(model1$coefficients$zero)-2 ) # number of variables in regression 2 - df1 
    #                   Full model                  base model 
    pchisq(2 * (logLik(model2) - logLik(model1)), df = degrees, lower.tail = FALSE)

    #model3 =multi_sample_zip 
    degrees = (length(model3$coefficients$count)+length(model3$coefficients$zero)-2 ) - (length(model2$coefficients$count)+length(model2$coefficients$zero)-2 ) # number of variables in regression 2 - df1 
    #                   Full model                  base model 
    pchisq(2 * (logLik(model3) - logLik(model2)), df = degrees, lower.tail = FALSE)

    #model4 =multi_sample_zip 
    degrees = (length(model4$coefficients$count)+length(model4$coefficients$zero)-2 ) - (length(model3$coefficients$count)+length(model3$coefficients$zero)-2 ) # number of variables in regression 2 - df1 
    #                   Full model                  base model 
    pchisq(2 * (logLik(model4) - logLik(model3)), df = degrees, lower.tail = FALSE)

    #model5 =multi_sample_zip   FOR MODEL 4 & 5, REVERSE ORDER SINCE FULL MODEL IS 4 
    degrees = (length(model4$coefficients$count)+length(model4$coefficients$zero)-2 ) - (length(model5$coefficients$count)+length(model5$coefficients$zero)-2 ) # number of variables in regression 2 - df1 
    #                   Full model                  base model 
    pchisq(2 * (logLik(model4) - logLik(model5)), df = degrees, lower.tail = FALSE)

    # COMPARE MODEL 2 and 4 
    degrees = (length(model4$coefficients$count)+length(model4$coefficients$zero)-2 ) - (length(model2$coefficients$count)+length(model2$coefficients$zero)-2 ) # number of variables in regression 2 - df1 
    #                   Full model                  base model 
    pchisq(2 * (logLik(model4) - logLik(model2)), df = degrees, lower.tail = FALSE)

    # print all log likelihoods
    logLik(model1)

    # test non-linear relations
    multi_nonlinear_test_zeroinfl(variable_name_in = 'den',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'den3x3',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'denmax20x202',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'aetAave2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'cwdAave2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'campdist2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'allstationdist2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'den10x10',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'Pp30k2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'Pp30k2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = c('stationdist2','airdist2','stationdist2:airdist2'),root_in=0,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = c('Light5yrWIS2','log(aetAave2 + 0.5)','Light5yrWIS2:log(aetAave2 + 0.5)'),root_in=0,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = c('cwdAsd2','log(aetAave2)','cwdAsd2:log(aetAave2)'),root_in=0,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'campdist2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'den10x10',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'denmax50x502',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'aetAave2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'Ppall2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'zero')
    multi_nonlinear_test_zeroinfl(variable_name_in = 'cwdAsd2',root_in=2,zip_list_in = zip_list,count_or_zero_in = 'count')

    #nonlinear_test_zeroinfl(variable_name = 'stationdist2',root=2, regression = ten_sample_zip, count_or_zero = 'zero')
    #nonlinear_test_zeroinfl(variable_name = 'Pp30k2',root=2, regression = ten_sample_zip, count_or_zero = 'zero')
 
    # PLOT NON-LINEAR       
    plot_nonlinear_zeroinfl( regression=multi_sample_zip, non_lin_name='Ppall2',interacted=F, count_or_zero='zero',demeaned=T,mean.value = mean(data1976_2000$Ppall) )
    plot_nonlinear_zeroinfl( regression=multi_sample_zip, non_lin_name='cwdAave2',interacted=F, count_or_zero='count',demeaned=T,mean.value = mean(data1976_2000$cwdAave) )
    plot_nonlinear_zeroinfl( regression=multi_sample_zip, non_lin_name='aetAave2',interacted=F, count_or_zero='count' ,demeaned=T,mean.value = mean(data1976_2000$aetAave))
    plot_nonlinear_zeroinfl( regression=multi_sample_zip, non_lin_name='denmax50x502',interacted=F, count_or_zero='count',demeaned=T,mean.value = mean(data1976_2000$denmax50x50) )
    plot_nonlinear_zeroinfl( regression=multi_sample_zip, non_lin_name='denmax20x202',interacted=F, count_or_zero='count',demeaned=T, )

    #save(multi_sample_zip,data1976_2000,data1951_1975,universal_variables, file="C:\\Users\\mmann1123\\Desktop\\CrapFolder\\data.RData")

    # Check to see if dispersion parameter changes over time 
    # http://sites.stat.psu.edu/~ajw13/stat501/SpecialTopics/ComparingModelSlopes.pdf
    eq = fire_poly ~cwdAave2+I(cwdAave2^2)+aetAave2+I(aetAave2^2)  +log(Slope2+.5)  +denmax50x502+I(denmax50x502^2)+campdist2 +PubLandDum  |  Ppall2+I(Ppall2^2) +Light5yrWIS2+log(elev2+.5)  
    current = pscl::zeroinfl(  eq, dist='negbin',data= data1976_2000[data1976_2000$train==T,] )
    past = pscl::zeroinfl(  eq, dist='negbin',data= data1951_1975[data1951_1975$train==T,] )
    curt_theta = coefficients(summary(current))$count['Log(theta)',]
    past_theta = coefficients(summary(past))$count['Log(theta)',]
    z = (3.5275651 - 0.60323602)/sqrt(5.1832531+0.31660087)
    z    
 
# #####  prediction interval backcast 1 #####
# sample_data = backcast_data_z[sample(1:dim(backcast_data_z)[1],5000),]
# foo = predict.zeroinfl.se( multi_sample_zip, sample_data ,se=TRUE,type="response", MC=1000)
# 
# # plot ordered 'response' for sample 
#   windows()
#   indx <- order(foo[[1]])
#   n <- length(indx)
#   plot(1:n, foo[[1]][indx],
#        type="n",
#        ylim=range(cbind(foo[[2]]$lower,foo[[2]]$upper)),
#        xlab="Order Statistic",
#        ylab="Predicted Value")
#   segments(x0=1:n,x1=1:n,
#            y0=foo[[2]]$lower[indx],
#            y1=foo[[2]]$upper[indx],
#            col=gray(.45),
#            lwd=.1)
#   points(1:n,foo[[1]][indx],cex=.5)
#  
# #####  prediction interval forecast 1 #####
# sample_data = data2001_2025_GA2[sample(1:dim(data2001_2025_GA2)[1],5000),]
# foo = predict.zeroinfl.se( multi_sample_zip, sample_data ,se=TRUE,type="response", MC=1000)
# 
# # plot ordered 'response' for sample 
# windows()
# indx <- order(foo[[1]])
# n <- length(indx)
# plot(1:n, foo[[1]][indx],
#      type="n",
#      ylim=range(cbind(foo[[2]]$lower,foo[[2]]$upper)),
#      xlab="Order Statistic",
#      ylab="Predicted Value")
# segments(x0=1:n,x1=1:n,
#          y0=foo[[2]]$lower[indx],
#          y1=foo[[2]]$upper[indx],
#          col=gray(.45),
#          lwd=.1)
# points(1:n,foo[[1]][indx],cex=.5)


# foo2 =  data.frame(pred=unlist(foo[[1]]))
#  
# foo2$ucl <-  foo[[1]]  + 1.96 *  foo[[2]]$se 
# foo2$lcl <- foo[[1]] - 1.96 * foo[[2]]$se
# foo2 = foo2[order(foo[[1]]),]
# foo2$se = foo[[2]]$se
# foo2$indx = 1:length(foo2$ucl)
# 
# qplot(indx, pred, data=foo2 ) +
#   geom_smooth(aes(ymin = lcl, ymax = ucl), data=foo2, stat="identity")
# 
# c <- ggplot(foo2, aes(indx, pred))
# c + stat_smooth()




  # this code used to create figures used in the fire paper. 
  ##############################################################
  # backcast plots hold natural and human variables at mean
  ## choose ##
  backcast_period = "1951_1975"    # use long form name example: "1941_1970"
  estimation_period = "1976_2000"  # period used in ZIP model above 
  sample_zip = multi_sample_zip   # choose which estimated regression to use.  to use second put 2
  
  ## run ##
  # get data
  backcast_data_z= get(paste("data",backcast_period,sep=""),envir=.GlobalEnv)
  current_data_z = get(paste("data",estimation_period,sep=""),envir=.GlobalEnv)
    
  #predict using mean coefficients from zeroinflate_it
  #backcast fire  probability of no fire
  backcast = predict(sample_zip, backcast_data_z, type="prob") #CHECK THIS-THIS IS PREDICTION
  backcast = backcast[,1]                          # prob of zero fires
  raster_example = raster(universal_variables[1])  # example raster to use in rasterize
  pts = backcast_data_z[,c('x','y')]              # points that match backcast_data 
  ptscurrent = current_data_z[,c('x','y')]              # points that match backcast_data 
  
        # plot all variables used in regression
  #       for(namer in c('cwdAave2','aetAave2','Slope2','denmax50x502','campdist2','PubLandDum')){
  #         test <- rasterize(pts,raster_example ,data1976_2000[,paste(namer)],fun=sum)
  #         windows()
  #         plot(test, main=paste(namer))
  #       }
  #         

      # fire polygons
      
      fire <- rasterize(pts,raster_example ,data1976_2000$fire_poly,fun=sum)
      windows()
      plot(fire, main="Actual Fire Count",zlim=c(0,4))
 
      fire_back <- rasterize(pts,raster_example ,data1951_1975$fire_poly,fun=sum)
      windows()
      plot(fire+fire_back, main='Total Fire Count 1951-2000')

      val <- getValues((fire+fire_back))
      xy <- as.data.frame(coordinates(fire+fire_back))
      xy <- cbind(xy,val)
      windows()
      ggplot(na.omit(xy), aes(x=x, y=y, fill=factor(val)))+ geom_raster()+ scale_fill_manual(name='Fire Count',values=rev(terrain.colors(10)))+xlab('')+ylab('')+ theme(panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "grey"),panel.grid.minor = element_line(colour = "grey90"))
      
      windows()
      ggplot(na.omit(xy), aes(x=x, y=y, fill=factor(val)))+ geom_raster()+ scale_fill_manual(name='Fire Count',values=rev(terrain.colors(10)))+xlab('')+ylab('')+ 
          theme(panel.background = element_rect(fill = "grey85"),panel.grid.major = element_line(colour = "White"),panel.grid.minor = element_line(colour = "white"))

  
      # then
      backcast_prob_no_fire <- rasterize(pts,raster_example ,backcast,fun=sum)
      backcast_prob_atleastone_fire <<- 1-backcast_prob_no_fire
      windows()
      plot(backcast_prob_atleastone_fire, main="probability of 1 fire backcast")
      
      #current fire  probability of no fire
      current_prob_fire = predict(sample_zip, current_data_z, type="prob")
      current_prob_fire = current_prob_fire[,1]
      current_prob_no_fire <- rasterize(ptscurrent,raster_example ,current_prob_fire,fun=sum)
      current_prob_atleastone_fire <<- 1-current_prob_no_fire
      windows()
      plot(current_prob_atleastone_fire, main="current_prob_atleastone_fire")
      writeRaster(current_prob_atleastone_fire, filename="G://Faculty//Mann//Share/Fire/Fire_All_Script_Outputs\\fire_prob_7599.tif", format="GTiff", overwrite=TRUE)
      # current fire count
      current_resp_fire <- predict(sample_zip,current_data_z, type="response")
      current_resp_fire <<-rasterize(ptscurrent,raster_example,current_resp_fire,fun=sum)  # raster of human influence
      writeRaster(current_resp_fire, filename="G://Faculty//Mann//Share/Fire/Fire_All_Script_Outputs\\fire_count_7600.tif", format="GTiff", overwrite=TRUE)
      windows()
      plot(current_resp_fire, main="current response")
      # backcast fire count
      backcast_resp_fire <- predict(sample_zip,backcast_data_z, type="response")
      backcast_resp_fire <<-rasterize(pts,raster_example,backcast_resp_fire,fun=sum)  # raster of human influence
      windows()
      plot(backcast_resp_fire, main="Backcast response")
      
      # difference in coutn
      windows()
      plot(current_resp_fire-backcast_resp_fire, main= 'Difference in count back and now')
  
      # mean FRI
      windows()
      library(fields)
      image.plot(25/current_resp_fire, main="current Mean Fire return", zlim=c(0,250) )
      mri = 25/current_resp_fire
  
      
    
      #past FRI
      windows()
      image.plot(25/backcast_resp_fire, main="paste Mean Fire return", zlim=c(0,250) )

      #writeRaster(mri, filename="C:/Users/mmann1123/Desktop/Share/Fire/Fire_All_Script_Outputs\\fire_FRI_7599.tif", format="GTiff", overwrite=TRUE)
  
      # summary
      print(paste('Estimating:',round(cellStats(current_resp_fire,stat=sum)),'cells burned out of:',sum(current_data_z$fire_poly),'actual in',estimation_period,sep=" "))
      print(paste('Estimating:',round(cellStats(backcast_resp_fire,stat=sum)),'cells burned out of:',sum(backcast_data_z$fire_poly),'actual in',backcast_period,sep=" "))
  

##############################################################
# forecast plots 
## choose ##

  yearsinperiod = 25
  density_vars = c('denmax50x502', 'Rdenmax50x502','Udenmax50x502')   # first density variable must be the variable to be replaced (ie variable used in regression)
  
  # designate which periods are 'future scenarios' and model scenario abreviations
  fut_decade_group_names =  c('2001_2025','2026_2050')
  fut_alt_decade_group_names = c('0125','2650')
  model_abrev = c("G","P") #, "P"
  scenario_abrev = c("A2" )
  estimation_period = "1976_2000"  # period used in ZIP model above 

  
  future_scenario = do.call(paste, c(expand.grid(fut_decade_group_names,paste(model_abrev,scenario_abrev,sep='') ),sep="_"))
  
  raster_example = raster(get(paste('alldata',future_scenario[1],sep=''),envir=.GlobalEnv),1)  # example raster to use in rasterize
  pts = get(paste('data',estimation_period,sep=''), envir=.GlobalEnv)[,c('x','y')]  # points that match  

  
  ####################################################
  # Create all basic plots for estimation period
    
  # base year prob of at least one fire
  probs_base = predict(multi_sample_zip,get(paste('data',estimation_period,sep=''),envir=.GlobalEnv), type='prob' )
  probs_base_atleastone_fire <- 1- rasterize(pts,raster_example,probs_base[,1],fun=sum)
  
  #base year count 
  count_base = predict(multi_sample_zip,get(paste('data',estimation_period,sep=''),envir=.GlobalEnv), type='response' )
  count_base = rasterize(pts,raster_example,count_base,fun=sum)
  
  # base year MFRI 
  mri_base = yearsinperiod/count_base
  windows()
  image.plot(mri_base, main="Current Mean Fire Return", zlim=c(50,500),col = rev(tim.colors(10))  )
  mri_base_hold = mri_base    
  mri_base_hold[mri_base_hold<500]=NA
  plot(mri_base_hold,col='#08088A',add=T, legend=F)  
  mri_base_hold[mri_base_hold>50]=NA
  plot(mri_base_hold,col="#800000",add=T, legend=F)  

  val <- getValues(mri_base)
  xy <- as.data.frame(coordinates(mri_base))
  xy <- cbind(xy,val)
  windows()
  ggplot(na.omit(xy), aes(x=x, y=y, fill=val))+ geom_raster() + scale_fill_gradient2(low='red',  mid="yellow",  high="dark green", limits=c(0,500),midpoint=250,name="MFRI")+ coord_equal()+xlab('')+ylab('') 
 
  windows()
  ggplot(na.omit(xy), aes(x=x, y=y, fill=val))+ geom_raster(data=subset(na.omit(xy),val<500))+ scale_fill_gradient2(low='red',  mid="yellow",  high="dark green", limits=c(min(xy$val,na.rm=T),500),midpoint=250,name="MFRI") +
  geom_raster(data=subset(na.omit(xy),val>=500),fill='dark green') + coord_equal()+xlab('')+ylab('') 

  # change from past  MFRI 
  mri_past = yearsinperiod/backcast_resp_fire
  mfri_change = mri_base-mri_past 
  windows()
  plot(mfri_change, main=paste('MFRI: Change from Past to Current'), zlim=c(-150,150),col=rev(topo.colors(6)) )  
 
  low=mfri_change
  low[low>-150]=NA
  plot(low,col='#FFBF00',add=T, legend=F)
  high=mfri_change
  high[high<150]=NA
  plot(high,col='#08088A',add=T, legend=F)  

  val <- getValues(mfri_change)
  xy <- as.data.frame(coordinates(mri_base))
  xy <- cbind(xy,val)
  
  xy3=  na.omit(xy) 
  xy3$val = factor(cut(xy3$val,breaks=c(-Inf,-150,-100,-50,0,50,100,150,Inf)),levels=levels(cut(xy3$val,breaks=c(-Inf,-150,-100,-50,0,50,100,150,Inf))),ordered = T ,labels = c( '--150',"-150,-100","-100,-50","-50,0","0,50","50,100","100,150","150+"   ))
  windows()
  ggplot(xy3, aes(x=x, y=y, fill=val))+ geom_raster()+scale_fill_manual(values=c('#AB241D','#D73027','#E37D52','#E3C652','#B1C765','#6FB03C','#1A9850','#13783E')  ,name=  'CHG MFRI' )+coord_equal()+xlab('')+ylab('') 
  windows()
  plot(mfri_change,zlim=c(-150,150))
#   windows()
#   ggplot(na.omit(xy), aes(x=x, y=y, fill=val))+ geom_raster(data=subset(na.omit(xy),-150<val&val<150))+ scale_fill_gradient2(low='red',  mid="yellow",  high="dark green", limits=c(-150,150),midpoint=0,name="MFRI") +
#     geom_raster(data=subset(na.omit(xy),val>=150),fill='dark green') + 
#      geom_raster(data=subset(na.omit(xy),val<=-150),fill= '#BD1313') +coord_equal()+xlab('')+ylab('') 
  
#   xy2= subset(na.omit(xy),val>-150 &val<150)
#   xy2$val = factor(cut(xy2$val,breaks=6),levels=rev(levels(factor(cut(xy2$val,breaks=6)))),ordered = T ,labels = c( "(-)-150,-100","-100,-50","-50,0","0,50","50,100","100,150(+)"   ))
#   windows()
#   ggplot(xy2, aes(x=x, y=y, fill=val))+ geom_raster()+ scale_fill_manual(values=c('#D73027','#E37D52','#E3C652','#B1C765','#6FB03C','#1A9850')  ,name="MFRI")+ 
#   geom_raster(data=subset(na.omit(xy),val>=150),fill='dark green') + 
#   geom_raster(data=subset(na.omit(xy),val<=-150),fill= '#BD1313') +coord_equal()+xlab('')+ylab('') 
#  
                                                                      
 

  
  require(gridExtra)
  require(fields)
  
  #  GFDL wet in southern deserts, PCM wet in north coast and central valley 
  windows()  
  plot(raster(alldata2026_2050_PA2,"aetAave" )-raster(alldata2001_2025_PA2,"aetAave" ),main='PA2 aet change')
  windows()  
  plot(raster(alldata2026_2050_GA2,"aetAave" )-raster(alldata2001_2025_GA2,"aetAave" ),main='GA2 aet change')

  #export response for past, future scenarios and housing scenarios
    # DONE  for(data_name in c( backcast_period,estimation_period)){
    #    #past
    #     temp_data = get(paste('data',data_name,sep=''),envir=.GlobalEnv)
    #     count_temp = predict(multi_sample_zip,temp_data, type='response' )
    #     count_temp = rasterize(ptscurrent,raster_example,count_temp,fun=sum)   
    #     writeRaster(count_temp,paste("G://Faculty//Mann//Share/Fire/Fire_All_Script_Outputs\\Firecount",data_name,".tif",sep=''), format="GTiff", overwrite=TRUE)
    #   }       
    #   for(housing in density_vars){
    #       for(data_name in c( future_scenario)){
    #         print(paste('working on ',data_name, housing))
    #         # prob of at least one fire
    #         temp_data = get(paste('data',data_name,sep=''),envir=.GlobalEnv)
    #         temp_data[,paste(density_vars[1])] = temp_data[,paste(housing)] # replace housing data 
    #         count_temp = predict(multi_sample_zip,temp_data, type='response' )
    #         count_temp = rasterize(ptscurrent,raster_example,count_temp,fun=sum)   
    #         writeRaster(count_temp,paste("G://Faculty//Mann//Share/Fire/Fire_All_Script_Outputs\\Firecount",data_name,housing,".tif",sep=''), format="GTiff", overwrite=TRUE)
    #       }         
    #   }


# plot MFRI for Future scenarios
for(data_name in future_scenario){
  # count 
  count_fut = predict(multi_sample_zip,get(paste('data',data_name,sep=''),envir=.GlobalEnv), type='response' )
  count_fut <- rasterize(pts,raster_example,count_fut,fun=sum)
  count_fut2 = predict(multi_sample_zip,get(paste('data',data_name,sep=''),envir=.GlobalEnv), type='response' )
  count_fut2 <- rasterize(pts,raster_example,count_fut2,fun=sum)
  test = count_fut2 - count_fut
  
  # mfri 
  mri_fut = yearsinperiod/count_fut
  assign(paste('mfri_fut_',data_name,sep=''),mri_fut ) #assign them for use later on mfri_fut_2026_2050_PA2
#   windows()
#   image.plot(mri_fut,main=paste("MFRI",data_name ), zlim=c(0,500),col = rev(tim.colors(10))  ) 
#   mri_fut_hold = mri_fut    
#   mri_fut_hold[mri_fut_hold<500]=NA
#   plot(mri_fut_hold,col='#08088A',add=T, legend=F)  
   
  val = getValues(mri_fut)
  xy = as.data.frame(coordinates(mri_base))
  xy = cbind(xy,val)
  xy3 =  na.omit(xy) 
  windows()
  a=ggplot(xy3, aes(x=x, y=y, fill=val))+ geom_raster()+scale_fill_gradient2(low='red',mid='yellow',high='dark green',name='MFRI',limits=c(0,500),midpoint=250)+
    coord_equal()+xlab('')+ylab('') +ggtitle(paste(data_name,'MFRI'))    
  plot(a)
}
windows()
image.plot(mfri_fut_2026_2050_PA2 - mfri_fut_2026_2050_GA2,zlim=c(-500,500))


# plot change MFRI for Future scenarios
  for(data_name in future_scenario){
    # prob of at least one fire
    # probs_fut = predict(multi_sample_zip,get(paste('data',data_name,sep=''),envir=.GlobalEnv), type='prob' )
    # prob_atleastone_fire <- 1- rasterize(pts,raster_example,probs_fut[,1],fun=sum)
    # assign(paste('prob1pls_fut_',data_name,sep=''),prob_atleastone_fire ) #assign them for use later on mfri_fut_2026_2050_PA2
         
    # count 
    count_fut = predict(multi_sample_zip,get(paste('data',data_name,sep=''),envir=.GlobalEnv), type='response' )
    count_fut <- rasterize(pts,raster_example,count_fut,fun=sum)
  
    # summary
    print('not sure if this is best way')
    print(paste('Estimating:',round(cellStats(mri_base,stat=sum)),'cells burned in:',estimation_period,sep=" "))
    print(paste('Estimating:',round(cellStats(count_fut,stat=sum)),'cells burned in:',data_name,sep=" "))
    
    # mfri 
    mri_fut = yearsinperiod/count_fut
    assign(paste('mfri_fut_',data_name,sep=''),mri_fut ) #assign them for use later on mfri_fut_2026_2050_PA2
    assign(paste('DIFMFRI',data_name,sep='_'),value=mri_fut-mri_base)  
    
#     windows()
#     image.plot(mri_fut,main=paste("MFRI",data_name ), zlim=c(0,500),col = rev(tim.colors(10))  ) 
#      mri_fut_hold = mri_fut    
#      mri_fut_hold[mri_fut_hold<500]=NA
#      plot(mri_fut_hold,col='#08088A',add=T, legend=F)  
      
#        
#     windows(width=30,height=10)
#     par(mfrow=c(1,3)) 
#     a = plot(mri_fut, main=paste('MFRI:',data_name), zlim=c(0,250) )
#         mri_fut2 = mri_fut     
#         mri_fut2[mri_fut2<250]=NA
#         plot(mri_fut2,col='#0B610B',add=T, legend=F)     
#     b = plot(mri_base, main=paste('MFRI:',estimation_period), zlim=c(0,250))
#          mri_base2 = mri_base     
#          mri_base2[mri_base2<250]=NA
#          plot(mri_base2,col='#0B610B',add=T, legend=F)   
#     c = plot(mri_fut-mri_base, main=paste('MFRI:',data_name,'-',estimation_period), zlim=c(-150,150),col=rev(topo.colors(6)) )  
#          mri_basedif = mri_fut-mri_base     
#          mri_basedif[mri_basedif>-150]=NA
#          plot(mri_basedif,col='#FFBF00',add=T, legend=F)  
#          mri_basedif = mri_fut-mri_base     
#          mri_basedif[mri_basedif<150]=NA
#          plot(mri_basedif,col='#08088A',add=T, legend=F)  
#     #
#     windows()
#      c = plot(mri_fut-mri_base, main=paste('MFRI:',data_name,'-',estimation_period), zlim=c(-150,150),col=rev(topo.colors(6)) )  
#      mri_basedif = mri_fut-mri_base     
#      mri_basedif[mri_basedif>-150]=NA
#      plot(mri_basedif,col='#FFBF00',add=T, legend=F)  
#      mri_basedif = mri_fut-mri_base     
#      mri_basedif[mri_basedif<150]=NA
#      plot(mri_basedif,col='#08088A',add=T, legend=F)  
#      remove(mri_fut, count_fut)
#      

    
     val <- getValues(mri_fut-mri_base)
     xy <- as.data.frame(coordinates(mri_base))
     xy <- cbind(xy,val)
     
     xy3=  na.omit(xy) 
     xy3$val = factor(cut(xy3$val,breaks=c(-Inf,-150,-100,-50,0,50,100,150,Inf)),levels=levels(cut(xy3$val,breaks=c(-Inf,-150,-100,-50,0,50,100,150,Inf))),ordered = T ,labels = c( '--150',"-150,-100","-100,-50","-50,0","0,50","50,100","100,150","150+"   ))
     windows()
     a=ggplot(xy3, aes(x=x, y=y, fill=val))+ geom_raster()+scale_fill_manual(values=c('#AB241D','#D73027','#E37D52','#E3C652','#B1C765','#6FB03C','#1A9850','#13783E')  ,name=  'CHG MFRI' )+
       coord_equal()+xlab('')+ylab('') +ggtitle(paste(data_name,'Change'))    
     plot(a)
     windows()
     plot(mri_fut-mri_base,zlim=c(-150,150),main=paste(data_name,'Change'))
  }

  ###############################################
  # prediction agreement 
  for(scen in c('GA2', 'PA2')){
    # create stacks with 1 for +MFRI and -1 for -MFRI for each scenario
    # they need to be stacked in chronological order 
    print(paste('working on',scen))
    lister = ls()[grep('DIFMFRI',ls())]
    chnagestacker = list()
    for(araster in lister[grep(scen,lister)] ){
      print(paste('stacking difference of',araster ))
      chnagestacker = c(chnagestacker,(get(araster))) 
    }
    print(paste('logical of',lister[grep(scen,lister)] ))
    for(i in 1:length(chnagestacker)){  
      chnagestacker[[i]][chnagestacker[[i]]>0]= 1
      chnagestacker[[i]][chnagestacker[[i]]<0]=-1
    } 
    assign(paste('MFRILOGICSTACK',scen,sep=''),chnagestacker)
  }
  
#   # agree + MFRI
#   windows()
#   plot(MFRILOGICSTACKPA2[[1]]==MFRILOGICSTACKGA2[[1]], main = 'model agreement 2001 2025')
#   windows()
#   plot(MFRILOGICSTACKPA2[[2]]==MFRILOGICSTACKGA2[[2]], main = 'model agreement 2026 2050')
#   windows()
#   plot(MFRILOGICSTACKPA2[[1]]==1 & MFRILOGICSTACKGA2[[1]]==1,col=c( 'white','red'), main = '+- model agreement 2001 2025')
#   plot( (MFRILOGICSTACKPA2[[1]]==-1 & MFRILOGICSTACKGA2[[1]]==-1),col=c('white' ,'blue' ), main = '+- model agreement 2001 2025', add=T)
#   windows()
# 
    hold_raster = MFRILOGICSTACKPA2[[2]]==1 & MFRILOGICSTACKGA2[[2]]==1
#   plot(hold_raster, main = '+ model agreement 2026 2050')
#   windows()
   hold_raster[MFRILOGICSTACKPA2[[2]]==-1 & MFRILOGICSTACKGA2[[2]]==-1]=-1
#   plot(factor(hold_raster), main = '- model agreement 2026 2050')

  val <- getValues(hold_raster)
  xy <- as.data.frame(coordinates(hold_raster))
  xy <- cbind(xy,val)
  xy$val = factor(xy$val, levels=c(1,0,-1), labels = c('+ MFRI','None','- MFRI'))
  windows()
  ggplot(na.omit(xy), aes(x=x, y=y, fill=factor(val))) + scale_fill_manual(values = c("#31B404","grey","#DF013A"),name="Agreement") + geom_raster()  + coord_equal()+xlab('')+ylab('')
  ######
  hold_raster2 = MFRILOGICSTACKPA2[[1]]==1 & MFRILOGICSTACKGA2[[1]]==1
  #plot( hold_raster2, main = 'model agreement 2001 2025')
  #windows()
  hold_raster2[MFRILOGICSTACKPA2[[1]]==-1 & MFRILOGICSTACKGA2[[1]]==-1]=-1
  #plot( ( hold_raster2), main = 'model agreement 2001 2025')
  
  val <- getValues( hold_raster2)
  xy <- as.data.frame(coordinates( hold_raster2))
  xy <- cbind(xy,val)
  xy$val = factor(xy$val, levels=c(1,0,-1), labels = c('+ MFRI','None','- MFRI'))
  windows()
  ggplot(na.omit(xy), aes(x=x, y=y, fill=factor(val))) + scale_fill_manual(values = c("#31B404","grey","#DF013A"),name="Agreement") + geom_raster()  + coord_equal()+xlab('')+ylab('')

  ####################################################
  # for each period compare MFRI of scenarios 
  windows(width=15,height=10)
  par(mfrow=c(1,length(fut_decade_group_names_in)))
  for(fut_decade in fut_decade_group_names_in){
    period_scenarios = future_scenario[grep(fut_decade,future_scenario)]
    plot(get(paste('mfri_fut_',period_scenarios[1],sep=''))-get(paste('mfri_fut_',period_scenarios[2],sep='')),zlim=c(-150,150),col=topo.colors(50),main=paste('mfri_fut_',period_scenarios[1],' - mfri_fut_',period_scenarios[2],sep=''))   
  }
  
  ####################################################
  # plot out the histogram of 1, 2,3,4,5 burned cells
  countholder = as.numeric()
  scenarioholder = as.character()  
  for(data_name in c('1951_1975',estimation_period,future_scenario)){
    print(paste('working on ',data_name))
    # prob of at least one fire
    probs_fut = predict(multi_sample_zip,get(paste('data',data_name,sep=''),envir=.GlobalEnv), type='prob' )
    dataforfirehist = round(colSums(probs_fut[,2:dim(probs_fut)[2]],na.rm=T))
    for(j in 1:length(dataforfirehist)){
      countholder=c(countholder,rep(x=j,dataforfirehist[j]))
      scenarioholder =c(scenarioholder,rep(x=data_name,dataforfirehist[j]))                           
    } 
    
  }  
  histdataframe  =  data.frame(FireCount=countholder, Scenario=scenarioholder)
  windows(width=15,height=10)
  ggplot(histdataframe, aes(x=countholder, fill=Scenario)) + geom_histogram(binwidth=1,  position="dodge") +ylab('Pixels Burned')+ xlim(.9,4) +xlab('Fire Event Count')    

  # plot in Km2 
  aghistdataframe=aggregate(FireCount~Scenario,data=histdataframe,sum)
  aghistdataframe$year = c('1951_1975','1976_2000','2001_2025','2001_2025','2026_2050','2026_2050')
  aghistdataframe$FireCount = aghistdataframe$FireCount*1.1664   # convert 1080x1080m to 1kmx1km
  # calc % change relative to 1975-2000
  pcm = aghistdataframe[c(1,2,4,6),]
  for (j in 3:(length(pcm)+1)){print((pcm$FireCount[j]-pcm$FireCount[pcm$Scenario =='1976_2000'])/pcm$FireCount[pcm$Scenario =='1976_2000'])}
  gfdl = aghistdataframe[c(1,2,3,5),]
  for (j in 3:(length(gfdl)+1)){print((gfdl$FireCount[j]-gfdl$FireCount[gfdl$Scenario =='1976_2000'])/gfdl$FireCount[gfdl$Scenario =='1976_2000'])}


  levels(aghistdataframe$Scenario) =   c('HIST_EST','HIST_EST',  'GFDL_A2','PCM_A2',  'GFDL_A2','PCM_A2' ) 
  aghistdataframe$Scenario = factor(aghistdataframe$Scenario,levels=c('GFDL_A2','PCM_A2'),ordered=T)

  windows()
  ggplot(aghistdataframe, aes(y=FireCount,x=year, fill=Scenario)) + geom_bar(binwidth=1,  position="dodge",stat='identity')+coord_cartesian(ylim=c(25000,31000))+
     ylab(expression(paste('Burned Area km'^'2'))) +xlab('Period')
  
  # double check these values 
  fire_perm2 = fire_perm
  fire_perm2$year = as.numeric(as.character(fire_perm$YEAR_) )
  fire_perm2=fire_perm2[fire_perm2$year>=1976&fire_perm2$year<=2000&!is.na(fire_perm2$year),]
  sum(fire_perm2$GIS_ACRES)
  #7959881 acres or 32,212.49 km2  model underpredicting 
 
  fire_perm2=fire_perm2[fire_perm2$year>=1950&fire_perm2$year<1976&!is.na(fire_perm2$year),]
  sum(fire_perm2$GIS_ACRES) # 6410383 acres   25,941.9 km2  slight over prediction
  


  ####################################################
  # plot out the histogram of 1, 2,3,4,5 burned cells FOR DIFFERNT HOUSING SCENARIOS
  countholder2 = as.numeric()
  scenarioholder = as.character()
  
  for(housing in density_vars){
    for(data_name in c( future_scenario)){
      print(paste('working on ',data_name, housing))
      # prob of at least one fire
      temp_data = get(paste('data',data_name,sep=''),envir=.GlobalEnv)
      temp_data[,paste(density_vars[1])] = temp_data[,paste(housing)] # replace data 
      probs_fut = predict(multi_sample_zip,temp_data, type='prob' )
      dataforfirehist = round(colSums(probs_fut[,2:dim(probs_fut)[2]],na.rm=T))
      print(dataforfirehist)
      for(j in 1:length(dataforfirehist)){
        countholder2=c(countholder2,rep(x=j,dataforfirehist[j]))
        scenarioholder =c(scenarioholder,rep(x=paste(data_name,housing,sep='_'),dataforfirehist[j]))                           
      } 
    }    
  }

  histdataframe  =  data.frame(countholder2=countholder2, scenarioholder=scenarioholder)
  windows(width=15,height=10)
  ggplot(histdataframe, aes(x=countholder2, fill=scenarioholder)) + geom_histogram(binwidth=1,  position="dodge") +xlim(.9,4)  


  ####################################################
  # plot out the histogram of 1, 2,3,4,5 burned cells FOR DIFFERNT HOUSING SCENARIOS 
  # & CURRENT CLIMATE
  countholder2 = as.numeric()
  scenarioholder = as.character()
  
  for(housing in density_vars){
    for(data_name in c( future_scenario[2:2])){
      print(paste('working on ',data_name, housing))
      # prob of at least one fire
      temp_data = get(paste('data',estimation_period,sep=''),envir=.GlobalEnv) # get current climate
      fut_data = get(paste('data',data_name,sep=''),envir=.GlobalEnv) 
      temp_data[,paste(density_vars[1])] = fut_data[,paste(housing)] # replace data 
      probs_fut = predict(multi_sample_zip,temp_data, type='prob' )
      
      # get change in MFRI
      probs_cnt = yearsinperiod/predict(multi_sample_zip,temp_data, type='response' )
      probs_cnt = rasterize(pts,raster_example,probs_cnt,fun=sum)-yearsinperiod/count_base
      probs_cnt[probs_cnt==0]=NA
      
      windows()
      plot(probs_cnt, main=paste("MFRI",data_name,housing),col=rev(topo.colors(50)),zlim=c(-350,350))
      probs_cnt_hold = probs_cnt
      probs_cnt_hold[probs_cnt_hold>-350]=NA
      plot(probs_cnt_hold,col='#FFBF00',add=T, legend=F) 
      probs_cnt_hold = probs_cnt
      probs_cnt_hold[probs_cnt_hold<150]=NA
      plot(probs_cnt_hold,col='#08088A',add=T, legend=F) 
      
      dataforfirehist = round(colSums(probs_fut[,2:dim(probs_fut)[2]],na.rm=T))
      print(dataforfirehist)
      for(j in 1:length(dataforfirehist)){
        countholder2=c(countholder2,rep(x=j,dataforfirehist[j]))
        scenarioholder =c(scenarioholder,rep(x=paste(data_name,housing,sep='_'),dataforfirehist[j]))                           
      } 
    }    
  }
  
  histdataframe  =  data.frame(countholder2=countholder2, scenarioholder=scenarioholder)
  windows(width=15,height=10)
  ggplot(histdataframe, aes(x=countholder2, fill=scenarioholder)) + geom_histogram(binwidth=1,  position="dodge") +xlim(.9,4)  


##############################################################
#  create MFRI from actual a data
#  historgram by region and vegetation class
  grep('fire',names(data1976_2000))
  firecount7600 = rasterize(pts,raster_example,data1976_2000$fire_poly,fun=sum) 
  firecount5175 = rasterize(pts,raster_example,data1951_1975$fire_poly,fun=sum) 
  
  GAP = raster('G:/Faculty/Mann/Share/GAP Data/GAP_Raster.tif') 
  windows()
  plot(GAP)

  MFRI_actual = 50/(firecount7600 + firecount5175)
  plot(MFRI_actual)
  #writeRaster(MFRI_actual, 'G:/Faculty/Mann/Share/Fire/MFRI_explore/MFRI_Actual.tif')
  #export to mapbox
  mri = mri_base
  mri2 = mri_base
  mri2[mri2>400]=400
  EPSG <- make_EPSG()
  subset(EPSG, code==4326)
  projectRaster(mri2,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs', res=0.05,  method='bilinear',filename='G:/Faculty/Mann/Share/Fire/MFRI_explore/MFRI_nopublic.tif',overwrite=T,format="GTiff")
  a = raster('G:/Faculty/Mann/Share/Fire/MFRI_explore/MFRI.tif')
  #writeRaster(mri2,'G:/Faculty/Mann/Share/Fire/MFRIunproj.tif')

  # histogram of MFRI
  data = data.frame(MFRI= (getValues(mri)),GAP= (getValues(GAP)), Jepson = getValues(raster(alldata1976_2000,'Jepson')) )  
  data = na.omit(data)
  data$Jepson = factor(data$Jepson,labels=c('Cascade','Central W','E of Sierra','C Valley','Modoc','Mojave D','NWest','Sierra','Sonoran D','SWest'))
  labels = data.frame(WHR1=attributes(GAP)$data@attributes[[1]]$WHR1)
  labels$sort = 1:length(labels$WHR1) 
  WHRTYPES=read.dbf('G:/Faculty/Mann/Share/GAP Data/WHRTYPES.dbf')
  labels=merge(labels, WHRTYPES, by ='WHR1')
  labels = labels[order(labels$sort),]
 
  data$GAP = factor(data$GAP,labels=c(paste(labels$WHRTYPE)))
  
  # calculate modes
  aggregate(MFRI~GAP,data,FUN=median)[order(aggregate(MFRI~GAP,data,FUN=median)$MFRI),]




  data$MFRI[data$MFRI>600]=NA
  windows()
  ggplot(data, aes(x=MFRI))+ geom_histogram(binwidth = 25)+ coord_cartesian(xlim = c(0, 500),ylim=c(0,25000))+ggtitle('MFRI') 
  windows()
  ggplot(data, aes(x=MFRI,fill=factor(Jepson)))+ geom_bar(binwidth = 25,position="dodge")+ coord_cartesian(xlim = c(0, 400),ylim=c(0,5000))+ggtitle('MFRI') 
  windows()
  ggplot(data, aes(x=MFRI))+ geom_bar(binwidth = 25)+ coord_cartesian(xlim = c(0, 500),ylim=c(0,3000))+ggtitle('MFRI') +facet_wrap( ~ GAP)
  

  windows()
  ggplot(data, aes(x=MFRI))+ geom_histogram(aes(y = ..density..),binwidth = 25)+ coord_cartesian(xlim = c(1, 500),ylim=c(0,.01)) +ylab('Density') +facet_wrap( ~ GAP,ncol=6)+ theme(strip.text.x = element_text(size = 7,  angle = 0), axis.text.x = element_text(angle=-45, vjust=1,hjust=.25), axis.text.y = element_text(size=8))
 
 
#   ####################################################
#   # plot out the SD in prob 1+ fires FOR DIFFERNT HOUSING SCENARIOS
#   for(data_name in c( future_scenario)){
#     windows(width=10,height=10)
#     #par(mfrow=c(1,3))
#     prob_stack = stack()
#     plot_holder = list()
#     for(housing in density_vars){
#       print(paste('working on ',data_name, housing))
#       
#       # prob of at least one fire
#       temp_data = get(paste('data',data_name,sep=''),envir=.GlobalEnv)
#       temp_data[,paste(density_vars[1])] = temp_data[,paste(housing)]
#       
#       probs_t1 = predict(multi_sample_zip,temp_data , type='prob' )
#       probs_t1 <- 1- rasterize(pts,raster_example,probs_t1[,1],fun=sum)
#       plot_holder = c(plot_holder,probs_t1)
#     }
#     prob_stack =  raster::stack(plot_holder)
#     stack_mean = mean(prob_stack)
#     diff_list = list()
#     for(i in 1:dim(prob_stack)[3]){
#       diff2 = (raster(prob_stack,i) - stack_mean)^2
#       diff_list=c(diff_list,diff2)
#     } 
#     sd = sum(stack(diff_list))
#     plot(sd, main = paste('SD',data_name) )
#   }
#    
   
  
##############################################################
# Human vs natural influence 
# get mean values for backcast

    human_var_names_to_search = c("den",'den3x3','den5x5',"den10x10","denmax20x202","denmax50x502"  ,"NPark","PubLand","AllRoads","Incorp","PrimSec","PrimSec2","Pp30k","Pp20k","Ppall","Ppall2", 'campdist','campdist2','allstationdist2',"stationdist","stationdist2","airdist","airdist2")     # not using grep, be explicit 
    public_land = c( 'PubLandDum') #c("NPark","PubLand", )
    human_wo_public_var_names_to_search = human_var_names_to_search %w/o% public_land
    factors=c() #"HydroReg"
    sample_zip = multi_sample_zip

    # get mean values for current
    current_mn =  as.data.frame( t(colMeans(current_data_z, na.rm=T) ) )
    current_names = names(current_mn)
    rowsc = dim(current_data_z)[1]
    current_mn = matrix(rep(as.numeric( current_mn),rowsc),rowsc,byrow=T )  # createdataframe of all column means while matching dim of input data
    colnames(current_mn) = current_names
 
    human_influence_loc_current = multi_grep_character(human_var_names_to_search,colnames(current_mn))  #col # of human variables
    if(!is.null(factors)){ human_influence_loc_current = unique(human_influence_loc_current %w/o% multi_grep_character(factors,colnames(current_mn))  )}   # remove factors so doesn't hold at mean
    wo_human_influence_loc_current = seq(1,dim(current_mn)[2]) %w/o% human_influence_loc_current
    if(!is.null(factors)){wo_human_influence_loc_current = unique( wo_human_influence_loc_current %w/o% multi_grep_character(factors,colnames(current_mn))  ) }# remove factors so doesn't hold at mean 
    
    null = as.data.frame(current_mn) # has to be data.frame here to set logicals
    null[,paste(public_land)]=F      # set public land to 'false' 
    
    #hold all but natural at mean public land at zero 
    hm_mn_pb_0 = current_data_z              # actual data 
    hm_mn_pb_0[,unique(human_influence_loc_current)] = current_mn[,unique(human_influence_loc_current)]  # set human effects at mean values 
    hm_mn_pb_0[,paste(public_land)] = F       # set public land 'off' 

    # turn 'on' public lands
    hm_mn_pb_1 = hm_mn_pb_0
    for(hold in public_land){  
        # use this, don't switch all to 1, b/c that would be as if all lands were public
        hm_mn_pb_1[, paste(hold)]  = current_data_z[,paste(hold)] 
     }
    
    # turn on all, public land =1 where  public land exists
    all_normal_pb_1 = current_data_z
    
    null = predict(object=sample_zip, newdata=as.data.frame(null), type='response')          # predict for all held at mean and publand=0
    null = rasterize(ptscurrent,raster_example,null,fun=sum)                                 # convert to matrix      
    hm_mn_pb_0 = predict(sample_zip, hm_mn_pb_0, type='response')
    hm_mn_pb_0 = rasterize(ptscurrent,raster_example,hm_mn_pb_0,fun=sum)
    hm_mn_pb_1 = predict(sample_zip, hm_mn_pb_1, type='response')
    hm_mn_pb_1 = rasterize(ptscurrent,raster_example,hm_mn_pb_1,fun=sum)
    all_normal_pb_1 = predict(sample_zip, all_normal_pb_1, type='response')
    all_normal_pb_1 = rasterize(ptscurrent,raster_example,all_normal_pb_1,fun=sum)
    pub_effect = ( hm_mn_pb_1 - hm_mn_pb_0  )
    nat_effect = ( hm_mn_pb_0 )  # - null
    hum_effect = ( all_normal_pb_1 - hm_mn_pb_1 )  

    color  = terrain.colors(45)
    windows()
    color1 = color
    color1[1] = "#BDBDBD"
    plot(  ( nat_effect), main='Natural',col=color1  )
    val <- getValues(( nat_effect))
    xy <- as.data.frame(coordinates(mri_base))
    xy <- cbind(xy,val)
    xyN=  na.omit(xy) 
    windows()
    library(scales)
    ggplot(xyN, aes(x=x, y=y, fill=val))+ geom_raster()+  scale_fill_gradient2(low=  ('dark green'),mid= (terrain.colors(10)[6]),high=('#BF2121'),midpoint=0.11, name='FIRES',space="Lab" )+ 
    geom_raster(data=subset((xyN),val<=1e-4),fill='grey')  +  coord_equal()+xlab('')+ylab('')+ggtitle('Natural Contribution')
   
 

    windows()
    color2 = color 
    #color2[26] = "#BDBDBD"
    #color2[39:45] = "#800000"
    plot(  hum_effect, main='  Human', col=color2)
    val <- getValues(( hum_effect))
    xy <- as.data.frame(coordinates(mri_base))
    xy <- cbind(xy,val)
    xyh=  na.omit(xy) 
    windows()
    library(scales)
    ggplot(xyh, aes(x=x, y=y, fill=val))+ geom_raster()+  scale_fill_gradient2(low=  ('dark green'),mid= 'grey',high=('#BF2121'),midpoint=0, name='FIRES',space="Lab" )+ 
      geom_raster(data=subset((xyh),val<=1e-6&val>=-1e-6),fill='grey')  +  coord_equal()+xlab('')+ylab('')+ggtitle('Human Contribution')

    # plot Human  against distance from urban areas
    val_dst = getValues(raster(alldata1976_2000,'Incorp'))  #  Ppall Incorp
    val_jep = getValues(raster(alldata1951_1975,'Jepson'))
    val_pub = getValues(( pub_effect))
    xyhdist = cbind(xy,val,val_dst,val_jep,val_pub)
    xyhdist = na.omit(xyhdist)
    xyhdist$val_humpub =     xyhdist$val_pub +     xyhdist$val 
    xyhdist$val_jep = factor(xyhdist$val_jep,labels=c('Cascade','Central West','E of Sierra','C Valley','Modoc','Mojave D','NWest','Sierra','Sonoran D','South West'))
    xyhdist = subset(xyhdist,val_jep=='Central West'|val_jep=='Sierra'|val_jep=='South West')
    windows() # used in TNC report
    ggplot(xyhdist, aes(x=val_dst , y=val)) + geom_point(alpha=.1)+ geom_hline(aes(yintercept=0), linetype="dashed",colour='grey',size=.8)+ stat_smooth(fill="grey", colour="blue", size=2, alpha = 0.2) +facet_grid(.~val_jep)+
       xlab('Distance to Urban Edge (m)')+ylab('Human Contribution to Fire Count')
    windows()
    ggplot(xyhdist, aes(x=val_humpub , y=val)) + geom_point(alpha=.1)+ geom_hline(aes(yintercept=0), linetype="dashed",colour='grey',size=.8)+ stat_smooth(fill="grey", colour="blue", size=2, alpha = 0.2) +facet_grid(.~val_jep)+
      xlab('Distance to Urban Edge (m)')+ylab('Human Contribution to Fire Count')


    windows()
    plot(  ( 25/hum_effect), main='Human',col=rev(terrain.colors(45)) ,zlim=c(0,350))
    mri_base_hold = 25/hum_effect   
    mri_base_hold[mri_base_hold<350]=NA
    plot(mri_base_hold,col='#21610B',add=T, legend=F)

   

    #     hum_effect1= hum_effect<=0.01 & hum_effect>=-0.01  
    #     hum_effect1[hum_effect1<=0] = NA 
    #     a=plot(hum_effect1>=0, add=T,col="#BDBDBD",legend=F)
    #     a=plot((hum_effect1*hum_effect1>=0), add=T,col=rainbow(5),legend=F)
    #    windows()
    #    plot(  nat_effect+ pub_effect+hum_effect , main=' all')
 
    windows()
    image.plot( 25/all_normal_pb_1 , main=' MFRI', zlim=c(25,550), col = tim.colors(64)[length(tim.colors(64)):1])
    #save(null,hm_mn_pb_0,hm_mn_pb_1,all_normal_pb_1,file="C:/Users/mmann1123/Desktop/CrapFolder/data.RData")
    image.plot( log(25/all_normal_pb_1) , main=' MFRI', col = tim.colors(64)[length(tim.colors(64)):1])

    
    # ternary plots 
    nat_effect_sum=sum(values(nat_effect),na.rm=T)
    hum_effect_sum=sum(abs(values(hum_effect)),na.rm=T)
    pub_effect_sum=sum(abs(values(pub_effect)),na.rm=T)
    total_sum = pub_effect_sum+nat_effect_sum+hum_effect_sum
    nat_effect_p = nat_effect_sum /total_sum*100
    hum_effect_p = hum_effect_sum /total_sum*100
    pub_effect_p = pub_effect_sum /total_sum*100
    
    terdata = data.frame(nat=nat_effect_p,hum=hum_effect_p,pub=hum_effect_p)
    lbs <- c( "nat", "hum","pub")
    clr = 1975
    pts = factor('one')
    
    # SEE TERNARY_PLOTS_HUMAN_ENVIR_PUB.r IN PLOTS FOLDER 
    #load("G:/Faculty/Mann/Share/terary plots.RData")
    #ternary(terdata$pub, terdata$nat, terdata$hum, clr = clr, pts = pts, lbs = lbs)
    #                p + labs(shape = "Feldspar", colour = "Temperature (C)")
    #     


############################################
    dim(alldata1951_1975)
    alldata1951_1975 = stack(alldata1951_1975,  Jepson = raster("G:\\Faculty\\Mann\\Other\\Fire\\Jepson.tif"))
    alldata1976_2000 = stack(alldata1976_2000,  Jepson = raster("G:\\Faculty\\Mann\\Other\\Fire\\Jepson.tif"))
    
    
    stacks = ls(pattern="alldata19") # must do this first
    
    proj = proj4string(readOGR("G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FirePerim11_1","FRAP_Fire111"))
    fire_perm = readShapePoly("G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FirePerim11_1\\FRAP_Fire111.shp", proj4string=CRS(proj))
    fire_perm = spTransform(fire_perm, CRS=CRS( proj4string(get(stacks[1])) ))
    
    for( j in 1:length(stacks) ){  # for all stacks in workspace
      print(paste("working on stack ",stacks[j]))
      temp_stack = get(stacks[j] )  # pull out of temporary memory to avoid problems. 
      if(j==1){pts <- raster::rasterToPoints(raster::raster(get(ls(pattern="alldata")[1]),1) )           #just a layer to get point locations from
               pts<-pts[,c(1,2)]
               pts= as.data.frame(pts)
               coordinates(pts) = ~x+y
               proj4string(pts) = proj4string(temp_stack)
      }
    
      #extractor = getValues(alldata2001_2025_PA2)  # should be faster but I can't figure it out
      extractor =  as.data.frame( extract(temp_stack,pts)) # pull data to points 
      extractor$x = coordinates(pts)[,'x']
      extractor$y = coordinates(pts)[,'y']
    
      
      # get future model scenario combinations
      hist_group_names = decade_group_names[-c(multi_grep_character(fut_decade_group_names,decade_group_names))]
      model_scen =  do.call(paste, c(expand.grid(model_abrev,scenario_abrev ),sep=""))
      futr_group_names = do.call(paste, c(expand.grid(fut_decade_group_names,model_scen ),sep="_"))
      hist_futr_group_names = c(hist_group_names,futr_group_names)
      
      print('         counting overlapping fire polygons for all points')
      # limit polyons to period of interest
      # any run if there is more than one fire in this period
      if(sum(fire_perm$YEAR_ %in% seq(as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][1]), as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][2])))!=0){
        fire_perm_period = fire_perm[fire_perm$YEAR_ %in% seq(as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][1]), as.numeric(strsplit(hist_futr_group_names[j],'_')[[1]][2])     ),]
        #fire_perm_period = fire_perm[fire_perm$YEAR_ %in% seq(as.numeric(strsplit(decade_group_names[j],'_')[[1]][1]),as.numeric(strsplit(decade_group_names[j],'_')[[1]][2])     ),]
        # sum up number of overlapping fire polygons 
        extractor$fire_poly = sapply(over(pts, geometry(fire_perm_period), returnList = TRUE), length)
      }  
      assign( paste("pnt_",stacks[j],sep="") ,extractor)
      #    do.call(remove, list("extractor", stacks[j], paste("pnt_",stacks[j],sep="")))  # clear up some memory
    } 
        
    # add labels 
    pnt_alldata1951_1975$Jepson1 = factor(pnt_alldata1951_1975$Jepson, labels=c('Cascades','Central W','E Sierra','C Valley','Modoc','Mojave','North W','Sierras','Sonoran', 'South W'))
    pnt_alldata1976_2000$Jepson1 = factor(pnt_alldata1976_2000$Jepson, labels=c('Cascades','Central W','E Sierra','C Valley','Modoc','Mojave','North W','Sierras','Sonoran', 'South W'))
    
    # plot fire counts by ecoregion
    windows()
    ggplot(na.omit(pnt_alldata1976_2000[sample(1:dim(pnt_alldata1976_2000)[1],50000),]), aes(fire_poly, fill = Jepson1)) + geom_histogram(binwidth = 1,na.rm=T) + facet_grid(Jepson1 ~  ., margins = F  )+coord_cartesian(xlim=c(1,4),ylim=c(0,900)) +xlab('Fire Count')+ylab('Number of Pixels')
    ggplot(na.omit(pnt_alldata1976_2000[sample(1:dim(pnt_alldata1976_2000)[1],50000),]), aes(fire_poly, fill = Jepson1)) + geom_density(na.rm=T ) + facet_grid(Jepson1 ~  ., margins = F,scales='free_y'  )  +xlab('Fire Count')+ylab('Number of Pixels')
    
    
    qplot(fire_poly, ..stack.., data=na.omit(pnt_alldata1976_2000[sample(1:dim(pnt_alldata1976_2000)[1],50000),]), geom="density", fill=Jepson1, position=, adjust=5)
    ggplot(na.omit(pnt_alldata1976_2000[sample(1:dim(pnt_alldata1976_2000)[1],50000),]), aes(fire_poly ,fill = Jepson1,order = -as.numeric(Jepson1))) + geom_bar(binwidth = 1)  +coord_cartesian(xlim=c(1,5), ylim=c(0,3200))  
    
    
    
    
    # Map of Ecoregions
    #Extract Legend 
    g_legend<-function(a.gplot){ 
      tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
      legend <- tmp$grobs[[leg]] 
      return(legend)} 
    
    library('maptools')
    gpclibPermit()
    library(plyr)
    jepson = readOGR(dsn="G:\\Faculty\\Mann\\Other\\Fire", layer="Jepson_simplified")
    jepson@data$id = rownames(jepson@data)
    jepson.points = fortify(jepson, region="id")
    jepson.df = join(jepson.points, jepson@data, by="id")   # join the data back in 
    
    a=ggplot(jepson.df) + 
      aes(long,lat,group=group,fill=ECOREGION) + geom_polygon() +geom_path(color="white") +xlab('')+ylab('') 
    b= a+ theme(axis.text  = element_blank(), axis.ticks =element_blank(),legend.position="none") #+coord_equal()
    
    # next attempt 
    library(gridExtra)
    
    pnt_alldata1951_1975$Jepson2 = factor(pnt_alldata1951_1975$Jepson, labels=c('CASCADE RANGES','CENTRAL WESTERN CALIFORNIA','EAST OF SIERRA NEVADA','GREAT CENTRAL VALLEY','MODOC PLATEAU','MOJAVE DESERT','NORTHWESTERN CALIFORNIA','SIERRA NEVADA','SONORAN DESERT', 'SOUTHWESTERN CALIFORNIA')  )
    pnt_alldata1976_2000$Jepson2 = factor(pnt_alldata1976_2000$Jepson, labels=c('CASCADE RANGES','CENTRAL WESTERN CALIFORNIA','EAST OF SIERRA NEVADA','GREAT CENTRAL VALLEY','MODOC PLATEAU','MOJAVE DESERT','NORTHWESTERN CALIFORNIA','SIERRA NEVADA','SONORAN DESERT', 'SOUTHWESTERN CALIFORNIA') )
    c=ggplot(na.omit(pnt_alldata1976_2000 ), aes(fire_poly ,fill = Jepson2,order = -as.numeric(Jepson2))) + geom_bar(binwidth = 1,aes(y = (..count..)/sum(..count..)*100) ) +coord_cartesian(xlim=c(1,5), ylim=c(0,7))  + theme(legend.position="none") + xlab('Number of Fires')+ ylab('Percentage of Pixels')
    
    windows()
    legend <- g_legend(a) 
    legend=gTree(children=gList(legend), cl="legendGrob")
    widthDetails.legendGrob <- function(x) unit(10, "cm")
    sidebysideplot <- grid.arrange(c,b, ncol=2,legend=legend)
    
    
    # Plot fire counts by density class
    library(RColorBrewer)
    pnt_alldata1976_2000$Den2 =  pnt_alldata1976_2000$den 
    pnt_alldata1976_2000$Den2[pnt_alldata1976_2000$Den2 >= 4] = 'Very-High'
    pnt_alldata1976_2000$Den2[pnt_alldata1976_2000$Den2 >= 1 & pnt_alldata1976_2000$Den2 < 4] = 'High'
    pnt_alldata1976_2000$Den2[pnt_alldata1976_2000$Den2 >= 0.41 & pnt_alldata1976_2000$Den2 < 1] = 'Medium'
    pnt_alldata1976_2000$Den2[pnt_alldata1976_2000$Den2 >= 0.025 & pnt_alldata1976_2000$Den2 < 0.41] = 'Low'
    pnt_alldata1976_2000$Den2[pnt_alldata1976_2000$Den2 < 0.025 ]       = 'Sparse'
    pnt_alldata1976_2000$Den2= factor(pnt_alldata1976_2000$Den2,levels=c('Very-High','High','Medium','Low','Sparse'),ordered=T)
    ?brewer.pal
    my.cols <- brewer.pal(5, "Greys")
    a=ggplot(na.omit(pnt_alldata1976_2000 ), aes(fire_poly ,fill = Den2,order = -as.numeric(Den2))) + geom_bar(binwidth = 1,aes(y = (..count..)/sum(..count..)*100) ) +coord_cartesian(xlim=c(1,5), ylim=c(0,7))   + xlab('Number of Fires')+ ylab('Percentage of Pixels')
    a+scale_fill_manual(values = my.cols,name ='Housing Density Class') 
    



# #########################################
#     # try with holding density to 0 
# 
#     # get mean values for current
#     current_mn =  as.data.frame( t(colMeans(current_data_z, na.rm=T) ) )
#     current_names = names(current_mn)
#     rowsc = dim(current_data_z)[1]
#     current_mn = matrix(rep(as.numeric( current_mn),rowsc),rowsc,byrow=T )  # createdataframe of all column means while matching dim of input data
#     colnames(current_mn) = current_names
#     
#     human_influence_loc_current = multi_grep_character(human_var_names_to_search,colnames(current_mn))  #col # of human variables
#     human_influence_loc_current = unique(human_influence_loc_current %w/o% multi_grep_character(factors,colnames(current_mn))  )# remove factors so doesn't hold at mean
#     wo_human_influence_loc_current = seq(1,dim(current_mn)[2]) %w/o% human_influence_loc_current
#     wo_human_influence_loc_current = unique( wo_human_influence_loc_current %w/o% multi_grep_character(factors,colnames(current_mn))  )# remove factors so doesn't hold at mean 
#     den_influence_loc_current = multi_grep_character('den',colnames(current_mn))  #col # of human variables
# 
#     null = current_mn
#     null[,paste(public_land)]=0
#     #hold all but natural at mean 
#     hm_mn_pb_0 = current_data_z
#     hm_mn_pb_0[,human_influence_loc_current] = current_mn[,human_influence_loc_current]
#     hm_mn_pb_0[,den_influence_loc_current] = 0   # zero out density 
# 
#     hm_mn_pb_0[,paste(public_land)]=0
#     # turn on public lands
#     hm_mn_pb_1 = hm_mn_pb_0
#     for(hold in public_land){  # use this, don't switch all to 1, b/c that would be as if all lands were public
#         hm_mn_pb_1[, paste(hold)]  = current_data_z[,paste(hold)] 
#     }
#     # turn on all
#     all_normal_pb_1 = current_data_z
#     
#     null = predict(sample_zip, as.data.frame(null), type='response')
#     null = rasterize(ptscurrent,raster_example,null,fun=sum)
#     hm_mn_pb_0 = predict(sample_zip, as.data.frame(hm_mn_pb_0), type='response')
#     hm_mn_pb_0 = rasterize(ptscurrent,raster_example,hm_mn_pb_0,fun=sum)
#     hm_mn_pb_1 = predict(sample_zip, as.data.frame(hm_mn_pb_1), type='response')
#     hm_mn_pb_1 = rasterize(ptscurrent,raster_example,hm_mn_pb_1,fun=sum)
#     all_normal_pb_1 = predict(sample_zip, as.data.frame(all_normal_pb_1), type='response')
#     all_normal_pb_1 = rasterize(ptscurrent,raster_example,all_normal_pb_1,fun=sum)
#     
# 
#     nat_effect = ( hm_mn_pb_0   )  #  don't subtract null 
#     pub_effect = (hm_mn_pb_1 -hm_mn_pb_0  )
#     hum_effect = ( all_normal_pb_1 -hm_mn_pb_1   )
#     
#     color  = terrain.colors(45)
#     windows()
#     color1 = color
#     color1[1] = "#BDBDBD"
#     plot(  ( nat_effect), main='natural',col=color1  )
#     windows()
#     plot(  (pub_effect ), main='  Public', col= color1 )
#     windows()
#     color2 = color[length(color):1]
#     color2[29] = "#BDBDBD"
#     plot(  hum_effect, main='  Human', col=color2)
#     windows()
#     plot(  nat_effect+ pub_effect+hum_effect , main=' all')
#     windows()
#     plot( all_normal_pb_1 , main=' actual')
 
      



 
  





##############################################################
# get boot strapped standard errors etc
#  READ THIS  http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm

library(boot)
f <- function(data, i) {
    require(pscl) 
    m <<- zeroinfl( fire ~ cwd_sum + log(tmax_ave + 1) + log(tmax_ave_P + 1) + public_land | log(aet_seas + 1) + I(log(aet_seas + 1)^2) + log(pet_seas + 1) + log(ppt_seas + 1) + Den + I(Den^2) + log(ppt_seas_P + 1) + log(aet_seas_P + 1) + log(pet_seas_P + 1) + log(OceanDistEuc + 1), data = data[i, ],
                    start = list(count =  c(-1.357213e+01, 4.253313e-04 ,9.704480e-01, 2.797365e+00, 7.082466e-01), 
                                 zero = c( 3.0330553,-17.3544455, 3.4975830 , 25.9207924 , -0.5352728 ,  2.4481140,
                                           -0.1144803 , -0.7177797 , -4.5465911, -16.4944435 , -0.2851231) ))
    as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}
set.seed(10)

res <- boot(data=data1971_2000[data1971_2000$train==T,], f, R = 2000 , parallel = "snow", ncpus = 4)
res

# estimated pvalues        http://127.0.0.1:21051/help/library/boot/html/boot.html
pvalues = data.frame(sapply(seq(1,length(res$t0),by=2), function(x) sum(abs(res$t[,x]-1) > abs(res$t0[x]-1))/(1+res$R) ))
row.names(pvalues) = names(coef(m))
pvalues


### calculate confidence intervals  would be better to use bca but not working
data.frame(param=res$t0[seq(1,length(res$t0),by=2)], )
parms <- t(sapply(seq(1,length(res$t0),by=2), function(i) {
    out <- boot.ci(res, index = c(i, i + 1), type = c("percent"))
    with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))
row.names(parms) <- names(coef(m))
parms




#############################################################################
#############################################################################
#############################################################################
#############################################################################

library(gstat)
coordinates(data1976_2000)=~x+y
datain = data1976_2000[data1976_2000$fire_poly>=1,]
proj4string(datain) = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
v=variogram(fire_poly  ~ x+y+aetave+cwdave+aetave:cwdave,cutoff=5e4,data=datain  )
v
v.fit <- fit.variogram(v, model = vgm(psill=0.24, "Sph", 50000, nugget=.03))
windows()
plot(v, model = v.fit, main="Semivariogram - Fire>=1") # view result




coordinates(data1976_2000)=~x+y
proj4string(data1976_2000) = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
data1976_2000$fire2= data1976_2000$fire_poly>=1
v=variogram(fire_poly  ~ x+y+aetave+cwdave+aetave:cwdave,cutoff=5e4,data=data1976_2000[sample(nrow(data1976_2000),10000,replace=F),]  )
v
v.fit <- fit.variogram(v, model = vgm(psill=0.24, "Sph", 50000, nugget=.03))
windows()
plot(v, model = v.fit, main="Semivariogram - Fire>=1") # view result












# windows()
# plot(v,   main="Semivariogram - Fire>=1") # view result
# samplei = na.omit( as.data.frame(data1976_2000[sample(dim(data1976_2000)[1],100000,replace=F ),] ))
# samplei = na.omit(as.data.frame(data1976_2000))
# 
# for(i in 1:10){
#     print(i)
#     if(dim(samplei)[1]>0){
#         sample_hydro = samplei[samplei$HydroReg==i,]
#         coordinates(sample_hydro)=~x+y
#         
#         v2=variogram(I(fire_poly>=1)~ x+y, cutoff=250000, data= sample_hydro )
#         windows()
#         plot(v2 )    
#         print(v2$gamma)
#     }
# }




inner = list("B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_0024.tif",
             "B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_2550.tif",
             "B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_5074.tif",
             "B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_7599.tif")

for( i in 1:length(inner)){
    innerR = raster(paste(inner[[i]][1],sep=''))
    print(paste(proj4string(innerR),'.....'))
    inner3x3 <- focal(innerR, w=matrix(1/9,nrow=3,ncol=3),na.rm=T, overwrite=T)     #average moving window
    inner5x5 <- focal(innerR, w=matrix(1/25,nrow=5,ncol=5),na.rm=T, overwrite=T)     #average moving window
    proj4string(inner3x3) =  "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    proj4string(inner5x5) =  "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    writeRaster(inner3x3, paste(substr(inner[[i]][1],1,nchar(inner[[i]][1])-4 ),"_3x3.tif",sep=''), overwrite=T)
    writeRaster(inner5x5, paste(substr(inner[[i]][1],1,nchar(inner[[i]][1])-4 ),"_5x5.tif",sep=''),overwrite=T)
    print(paste(proj4string(inner3x3)))
    print(paste(proj4string(inner5x5)))
    
}

a = raster('E:\Aggregated1080\Summary_Files\\OtherVariables\\den_5074_3x3.tif')
proj4string(a)
=proj4string(raster('B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_5074.tif'))
writeRaster(a, paste(substr(inner[[i]][1],1,nchar(inner[[i]][1])-4 ),"_3x3.tif",sep=''), overwrite=T)

a = raster('B:\\Aggregated1080\\Summary_Files\\OtherVariables\\den_5074.tif')
proj4string(a)

windows()
plot(a)






###################################################################################
#  CREATE VARIABLES
###################################################################################


# rasterize traffic 
example = raster("G:/Faculty/Mann/Other/Fire/AllRoads.tif" )
proj = proj4string(raster("G:/Faculty/Mann/Other/Fire/AllRoads.tif" ))

traffic = readOGR("G:\\Faculty\\Mann\\Share\\CA Traffic","AADT_1m_proj",disambiguateFIDs=T,verbose=T,dropNULLGeometries=T)

# getinfo.shape("G:\\Faculty\\Mann\\Share\\CA Traffic\\AADT_1m_proj.shp")
# 
# traffic2 = readShapeLines("G:\\Faculty\\Mann\\Share\\CA Traffic\\AADT_1m_proj.shp", proj4string=proj)


AADT = rasterize(traffic,example,field='AADT', fun=sum)
writeRaster(AADT,"G:/Faculty/Mann/Other/Fire/AADT.tif")
total_miles = rasterize(traffic,example,field='Miles', fun=sum)
writeRaster(total_miles,"G:/Faculty/Mann/Other/Fire/total_miles.tif")
congestion = AADT/total_miles
writeRaster(congestion,"G:/Faculty/Mann/Other/Fire/congestionAADTperMile.tif")

den_7600= raster(  "G:\\Faculty\\Mann\\Other\\Fire\\den_7600_proj.tif" )
den_0125 = raster(  "G:\\Faculty\\Mann\\Other\\Fire\\den_0125_proj.tif"  )
den_2650 = raster( "G:\\Faculty\\Mann\\Other\\Fire\\den_2650_proj.tif" )

cong_stack = stack(congestion,den_7600,den_0125,den_2650)




proj = proj4string(readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.9TUrbNS+D20km","SpecifyEcoCity6.9TUrbNS+D20km_SM"))
housing = readShapePoly("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.9TUrbNS+D20km\\SpecifyEcoCity6.9TUrbNS+D20km_SM", proj4string=CRS(proj))
[1] "SUM090"     "COUNTY"     "sort"       "CountyName" "TRACT"      "BLKGRP"     "POP00"      "Tot00"     
[9] "Est_Acr"    "TotPxl"     "InhbPxl"    "CumAdj_49"  "CumAdj_59"  "CumAdj_69"  "CumAdj_79"  "CumAdj_89" 
[17] "CumAdj_99"  "Acres"      "URBAN"      "RURAL"      "id"         "HuDac4_39"  "HuDac4_49"  "HuDac4_59" 
[25] "HuDac4_69"  "HuDac4_79"  "HuDac4_89"  "HuDac4_99"  "HuDac4_109" "HuDac4_119" "HuDac4_129" "HuDac4_139"
[33] "HuDac4_149" "HuD_Rd25_1" "HuD_Rd25_2" "HuD_Rd25_3" "HuD_Rd25_4" "HuD_Rd25_5" "HuD_Ud25_1" "HuD_Ud25_2"
[41] "HuD_Ud25_3" "HuD_Ud25_4" "HuD_Ud25_5" "HuD_Ud50_1" "HuD_Ud50_2" "HuD_Ud50_3" "HuD_Ud50_4" "HuD_Ud50_5"
[49] "HuD_Rd50_1" "HuD_Rd50_2" "HuD_Rd50_3" "HuD_Rd50_4" "HuD_Rd50_5" "ChgB9949"   "ChgRd50"    "ChgUd50"   
[57] "RDfBd50"    "UDfBd50"    "Max"        "Min"        "Rd25a_109"  "Rd25a_119"  "Rd25a_129"  "Rd25a_139" 
[65] "Rd25a_149"  "Ud25a_109"  "Ud25a_119"  "Ud25a_129"  "Ud25a_139"  "Ud25a_149" 


#NOTES: use '/' instead of '\\' ,  polygon file name can't include '+'


########################################################
# Rasterize historical future housing   *use rasterize2aggregate* much better

##Hisotical 
mask = "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"
snap_raster = 'G:/Faculty/Mann/Historic_BCM/Aggregated1080/aet1951oct.tif'
cellsize1 = 270
cellsize_ag = 1080
cell_assignment = "MAXIMUM_COMBINED_AREA"
poly_path = 'G:\\Faculty\\Mann\\Other\\Fire\\'
poly_name = "SpecifyEcoCity6920km_SM.shp"
in_features = paste(poly_path,poly_name,sep="")

for( i in seq(39,149,10)){
  value_field = paste("HuDac4_",i,sep='')
  out_raster_path_name = paste("G:\\Faculty\\Mann\\Other\\Fire\\",value_field,"a",".tif",sep='')
  py.rasterize2aggregate(poly_path,poly_name,snap_raster,in_features,value_field,out_raster_path_name,cell_assignment,cellsize1,cellsize_ag,mask)
  print(paste("year ", i, sep=''))
}

# get mean housing values for decade_group_names
#decade_group_names = c(  '1951_1975', '1976_2000', '2001_2025', '2026_2050') # keep in chronological order & must be mutually exclusive and seperated by "_" 1971_2000

for( i in seq(39,149,10)){
  # read in all density rasters store as D39, D49 etc
  assign(paste('D',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\HuDac4_',i,'a','.tif',sep='')  ))
}
 
den_5175 = mean(D49,D59,mean(D69,D79))
den_7600 = mean(D79,D89,D99)
den_0125 = mean(D99,D109,mean(D119,D129))
den_2650 = mean(D129,D139,D149)
 
writeRaster(den_5175, "G:\\Faculty\\Mann\\Other\\Fire\\den_5175.tif", overwrite=T)
writeRaster(den_7600, "G:\\Faculty\\Mann\\Other\\Fire\\den_7600.tif", overwrite=T)
writeRaster(den_0125, "G:\\Faculty\\Mann\\Other\\Fire\\den_0125.tif", overwrite=T)
writeRaster(den_2650, "G:\\Faculty\\Mann\\Other\\Fire\\den_2650.tif", overwrite=T)
 
# get focal 
den3x3_5175 <- focal(den_5175, w=matrix(1/9,nrow=3,ncol=3),na.rm=T,pad=T) 
den5x5_5175 <- focal(den_5175, w=matrix(1/25,nrow=5,ncol=5),na.rm=T,pad=T) 
den10x10_5175 <- focal(den_5175, w=matrix(1/121,nrow=11,ncol=11),na.rm=T,pad=T)
den20x20_5175 <- focal(den_5175, w=matrix(1/441,nrow=21,ncol=21),na.rm=T,pad=T)

den3x3_7600 <- focal(den_7600, w=matrix(1/9,nrow=3,ncol=3),na.rm=T,pad=T) 
den5x5_7600 <- focal(den_7600, w=matrix(1/25,nrow=5,ncol=5),na.rm=T,pad=T) 
den10x10_7600 <- focal(den_7600, w=matrix(1/121,nrow=11,ncol=11),na.rm=T,pad=T)
den20x20_7600 <- focal(den_7600, w=matrix(1/441,nrow=21,ncol=21),na.rm=T,pad=T)

den3x3_0125 <- focal(den_0125, w=matrix(1/9,nrow=3,ncol=3),na.rm=T,pad=T) 
den5x5_0125 <- focal(den_0125, w=matrix(1/25,nrow=5,ncol=5),na.rm=T,pad=T) 
den10x10_0125 <- focal(den_0125, w=matrix(1/121,nrow=11,ncol=11),na.rm=T,pad=T)
den20x20_0125 <- focal(den_0125, w=matrix(1/441,nrow=21,ncol=21),na.rm=T,pad=T)

den3x3_2650 <- focal(den_2650, w=matrix(1/9,nrow=3,ncol=3),na.rm=T,pad=T) 
den5x5_2650 <- focal(den_2650, w=matrix(1/25,nrow=5,ncol=5),na.rm=T,pad=T) 
den10x10_2650 <- focal(den_2650, w=matrix(1/121,nrow=11,ncol=11),na.rm=T,pad=T)
den20x20_2650 <- focal(den_2650, w=matrix(1/441,nrow=21,ncol=21),na.rm=T,pad=T)

writeRaster(den3x3_5175, "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_5175.tif", overwrite=T)
writeRaster(den5x5_5175, "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_5175.tif", overwrite=T)
writeRaster(den10x10_5175, "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_5175.tif", overwrite=T)
writeRaster(den20x20_5175, "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_5175.tif", overwrite=T)
writeRaster(den3x3_7600, "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_7600.tif", overwrite=T)
writeRaster(den5x5_7600, "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_7600.tif", overwrite=T)
writeRaster(den10x10_7600, "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_7600.tif", overwrite=T)
writeRaster(den20x20_7600, "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_7600.tif", overwrite=T)
writeRaster(den3x3_0125, "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_0125.tif", overwrite=T)
writeRaster(den5x5_0125, "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_0125.tif", overwrite=T)
writeRaster(den10x10_0125, "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_0125.tif", overwrite=T)
writeRaster(den20x20_0125, "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_0125.tif", overwrite=T)
writeRaster(den3x3_2650, "G:\\Faculty\\Mann\\Other\\Fire\\den3x3_2650.tif", overwrite=T)
writeRaster(den5x5_2650, "G:\\Faculty\\Mann\\Other\\Fire\\den5x5_2650.tif", overwrite=T)
writeRaster(den10x10_2650, "G:\\Faculty\\Mann\\Other\\Fire\\den10x10_2650.tif", overwrite=T)
writeRaster(den20x20_2650, "G:\\Faculty\\Mann\\Other\\Fire\\den20x20_2650.tif", overwrite=T)

# Future 
mask = "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"
snap_raster = 'G:/Faculty/Mann/Historic_BCM/Aggregated1080/aet1951oct.tif'
cellsize1 = 270
cellsize_ag = 1080
cell_assignment = "MAXIMUM_COMBINED_AREA"
poly_path = 'G:\\Faculty\\Mann\\Other\\Fire\\'
poly_name = "SpecifyEcoCity6920km_SM.shp"
in_features = paste(poly_path,poly_name,sep="")

for( i in seq(109,149,10)){
  value_field = paste("Ud25a_",i,sep='')
  out_raster_path_name = paste("G:\\Faculty\\Mann\\Other\\Fire\\",value_field,"a",".tif",sep='')
  py.rasterize2aggregate(poly_path,poly_name,snap_raster,in_features,value_field,out_raster_path_name,cell_assignment,cellsize1,cellsize_ag,mask)
  print(paste("year ", i, sep=''))
}

for( i in seq(109,149,10)){
  value_field = paste("Rd25a_",i,sep='')
  out_raster_path_name = paste("G:\\Faculty\\Mann\\Other\\Fire\\",value_field,"a",".tif",sep='')
  py.rasterize2aggregate(poly_path,poly_name,snap_raster,in_features,value_field,out_raster_path_name,cell_assignment,cellsize1,cellsize_ag,mask)
  print(paste("year ", i, sep=''))
}

# get mean housing values for decade_group_names
#decade_group_names = c(  '1951_1975', '1976_2000', '2001_2025', '2026_2050') # keep in chronological order & must be mutually exclusive and seperated by "_" 1971_2000

for( i in seq(109,149,10)){
  # read in all density rasters store as D39, D49 etc
  assign(paste('U',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\Ud25a_',i,'a','.tif',sep='')  ))
}

for( i in seq(109,149,10)){
  # read in all density rasters store as D39, D49 etc
  assign(paste('R',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\Rd25a_',i,'a','.tif',sep='')  ))
}


Uden_0125 = mean(D99,U109,mean(U119,U129))
Uden_2650 = mean(U129,U139,U149)
Rden_0125 = mean(D99,R109,mean(R119,R129))
Rden_2650 = mean(R129,R139,R149)
remove(D39,D49,D59,D69,D79,D89,D99,D109,D119,D129,D139,D149,U39,U49,U59,U69,U79,U89,U99,U109,U119,U129,U139,U149,R39,R49,R59,R69,R79,R89,R99,R109,R119,R129,R139,R149)

for(scenario in c('Uden','Rden')){
  for(period in c('_0125','_2650')){
    writeRaster( get(paste(scenario,period,sep='')),paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,period,".tif",sep=''), overwrite=T) 
  } 
}

# calc focal 
for(scenario in c('Uden','Rden')){
  for(period in c('_0125','_2650')){
    for(focdist in c('3x3','5x5','10x10','20x20')){
      print(paste(scenario, period, focdist))
            if(focdist=='3x3'){foc =  focal(get(paste(scenario,period,sep='')), w=matrix(1/9,nrow=3,ncol=3),na.rm=T,pad=T) }
            if(focdist=='5x5'){foc =  focal(get(paste(scenario,period,sep='')), w=matrix(1/25,nrow=5,ncol=5),na.rm=T,pad=T) }
            if(focdist=='10x10'){foc =  focal(get(paste(scenario,period,sep='')), w=matrix(1/121,nrow=11,ncol=11),na.rm=T,pad=T) }
            if(focdist=='20x20'){foc =  focal(get(paste(scenario,period,sep='')), w=matrix(1/441,nrow=21,ncol=21),na.rm=T,pad=T) }
            print(paste('writing: ',"G:\\Faculty\\Mann\\Other\\Fire\\",scenario,focdist,period,".tif",sep=''))
            writeRaster( foc ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,focdist,period,".tif",sep=''), overwrite=T) 
    }
  } 
}


#################################
# CALCULATE URBAN BUFFER RASTERS PAST AND FUTURE
# grab data
  for( i in seq(39,149,10)){
    # read in all density rasters store as D39, D49 etc
    assign(paste('D',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\HuDac4_',i,'a','.tif',sep='')  ))
  }
  for( i in seq(109,149,10)){
    # read in all density rasters store as D39, D49 etc
    assign(paste('U',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\Ud25a_',i,'a','.tif',sep='')  ))
  }
  for( i in seq(109,149,10)){
    # read in all density rasters store as D39, D49 etc
    assign(paste('R',i,sep=''),raster( paste('G:\\Faculty\\Mann\\Other\\Fire\\Rd25a_',i,'a','.tif',sep='')  ))
  }
  
  den_5175 = mean(D49,D59,mean(D69,D79))
  den_7600 = mean(D79,D89,D99)
  den_0125 = mean(D99,D109,mean(D119,D129))
  den_2650 = mean(D129,D139,D149)
  Uden_0125 = mean(D99,U109,mean(U119,U129))
  Uden_2650 = mean(U129,U139,U149)
  Rden_0125 = mean(D99,R109,mean(R119,R129))
  Rden_2650 = mean(R129,R139,R149)
  remove(D39,D49,D59,D69,D79,D89,D99,D109,D119,D129,D139,D149,U39,U49,U59,U69,U79,U89,U99,U109,U119,U129,U139,U149,R39,R49,R59,R69,R79,R89,R99,R109,R119,R129,R139,R149)
  
  # create weights matrix  ( avoid default now w=3 is equivalent to w=matrix(1/9,3,3))
  # d the radius of the circle (in units of the CRS)


  c3x3 = focalWeight(den_5175,(3*1080/2), 'circle')
  c3x3[c3x3>0]=1
  c5x5 = focalWeight(den_5175,(5*1080/2), 'circle')
  c5x5[c5x5>0]=1
  c10x10 = focalWeight(den_5175,(11*1080/2), 'circle')
  c10x10[c10x10>0]=1
  c20x20 = focalWeight(den_5175,(21*1080/2), 'circle')
  c20x20[c20x20>0]=1
  c50x50 = focalWeight(den_5175,(51*1080/2), 'circle')
  c50x50[c50x50>0]=1
  
  
  # Gaussian filter for square cells
  g3x3 <- focalWeight(den_5175, (3*180), "Gauss")
  g5x5 <- focalWeight(den_5175, (5*180), "Gauss")
  g10x10 <- focalWeight(den_5175, (10*180), "Gauss")
  g20x20 <- focalWeight(den_5175, (20*180), "Gauss")
  g50x50 <- focalWeight(den_5175, (50*180), "Gauss")


  library(snow)
  beginCluster(4,type="SOCK")
  # calc focal 
  for(scenario in c('den')){
    for(period in c('_5175','_7600')){
      for(focdist in c('3x3','5x5','10x10','20x20','50x50')){  #
        print(paste(scenario, period, focdist))
        if(focdist=='3x3'){foc =  focal(get(paste(scenario,period,sep='')),w=c3x3,fun=max  ,na.rm=T,pad=T) }
        if(focdist=='5x5'){foc =  focal(get(paste(scenario,period,sep='')),w=c5x5, fun=max ,na.rm=T,pad=T) }
        if(focdist=='10x10'){foc =  focal(get(paste(scenario,period,sep='')),w=c10x10,fun=max ,na.rm=T,pad=T) }
        if(focdist=='20x20'){foc =  focal(get(paste(scenario,period,sep='')),w=c20x20, fun=max ,na.rm=T,pad=T) }
        if(focdist=='50x50'){foc =  focal(get(paste(scenario,period,sep='')),w=c50x50, fun=max ,na.rm=T,pad=T) }
        print(paste('writing: ',"G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''))
        writeRaster( foc ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''), overwrite=T) 
      }
    } 
  }
  
  
  # calc focal future
  for(scenario in c('den','Uden','Rden')){  #
    for(period in c('_0125','_2650')){
      for(focdist in c('3x3','5x5','10x10','20x20','50x50')){  #
        print(paste(scenario, period, focdist))
        if(focdist=='3x3'){foc =  focal(get(paste(scenario,period,sep='')),w=c3x3,fun=max  ,na.rm=T,pad=T) }
        if(focdist=='5x5'){foc =  focal(get(paste(scenario,period,sep='')),w=c5x5, fun=max ,na.rm=T,pad=T) }
        if(focdist=='10x10'){foc =  focal(get(paste(scenario,period,sep='')),w=c10x10,fun=max ,na.rm=T,pad=T) }
        if(focdist=='20x20'){foc =  focal(get(paste(scenario,period,sep='')),w=c20x20, fun=max ,na.rm=T,pad=T) }
        if(focdist=='50x50'){foc =  focal(get(paste(scenario,period,sep='')),w=c50x50, fun=max ,na.rm=T,pad=T) }
        print(paste('writing: ',"G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''))
        writeRaster( foc ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''), overwrite=T) 
      }
    } 
  }
  
  # create urban dummy
  setwd("G:\\Faculty\\Mann\\Other\\Fire\\")
  max_vars =  dir()[grep('max', dir())]
  
  for(file in max_vars){
    print(file)
    urb = raster(paste(as.character(getwd()),'/',file,sep='')) >=1 
    writeRaster( urb ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",strsplit(file, split='max')[[1]][1],'urb_',strsplit(file, split='max')[[1]][2],".tif",sep=''), overwrite=T) 
  }
   
  # Calc for gausin
  for(scenario in c('den')){
    for(period in c('_5175','_7600')){
      for(focdist in c('3x3','5x5','10x10','20x20','50x50')){  #
        print(paste(scenario, period, focdist))
        if(focdist=='3x3'){foc =  focal(get(paste(scenario,period,sep='')),w=g3x3,fun=max  ,na.rm=T,pad=T) }
        if(focdist=='5x5'){foc =  focal(get(paste(scenario,period,sep='')),w=g5x5, fun=max ,na.rm=T,pad=T) }
        if(focdist=='10x10'){foc =  focal(get(paste(scenario,period,sep='')),w=g10x10,fun=max ,na.rm=T,pad=T) }
        if(focdist=='20x20'){foc =  focal(get(paste(scenario,period,sep='')),w=g20x20, fun=max ,na.rm=T,pad=T) }
        if(focdist=='50x50'){foc =  focal(get(paste(scenario,period,sep='')),w=g50x50, fun=max ,na.rm=T,pad=T) }
        print(paste('writing: ',"G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'gmax',focdist,period,".tif",sep=''))
        writeRaster( foc ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'gmax',focdist,period,".tif",sep=''), overwrite=T) 
      }
    } 
  }
  
   
  # calc gauss future  not currently used 
  for(scenario in c('den','Uden','Rden')){  #
    for(period in c('_0125','_2650')){
      for(focdist in c('3x3','5x5','10x10','20x20','50x50')){  #
        print(paste(scenario, period, focdist))
        if(focdist=='3x3'){foc =  focal(get(paste(scenario,period,sep='')),w=c3x3,fun=max  ,na.rm=T,pad=T) }
        if(focdist=='5x5'){foc =  focal(get(paste(scenario,period,sep='')),w=c5x5, fun=max ,na.rm=T,pad=T) }
        if(focdist=='10x10'){foc =  focal(get(paste(scenario,period,sep='')),w=c10x10,fun=max ,na.rm=T,pad=T) }
        if(focdist=='20x20'){foc =  focal(get(paste(scenario,period,sep='')),w=c20x20, fun=max ,na.rm=T,pad=T) }
        if(focdist=='50x50'){foc =  focal(get(paste(scenario,period,sep='')),w=c50x50, fun=max ,na.rm=T,pad=T) }
        print(paste('writing: ',"G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''))
        writeRaster( foc ,paste("G:\\Faculty\\Mann\\Other\\Fire\\",scenario,'max',focdist,period,".tif",sep=''), overwrite=T) 
      }
    } 
  }
  
  
# create linear distance from urban areas based on housing model
for(raster_name in c("den_5175_proj.tif",  
           "den_7600_proj.tif",
           "den_0125_proj.tif", 
           "den_2650_proj.tif",
           "Uden_5175_proj.tif",  
           "Uden_7600_proj.tif",
           "Uden_0125_proj.tif", 
           "Uden_2650_proj.tif",
           "Rden_5175_proj.tif",  
           "Rden_7600_proj.tif",
           "Rden_0125_proj.tif", 
           "Rden_2650_proj.tif") ){
  
  
  snap_raster = 'G:/Faculty/Mann/Historic_BCM/Aggregated1080/aet1951oct.tif'
  mask = "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"
  
  poly_path = "G:\\Faculty\\Mann\\Other\\Fire\\"
  poly_name = paste(raster_name)
  out_raster_path_name =paste("G:\\Faculty\\Mann\\Other\\Fire\\","Dist",raster_name,sep='')
  maximum_distance = ""
  py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)
  
}

r = raster(out_raster_path_name)

#################################
# correct for issues on proj4string
  mask = "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"
  snap_raster = 'G:/Faculty/Mann/Historic_BCM/Aggregated1080/aet1951oct.tif'

  setwd("G:\\Faculty\\Mann\\Other\\Fire\\")
  poly_path ="G:\\Faculty\\Mann\\Other\\Fire"    #directory to read from 
  directory = dir()
  directory_names= directory[grep("den",directory)] # was also run on this
  #directory_names= directory[grep("north_southa",directory)]
  #directory_names= directory[grep("elev",directory)]
  #directory_names= directory[grep("slop",directory)]

  #only run this part if there are problem files
  directory_names = directory_names[-c(grep('_proj',directory_names),grep('.xml',directory_names),grep('.tfw',directory_names),grep('.ovr',directory_names))]

  #always run this
  #setwd("G:\\Faculty\\Mann\\Other\\Fire\\")
  #directory_names= "denmax5x5_0125.tif" 
  for(i in 1:length(directory_names)){
    raster_1 = raster(directory_names[i])
  
    if(proj4string(raster_1)=="+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
      in_raster = paste(getwd(),directory_names[i],sep='/')
      out_raster = paste(strsplit(in_raster,split='.tif'),'_proj','.tif',sep='')
      
      py.ProjectRaster_management(in_raster,out_raster,snap_raster,resampling_type='CUBIC',mask)
      print(paste(directory_names[i], 'done')) 
      }
  }
 

###############################################################################
# Other Environmental variables 
# Slope resample
in_raster_path_name = "G:\\Faculty\\Mann\\Share/elevation srtm/STRMGeoTiff/Slope_srtm_All_100m_clip.tif"
out_raster_path_name = "G:\\Faculty\\Mann\\Other\\Fire\\slope.tif"
resampling_type = "CUBIC"            # NEAREST  BILINEAR CUBIC MAJORITY 
py.resample(in_raster_path_name,snap_raster,out_raster_path_name,resampling_type,mask) 
slope = raster(out_raster_path_name)  # some reason isn't masking properly
slope = mask(slope,  raster("G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"))
writeRaster(slope,"G:\\Faculty\\Mann\\Other\\Fire\\slope.tif", overwrite=T)

# FVEG resample
in_raster_path_name = "G:\\Faculty\\Mann\\Share\\FVeg\\FVeg.tif"
out_raster_path_name = "G:\\Faculty\\Mann\\Other\\Fire\\FVeg.tif"
resampling_type = "NEAREST"            # NEAREST  BILINEAR CUBIC MAJORITY      ?MAJORITY DOESN"T WORK!?
py.resample(in_raster_path_name,snap_raster,out_raster_path_name,resampling_type,mask) 
 
# Jepson resample
in_raster_path_name = "G:\\Faculty\\Mann\\Share\\Environemental Factors/Jepson_raster.tif"
out_raster_path_name = "G:\\Faculty\\Mann\\Other\\Fire\\Jepson.tif"
resampling_type = "NEAREST"            # NEAREST  BILINEAR CUBIC MAJORITY      ?MAJORITY DOESN"T WORK!?
py.resample(in_raster_path_name,snap_raster,out_raster_path_name,resampling_type,mask) 


# elevation 
in_raster_path_name = "G:\\Faculty\\Mann\\Share/elevation srtm/STRMGeoTiff/srtm_ALL_100m.tif"
out_raster_path_name = "G:\\Faculty\\Mann\\Other\\Fire\\elev.tif"
resampling_type = "CUBIC"            # NEAREST  BILINEAR CUBIC MAJORITY 
py.resample(in_raster_path_name,snap_raster,out_raster_path_name,resampling_type,mask) 
elev = raster(out_raster_path_name)  # some reason isn't masking properly
elev = mask(elev,  raster("G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"))
writeRaster(elev,"G:\\Faculty\\Mann\\Other\\Fire\\elev.tif", overwrite=T)
# north south 
elev = raster("G:\\Faculty\\Mann\\Other\\Fire\\elev.tif")
aspect = terrain(elev,opt='aspect', unit='degrees')
north_south =  cos( pi/2+ aspect*pi/180)
writeRaster(north_south, "G:\\Faculty\\Mann\\Other\\Fire\\north_southa.tif", overwrite=TRUE)

# Lightining WIS
in_raster_path_name =  "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Lightning\\WIS Data\\Light5yrWIS.tif" 
out_raster_path_name = "G:\\Faculty\\Mann\\Other\\Fire\\LightWIS.tif"
resampling_type = "CUBIC"            # NEAREST  BILINEAR CUBIC MAJORITY      ?MAJORITY DOESN"T WORK!?
py.resample(in_raster_path_name,snap_raster,out_raster_path_name,resampling_type,mask) 


# distance measures   
snap_raster = 'G:/Faculty/Mann/Historic_BCM/Aggregated1080/aet1951oct.tif'
mask = "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"

poly_path = "G:/Faculty/Mann/Share/Fire Stations/FRAP 11_1/"
poly_name = "Fire_AirBase.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\air_dist.tif"  
maximum_distance = ""
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Campgrounds/"
poly_name = "CampGrounds.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\camp_dist.tif"  
maximum_distance = ""
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Fire Stations/FRAP 11_1/"
poly_name = "Fire_stations.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\station_dist.tif"
maximum_distance = ""
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/National Parks/"
poly_name = "nps_boundary.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\NPark.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Exclusion Layer/PublicLand_FRAP/Public Lands/"
poly_name = "PublicLand_FRAP.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\PubLand.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Exclusion Layer/"
poly_name = "All_Mrg_PubWatEasMil4.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\ExcldLand.tif"  
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/PopulatedPlaces_BTS/"
poly_name = "place.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\Ppall.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/PopulatedPlaces_BTS/"
poly_name = "place_20kplus.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\Pp20k.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/PopulatedPlaces_BTS/"
poly_name = "place_30kplus.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\Pp30k.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Incorporated Cities/"
poly_name = "IncorpCities2010.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\Incorp.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Exclusion Layer/Water_Census_CalAtlas_Join/"
poly_name = "Water_Join2.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\Water.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Roads/RoadsCensus2010_All/CACOUNTIES/"
poly_name = "Roads_Census2010_All.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\AllRoads.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Roads/RoadsCensus2010_All/CACOUNTIES/Roadsbytype/"
poly_name = "Prim_Secon_Join10v2.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\PrimSec.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

poly_path = "G:/Faculty/Mann/Share/Environemental Factors/Distance to Water/"
poly_name = "Water_CnAtlas_OceanBay_Disv3.shp"
out_raster_path_name ="G:\\Faculty\\Mann\\Other\\Fire\\OceanDistEuc.tif"
py.EucDistance(poly_path,poly_name,snap_raster,out_raster_path_name,maximum_distance,mask)

 
 

###################################################################################################
# # CALCULATE 10 YEAR ANNUAL STATISTICS  DONT USE 
#   library(raster)
#   library(rgdal)
#   library(snow)
#   input_path = "G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080"
#   years = seq(1951,2000)
#   months = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
#   variables = c("cwd","ppt","aet","pet","tmn","tmx")     #  *variable name*  with wildcards
#   
#   # Create path names
#   fullname = list()
#   for(variable in variables){
#       for(year in years){
#           for(month in months){
#              fullname = c( fullname, paste(variable,year,month,".tif",sep="") )    
#           }
#       }   
#   } 
# 
#   # Stack rasters   
#   decade.sum = list()
#   decade.mn = list()
#   decade.sd = list()
#   decade.max = list()
#   decade.min = list()
#   decade.rng = list()
# 
#   for(variable in variables[1]){
#     for(year in years[1:9]){
#        variable_annual=fullname[grepl(paste(variable, year, sep=''),fullname)]
#        print(paste('start',year))
#        # one variable one year
#        annual = raster::stack( paste(input_path,'\\',variable_annual,sep='') )
#        annual.sum = sum(annual)
#        annual.mn  = annual.sum / 12
#        annual.sd = calc( annual,sd )
#        annual.max = max( annual )
#        annual.min = min( annual )
#        annual.rng = range( annual )
#        # add annual stats to list 
#        decade.sum = c(decade.sum, annual.sum)
#        decade.mn = c(decade.mn, annual.mn)
#        decade.sd = c(decade.sd, annual.sd)
#        decade.max = c(decade.max, annual.max)
#        decade.min = c(decade.min, annual.min)
#        decade.rng = c(decade.rng, annual.rng)
#        print('end')
#     }   
#   }
#   # get mean of annual statistics
#   mean.annual.sum = mean(raster::stack(decade.sum))
#   mean.annual.mn  = mean(raster::stack(decade.mn))
#   mean.annual.sd  = mean(raster::stack(decade.sd))
#   mean.annual.max = mean(raster::stack(decade.max))
#   mean.annual.min = mean(raster::stack(decade.min))
#   mean.annual.rng = mean(raster::stack(decade.rng))


# change the names of files and rewrite them   
#   files = dir('G:\\Faculty\\Mann\\Other\\Fire\\')[grep('denmax', dir('G:\\Faculty\\Mann\\Other\\Fire\\'))]
#   files = files[-c(grep('tfw',files),grep('ovr',files),grep('xml',files),grep("denmax10x10_0125.tif",files))]
#   for(file in files){
#      
#     a = raster(paste('G:\\Faculty\\Mann\\Other\\Fire\\',file,sep=''))
#     parts = strsplit(as.character(file), split='_')
#     if(length(parts[[1]])!=3){
#       print('error')
#       break
#     }
#     name_new = paste(parts[[1]][1],parts[[1]][2],'_',parts[[1]][3],sep='')
#     writeRaster(a, paste('G:\\Faculty\\Mann\\Other\\Fire\\',name_new,sep=''), overwrite=T)
#   }




https://stat.ethz.ch/pipermail/r-help/2008-December/182806.html
> #aproximate prediction intervals with Poisson regression
  > fm_pois <- glm(art ~ fem, data = bioChemists, family = poisson)
> newdata <- na.omit(unique(bioChemists[, "fem", drop = FALSE]))
> prediction <- predict(fm_pois, newdata = newdata, se.fit = TRUE)
> ci <- data.frame(exp(prediction$fit + matrix(prediction$se.fit, ncol =
                                                 > 1) %*% c(-1.96, 1.96)))
> newdata$fit <- exp(prediction$fit)
> newdata <- cbind(newdata, ci)
> newdata$model <- "Poisson"
>
  > library(pscl)
> #aproximate prediction intervals with zero inflated poisson regression
  > fm_zip <- zeroinfl(art ~ fem | 1, data = bioChemists)
> fit <- predict(fm_zip)
> Pearson <- resid(fm_zip, type = "pearson")
> VarComp <- resid(fm_zip, type = "response") / Pearson
> fem <- bioChemists$fem
> bootstrap <- replicate(999, {
  >    yStar <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  >    predict(zeroinfl(yStar ~ fem | 1), newdata = newdata)
  > })
> newdata0 <- newdata
> newdata0$fit <- predict(fm_zip, newdata = newdata, type = "response")
> newdata0[, 3:4] <- t(apply(bootstrap, 1, quantile, c(0.025, 0.975)))
> newdata0$model <- "Zero inflated"
>
  > #compare the intervals in a nice plot.
  > newdata <- rbind(newdata, newdata0)
> library(ggplot2)
> ggplot(newdata, aes(x = fem, y = fit, min = X1, max = X2, colour =
                        > model)) + geom_point(position = position_dodge(width = 0.4)) +
  > geom_errorbar(position = position_dodge(width = 0.4))
>
  
