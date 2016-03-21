
#  This script aggregates 270m data to 1080, masks outofstate and water
#  and creates time period  statistics 
#
#

exec('''
#################################################
print('Starting program')

# create mask for all data at 1080m
import arcpy
from arcpy.sa import *
from arcpy import env
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("spatial")

# Local variables:
#californiamaskraster270_tif = "F:\\Aggregated1080\\californiamaskraster270.tif"
#californiamaskraster1080_tif = "F:\\Aggregated1080\\californiamaskraster1080_ag.tif"

projection = "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]", "NEAREST", "270", "", "", "PROJCS['USA_Contiguous_Albers_Equal_Area_Conic_USGS_version',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-96.0],PARAMETER['Standard_Parallel_1',29.5],PARAMETER['Standard_Parallel_2',45.5],PARAMETER['Latitude_Of_Origin',23.0],UNIT['Meter',1.0]]"
#StatePoly = "C:\\Users\\mmann1123\\Desktop\\Share\\Boundary_Files\\StatePoly.shp"
#StatePolyTeale = "F:\\Resampled1080\\StatePolyTeale"

# Process: Polygon to Raster
#arcpy.PolygonToRaster_conversion(StatePoly, "OBJECTID", californiamaskraster270_tif, "MAXIMUM_AREA", "NONE", "270")

# Process: Resample
#arcpy.gp.Aggregate_sa(californiamaskraster270_tif, californiamaskraster1080_tif, "4", "MINIMUM", "EXPAND", "DATA")



###################################################
# set up environment
ca_mask =  "G:\\Faculty\\Mann\\Other\\Fire\\californiamaskraster1080_ag_NoWater4.tif"
arcpy.env.snapRaster = ca_mask                         # Set Snap Raster environment
arcpy.env.mask = ca_mask


###################################################
# get list of all files to resample
print('Starting resample')
import string
import re
env.workspace = "F:\\Historic_BCM\\TransientMonthlyFiles"    # F:\\TransientMonthlyFiles for historical     
# overwrite files 
arcpy.env.overwriteOutput = True
# List all ascii files 
asciis = arcpy.ListFiles("*.asc")
''')

def make_unique(seq):
   # Not order preserving    
   Set = set(seq)
   return list(Set)


#####################
## verify inputs ### ##MAKES SURE THE FILES WORK
env.workspace = "F:\\PCMA2_BCM"
for years in range(1950,2011):  #1950,2011  2001,2050
    print( years)
    year_i = arcpy.ListFiles("*"+str(years)+"*"  )
    year_i = list(set(year_i)-set(arcpy.ListFiles("*bor*"))-set(arcpy.ListFiles("*wy*"))-set(arcpy.ListFiles("*rch*"))-set(arcpy.ListFiles("*sbl*"))-set(arcpy.ListFiles("*pck*"))-set(arcpy.ListFiles("*chill*"))-set(arcpy.ListFiles("*mlt*"))-set(arcpy.ListFiles("*sub*"))-set(arcpy.ListFiles("*exc*"))-set(arcpy.ListFiles("*str*"))-set(arcpy.ListFiles("*snw*"))-set(arcpy.ListFiles("*run*"))-set(arcpy.ListFiles("*HST*"))-set(arcpy.ListFiles("*slp*"))-set(arcpy.ListFiles("*cov*"))-set(arcpy.ListFiles("*.ctl"))-set(arcpy.ListFiles("*wy*")) -set(arcpy.ListFiles("*HST*")) ) # removes all wy names from aet
    print(str(years)+":"+ str(len(year_i)))
    #looks ok all years have 72 asc files 

    ### find addition or missing 
    #years = 2007
    #year_2007 = arcpy.ListFiles("*"+str(years)+"*"  )
    #year_2007 = list(set(year_2007)-set(arcpy.ListFiles("*wy*"))-set(arcpy.ListFiles("*rch*"))-set(arcpy.ListFiles("*sbl*"))-set(arcpy.ListFiles("*pck*"))-set(arcpy.ListFiles("*chill*"))-set(arcpy.ListFiles("*mlt*"))-set(arcpy.ListFiles("*sub*"))-set(arcpy.ListFiles("*exc*"))-set(arcpy.ListFiles("*str*"))-set(arcpy.ListFiles("*snw*"))-set(arcpy.ListFiles("*run*"))-set(arcpy.ListFiles("*HST*"))-set(arcpy.ListFiles("*slp*"))-set(arcpy.ListFiles("*cov*"))-set(arcpy.ListFiles("*.ctl"))-set(arcpy.ListFiles("*wy*")) -set(arcpy.ListFiles("*HST*")) ) # removes all wy names from aet
    #for j in range(0,(len(year_2007) )):
    #    suber = year_2007[j]
    #    year_2007[j] =   suber[0:3]+suber[-7:-4]
    #    print(year_2007[j] )
    #year_2007.sort()    
    #
    #years = 2008
    #year_2008 = arcpy.ListFiles("*"+str(years)+"*"  )
    #year_2008 = list(set(year_2008)-set(arcpy.ListFiles("*wy*"))-set(arcpy.ListFiles("*rch*"))-set(arcpy.ListFiles("*sbl*"))-set(arcpy.ListFiles("*pck*"))-set(arcpy.ListFiles("*chill*"))-set(arcpy.ListFiles("*mlt*"))-set(arcpy.ListFiles("*sub*"))-set(arcpy.ListFiles("*exc*"))-set(arcpy.ListFiles("*str*"))-set(arcpy.ListFiles("*snw*"))-set(arcpy.ListFiles("*run*"))-set(arcpy.ListFiles("*HST*"))-set(arcpy.ListFiles("*slp*"))-set(arcpy.ListFiles("*cov*"))-set(arcpy.ListFiles("*.ctl"))-set(arcpy.ListFiles("*wy*")) -set(arcpy.ListFiles("*HST*")) ) # removes all wy names from aet
    #for j in range(0,(len(year_2008) )):
    #    suber = year_2008[j]
    #    year_2008[j] =   suber[0:3]+suber[-7:-4]
    #    print(year_2008[j] )
    #year_2008.sort()   
    #
    #set(year_2008) not in set(year_2007)
    #set(year_2008) - set(year_2007)  # looks like -set(arcpy.ListFiles("*bor*")) needs to be added 







   # THESE HAVE BEEN FIXED      SOMETIME YOU NEED TO REQUEST A BACKUP VERSION OF A FILE FROM THE FLINTS - THEY GET CORRUPTED. 
   #problem .  GFDLA2_BCM
   #       files =[   'ppt2003feb.asc', done d    
   #  'ppt2019feb.asc', done d
   #  'ppt2023jun.asc',done d 
   #  'ppt2023dec.asc',done d
   #  'ppt2025feb.asc',done d
   #  'ppt2025nov.asc',done d
   #  'ppt2027jan.asc',done d
   #  'ppt2027may.asc',done d
   #  'ppt2028mar.asc',done d
   #  'ppt2028aug.asc',done d
   #  'ppt2033mar.asc',done d
   #  'ppt2035oct.asc',done d
   #  'ppt2039oct.asc']done d
   #all other GFDLA2_BCM ppt is done with above exceptions
   #all other GFDLA2_BCM rch is done

   # "F:\\PCMA2_BCM" range(2001,2004) for rch missing : DONE!
   # "F:\\PCMA2_BCM" ppt all done 
   # Historic_BCM ppt all done
   # Historic_BCM rch all done




###############################
# CHANGE RESOLUTION 

for justonce in range(1):     # this allows me to run all aggregations at once 

    years_im_lookin_at= range(2001,2004+1) # 1950,2000 1950,2010  2001,2050
    env.workspace = "I:\\PCMA2_BCM" #I:\\Historic_BCM\\TransientMonthlyFiles "F:\\GFDLA2_BCM" # #"F:\\PCMA2_BCM"             #"F:\\Historic_BCM\\TransientMonthlyFiles"       
    months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"] #
     

  ###########
    ### tmx ###
     
    for year in years_im_lookin_at:  #
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "tmx" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "MEAN", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")
    
    

    ###########
    ### ppt ###

    for year in years_im_lookin_at:  #
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "ppt" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "SUM", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")
    

    ###########
    ### rch ###
 
    for year in years_im_lookin_at:   
      # Copy the features from the workspace to a folder
      for month in months:
          aet_not_other = "rch" + str(year) +str(month)+".asc"
          print(aet_not_other)
          arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
          output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
          print(output_path_name)
          arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "SUM", "EXPAND", "DATA")
          arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
          arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")


    
    ###########          
    ### aet ###

     for year in years_im_lookin_at:
       print('working on year: '+ str(year))
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "aet" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"  # "F:\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "MEAN", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")

    ###########
    ### cwd ###

    for year in years_im_lookin_at:   
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "cwd" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "MEAN", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")


    ###########
    ### pet ###
       
    for year in years_im_lookin_at:  #
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "pet" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "MEAN", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")
    


    ###########
    ### tmn ###
    
    for year in years_im_lookin_at:  #
       # Copy the features from the workspace to a folder
       for month in months:
           aet_not_other = "tmn" + str(year) +str(month)+".asc"
           print(aet_not_other)
           arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
           output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
           print(output_path_name)
           arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "MEAN", "EXPAND", "DATA")
           arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
           arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")


#       for use on a list of files 
 
       env.workspace = "I:\\GFDLA2_BCM"   
       for file1 in files:  #
              # Copy the features from the workspace to a folder
        
              aet_not_other = file1
              print(aet_not_other)
              arcpy.GetRasterProperties_management(aet_not_other,"CELLSIZEX")
              output_path_name = str(env.workspace)+"\\Aggregated1080\\" + string.split(aet_not_other,'.')[0] + ".tif"
              print(output_path_name)
              arcpy.gp.Aggregate_sa(aet_not_other, output_path_name, "4", "SUM", "EXPAND", "DATA")
              arcpy.GetRasterProperties_management(output_path_name,"CELLSIZEX")
              arcpy.DefineProjection_management(output_path_name, "PROJCS['NAD_1983_California_Teale_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',-4000000.0],PARAMETER['Central_Meridian',-120.0],PARAMETER['Standard_Parallel_1',34.0],PARAMETER['Standard_Parallel_2',40.5],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")
 
  
     

########################################################################################
########################################################################################


######################
### verify outputs ### 
env.workspace = "G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080"
months = ["*jan*","*feb*","*mar*","*apr*","*may*","*jun*","*jul*","*aug*","*sep*","*oct*","*nov*","*dec*"]

for years in range(1957,1958):   # 1950,2010+1 range()2001,2051
    print( years)
    print( ' year_i month_i    ==>   number of variables available for each year')
    year_i = arcpy.ListFiles("*"+str(years)+"*"  )
    year_i = list(set(year_i)-set(arcpy.ListFiles("*tfw*"))-set(arcpy.ListFiles("*xml*"))-set(arcpy.ListFiles("*bor*"))-set(arcpy.ListFiles("*wy*"))-set(arcpy.ListFiles("*HST*"))-set(arcpy.ListFiles("*slp*"))-set(arcpy.ListFiles("*cov*"))-set(arcpy.ListFiles("*.ctl"))-set(arcpy.ListFiles("*wy*")) -set(arcpy.ListFiles("*HST*")) ) # removes all wy names from aet
    print(str(years)+":"+ str(len(year_i)))
    for month in months:
        year_month_i = list(set(year_i) & set(arcpy.ListFiles("*"+str(month)+"*"  )))
        print(str(years)+":"+str(month)+ str(len(year_month_i)))
        print(year_month_i)
 
 

########################################################################################
########################################################################################


#############
### stats ###      1926-1950 1951-1975 1976-2000 2001-2025 2026-2050
# NOT USING!!!!!!! (OLD PERIOD DEFINITIONS) 1925-1949 1950-1974 1975-1999 2000-2024 2025-2049
#                

#  naming conventions> Historical: tmin1926_1950_sum  Future: tmin2070_2099_sum_PA2

exec('''
env.workspace = "G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080"
years = range(2001,2050 +1) #    1951,2000      # total range of years 

months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
variables = ["cwd","ppt","aet","pet","tmn","tmx"]     #  *variable name*  with wildcards
# build all combinations of names store in list tifnames
tifnames = []
i = 0
for variable in variables:
   for year in years:
      for month in months:
         fullname = str(variable)+str(year)+str(month)+".tif"
         tifnames.append(fullname)
         i = i+1 

from numpy  import *

# limit to years to decades of interest
base_year = [2001,2026]    #1951,1976   1926,  #   #2000-1976 =24
years_to_end = 24
lister = ["*cwd*","*ppt*","*aet*","*pet*","*tmn*","*tmx*"]     #  *variable name*  with wildcards   
months = ["*jan*","*feb*","*mar*","*apr*","*may*","*jun*","*jul*","*aug*","*sep*","*oct*","*nov*","*dec*"]
''')


#  DECADAL STATISTICS ON YEARLY DATA 
#  all statistics done each calendar year then averaged over 25 year period 
for variable in variables[0:1]:
    env.scratchWorkspace = "G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080\\Scratch.gdb"
    env.workspace =    "G:\\Faculty\\Mann\\GFDLA2_BCM\\Aggregated1080"                
    scenario =  "_GA2"   # "" for historical  "_PA2"
    # limit to current variable 
    relevant_tifnames = filter(lambda x: str(variable) in x,tifnames)
    print('FOR VARIABLE:'+str(variable))
    
    for year in base_year[0:1]:
        
        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
        # limit to time period of interest 
        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
        decadal_holder_Amn = [] 
        decadal_holder_Asd = [] 
        decadal_holder_Acov = [] 
        decadal_holder_Asum = [] 
        decadal_holder_Amax = [] 
        decadal_holder_Amin = [] 
        decadal_holder_Arng = []


        for year_i in range(year, (year+years_to_end+1)):
            print('working on year ',year_i)
            relevant_tifnames_year_i = filter(lambda x: str(year_i) in x,relevant_tifnames_year)
            print(relevant_tifnames_year_i)
            print('following should equal 1 ')
            print(len(relevant_tifnames_year_i)/ (12) ) 
            print('calculating annual group mean '+str(variable))
            mean = CellStatistics(relevant_tifnames_year_i, "MEAN", "DATA")
            decadal_holder_Amn.append(mean)
            print('calculating annual group std '+str(variable))
            sd = CellStatistics(relevant_tifnames_year_i, "STD", "DATA")
            decadal_holder_Asd.append(sd)
            print('saving decade '+str(variable))
            cov = sd/mean
            decadal_holder_Acov.append(cov)
            del(mean)
            del(sd)
            del(cov)
            sumer = CellStatistics(relevant_tifnames_year_i, "SUM", "DATA")
            decadal_holder_Asum.append(sumer)
            del(sumer)
            print('calculating annual group max '+str(variable))
            maxer = CellStatistics(relevant_tifnames_year_i, "MAXIMUM", "DATA")
            decadal_holder_Amax.append(maxer)
            del(maxer)
            print('calculating annual group min '+str(variable))
            miner = CellStatistics(relevant_tifnames_year_i, "MINIMUM", "DATA")
            decadal_holder_Amin.append(miner)
            del(miner)
            print('calculating annual group  range '+str(variable))
            ranger = CellStatistics(relevant_tifnames_year_i, "RANGE", "DATA")
            decadal_holder_Arng.append(ranger)
            del(ranger)

        # get average of annual statistics 
        print('calculating averages of annual statistics')
        Amean=CellStatistics(decadal_holder_Amn,  "MEAN", "DATA")
        Asd=  CellStatistics(decadal_holder_Asd,  "MEAN", "DATA")
        Acov= CellStatistics(decadal_holder_Acov, "MEAN", "DATA")
        Asum= CellStatistics(decadal_holder_Asum, "MEAN", "DATA")
        Amax= CellStatistics(decadal_holder_Amax, "MEAN", "DATA")
        Amin= CellStatistics(decadal_holder_Amin, "MEAN", "DATA")
        Arng= CellStatistics(decadal_holder_Arng, "MEAN", "DATA")
        del( decadal_holder_Amn,decadal_holder_Asd,decadal_holder_Acov,decadal_holder_Asum,decadal_holder_Amax,decadal_holder_Amin,decadal_holder_Arng )
        print('saving')

        Amean.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Aave'+scenario+'.tif')
        print('1')
        Asd.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Asd'+scenario+'.tif')
        print('1')
        Acov.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Acov'+scenario+'.tif')
        print('1')
        Asum.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Asum'+scenario+'.tif')
        print('1')
        Amax.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Amax'+scenario+'.tif')
        print('1')
        Amin.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Amin'+scenario+'.tif')
        print('1')
        Arng.save(str(env.workspace)+'\\Summary_Files2\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Arng'+scenario+'.tif')
        print('1')


      
#  MONTHLY AVERAGES 
#  all statistics based on all months for all years in 25 year period
#for variable in variables:
#    months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
#    # limit to current variable 
#    relevant_tifnames = filter(lambda x: str(variable) in x,tifnames)
#    print('FOR VARIABLE:'+str(variable))
#    
#    for year in base_year:
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        # limit to time period of interest 
#        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#        decadal_holder_Mmn = [] 
#        decadal_holder_Msd = [] 
#        decadal_holder_Mcov = [] 
#        decadal_holder_Msum = [] 
#        decadal_holder_Mmax = [] 
#        decadal_holder_Mmin = [] 
#        decadal_holder_Mrng = [] 
#
#        for month_i in months:
#            relevant_tifnames_month_i = filter(lambda x: str(month_i) in x,relevant_tifnames_year)
#            print(relevant_tifnames_month_i)
#            print('following should equal ',years_to_end+1)
#            print(len(relevant_tifnames_year)/ (12) ) 
#            print('calculating monthly group mean '+str(variable))
#            mean = CellStatistics(relevant_tifnames_month_i, "MEAN", "DATA")
#            decadal_holder_Mmn.append(mean)
# 
#        # get average of monthly averages statistics 
#        print('calculating stats for average of monthies statistics')
#
#        Mmean=CellStatistics(decadal_holder_Mmn, "MEAN", "DATA")
#        Msd=  CellStatistics(decadal_holder_Mmn, "STD", "DATA")
#        Mcov= Mmean /  Msd
#        Msum= CellStatistics(decadal_holder_Mmn, "SUM", "DATA")
#        Mmax= CellStatistics(decadal_holder_Mmn, "MAXIMUM", "DATA")
#        Mmin= CellStatistics(decadal_holder_Mmn, "MINIMUM", "DATA")
#        Mrng= CellStatistics(decadal_holder_Mmn, "RANGE", "DATA")
#        print('saving files')
#        Mmean.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Mave.tif')
#        Msd.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Msd.tif')
#        Mcov.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Mcov.tif')
#        Msum.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Msum.tif')
#        Mmax.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Mmax.tif')
#        Mmin.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Mmin.tif')
#        Mrng.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_Mrng.tif')

 
 #for variable in variables:
#    env.workspace = "F:/Historic_BCM/TransientMonthlyFiles/Aggregated1080/"     # this has to be here?!
#    # limit to current variable 
#    relevant_tifnames = filter(lambda x: str(variable) in x,tifnames)
#    print('FOR VARIABLE:'+str(variable))
#    
#    for year in base_year:
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        # limit to time period of interest 
#        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#
#        print('following should equal 1 ')
#        print(len(relevant_tifnames_year)/ (12*years_to_end) ) 
#        print('calculating decade group mean '+str(variable))
#        mean = CellStatistics(relevant_tifnames_year, "MEAN", "DATA")
#        print('saving decade '+str(variable))
#        mean.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_ave.tif')
#        print('calculating decade group std '+str(variable))
#        sd = CellStatistics(relevant_tifnames_year, "STD", "DATA")
#        print('saving decade '+str(variable))
#        sd.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_sd.tif')
#        cov = sd/mean
#        cov.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_cov.tif')
#        del(mean)
#        del(sd)
#        del(cov)
#        print('calculating decade group sum '+str(variable))
#        sumer = CellStatistics(relevant_tifnames_year, "SUM", "DATA")
#        print('saving decade '+str(variable))
#        sumer.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_sum.tif')
#        del(sumer)
#        print('calculating decade group max '+str(variable))
#        maxer = CellStatistics(relevant_tifnames_year, "MAXIMUM", "DATA")
#        print('saving decade '+str(variable))
#        maxer.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_max.tif')
#        del(maxer)
#        print('calculating decade group min '+str(variable))
#        miner = CellStatistics(relevant_tifnames_year, "MINIMUM", "DATA")
#        print('saving decade '+str(variable))
#        miner.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_min.tif')
#        del(miner)
#        print('calculating decade group  range '+str(variable))
#        ranger = CellStatistics(relevant_tifnames_year, "RANGE", "DATA")
#        print('saving decade '+str(variable))
#        ranger.save(str(env.workspace)+'\\Summary_Files\\'+variable+str(year)+"_"+str(year+years_to_end)+'_rng.tif')
#        del(ranger)
#
 
 
 
 
  
  ################################################################################
# create variables describing abnormal ppt and cwd events by counting # events +-1SD  (basis is ANNUAL)
# 
# Definitions: 
#  Evt
#  Ammcwd = count of number of annaul events with mean(cwd)-1/2SD 
#  Ampcwd = count of number of annaul events with mean(cwd)+1/2SD 
#  Apmcwd = count of number of annaul events with mean(cwd)-1/2SD followed by mean(cwd)+1/2SD
#  Ammppt
#  Ampppt
#  Apmppt = count of number of annaul events with mean(cwd)+1/2SD followed by mean(cwd)-1/2SD
#exec('''
#base_year = [ 2001,2026  ]    #1926,1951,1976  #  #1999-1975 =24  
#years_to_end = 24
#scenario = "_PA2"    # "" for historical
#env.scratchWorkspace = "G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080\\Scratch.gdb"
#
#years = range(2001,2050+1) #     range(1951,2000 +1) years = range(2001,2050+1)       # total range of years 
#months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
#variables = ["cwd","ppt","aet","pet","tmn","tmx"]     #  *variable name*  with wildcards
## build all combinations of names store in list tifnames
#tifnames = []
#i = 0
#for variable in variables:
#   for year in years:
#      for month in months:
#         fullname = str(variable)+str(year)+str(month)+".tif"
#         tifnames.append(fullname)
#         i = i+1 
#example_R = Raster('G:\\Faculty\\Mann\\Historic_BCM\\Aggregated1080\\cwd1997aug.tif')
#cellSize=example_R.meanCellHeight
#extent=example_R.extent
#arcpy.env.overwriteOutput = True
#''')



 # Works
#for variable in ['ppt','cwd' ]: #'ppt','cwd'
#    env.workspace = "G:\\Faculty\\Mann\\PCMA2_BCM\\Aggregated1080"    # where aggregated files are not summary  G:\\Faculty\\Mann\\GFDLA2_BCM\\Aggregated1080
#    # limit to current variable 
#    print('FOR VARIABLE:'+str(variable))
#    relevant_tifnames = filter(lambda x: str(variable) in x,tifnames)
#    for year in base_year:
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        mean_R = Raster(str(env.workspace)+'/Summary_Files/'+variable+str(year)+"_"+str(year+years_to_end)+'_Aave'+scenario+'.tif')
#        sd_R   = Raster(str(env.workspace)+'/Summary_Files/'+variable+str(year)+"_"+str(year+years_to_end)+'_Asd'+scenario+'.tif')
#        meanplus_R = Plus(mean_R, Times(sd_R,.5) ) # scale influence of SD                  #was this: not sure what for Times(Plus(mean_R, sd_R),.5)         
#        meanminus_R = Minus(mean_R, Times(sd_R,.5) )               #Times(Minus(mean_R , sd_R),.5)
#        
#        # for each period create holder for event counts 
#        meanplus_logic_sum_ALL =    Times(mean_R,0)        #       CreateConstantRaster(0, "FLOAT", cellSize, extent)
#        meanminus_logic_sum_ALL=   Times(mean_R,0)                         # old way : CreateConstantRaster(0, "FLOAT", cellSize, extent)
#        
#        # calculate mean for each year in period and store in list 
#        year_holder_Amn = [] 
#        for year_i in range(year, (year+years_to_end+1)):
#            print('working on year ',year_i)
#            relevant_tifnames_year_i = filter(lambda x: str(year_i) in x,relevant_tifnames)
#            print(relevant_tifnames_year_i)
#            print('following should equal 1 ')
#            print(len(relevant_tifnames_year_i)/ (12) ) 
#            print('calculating annual group mean '+str(variable))
#            mean = CellStatistics(relevant_tifnames_year_i, "MEAN", "DATA")
#            year_holder_Amn.append(mean)
#            
#        # compare annual means to period meanplus or mean minus 1sd store in logic holders
#        logic_holder_meanplus = [] 
#        logic_holder_meanminus= []
#        for year_holder_Amn_i in year_holder_Amn:
#            # count when elements of holder (annual means) are greater or less than the mean+-SD
#            meanplus_logic_i = year_holder_Amn_i>meanplus_R
#            meanminus_logic_i = year_holder_Amn_i<meanminus_R
#            
#            # store logic for finding years with above mean+SD followed by below mean-SD
#            logic_holder_meanplus.append(meanplus_logic_i)
#            logic_holder_meanminus.append(meanminus_logic_i)
#            
#            # sum logicals to count pixels with above below average+-SD
#            meanplus_logic_sum_ALL  = Plus(meanplus_logic_sum_ALL , meanplus_logic_i)
#            meanminus_logic_sum_ALL = Plus(meanminus_logic_sum_ALL , meanminus_logic_i)
#            print('loop next')  
#            
#        logic_holder_wet_next_dry = []
#        for i in range(0,(len(logic_holder_meanplus)-1 )  ):    # -1 b/c time leadused 
#            # find years with above mean+SD followed by below mean-SD
#            if(variable == "ppt"):
#                wet_now =  logic_holder_meanplus[i] == 1 
#                dry_then = logic_holder_meanminus[i+1] == 1
#                wet_next_dry = BooleanAnd(wet_now, dry_then)  # equals one if both wet_now==dry_then == 1 zero otherwise
#                logic_holder_wet_next_dry.append(wet_next_dry)
#            if(variable == 'cwd'): # inverse is true for water deficit
#                wet_now = logic_holder_meanminus[i] == 1 
#                dry_then = logic_holder_meanplus[i+1] == 1
#                wet_next_dry = BooleanAnd(wet_now, dry_then)  # equals one if both wet_now==dry_then == 1 zero otherwise
#                logic_holder_wet_next_dry.append(wet_next_dry)             
#                        
#        sum_wet_next_dry= CellStatistics(logic_holder_wet_next_dry, "SUM", "DATA")
#        
#        print('saving: might take a while')   
#       # env.workspace =    "C:/Users/mmann1123/Desktop/Climate Summary/"
#        meanplus_logic_sum_ALL.save( str(env.workspace)+'/Summary_Files/'+variable+str(year)+"_"+str(year+years_to_end)+"_Amp"+scenario+'.tif')
#        print('one done')
#        meanminus_logic_sum_ALL.save(str(env.workspace)+'/Summary_Files/'+variable+str(year)+"_"+str(year+years_to_end)+"_Amm"+scenario+'.tif')
#        print('one done')
#        sum_wet_next_dry.save(str(env.workspace)+'/Summary_Files/'+variable+str(year)+"_"+str(year+years_to_end)+"_Apm"+scenario+'.tif')
#        print('one done')
#  
#  
#  
#        print('saving: might take a while')      
#        meanplus_logic_sum_ALL.save(str(env.workspace)+'\\Summary_Files\\Amp'+variable+str(year)+"_"+str(year+years_to_end)+scenario+'.tif')
#        print('one done')
#        meanminus_logic_sum_ALL.save(str(env.workspace)+'\\Summary_Files\\Amm'+variable+str(year)+"_"+str(year+years_to_end)+scenario+'.tif')
#        print('one done')
#        sum_wet_next_dry.save(str(env.workspace)+'\\Summary_Files\\Apm'+variable+str(year)+"_"+str(year+years_to_end)+scenario+'.tif')
#        print('one done')





# Dont need stuff below:_________________________________________________________________


################################################################################
# create variables describing abnormal ppt and cwd events by counting # events +-1SD  
# (basis is MOTHLY )  year mean and sd compared to monthly variation 
# Definitions: 
#  Evt
#  mmcwd = count of number of events with mean(cwd)-1SD 
#  mpcwd = count of number of events with mean(cwd)+1SD 
#  pmcwd = count of number of events with mean(cwd)-1SD followed by mean(cwd)+1SD
#  mmppt
#  mpppt
#  pmppt = count of number of events with mean(cwd)+1SD followed by mean(cwd)-1SD
#example_R = Raster('F:\\Aggregated1080\\Summary_Files\\tmx1950_1974_ave.tif')
#cellSize=example_R.meanCellHeight
#extent=example_R.extent
# # Works
#for variable in ['ppt','cwd']:
#    # limit to current variable 
#    relevant_tifnames = filter(lambda x: str(variable) in x,tifnames)
#    print('FOR VARIABLE:'+str(variable))
#    
#    for year in base_year:
#        # limit to time period of interest 
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#        holder = [] 
#        meanplus_logic_i_A = CreateConstantRaster (0, "FLOAT", cellSize, extent)
#        meanminus_logic_i_A = CreateConstantRaster (0, "FLOAT", cellSize, extent)
#
#        for year_i in range(year,(year+years_to_end+1)):
#              # iterate through years calcuate mean and SD for each year
#              relevant_tifnames_year_i = [v for v in relevant_tifnames_year if int(re.sub('[^0-9]','',v) or 0) == year_i ]
#              print('calculating decade group mean '+str(variable))
#              mean_i = CellStatistics(relevant_tifnames_year_i, "MEAN", "DATA")
#              print('appending mean raster to list')
#              holder.append(mean_i)
#               
#        SD_of_annual_mean = CellStatistics(holder , "STD", "DATA")
#        MEAN_annual_mean = CellStatistics(holder , "MEAN", "DATA")
#        meanplus =  Plus(MEAN_annual_mean,SD_of_annual_mean)
#        meanminus = Minus(MEAN_annual_mean,SD_of_annual_mean)
#        logic_holder_meanplus = [] 
#        logic_holder_meanminus= []
#        for holder_i in holder:
#            # count when elements of holder (annual means) are greater or less than the mean+-SD
#            meanplus_logic_i = holder_i>meanplus
#            meanminus_logic_i = holder_i<meanminus
#            # sum logicals for areas with above below average+-SD
#            meanplus_logic_i_A = meanplus_logic_i_A + meanplus_logic_i
#            meanminus_logic_i_A = meanminus_logic_i_A + meanminus_logic_i
#            
#            # store logic for finding years with above mean+SD followed by below mean-SD
#            logic_holder_meanplus.append(meanplus_logic_i)
#            logic_holder_meanminus.append(meanminus_logic_i)
#        
#        logic_holder_wet_next_dry = []
#        for i in range(1,(len(logic_holder_meanplus)-1 )  ):  
#            # find years with above mean+SD followed by below mean-SD
#            if(variable == "ppt"):
#                wet_now = logic_holder_meanplus[i] == 1 
#                dry_then = logic_holder_meanminus[i+1] == 1
#                wet_next_dry = BooleanAnd(wet_now, dry_then)  # equals one if both wet_now==dry_then == 1 zero otherwise
#                logic_holder_wet_next_dry.append(wet_next_dry)
#            if(variable == 'cwd'): # inverse is true for water deficit
#                wet_now = logic_holder_meanminus[i] == 1 
#                dry_then = logic_holder_meanplus[i+1] == 1
#                wet_next_dry = BooleanAnd(wet_now, dry_then)  # equals one if both wet_now==dry_then == 1 zero otherwise
#                logic_holder_wet_next_dry.append(wet_next_dry)             
#                               
#        sum_wet_next_dry= CellStatistics(logic_holder_wet_next_dry, "SUM", "DATA")
#        
#        print('saving: might take a while')      
#        meanplus_logic_i_A.save(str(env.workspace)+'\\Summary_Files\\mp'+variable+str(year)+"_"+str(year+years_to_end)+'.tif')
#        meanminus_logic_i_A.save(str(env.workspace)+'\\Summary_Files\\mm'+variable+str(year)+"_"+str(year+years_to_end)+'.tif')
#        sum_wet_next_dry.save(str(env.workspace)+'\\Summary_Files\\pm'+variable+str(year)+"_"+str(year+years_to_end)+'.tif')


  

#################################################################################
# Calculate ave precip in coldest & hotest month
months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
# OUTPUTS = ave_ppt_tmn <- ave precip in coldest month
# 
# creates mean value of ppt for coldest month 
#for variable in ['ppt']:
#    # limit to current variable 
#    relevant_tifnames = filter(lambda x: 'tmn' in x,tifnames)
#    print('FOR VARIABLE:'+str(variable))
#    
#    for year in base_year:
#        # limit to time period of interest 
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#        holder_year_variable_at_min_temp = [] 
#            
#        for year_i in range(year,(year+years_to_end+1)):
#            print(str(year_i))
#            # iterate through years store month of coldest month
#            # store min values then use logical to find which month it occured
#            relevant_tifnames_year_i = [v for v in relevant_tifnames_year if int(re.sub('[^0-9]','',v) or 0) == year_i ]
#            min_temp_year_i = CellStatistics(relevant_tifnames_year_i, "MINIMUM", "DATA")
#            holder_monthly_logic_times_variable = [] 
#            for month_j in months:
#                # get variable for month 
#                variable_tifnames_year_month_j = filter(lambda x: str(variable)+str(year_i)+str(month_j)+'.tif' in x,tifnames)
#                tmn_year_month_j = Raster(str(env.workspace)+'\\'+ filter(lambda x:  str(year_i)+str(month_j)+'.tif' in x,relevant_tifnames_year_i)[0]  )
#                monthly_logic = tmn_year_month_j == min_temp_year_i     # is the month equal to the coldest of the year for any pixels 
#                variable_year_month_j = Raster(str(env.workspace)+'\\'+str(variable)+str(variable_tifnames_year_month_j[0][3:] ) )
#                monthly_logic_times_variable = monthly_logic * variable_year_month_j  # variable times 1 if lowest temp and * 0 if not
#                holder_monthly_logic_times_variable.append(monthly_logic_times_variable)
#                del(monthly_logic_times_variable)
#            
#            year_variable_at_min_temp = CellStatistics(holder_monthly_logic_times_variable, "SUM", "DATA")    # sum for all months average temp times 1 if lowest temp and #* 0 if not
#            holder_year_variable_at_min_temp.append(year_variable_at_min_temp)   
#            del(year_variable_at_min_temp) 
#        year_variable_at_min_temp = CellStatistics(holder_year_variable_at_min_temp, "MEAN", "DATA")   
#        print('saving might take a while')
#        year_variable_at_min_temp.save(str(env.workspace)+'\\Summary_Files\\ave_'+variable+'_tmn'+str(year)+"_"+str(year+years_to_end)+'.tif')   

 
# creates mean value of ppt for warmest month 
# OUTPUTS = ave_ppt_tmx <- ave precip in warmest month
#for variable in ['ppt']:
#    # limit to current variable 
#    relevant_tifnames = filter(lambda x: 'tmx' in x,tifnames)
#    print('FOR VARIABLE:'+str(variable))
#    
#    for year in base_year:
#        # limit to time period of interest 
#        print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#        relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#        holder_year_variable_at_max_temp = [] 
#            
#        for year_i in range(year,(year+years_to_end+1)):
#            print(str(year_i))
#            # iterate through years store month of coldest month
#            # store min values then use logical to find which month it occured
#            relevant_tifnames_year_i = [v for v in relevant_tifnames_year if int(re.sub('[^0-9]','',v) or 0) == year_i ]
#            max_temp_year_i = CellStatistics(relevant_tifnames_year_i, "MINIMUM", "DATA")
#            holder_monthly_logic_times_variable = [] 
#            for month_j in months:
#                # get variable for month 
#                variable_tifnames_year_month_j = filter(lambda x: str(variable)+str(year_i)+str(month_j)+'.tif' in x,tifnames)
#                tmx_year_month_j = Raster(str(env.workspace)+'\\'+ filter(lambda x:  str(year_i)+str(month_j)+'.tif' in x,relevant_tifnames_year_i)[0]  )
#                monthly_logic = tmx_year_month_j == max_temp_year_i     # is the month equal to the coldest of the year for any pixels 
#                ppt_year_month_j = Raster(str(env.workspace)+'\\'+str(variable)+str(variable_tifnames_year_month_j[0][3:] ) )
#                monthly_logic_times_variable = monthly_logic * ppt_year_month_j  # variable times 1 if lowest temp and * 0 if not
#                holder_monthly_logic_times_variable.append(monthly_logic_times_variable)
#                del(monthly_logic_times_variable)
#            
#            year_variable_at_max_temp = CellStatistics(holder_monthly_logic_times_variable, "SUM", "DATA")    # sum for all months average temp times 1 if lowest temp and #* 0 if not
#            holder_year_variable_at_max_temp.append(year_variable_at_max_temp)   
#            del(year_variable_at_max_temp) 
#        year_variable_at_max_temp = CellStatistics(holder_year_variable_at_max_temp, "MEAN", "DATA")   
#        print('saving might take a while')
#        year_variable_at_max_temp.save(str(env.workspace)+'\\Summary_Files\\ave_'+variable+'_tmx'+str(year)+"_"+str(year+years_to_end)+'.tif')   

  
 
 
 #################################################################################
# creates  mean temp mean(max and min temp) in the wettest month   
#months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]

#OUTPUT = ave_tmp_pptmx
#relevant_tifnames = filter(lambda x: 'ppt' in x,tifnames)
#print('FOR VARIABLE:'+'ppt')

#for year in base_year:
#    # limit to time period of interest 
#    print('limiting to '+str(year)+' to '+ str((year+years_to_end)))
#    relevant_tifnames_year = [v for v in relevant_tifnames if int(re.sub('[^0-9]','',v) or 0) >= year and int(re.sub('[^0-9]','',v) or 0) <= year+years_to_end ]
#    holder_year_variable_at_wettest_month = [] 
#        
#    for year_i in range(year,(year+years_to_end+1)):
#        print(str(year_i))
#        # iterate through years  
#        # store min values then use logical to find which month it occured
#        relevant_tifnames_year_i = [v for v in relevant_tifnames_year if int(re.sub('[^0-9]','',v) or 0) == year_i ]
#        # max ppt in year_i 
#        max_ppt_year_i = CellStatistics(relevant_tifnames_year_i, "MAXIMUM", "DATA")
#        holder_monthly_logic_times_variable = [] 
#        for month_j in months:
#            # get ppt for month and compare to maximum annual ppt
#            ppt_tifnames_year_month_j = filter(lambda x: 'ppt'+str(year_i)+str(month_j)+'.tif' in x,tifnames)
#            raster_j = Raster(str(env.workspace)+'\\'+str(ppt_tifnames_year_month_j[0]))
#            monthly_logic = raster_j == max_ppt_year_i     # is the month equal to the wettest of the year for any pixels 
#            # get tmn tmx for month and year 
#            tmn_tifnames_year_month_j = filter(lambda x: 'tmn'+str(year_i)+str(month_j)+'.tif' in x,tifnames)
#            tmx_tifnames_year_month_j = filter(lambda x: 'tmx'+str(year_i)+str(month_j)+'.tif' in x,tifnames)
#            # if month is wettest store (tmn+tmx)/2
#            tmn_year_month_j = Raster(str(env.workspace)+'\\'+ tmn_tifnames_year_month_j[0]  )
#            tmx_year_month_j = Raster(str(env.workspace)+'\\'+ tmx_tifnames_year_month_j[0]  )
#            monthly_logic_times_variable = monthly_logic * ((tmn_year_month_j+tmx_year_month_j)/2)  # average temp times 1 if lowest temp and * 0 if not
#            holder_monthly_logic_times_variable.append(monthly_logic_times_variable)
#            del(monthly_logic_times_variable)
#        # for all months sum up mean temp for wettest month (only one month is choosen as wettest using logic above)      
#        year_tave_at_max_ppt = CellStatistics(holder_monthly_logic_times_variable, "SUM", "DATA")    # sum for all months average temp times 1 if lowest temp and * 0 if #not
#        holder_year_variable_at_wettest_month.append(year_tave_at_max_ppt)   
#        del(year_tave_at_max_ppt) 
#    year_tave_at_max_ppt = CellStatistics(holder_year_variable_at_wettest_month, "MEAN", "DATA")   
#    print('saving might take a while')
#    year_tave_at_max_ppt.save(str(env.workspace)+'\\Summary_Files\\ave_tmp_pptmx'+str(year)+"_"+str(year+years_to_end)+'.tif')   


## change raster names for scenarios if needed... 
import os
env.workspace = "F:\\Aggregated1080\\Summary_Files"
lister = arcpy.ListFiles('*.tif')

for i in range(1,(len(lister)+1) ):
    print('working on '+ str(i))
    a = Raster(lister[i])
    a.save("F:\\Aggregated1080\\Summary_Files2\\"+os.path.splitext(lister[i])[0]+'_GA2.tif')
#################################################################################
#        # spring variables  Jan - April 
#        v1_not_other_spring =  set(v1_not_other2)
#        for month_i in months[5:12]:
#            v1_not_other_spring = set(v1_not_other_spring)-set(arcpy.ListFiles(str(month_i)))     # removes all wy names from aet
#
#        print('following should be round number')
#        print(len(v1_not_other_spring)/ 24 ) 
#        print('calculating spring mean '+str(variable))
#        mean = CellStatistics (v1_not_other_spring, "MEAN", "DATA")
#        mean.save(variable[1:-1]+"spr"+str(j)+"_"+str(j+years_to_end)+'_ave.tif')
#        print('calculating spring std '+str(variable))
#        sd = CellStatistics (v1_not_other_spring, "STD", "DATA")
#        sd.save(variable[1:-1]+"spr"+str(j)+"_"+str(j+years_to_end)+'_sd.tif')
#        print('calculating spring sum '+str(variable))
#        sumer = CellStatistics (v1_not_other_spring, "SUM", "DATA")
#        sumer.save(variable[1:-1]+"spr"+str(j)+"_"+str(j+years_to_end)+'_sum.tif')
#
#
#        # fseason variables    May - Dec
#        v1_not_other_fseason =  set(v1_not_other2)
#        for month_i in months[0:4]:
#            v1_not_other_fseason = set(v1_not_other_fseason)-set(arcpy.ListFiles(str(month_i)))     # removes all wy names from aet
#
#        print('following should be round number')
#        print(len(v1_not_other_fseason)/ 24 ) 
#        print('calculating fire season mean '+str(variable))
#        mean = CellStatistics (v1_not_other_fseason, "MEAN", "DATA")
#        mean.save(variable[1:-1]+"fse"+str(j)+"_"+str(j+years_to_end)+'_ave.tif')
#        print('calculating fire season std '+str(variable))
#        sd = CellStatistics (v1_not_other_fseason, "STD", "DATA")
#        sd.save(variable[1:-1]+"fse"+str(j)+"_"+str(j+years_to_end)+'_sd.tif')
#        print('calculating fire season sum '+str(variable))
#        sumer = CellStatistics (v1_not_other_fseason, "SUM", "DATA")
#        sumer.save(variable[1:-1]+"fse"+str(j)+"_"+str(j+years_to_end)+'_sum.tif')




 
 
  


# get directory elements
directory = os.listdir("F:\\TransientMonthlyFiles")


# create grep that looks for partial match
exec('''
import os
import re
def grep(string,list):
    expr = re.compile(string)
    return filter(expr.search,list)
''')
#
lists = ['.asc','aet']
a=grep(directory,lists)
 

# Two alternate solutions, courtesy of Gene H
# These are more elegant, and more detailed descriptions can be found here:
# http://www.faqs.org/docs/diveintopython/apihelper_filter.html
# http://www.faqs.org/docs/diveintopython/regression_filter.html
#
#def grep(string,list):
#    expr = re.compile(string)
#    return [elem for elem in list if expr.match(elem)]
#
#def grep(string,list):
#    expr = re.compile(string)
#    return filter(expr.search,list)

# Note a subtle difference between the above two:
# because the first uses expr.match(), it will only return exact matches
# while the latter will return a list containing strings that contain the
# search string in any position
# e.g.:
# list = ['normalize','size','nonzero','zenith']
# grep('ze',list)
# First one returns: ['zenith']
# Second one returns: ['normalize','size','nonzero','zenith']
