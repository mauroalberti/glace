# glace

Fortran 95 tools for the processing and analysis of ICESat data

*ReadGLA12*: reads a set of input GLA12 files, stored in rel. 34 format (the current one as of 2016-04-30).

*HeightVarGla*: calculates height differences between elevation pairs within a user-defined search radius. 


## Compilation

Since data in GLA12 files are in big-endian notation (the Unix default), when working in Windows you must compile the program using the big-endian data option. 
For instance, with the free g95 compiler, the command line should include the "-fendian=BIG" option, e.g.: 

```
 > g95 ReadGLA12_v5.0.f95 -fendian=BIG -o readgla12.exe
 > g95 HeightVarGLA_v1.0.f95 -fendian=BIG -o heightvar.exe
```

## ReadGLA12

This Fortran console program reads data from a set of binary ICESat files, filters them according to user defined geographic, temporal and quality settings and writes the resulting records into both ascii files, for GIS import as point layers, and also a binary file, to be used with the "HeightVarGLA" program for the detection of height differences between neighbouring elevations).

The original version of the algorithm is described in Alberti & Biscaro (2010).

### Program input

The program input is constituted by a set of GLA12 files, plus a pair of text files, a former listing the GLA12 files to analyse and a latter a set of analysis parameters. All this files ahve to be stored in a single directory reachable by the program (for instance change the working directory to this directory before running the exe file)

#### List of GLA12 files

The list of GLA12 binary files to read is composed by one record for line, e.g.: 

> GLA12_03022021_r2973_428_L1A.P1387_01_00 
> GLA12_03092517_r2973_428_L2A.P1387_01_00 
> GLA12_03100413_r2973_428_L2A.P1387_01_00 
> GLA12_04021721_r2973_428_L2B.P1387_01_00 
> GLA12_04051816_r2973_428_L2C.P1387_01_00 
> GLA12_04100321_r2973_428_L3A.P1387_01_00 
> GLA12_05021715_r2973_428_L3B.P1387_01_00 
> GLA12_05052016_r2973_428_L3C.P1387_01_00  
> ......


#### Analysis parameters

An example of parameters is:

> -90 90  
> -180 180  
> 2005 1 1 2009 1 1  
> 300 100  

where: 

> -90 90 # minimum and maxixum latitude  
> -180 180 # minimum and maxixum longitude  
> 2005 1 1 2009 1 1 # time window: initial and final year month day; if not required: -1 as first value  
> 300 100 # filters on saturation elevation correction and gain value; when not desired substitute value with -1 (e.g. 300 -1 or -1 100 or -1 -1 )   


### Program output

The output consists of a set of ascii files for GIS import, each one corresponding to an input Glas binary file, and a binary file for height variation analysis with the HeightVarGLA program. 

You can find further methodology details in Alberti & Biscaro (2010).

## Height variations in nearest measures

This algorithm calculates height differences between elevation pairs within a user-defined search radius. Suggested values are between 50 and 500-1 000 m, depending on the desired output resolution and on the slope and roughness of the topographic surfaces.

The user can choose to output results only within a predefined range of the elevation differences, e.g., all pairs with height differences between 0.5 m and 5 m. 

More details are in Alberti & Biscaro (2010).

### Program input	
			
The input for the elaborations consists of one or two binary files (with .dat extension) created by the ReadGLA12 program, storing data for a specific geographic domain acquired in a single time interval (when using one input file) or in two different time intervals (two input files). 

### Program output	

Filtered-in pairs are written in an output text file that can be easily imported into GIS software. A metadata file stores information about the analysis session and analysed tracks (e.g., time, input files, used thresholds on distance and elevation differences, and summary results for each track). 

## References

Alberti, M., Biscaro, D., 2010. Height variation detection in polar regions from ICESat satellite altimetry. Computers & Geosciences 36, 1-9.

<i>GLA 12 Records: Release 34</i> at <a href="http://nsidc.org/data/docs/daac/glas_altimetry/gla12_records_r34.html">http://nsidc.org/data/docs/daac/glas_altimetry/gla12_records_r34.html</a>

