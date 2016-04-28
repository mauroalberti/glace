# glace
Tools for processing and analysis of ICESat data

ReadGLA12: reads a set of input GLA12 files.

The Fortran 90/95 algorithm reads data from a set of binary ICESat files, filters them according to user defined geographic, temporal and quality settings and writes the resulting records into ascii files (for GIS import as point layers) and binary files (for analysis with the algorithm for the detection of height differences between neighbouring elevations).	The original version of the algorithm is described in Alberti & Biscaro (2010).

Language: Fortran 90/95
Compiler: g95 - Windows
Version: 2010-06-26
Code: readgla12.zip

Since data in GLA12 files are in big-endian notation (the Unix default), when working in Windows you must compile the program using the big-endian data option. 
For instance, with the free g95 compiler, the command line should include the "-fendian=BIG" option, e.g.: 
  prompt> g95 ReadGLA12_v5.0.f95 -fendian=BIG -o readgla12.exe

Two ascii file, the former with the list of GLA12 binary files to read (one record for line), e.g.: 

GLA12_03022021_r2973_428_L1A.P1387_01_00 
GLA12_03092517_r2973_428_L2A.P1387_01_00 
GLA12_03100413_r2973_428_L2A.P1387_01_00 
GLA12_04021721_r2973_428_L2B.P1387_01_00 
GLA12_04051816_r2973_428_L2C.P1387_01_00 
GLA12_04100321_r2973_428_L3A.P1387_01_00 
GLA12_05021715_r2973_428_L3B.P1387_01_00 
GLA12_05052016_r2973_428_L3C.P1387_01_00 
...... 

the latter with the analysis parameters, e.g.: 

-90 90 
-180 180 
2005 1 1 2009 1 1 
300 100 

where: 

-90 90 #minimum and maxixum latitude 
-180 180 #minimum and maxixum longitude 
2005 1 1 2009 1 1 #time window: initial and final year month day; if not required: -1 as first value 
300 100 #filters on saturation elevation correction and gain value; when not desired substitute value with -1 (e.g. 300 -1 or -1 100 or -1 -1 ) 

A set of ascii files for GIS import, each one corresponding to an input Glas binary file, and a binary file for height variation analysis with the HeightVarGLA_v1.0 program. You can find further methodology details in Alberti & Biscaro (2010).


References

Alberti, M., Biscaro, D., 2010. Height variation detection in polar regions from ICESat satellite altimetry. Computers & Geosciences 36, 1-9.




