# Here is the documentation for the old pipeline in the calculation of EBVs

The goal here is to review all the steps in the old program to locate which parts are where and be able to discern which parts can safely be discarded as DMU and DMU trace will take over certain parts of it.

The files here are for the "mjolk" run. It seems to be needed to keep running parts of this in order for other traits than milk to be correct. Has something to do with data cleaning and such things - AFAIK it is fertility that is the culprit.

It is needed to document these old functions in order to see which parts of them are essential to port to a new pipeline and which parts can be safely be thrown out. 


## Function of this program
These programs are called from the command line in a specific order and some of them require editing before being run.

Name of program - input - output
1. Preppaprod.f - pedigree and observations from huppa - **Wxx0**
2. Codeherd.f - Wxx0 - wxx1 (used internally within function - **Wxx2**
3. allped. f Wxx2 - **Wxx2**
4. codeid - Wxx2 - **Rmain**
5. Rmain - Ped.txt
6. 
