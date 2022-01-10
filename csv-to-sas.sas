libname datap 'D:\repos\project_repos\tools_sim-dkreg\simdata';
libname raw 'D:\OneDrive\Aalborg Universitet\CALDISS_commons - Documents\workshop_håndtering-registerdata_nov21\sas_regdat';
run;

proc import datafile="D:\repos\project_repos\tools_sim-dkreg\simdata\dream_sim_2015.csv"
     out=raw.dream_sim_2015
     dbms=csv
     replace;
     getnames=yes;
run;

proc import datafile="D:\repos\project_repos\tools_sim-dkreg\simdata\dream_sim_y_9715.csv"
     out=raw.dream_sim_y_9715
     dbms=csv
     replace;
     getnames=yes;
run;
