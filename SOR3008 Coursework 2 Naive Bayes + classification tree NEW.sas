/* Creating binary for obese */

proc freq data=SOR3008.obesity;
    tables NObeyesdad;
run;

data SOR3008.obesity2;
    set SOR3008.obesity; /* Replace with your actual dataset name if different */
    length Is_Obese $3;
    if NObeyesdad in ('insufficient_weight', 'normal_weight', 'Overweight_Level_I', 'Overweight_Level_II') then Is_Obese = 'no';
    else Is_Obese = 'yes';
run;

proc freq data=SOR3008.obesity2;
    tables is_obese;
run;


/* 2. Create training (70%), validation (10%), and test (20%) sets
       - Set the seed to 3008  */

data SOR3008.obesity2;
   set SOR3008.obesity2;
   /* Generate a random uniform number using seed 3008 */
   u = ranuni(3008);
run;

data train valid test;
   set SOR3008.obesity2;
   if u < 0.70 then output train;
   else if u < 0.80 then output valid;
   else output test;
run;

/* 3.Export the training, validation, and test sets to CSV files */

proc export data=train outfile="/home/u63763360/SOR3008Files/train.csv" dbms=csv replace;
run;

proc export data=valid outfile="/home/u63763360/SOR3008Files/valid.csv" dbms=csv replace;
run;

proc export data=test outfile="/home/u63763360/SOR3008Files/test.csv" dbms=csv replace;
run;

/*4. Build a Classification Tree using PROC HPSPLIT
       - Uses the training set. */
      
proc contents data=SOR3008.obesity;
run;

proc hpsplit data=train seed=3008;

   class Gender CAEC CALC family_history_with_overweight FAVC SMOKE MTRANS SCC Is_obese; 
   
   model Is_obese = MTrans CALC TUE FAF SCC CH2O SMOKE CAEC NCP FCVC FAVC family_history_with_overweight Weight Height Age Gender ;
   prune costcomplexity;
   output out=tree_scored;
run;

proc contents data=train;
run;


proc hpbnet data=train structure=Naive;
   target Is_obese;
   input MTrans CALC TUE FAF SCC CH2O SMOKE CAEC NCP FCVC FAVC family_history_with_overweight Weight Height Age Gender;
   output network=net varselect=vs varlevel=varl parameter=parm fit=fitstats pred=prediction;
run;

/* Assessing the output of the SAS Naive Bayes procedure */
proc contents data=prediction;
run;



proc contents data=scored;
run;

data scored;
   set prediction;
   
   /* Find the maximum probability */
   maxval = max("P_Is_ObeseYES"n, "P_IS_ObeseNO"n);
   
   /* Assign class based on which variable matches maxval */
   if maxval = "P_Is_ObeseYES"n then predicted_class = 'yes';
   else if maxval = "P_Is_ObeseNO"n then predicted_class = 'no';

run;

/* Step 1: Compute Confusion Matrix */
proc freq data=scored;
   tables Is_Obese * predicted_class / norow nocol nopercent;
   title "Confusion Matrix for Bayesian Network";
run;

/* Step 2: Compute Accuracy */
proc sql;
    select sum(case when Is_obese = predicted_class then 1 else 0 end) / count(*) as Accuracy
    from scored;
quit;

/*Loglikelihood*/

data log_likelihood;
    set scored;
    
    /* Assign the predicted probability corresponding to the actual class */
    if Is_Obese = 'yes' then log_prob = log("P_Is_ObeseYES"n);
    else if Is_Obese = 'no' then log_prob = log("P_Is_ObeseNO"n);
run;

/* Sum up the log probabilities */
proc means data=log_likelihood sum;
    var log_prob;
    title "Log-Likelihood of the Bayesian Network Model";
run;

/* Step 4: Compute and Plot ROC Curve */
proc logistic data=scored plots=roc plots(maxpoints=none);
   model Is_Obese(event='yes') = "P_Is_ObeseYES"n / outroc=roc_data;
run;



proc sgplot data=roc_data;
   series x=_FALPOS_ y=_POS_ / markers lineattrs=(thickness=2 color=blue);
   xaxis label="False Positive Rate (Specificity)";
   yaxis label="True Positive Rate (Sensitivity)";
   title "ROC Curve for Bayesian Network";
run;


