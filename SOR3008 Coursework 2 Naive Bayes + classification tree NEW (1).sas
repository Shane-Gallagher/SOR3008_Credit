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
       - Set the seed to 3008 - Don't need this code as I have imported the datasets from R already 

data SOR3008.obesity2;
   set SOR3008.obesity2;
   u = ranuni(3008);
run;

data train valid test;
   set SOR3008.obesity2;
   if u < 0.70 then output train;
   else if u < 0.80 then output valid;
   else output test;
run;


proc export data=train outfile="/home/u63763360/SOR3008Files/train.csv" dbms=csv replace;
run;

proc export data=valid outfile="/home/u63763360/SOR3008Files/valid.csv" dbms=csv replace;
run;

proc export data=test outfile="/home/u63763360/SOR3008Files/test.csv" dbms=csv replace;
run;

*/

/*4. Build a Classification Tree using PROC HPSPLIT
       - Uses the training set. */
      
proc contents data=SOR3008.obesity;
run;

proc hpsplit data=SOR3008.train seed=3008;

   class Gender CAEC CALC family_history_with_overweight FAVC SMOKE MTRANS SCC Is_obese; 
   
   model Is_obese = MTrans CALC TUE FAF SCC CH2O SMOKE CAEC NCP FCVC FAVC family_history_with_overweight Weight Height Age Gender ;
   prune costcomplexity;
   output out=tree_scored;
run;

proc contents data=SOR3008.train;
run;

proc freq data=SOR3008.train;
   tables Is_obese;
run;

proc freq data=SOR3008.test;
   tables Is_obese;
run;

data SOR3008.train_weighted;
   set SOR3008.train;
   /* Suppose we want obese cases to count 3 times, and non‐obese just once */
   if Is_Obese = 'no' then freq_weight = 3;
   else if Is_obese = 'yes' then freq_weight = 1;
run;

proc contents data=SOR3008.train_weighted;
run;

/*Freq statement doesn't work, instead we will manually insert the threshold in the assessment */

proc hpbnet data=SOR3008.train structure=Naive;
   target Is_obese;
   input MTrans CALC TUE FAF SCC CH2O SMOKE CAEC NCP FCVC FAVC family_history_with_overweight Weight Height Age Gender;
   output network=net varselect=vs varlevel=varl parameter=parm fit=fitstats pred=prediction;
   code file = "/home/u63763360/SOR3008Files/scoring_code.sas";
run;



/* Assessing the output of the SAS Naive Bayes procedure */
proc contents data=prediction;
run;


data SOR3008.scored_test;
   set SOR3008.test;
   %include "/home/u63763360/SOR3008Files/scoring_code.sas";
run;


data SOR3008.scored_test;
   set SOR3008.scored_test;
   
   /* Assign class based on the probability threshold (freq of obese is 72% in the training data) */
   if "P_Is_ObeseYES"n >= 0.72 then predicted_obese = 'yes';
   else predicted_obese = 'no';

run;

/* Step 1: Compute Confusion Matrix */
proc freq data=SOR3008.scored_test;
   tables Is_Obese * predicted_obese / norow nocol nopercent;
   title "Confusion Matrix for Bayesian Network";
run;

/* Step 2: Compute Accuracy */
proc sql;
    select sum(case when Is_obese = predicted_obese then 1 else 0 end) / count(*) as Accuracy
    from SOR3008.scored_test;
quit;

/*Loglikelihood*/

data log_likelihood;
    set SOR3008.scored_test;
    
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
proc logistic data=SOR3008.scored_test plots=roc plots(maxpoints=none);
   model Is_Obese(event='yes') = "P_Is_ObeseYES"n / outroc=roc_data;
run;



proc sgplot data=roc_data;
   series x=_FALPOS_ y=_POS_ / markers lineattrs=(thickness=2 color=blue);
   xaxis label="False Positive Rate (Specificity)";
   yaxis label="True Positive Rate (Sensitivity)";
   title "ROC Curve for Bayesian Network";
run;


