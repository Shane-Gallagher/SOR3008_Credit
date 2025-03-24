/*  1. Import the dataset  */

proc import datafile="credit_joined.csv" 
    out=credit_data
    dbms=csv 
    replace;
   getnames=yes;
run;

/* Classifying the status variable, using STATUS_NUM instead of STATUS */

data SOR3008.credit_data_num;
   set SOR3008.credit_data;
   if STATUS = 'C' then STATUS_NUM = -1; /* Paid off */
   else if STATUS = 'X' then STATUS_NUM = .; /* Missing or No Loan */
   else STATUS_NUM = input(STATUS, 8.); /* Convert numeric parts */
run;

/* 2. Create training (70%), validation (10%), and test (20%) sets
       - Set the seed to 3008  */

data SOR3008.credit_data_num;
   set SOR3008.credit_data_num;
   /* Generate a random uniform number using seed 3008 */
   u = ranuni(3008);
run;

data train valid test;
   set SOR3008.credit_data_num;
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

proc hpsplit data=train seed=3008;

   class STATUS_NUM CODE_GENDER FLAG_OWN_CAR FLAG_OWN_REALTY NAME_INCOME_TYPE NAME_EDUCATION_TYPE
   NAME_FAMILY_STATUS NAME_HOUSING_TYPE OCCUPATION_TYPE; 
   
   model STATUS_NUM = AMT_INCOME_TOTAL CNT_CHILDREN CNT_FAM_MEMBERS CODE_GENDER DAYS_EMPLOYED
   FLAG_OWN_CAR FLAG_OWN_REALTY MONTHS_BALANCE NAME_EDUCATION_TYPE NAME_HOUSING_TYPE
   NAME_INCOME_TYPE OCCUPATION_TYPE NAME_FAMILY_STATUS;
   prune costcomplexity;
   output out=tree_scored;
run;

proc contents data=train;
run;


proc hpbnet data=train structure=Naive;
   target STATUS_NUM;
   id ID;
   input AMT_INCOME_TOTAL CNT_CHILDREN CNT_FAM_MEMBERS CODE_GENDER DAYS_EMPLOYED
       FLAG_OWN_CAR FLAG_OWN_REALTY MONTHS_BALANCE NAME_EDUCATION_TYPE NAME_HOUSING_TYPE
       NAME_INCOME_TYPE OCCUPATION_TYPE NAME_FAMILY_STATUS;
   output network=net varselect=vs varlevel=varl parameter=parm fit=fitstats pred=prediction;
run;

/* Assessing the output of the SAS Naive Bayes procedure */

data scored;
   set prediction;
   /* Create a predicted class variable based on a threshold of 0.5 */
   if _POST_ >= 0.5 then pred_class = 1;
   else pred_class = 0;
run;

proc contents data=scored;
run;

data scored;
   set scored;
   
   /* Find the maximum probability */
   maxval = max("P_STATUS_NUM-1"n, "P_STATUS_NUM0"n, "P_STATUS_NUM1"n, 
                "P_STATUS_NUM2"n, "P_STATUS_NUM3"n, "P_STATUS_NUM4"n, "P_STATUS_NUM5"n);
   
   /* Assign class based on which variable matches maxval */
   if maxval = "P_STATUS_NUM-1"n then predicted_class = -1;
   else if maxval = "P_STATUS_NUM0"n then predicted_class = 0;
   else if maxval = "P_STATUS_NUM1"n then predicted_class = 1;
   else if maxval = "P_STATUS_NUM2"n then predicted_class = 2;
   else if maxval = "P_STATUS_NUM3"n then predicted_class = 3;
   else if maxval = "P_STATUS_NUM4"n then predicted_class = 4;
   else if maxval = "P_STATUS_NUM5"n then predicted_class = 5;

drop pred_class;

run;

/* Step 1: Compute Confusion Matrix */
proc freq data=scored;
   tables STATUS_NUM * predicted_class / norow nocol nopercent;
   title "Confusion Matrix for Bayesian Network";
run;

/* Step 2: Compute Accuracy */
data accuracy_calc;
   set scored;
   correct = (STATUS_NUM = prediction_class);
run;

proc means data=accuracy_calc mean;
   var correct;
   title "Model Accuracy";
run;

data loglikelihood;
   set scored;

   /* Compute log-likelihood based on actual class */
   if STATUS_NUM = -1 then log_likelihood = log("P_STATUS_NUM-1"n);
   else if STATUS_NUM = 0 then log_likelihood = log("P_STATUS_NUM0"n);
   else if STATUS_NUM = 1 then log_likelihood = log("P_STATUS_NUM1"n);
   else if STATUS_NUM = 2 then log_likelihood = log("P_STATUS_NUM2"n);
   else if STATUS_NUM = 3 then log_likelihood = log("P_STATUS_NUM3"n);
   else if STATUS_NUM = 4 then log_likelihood = log("P_STATUS_NUM4"n);
   else if STATUS_NUM = 5 then log_likelihood = log("P_STATUS_NUM5"n);
run;

proc means data=loglikelihood sum;
   var log_likelihood;
   title "Log-Likelihood of Bayesian Network";
run;

/* Step 4: Compute and Plot ROC Curve */
proc logistic data=scored plots=roc plots(maxpoints=none);
   model STATUS_NUM(event='1') = "P_STATUS_NUM1"n / outroc=roc_data;
run;

/*ROC data is empty for some reason */


proc sgplot data=roc_data;
   series x=_FALPOS_ y=_POS_ / markers lineattrs=(thickness=2 color=blue);
   xaxis label="False Positive Rate (Specificity)";
   yaxis label="True Positive Rate (Sensitivity)";
   title "ROC Curve for Bayesian Network";
run;

/* Macro to assess the Naive Bayes Network (doesn't really work) */

%macro assess_BN(perf_data=scored, target=STATUS, pred=_PRED_, prob=_POST_);

/* Step 1: Compute Confusion Matrix */
proc freq data=&perf_dat;
   tables &target * &pred / norow nocol nopercent;
   title "Confusion Matrix for Bayesian Network";
run;

/* Step 2: Compute Accuracy */
data accuracy_calc;
   set &perf_data;
   correct = (&target = &pred);
run;

proc means data=accuracy_calc mean;
   var correct;
   title "Model Accuracy";
run;

/* Step 3: Compute Log-Likelihood */
data loglikelihood;
   set &perf_data;
   if &target = 1 then log_likelihood = log(&prob);
   else log_likelihood = log(1 - &prob);
run;

proc means data=loglikelihood sum;
   var log_likelihood;
   title "Log-Likelihood of Bayesian Network";
run;

/* Step 4: Compute and Plot ROC Curve */
proc logistic data=&perf_data;
   model &target(event=1) = &prob / outroc=roc_data;
run;

proc sgplot data=roc_data;
   series x=_FPR_ y=_TPR_ / markers lineattrs=(thickness=2 color=blue);
   xaxis label="False Positive Rate (1 - Specificity)";
   yaxis label="True Positive Rate (Sensitivity)";
   title "ROC Curve for Bayesian Network";
run;

%mend assess_BN;

/* Call the macro */
%assess_BN(perf_data=scored, target=STATUS_NUM, pred=pred_class, prob=_POST_);


/* Naive Bayes macro */

%macro nb(train=,score=,nclass=,target=,inputs=);
%let error=0;
%if %length(&train) = 0 %then %do;
	%put ERROR: Value for macro parameter TRAIN is missing ;
	%let error=1;
%end;
%if %length(&score) = 0 %then %do;
	%put ERROR: Value for macro parameter SCORE is missing ;
	%let error=1;
%end;
%if %length(&nclass) = 0 %then %do;
	%put ERROR: Value for macro parameter NCLASS is missing ;
	%let error=1;
%end;
%if %length(&target) = 0 %then %do;
	%put ERROR: Value for macro parameter TARGET is missing ;
	%let error=1;
%end;
%if %length(&inputs) = 0 %then %do;
	%put ERROR: Value for macro parameter INPUTS is missing ;
	%let error=1;
%end;
%if &error=1 %then %goto finish; /* line 23 */
%if %sysfunc(exist(&train)) = 0 %then %do;
	%put ERROR: data set &train does not exist ;
	%let error=1;
%end;
%if %sysfunc(exist(&score)) = 0 %then %do;
	%put ERROR: data set &score does not exist ;
	%let error=1;
%end;
%if &error=1 %then %goto finish; /* line 32 */
%LET nvar=0;
%do %while (%length(%scan(&inputs,&nvar+1))>0);
	%LET nvar=%eval(&nvar+1);
%end; /* line 36 */
proc freq data=&train noprint;
	tables &target / out=_priors_ ;
run;
%do k=1 %to &nclass;
	proc sql noprint;
		select percent, count into :Prior&k, :Count&k
		from _priors_
		where &target=&k;
	quit;
%end; /*k;  line 46 */
%do i=1 %to &nvar;
	%LET var=%scan(&inputs,&i);
	%do j=1 %to &nclass;
		proc freq data=&train noprint;
			tables &var / out=_&var.&j (drop=count) missing;
			where &target=&j;
		run;
	%end; /* j;*/
	data _&var ;
	merge %do k=1 %to &nclass;
	_&var.&k (rename=(percent=percent&k))
	%end; ;
		by &var;
	%do k=1 %to &nclass; if percent&k=. then percent&k=0; %end;
	run;
	proc sql;
		create table temp AS
		select a.*
		%do k=1 %to &nclass;
		, b.percent&k as percent&K._&var
		%end;
		from &score as a left join _&var as b
		on a.&var=b.&var;
	quit;
	data &score;
		set temp;
	run;
	proc datasets library=work nolist;
   		delete temp;
	run;
%end; /*i;  line 71 */
data &score (drop=L maxprob product
	%do i=1 %to &nclass; percent&i._: %end;);
	set &score;
	maxprob=0;
	_class_=.;
	%do k=1 %to &nclass;
		array vars&k (&Nvar)
		%do i=1 %to &nvar; percent&K._%scan(&inputs,&i) %end; ;
		product=log(&&Prior&k);
		do L=1 to &nvar;
			if vars&k(L)>0 then product=product+log(vars&k(L)); else
			product=product+log(0.5)-log(&&count&k);
		end;
		given_data_prob_class_&k=exp(product-(&nvar+1)*log(100));
		if product>maxprob then do; maxprob=product; _class_=&k; end;
	%end; /*k;*/
run;
%finish: ;
%mend NB; /* line 90 */


%nb(train=train,score=test,nclass=5,target=STATUS_NUM,inputs=AMT_INCOME_TOTAL CNT_CHILDREN CNT_FAM_MEMBERS CODE_GENDER DAYS_EMPLOYED
       FLAG_OWN_CAR FLAG_OWN_REALTY MONTHS_BALANCE NAME_EDUCATION_TYPE NAME_HOUSING_TYPE
       NAME_INCOME_TYPE OCCUPATION_TYPE NAME_FAMILY_STATUS);




/* BNC Diagram macro */

%macro createBNCdiagram(target, outnetwork);
   /* Extract Bayesian Network Structure */
   data outstruct;
      set &outnetwork;
      if strip(upcase(_TYPE_)) eq 'STRUCTURE' then output;
      keep _nodeid_ _childnode_ _parentnode_;
   run;

   /* Create network links */
   data networklink;
      set outstruct;
      linkid = _N_;
      label linkid = "Link ID";
   run;

   /* Get unique nodes from parent and child nodes */
   proc sql;
      create table work._node1 as
      select distinct _CHILDNODE_ as node from networklink;

      create table work._node2 as
      select distinct _PARENTNODE_ as node from networklink;
   quit;

   /* Combine unique node lists */
   proc sql;
      create table work._node as
      select node from work._node1
      UNION
      select node from work._node2;
   quit;

   /* Classify nodes as TARGET or INPUT */
   data bnc_networknode;
      length NodeType $32.;
      set work._node;
      if strip(upcase(node)) eq strip(upcase("&target")) then do;
         NodeType = "TARGET";
         NodeColor = 2;
      end;
      else do;
         NodeType = "INPUT";
         NodeColor = 1;
      end;
      label NodeType = "Node Type";
      label NodeColor = "Node Color";
   run;

   /* Separate parent and child nodes */
   data parents(rename=(_parentnode_ = _node_)) 
        children(rename=(_childnode_ = _node_)) 
        links;
      length _parentnode_ _childnode_ $32;
      set networklink;
      keep _parentnode_ _childnode_;
   run;

   /* Get unique node list */
   data nodes;
      set parents children;
   run;

   /* Sort nodes */
   proc sort data=nodes;
      by _node_;
   run;

   /* Remove duplicate nodes */
   data nodes;
      set nodes;
      by _node_;
      if first._node_;
      _Parentnode_ = _node_;
      _childnode_ = "";
   run;

   /* Merge node color and type */
   data nodes;
      merge nodes bnc_networknode (rename=(node=_node_ nodeColor=_nodeColor_ nodeType=_nodeType_));
      by _node_;
   run;

   /* Ensure consistent color mapping */
   proc sort data=nodes;
      by _nodeType_;
   run;

   /* Combine nodes and links */
   data bnc_networksummary(drop=_shape_ _nodecolor_ _nodepriority_ _shape_ _nodeID_ _nodetype_ _linkdirection_) 
        bnc_networksummaryall;
      length _parentnode_ _childnode_ $32;
      set nodes links;
      drop _node_;

      if _childnode_ EQ "" then do;
         _nodeID_ = _parentnode_;
         _nodepriority_ = 1;
         _shape_ = "OVAL";
      end;
      else do;
         _linkdirection_ = "TO";
         output bnc_networksummary;
      end;
      output bnc_networksummaryall;
      label _linkdirection_ = "Link Direction";
   run;

   /* Cleanup temporary datasets */
   proc datasets lib=work nolist nowarn;
      delete _node _node1 _node2 nodes links parents children;
   run;
   quit;

   /* Define Bayesian Network Diagram Template */
   proc template;
      define statgraph bpath;
         begingraph / DesignHeight=720 DesignWidth=720;
         entrytitle "Bayesian Network Diagram";
         layout region;
         pathdiagram fromid=_parentnode_ toid=_childnode_ /
            arrangement=GRIP
            nodeid=_nodeid_
            nodelabel=_nodeID_
            nodeshape=_shape_
            nodepriority=_nodepriority_
            linkdirection=_linkdirection_
            nodeColorGroup=_NodeColor_
            textSizeMin=10;
         endlayout;
         endgraph;
      end;
   run;

   /* Render the Bayesian Network Diagram */
   ods graphics;
   proc sgrender data=bnc_networksummaryall template=bpath;
   run;

%mend createBNCdiagram;


/* Call the macro */
%createBNCdiagram;

createBNCdiagram(STATUS_NUM, net);

