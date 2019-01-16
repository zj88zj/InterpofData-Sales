OPTIONS PS=55 LS=70;
/* Read dataset from a text file comma delimited */

DATA sasuser.hospital;
INFILE 'hospital.txt' DELIMITER=',';
INPUT ZIP $  HID $ CITY $ STATE $ BEDS RBEDS OUTV ADM SIR SALESY
         SALES12 TH TRAUMA REHAB HIP95 KNEE95 HIP96
         KNEE96 FEMUR96;
run;

data hospital;

set sasuser.hospital;

ARRAY X {12} BEDS RBEDS HIP95 KNEE95 HIP96 KNEE96 FEMUR96
         OUTV ADM SIR SALESY SALES12;

DO I=1 TO 7 ;
         X{I} = SQRT(X{I});
END;

DO I=8 TO 12 ;
         X{I} = LOG(1+X{I});
 END;

/* code for selecting subsets based on hoispital type */
*  gr = TH*4 + trauma*2 + rehab;
*  if gr > 1  ;
/* code for selecting subsets based on hospital location */
* IF STATE EQ 'FL' OR STATE EQ 'GA' or state='TX';

RUN;

/* Code for factor analysis in two stages, grouping teh variables */
/* into two subgroups */

PROC FACTOR data=hospital METHOD=PRIN NFACT=1 out=z;
var  HIP95 KNEE95 HIP96 KNEE96 FEMUR96;
RUN;

PROC FACTOR data=hospital METHOD=PRIN NFACT=3 ROTATE=VARIMAX out=z1;
var  BEDS RBEDS OUTV ADM SIR TH trauma rehab;
RUN;

data z1;
set z1;
factor4 = factor1;
keep factor2 factor3 factor4;
run;

data hospout;
merge z z1;
run;

/* Code for factor analysis into two stages */

PROC FACTOR data=hospital METHOD=PRIN NFACT=4 ROTATE=VARIMAX out=hospout;
var  BEDS RBEDS OUTV ADM SIR TH trauma rehab HIP95 KNEE95 HIP96 KNEE96 FEMUR96;
RUN;

/*cluster analysis using WARD */

PROC CLUSTER data=hospout METHOD=WARD;
VAR factor1-factor4;
COPY ZIP CITY STATE HID BEDS RBEDS OUTV ADM SIR
         HIP95 KNEE95 TH TRAUMA REHAB HIP96
         KNEE96 FEMUR96 SALES12 SALESY factor1-factor4 ;
run;
PROC TREE NOPRINT NCL=17 OUT=TXCLUST;
COPY ZIP CITY STATE HID BEDS RBEDS OUTV ADM SIR
         HIP95 KNEE95 TH TRAUMA REHAB HIP96
         KNEE96 FEMUR96 SALES12 SALESY factor1-factor4 ;
RUN;

/* produce the cluster summary and pick the best cluster*/

data hout;
set txclust;
keep factor1-factor4 sales12 cluster; run ;


proc sort data=Txclust;
by cluster;
run;
proc means noprint;
by cluster;
var SALES12  factor1-factor4;
output out=c mean= msales mf1-mf4;
run;

proc print;
run;


PROC SORT DATA=OUT;
BY CLUSTER;
RUN;


/* This assumes that cluster 9 is the best one */
data cl9;
 set txclust;
if cluster=9;
run;

PROC reg DATA=cl9;
model sales12 =  Factor1-factor4/ P R selection=b  ;
OUTPUT   OUT=C P=PRED R=RESID STDP=STDP;
run;




data C ;
set C;
*if _N_ = 11 then sales12=0;
rowp = exp(PRED+ 0.5*STDP*STDP)-1;
epred = exp(pred)-1;
sales12 = exp(sales12) -1;
gain = rowp - sales12;
run;

proc sort;
by gain;
proc print;
*var gain PRED STDP SALES12 epred;
run;




/* OTHER CODE ADDITIONS */

/* CODE for using  K-MEANS clustering */

proc fastclus data=hospout out=out maxc=8 maxiter=20;
VAR factor1-factor4;
run;


/* CODE FOR ESTIMATING GAIN for the special case when the cluster size is very small*/

data Cl9 ;
set Cl9;
sales12 = exp(sales12) -1;
run;

proc means data=cl9; var sales12;
run;
          /* suppose the mean of sales12 is 181.5 */
data cl9; set cl9;
gain = 181.5 - sales12;
run;

proc sort;
by gain;
proc print;
*var gain PRED STDP SALES12 epred;
run;