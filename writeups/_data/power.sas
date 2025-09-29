proc import datafile="C:\Users\cwied\Documents\R Directory\dissertation\ch1-bar3d\writeups\_data\paper-data.csv"
    out=work.ch1
    dbms=csv
    replace;
    getnames=yes;
    guessingrows=32767;
    delimiter=',';
run;

data work.ch1;
    set work.ch1;
    true = height * 100;
    logerrorcm = log2(abs(byHowMuch - true) + 1/8);
    error = true - byHowMuch;
    abserror = abs(byHowMuch - true);
    if type = 'NA' then delete;
run;


/* Run model to get estimates for power analysis */
ods exclude all;
proc glimmix data=work.ch1 plots=residualpanel;
    class plot ratioLabel type kit subject;
    model logerrorcm = ratioLabel type plot(ratioLabel);
    random intercept subject / subject=kit;
    lsmeans plot(ratioLabel);
    output out=work.ch1_predicted pred=yhat;
run;


proc sql;
    create table work.means as
    select distinct kit, plot, ratioLabel, type, yhat
    from work.ch1_predicted
    order by kit, ratioLabel, plot, type;
run;


data work.means2;
    set work.means;
     do i = 1 to 40;
        subject = i;
        output;
    end;
run;

proc print data=work.means2;
run;


proc glimmix data=work.means2;
    class plot ratioLabel type kit subject;
    model yhat = ratioLabel type plot(ratioLabel) / s;
    random intercept subject / subject=kit;
    parms (0.4107)(0.2398)(1.5578) / hold=1,2,3;
    contrast '17.8 2dd vs 3dd' plot(ratioLabel) 1 -1 0;
    contrast '17.8 2dd vs 3dp' plot(ratioLabel) 1 0 -1;
    contrast '17.8 3dd vs 3dp' plot(ratioLabel) 0 1 -1;

    contrast '26.1 2dd vs 3dd' plot(ratioLabel) 0 0 0 1 -1 0;
    contrast '26.1 2dd vs 3dp' plot(ratioLabel) 0 0 0 1 0 -1;
    contrast '26.1 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 1 -1;

    contrast '38.3 2dd vs 3dd' plot(ratioLabel) 0 0 0 0 0 0 1 -1 0;
    contrast '38.3 2dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 1 0 -1;
    contrast '38.3 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 1 -1;

    contrast '46.4 2dd vs 3dd' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 1 -1 0;
    contrast '46.4 2dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 1 0 -1;
    contrast '46.4 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 1 -1;

    contrast '56.2 2dd vs 3dd' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0;
    contrast '56.2 2dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1;
    contrast '56.2 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1;

    contrast '68.1 2dd vs 3dd' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0;
    contrast '68.1 2dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1;
    contrast '68.1 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1;

    contrast '82.5 2dd vs 3dd' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0;
    contrast '82.5 2dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1;
    contrast '82.5 3dd vs 3dp' plot(ratioLabel) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1;
    ods output tests3=overall_F contrasts=contrasts;
run;

ods exclude none;

data power;
    set overall_F contrasts;
    ncp=numdf*Fvalue;
    alpha=0.05;
    Fcrit=finv(1-alpha,numdf,dendf,0);
    Power=1-ProbF(Fcrit,numdf,dendf,ncp);
    run;
proc print data=power;
run;

proc sql;
    select count(*) as numPowSig
    from work.power
    where Effect not in ("ratioLabel", "type", "plot(ratioLabel)")
        and Power > 0.8;
run;
