% temperature-dependent R0 model
clc
clear all 
worldclim1 = xlsread('worldclim_average tem.xlsx',2);

N1=4405270*73.13*0.1848/365;%the population size for Northeast
N2=739409*73.13*0.1848/365;%the population size for central region
N3=2177350*73.13*0.1848/365;%the population size for Southwest

r=0.00833;% the recovery rate 

T1= worldclim1(:,2);%the average temperature from worldclim for 1970-2000
T2= worldclim1(:,3);
T3= worldclim1(:,4);


a1=0.000203.*T1.*(T1-11.7).*sqrt(42.3-T1);%biting rate 
bc1= -0.54.*T1.*T1+25.2.*T1-206;%vector competence
p1=-0.00083.*T1.*T1+0.037.*T1+0.522;%p=e^(-u),u:the adult mosquito mortality rate  
PDR1=0.000111.*T1.*(T1-14.7).*sqrt(34.4-T1);%the parasite development rate 
PEA1=-0.00924.*T1.*T1+0.453.*T1-4.77;%the mosquito egg-to-adult survival probability
MDR1=0.000111.*T1.*(T1-14.7).*sqrt(34-T1);%the larval mosquito development rate 
EFD1=-0.153.*T1.*T1+8.61*T1-97.7;%the number of eggs laid per female per day
R0_1=sqrt((a1.*a1.*bc1.*p1.^(1./PDR1).*EFD1.*PEA1.*MDR1)./(-N1.*r.*log(p1).*log(p1).*log(p1)));

%central region
a2=0.000203.*T2.*(T2-11.7).*sqrt(42.3-T2);
bc2= -0.54.*T2.*T2+25.2.*T2-206;
p2=-0.00083.*T2.*T2+0.037.*T2+0.522;
PDR2=0.000111.*T2.*(T2-14.7).*sqrt(34.4-T2);
PEA2=-0.00924.*T2.*T2+0.453.*T2-4.77;
MDR2=0.000111.*T2.*(T2-14.7).*sqrt(34-T2);
EFD2=-0.153.*T2.*T2+8.61*T2-97.7;
R0_2=sqrt((a2.*a2.*bc2.*p2.^(1./PDR2).*EFD2.*PEA2.*MDR2)./(-N2.*r.*log(p2).*log(p2).*log(p2)));

%Southwest
a3=0.000203.*T3.*(T3-11.7).*sqrt(42.3-T3);
bc3= -0.54.*T3.*T3+25.2.*T3-206;
p3=-0.00083.*T3.*T3+0.037.*T3+0.522;
PDR3=0.000111.*T3.*(T3-14.7).*sqrt(34.4-T3);
PEA3=-0.00924.*T3.*T3+0.453.*T3-4.77;
MDR3=0.000111.*T3.*(T3-14.7).*sqrt(34-T3);
EFD3=-0.153.*T3.*T3+8.61*T3-97.7;
R0_3=sqrt((a3.*a3.*bc3.*p3.^(1./PDR3).*EFD3.*PEA3.*MDR3)./(-N3.*r.*log(p3).*log(p3).*log(p3)));

A=[R0_1,R0_2,R0_3];

%plot
M=max(max(A));
plot(real(R0_1)/M,'linewidth',5,'Color',[128,128,128]/255);
hold on;
plot(real(R0_2)/M,'linewidth',5,'Color',[237,177,32]/255);
plot(real(R0_3)/M,'linewidth',5,'Color',[217,83,25]/255);
legend('Plain','Highland','Hill');
ylabel(' Relative R_{0}');
xlabel('Month');


