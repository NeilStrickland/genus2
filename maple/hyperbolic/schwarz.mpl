# The schwarzian differential operator

#@ schwarzian
schwarzian :=
 (f,z) -> diff(f,z,z,z)/diff(f,z) - (3/2) * (diff(f,z,z)/diff(f,z))^2;

# This is as in Proposition prop-S-p-inv
#@ S_p_inv
S_p_inv :=
 unapply((3/8)*(1/z^2 + 1/(z-a_P)^2 + 1/(z+a_P)^2 + 1/(z-1/a_P)^2 + 1/(z+1/a_P)^2) +
         (d_p_inv*z - 3*z^3/2)/r_P(z),z);

# These functions are as in Definition defn-schwarz-phi
schwarz_phi     := (z) -> (I - z)/(I + z);        #@ schwarz_phi    
schwarz_phi_inv := (w) -> I * (1 - w)/(1 + w);    #@ schwarz_phi_inv

#@ schwarz_psi
schwarz_psi :=
 (w) -> ((1 + I)/sqrt(2)*(sqrt(2)-am_H-w*ap_H))/(ap_H + (am_H-sqrt(2))*w);

#@ schwarz_psi_inv
schwarz_psi_inv :=
 (z) -> (-(sqrt(2) - am_H - (1-I)/sqrt(2)*ap_H*z))/((1-I)*(1-am_H/sqrt(2))*z - ap_H);

schwarz_alpha := (I - a_P)/(I + a_P);  #@ schwarz_alpha

# This is s^*_0 in Proposition prop-p-one
#@ S0_p1_inv
S0_p1_inv := (z) -> (9/2*(1-z^2)^4*(1-a_P^4)^2+96*a_P^4*z^2*(1+z^2)^2)/
                    ((1-z^2)^2*((1-z^2)^2*(1+a_P^2)^2+16*a_P^2*z^2)^2);

# This is s^*_1 in Proposition prop-p-one
#@ S1_p1_inv
S1_p1_inv := (z) -> -4*a_P^2/((1-z^2)^2*(1+a_P^2)^2+16*a_P^2*z^2);

#@ S_p1_inv 
S_p1_inv  := unapply(S0_p1_inv(z) + S1_p1_inv(z) * d_p_inv, z);

# We take p2 = xi o p1 as in Remark rem-heun
schwarz_xi := (z) -> 2/(z^2+1/z^2);               #@ schwarz_xi
schwarz_xi_inv := (Z) -> sqrt((sqrt(1-Z^2)-1)/Z); #@ schwarz_xi_inv

# Schwarzian of the inverse of p2
#@ S0_p2_inv
S0_p2_inv := (Z) -> 3/16*(9*Z^4*a_P^8+15*Z^3*a_P^8-72*Z^4*a_P^6+5*Z^2*a_P^8-48*Z^3*a_P^6+Z*a_P^8+254*Z^4*a_P^4+2*a_P^8-190*Z^3*a_P^4-16*Z*a_P^6-72*Z^4*a_P^2+150*Z^2*a_P^4+8*a_P^6-48*Z^3*a_P^2-34*Z*a_P^4+9*Z^4+12*a_P^4+15*Z^3-16*Z*a_P^2+5*Z^2+8*a_P^2+Z+2)/Z^2/(Z-1)^2/(Z+1)^2/(Z*a_P^4+a_P^4-6*Z*a_P^2+2*a_P^2+Z+1)^2;

#@ S1_p2_inv
S1_p2_inv := (Z) -> -1/2*a_P^2/(Z*a_P^4+a_P^4-6*Z*a_P^2+2*a_P^2+Z+1)/(Z+1)/Z/(Z-1);

#@ S_p2_inv
S_p2_inv := unapply(S0_p2_inv(Z) + d_p_inv * S1_p2_inv(Z),Z);

#@ heun_F00
heun_F00 := (Z) -> Z^(1/4)*(Z-1)^(1/4)*(Z+1)^(3/8)*((Z+1)*a_P^4+(-6*Z+2)*a_P^2+Z+1)^( 1/8);

#@ heun_F10
heun_F10 := (Z) -> Z^(3/4)*(Z-1)^(1/4)*(Z+1)^(3/8)*((Z+1)*a_P^4+(-6*Z+2)*a_P^2+Z+1)^(-3/8);

#@ heun_F01
heun_F01 := (Z) -> HeunG(-4*a_P^2/(a_P^4-2*a_P^2+1),
                        (5*a_P^4+5+(-8*d_p_inv-10)*a_P^2)/(64*a_P^4-128*a_P^2+64),
			1/8, 5/8, 1/2, 3/4,
			-8*Z*a_P^2/((Z+1)*a_P^4+(-6*Z+2)*a_P^2+Z+1));

#@ heun_F11
heun_F11 := (Z) -> HeunG(-4*a_P^2/(a_P^4-2*a_P^2+1),
			 (21+21*a_P^4+(-8*d_p_inv-138)*a_P^2)/(64*a_P^4-128*a_P^2+64),
			 5/8, 9/8, 3/2, 3/4,
			 -8*Z*a_P^2/((Z+1)*a_P^4+(-6*Z+2)*a_P^2+Z+1));

heun_F0 := unapply(heun_F00(Z) * heun_F01(Z),Z); #@ heun_F0
heun_F1 := unapply(heun_F10(Z) * heun_F11(Z),Z); #@ heun_F1

# Images of some points v_H[i] under schwarz_psi_inv, as in
# Definition defn-schwarz-phi

r_schwarz := (sqrt(2) - am_H)/ap_H;                                     #@ r_schwarz
s_schwarz := (ap_H*am_H - sqrt(2)*(a_H - a_H^3))/(1 - a_H^2 + 2*a_H^4); #@ s_schwarz
t_schwarz := a_H^2 * (sqrt(2)*ap_H - 2*a_H*am_H)/(1 - a_H^2 + 2*a_H^4); #@ t_schwarz

#@ v_HS
v_HS[ 0] :=  r_schwarz;
v_HS[ 1] := -r_schwarz;
v_HS[ 2] :=  -I * am_H/(ap_H + sqrt(2)*a_H);
v_HS[ 3] :=   I * am_H/(ap_H + sqrt(2)*a_H);

v_HS[ 6] :=  0;

v_HS[10] :=  t_schwarz - I * s_schwarz;
v_HS[11] :=  t_schwarz + I * s_schwarz;
v_HS[12] := -t_schwarz - I * s_schwarz;
v_HS[13] := -t_schwarz + I * s_schwarz;

# Images of some curves c_H[i] under schwarz_psi_inv, as in
# Definition defn-schwarz-phi

#@ c_HS
c_HS_p[3] := I * ap_H/am_H;
c_HS_p[5] := (sqrt(2) + I*am_H)/ap_H;
c_HS_r[3] := sqrt(2) * a_H / am_H;
c_HS_r[5] := sqrt(2)*am_H/ap_H;

c_HS[ 0] := (t) -> I * Tanh((t-Pi/4)*s_H[0]/Pi);
c_HS[ 1] := (t) ->   - Tanh((t-Pi/2)*s_H[1]/Pi);
c_HS[ 3] := (t) -> xi_curve(c_HS_p[3],2*(Pi/2-t)*s_H[2]/Pi);
c_HS[ 5] := (t) -> xi_curve(c_HS_p[5],log((2+ap_H)/sqrt(3-a_H^2))-2*t*s_H[3]/Pi);

#@ v_PS
v_PS[ 0] :=  1;
v_PS[ 1] := -1;
v_PS[ 2] := -I;
v_PS[ 3] :=  I;
v_PS[ 6] :=  0;

v_PS[10] :=  (I + a_P)/(I - a_P);
v_PS[11] :=  (I - a_P)/(I + a_P);
v_PS[12] := -(I - a_P)/(I + a_P);
v_PS[13] := -(I + a_P)/(I - a_P);

#@ c_PS
c_PS[ 0] := (t) -> I * tan(t - Pi/4);
c_PS[ 1] := (t) -> cos(t);
c_PS[ 3] := unapply(simplify(schwarz_phi(p_P(c_P[ 3](t)))),t);
c_PS[ 5] := unapply(simplify(schwarz_phi(p_P(c_P[ 5](t)))),t);


# This function corresponds to Definition defn-circle-fit

#@ series_circle_fit
series_circle_fit := proc(u0,u1,u2)
 local T;

 T := table();
 T["c"] := sqrt(u2/(u0*(u0*u2+u1^2)));
 T["b"] := u1^2/sqrt(8*u0*u2*(u0*u2+u1^2)+u1^4);
 T["bp"] := sqrt(1 + T["b"]^2);
 T["bm"] := sqrt(1 - T["b"]^2);
 T["d"]  := T["bp"]/T["bm"];
 T["r"]  := sqrt(2)*T["b"]/T["bm"];
 return(eval(T));
end:

#@ sample_circle_fit
sample_circle_fit := proc(L)
 local N,XX,XY,YY,XXX,XXY,XYY,YYY,L1,z0,u1,v1,r1,T;
 N := nops(L);
 z0 := add(z,z in L)/N;
 L1 := map(z -> [Re(z-z0),Im(z-z0)],L);
 XX := add(a[1]^2      ,a in L1);
 XY := add(a[1]*a[2]   ,a in L1);
 YY := add(a[2]^2      ,a in L1);
 XYY := add(a[1]*a[2]^2,a in L1);
 XXY := add(a[1]^2*a[2],a in L1);
 XXX := add(a[1]^3     ,a in L1);
 YYY := add(a[2]^3     ,a in L1);
 u1 := (1/2)*(YY*N*XYY+YY*N*XXX-XY*N*XXY-XY*N*YYY)/(XX*N*YY-XY^2*N);
 v1 := (1/2)*(-XY*N*XYY-XY*N*XXX+XX*N*XXY+XX*N*YYY)/(XX*N*YY-XY^2*N);
 r1 := sqrt(XX/N+YY/N+(u1^2+v1^2));

 T := table():
 T["centre"] := z0+u1+I*v1;
 T["radius"] := r1;

 return(eval(T));
end:

#@ tiny_p1_poles
tiny_p1_poles := [I*am_H/ap_H,-I*am_H/ap_H];

#@ tiny_p1_denom
tiny_p1_denom := (z) -> z^2 + (1-a_H^2)/(1+a_H^2);

#@ small_p1_poles
small_p1_poles := map(simplify_H,[
 seq(schwarz_psi_inv(act_Pi(W,v_H[7])),W in
    [[],[0],[2],[6],[0,6],[2,0],[2,3],[5,6],[2,0,6],[5,4,3],[0,7,0,6],[5,6,7,0,6]]),
 seq(schwarz_psi_inv(act_Pi(W,v_H[9])),W in
    [[],[0],[2],[4],[0,2],[0,7],[2,4],[5,4],[0,2,4],[5,6,7],[0,2,1,2],[0,7,0,1,2]])
]):

#@ small_p1_denom
small_p1_denom := (z) -> 
(z^4-2*z^2*(a_H^2+1)^2/(a_H^2-3)^2+(a_H^2+1)^2/(a_H^2-3)^2)*
(z^4-2*z^2*(a_H^4-4*a_H^2+7)/(a_H^2-3)^2+(a_H^4+3)^2/((a_H^2-3)^2*(a_H^2+1)^2))*
(z^4-2*z^2*(a_H^4+12*a_H^2-9)/(a_H^2-3)^2+(a_H^4+3)^2/((a_H^2-3)^2*(a_H^2+1)^2))*
(z^4-2*z^2*(a_H^2+1)*(a_H^10+a_H^8-12*a_H^6+12*a_H^4+4*a_H^2-4)/(a_H^6-a_H^4-2)^2+(a_H^4-2*a_H^2+2)^2*(a_H^2+1)^2/(a_H^6-a_H^4-2)^2)*
((-a_H^2+1)^2*(a_H^2+2)^2/((a_H^2-2)^2*(a_H^2+1)^2)+(2*(-a_H^2+1))*(a_H^2+2*a_H+2)*(a_H^2-2*a_H+2)*z^2/((a_H^2-2)^2*(a_H^2+1))+z^4)*
(z^4-2*z^2*(a_H^2+1)*(a_H^10+a_H^8+4*a_H^6-4*a_H^4+4*a_H^2-4)/(a_H^6-a_H^4-2)^2+(a_H^4-2*a_H^2+2)^2*(a_H^2+1)^2/(a_H^6-a_H^4-2)^2);

#@ schwarz_b_approx
schwarz_b_approx := (a) -> 0.9069138819607508504414623413486215941630984580580672212927769794572584355051327480722389917832436934-1.765474192590781737605598080177250320104598155751747434554168434791386658985099944462154268627034137*a+11.53828977378593312763653588864186488647822466461521372737570552945692216438365003085500639707627312*a^2-66.41329361574075935661075187078177669195661108874142415658152457434993361143811500196721781770233091*a^3+252.8047438337293960095779707025769471443777311418990076217550032752073341113370756286590049291734747*a^4-629.9078607801038186456330794089845793458290861471589728999709363487263998249145987081714492089597137*a^5+1020.358374393808620210881702144982983157533265009871970569144218800724280434738209698557221801944654*a^6-1048.740900936545828704047197583117104725092127778886883795987693295565268942430022674598654591538775*a^7+643.8321100777354751651656927733692084746561441636024348187111032073278314905473663698929203277312212*a^8-205.7057517407104396403773030537487539842131344061422435468478074909575133773836375067202807801834757*a^9+23.38607168239802899455924047642001005632471745805821178139327421692228478867776411933175717692491330*a^10:

#@ schwarz_c_approx
schwarz_c_approx := (a) -> 0.2056091544987268220798370509160122242052513020545736786917007651838825344075819746412368048506824587+2.119297075747641460259319400109999804985002300201054267836014494876919786679576153075604342457731568*a-16.97540822063646794986589307304361914439032663427820975529588004577721559151722238273988798645387885*a^2+111.4327378813141625581053815667432701695777351827357767550594836286884557435518311962928179512904584*a^3-497.8280364102672996457064931256207794883799477913510984282768168044619968942609452870938085877487818*a^4+1476.634802306891170011190691150734072380963465242558167864043549037878387924612396089910412834763057*a^5-2911.486921527754435081165570561377274531794255693475113920098627252088320106419198173223937280420437*a^6+3764.776092932513000365494958037208518369588704079674268647925524858402564822809018562240131432396151*a^7-3062.124943339760129219656222429329602995172168558081544206872235816258108390142091508467676886602552*a^8+1419.161466628264212988621492211939809097559453474182385809990554936216592522540270481899308288835179*a^9-285.5632912305122964461160136643477626369817953502233169909822129018875329729882841139629237168807451*a^10:

#@ schwarz_d_approx
schwarz_d_approx := (a) -> 0.6058377776502591150030967209491610331137537677546949034892689791801117384031868648857514189653909987/a^2-.5757005908854302939614660937646876562654754515805005628087462836547958761923587568388355714371388714/a+5.021674398133655864578418815878447581276579088374614078136131501609845114356760506017104132006848330-43.16353936606263677547932945959892277835334907696685728427630724492400682586012593021292899097419813*a+241.8407229892773138470807499563123837224094002654279722152557185141028854360356546579916505481536750*a^2-879.0714590308607269819325703549455540514915711553653262927964838656598213111121450666908004116820733*a^3+2076.040895943393049644902980965714646064319589501870239295214407029360258959685895189594048279798827*a^4-3142.620916760244192487301982269231601288293368722269041885451780412098378428607098882183962177408837*a^5+2932.327786651674606817506043850011956678724313527912316782251698822788840925528158290855391540457352*a^6-1532.037358618056904591964225256643925414126844321878829249902243192169256224299948152044642786850792*a^7+342.3168903638456056003706421234452452496222552478738690336322529610781181380798425890881658924475955*a^8:

