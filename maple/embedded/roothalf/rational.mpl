# This file contains information about points in EX^* whose coordinates
# are rational, or rational multiples of sqrt(2), or otherwise 
# arithmetically simple.

#@ c_rational
c_rational[0] := (t) -> [t^2-1,2*t,0,0] /~ (t^2+1);
c_rational_condition[0] := (t) -> type(t,rational);

c_rational[1] := (st) -> [ st[1],st[1],st[2],0];
c_rational_condition[1] :=
 (st) -> type(st,[rational,rational]) and 2*st[1]^2 + st[2]^2 = 1;

c_rational[2] := (st) -> [-st[1],st[1],st[2],0];
c_rational_condition[2] :=
 (st) -> type(st,[rational,rational]) and 2*st[1]^2 + st[2]^2 = 1;

#@ c_quasirational
c_quasirational[1] := (t) -> 
 [  1-t^2-2*t ,1-t^2-2*t,sqrt(2)*(1-t^2+2*t),0] /~ (2*(1+t^2));
c_quasirational_condition[1] := (t) -> type(t,rational);

c_quasirational[2] := (t) -> 
 [-(1-t^2-2*t),1-t^2-2*t,sqrt(2)*(1-t^2+2*t),0] /~ (2*(1+t^2));
c_quasirational_condition[2] := (t) -> type(t,rational);

c_quasirational[3] := (st) -> [0, st[2],sqrt(2)*st[1],-st[1]];
c_quasirational_condition[3] :=
 (st) -> type(st,[rational,rational]) and 3*st[1]^2 + st[2]^2 = 1;

c_quasirational[4] := (st) -> [-st[2],0,sqrt(2)*st[1], st[1]];
c_quasirational_condition[4] :=
 (st) -> type(st,[rational,rational]) and 3*st[1]^2 + st[2]^2 = 1;

c_quasirational[5] := (t) -> [sqrt((1-t)*t/(t+2)), 0, sqrt(2/(t+2)), -sqrt(t^2/(t+2))];

##################################################

# farey_count(n) is the number of rationals in [0,1] with denominator
# at most n.

#@ farey_count 
farey_count := (n) -> 1 + add(numtheory[phi](i),i=1..n):

# farey(n) is the list of rationals in [0,1] with denominator at most n.

#@ farey 
farey := proc(n)
 local F,a,b,c,d,a0,b0,k;

 a := 0;
 b := 1;
 c := 1;
 d := n;

 F := a/b;

 while c <= n do 
  k := floor((n+b)/d);
  a0 := a;
  b0 := b;
  a := c;
  b := d;
  c := k*c - a0;
  d := k*d - b0;
  F := F,a/b;
 od;

 return [F];
end:

##################################################
# This section is about solutions to 2 s^2 + t^2 = 1.

# two_roots is the set of roots of unity in Q(sqrt(-2)).

#@ two_roots 
two_roots := [1,-1];

# two_primes is a list of prime numbers that factor in Q(sqrt(-2))
# For each such prime p, two_pi[p] is the prime element of 
# Q(sqrt(-2)) in the upper half plane that divides p, and 
# two_u[p] is two_pi[p] / conjugate(two_pi[p]), which has
# norm one.

#@ two_primes 
two_primes := NULL;

#@ two_pi
#@ two_u
for i from 2 to 200 do
 p := ithprime(i);
 if modp(p,8) = 1 or modp(p,8) = 3 then
  two_primes := two_primes,p;
  uv := select(ab -> (ab[1]>0 and ab[2]>0),map(s -> subs(s,[a,b]),[isolve(p=2*a^2+b^2)]))[1]; 
  two_pi[p] := uv[1]*sqrt(-2) + uv[2]; 
  two_u[p] := expand(rationalize(two_pi[p]/conjugate(two_pi[p])));
 fi;
od;

# two_units_aux(n) is a list of units in Q(sqrt(-2)) with
# denominator equal to n.

#@ two_units_aux 
two_units_aux := proc(n)
 local ff,f,Q;
 if n = 1 then return {1}; fi;
 ff := ifactor(n);
 if type(ff,`*`) then ff := [op(ff)]; else ff := [ff]; fi;
 ff := map(f -> `if`(type(f,`^`),[op(f)],[f,1]),ff);
 ff := map(f -> two_u[op(op(1,f))]^op(2,f),ff);
 Q := {1};
 for f in ff do
  Q := {op(Q *~ f),op(Q /~ f)};
 od;
 Q := expand(rationalize(Q));
 return(Q);
end:

#@ two_circle 
two_circle := proc(max_denom)
 local NN,C;

 NN := {seq(n,n=1..max_denom)};
 NN := select(n -> mods({1,3,op(prime_factors(n))},8) = {1,3},NN);
 C := map(op,map(two_units_aux,NN));
 C := map(op,map(u -> expand(u *~ two_roots),C));
 C := map(u -> [Im(u)/sqrt(2),Re(u)],C);
 C := sort([op(C)]);
 return C;
end:

##################################################
# This section is about solutions to 3 s^2 + t^2 = 1.

# three_roots is the set of roots of unity in Q(sqrt(-3)).

#@ three_roots 
three_roots := simplify([seq(exp(Pi*I*k/3),k=0..5)]);

# three_primes is a list of prime numbers that factor in Q(sqrt(-3))
# For each such prime p, three_pi[p] is the prime element of 
# Q(sqrt(-3)) in the upper half plane that divides p, and 
# three_u[p] is three_pi[p] / conjugate(three_pi[p]), which has
# norm one.

#@ three_primes 
three_primes := NULL:

#@ three_pi
#@ three_u
for i from 3 to 200 do
 p := ithprime(i);
 if mods(p,3) = 1 then
  three_primes := three_primes,p;
  uv := select(ab -> (ab[1]>0 and ab[2]>0),map(s -> subs(s,[a,b]),[isolve(p=3*a^2+b^2)]))[1]; 
  three_pi[p] := uv[1]*sqrt(-3) + uv[2]; 
  three_u[p] := expand(rationalize(three_pi[p]/conjugate(three_pi[p])));
 fi;
od:

# three_units_aux(n) is a list of units in Q(sqrt(-3)) with
# denominator equal to n.

#@ three_units_aux 
three_units_aux := proc(n)
 local ff,f,Q;
 if n = 1 then return {1}; fi;
 ff := ifactor(n);
 if type(ff,`*`) then ff := [op(ff)]; else ff := [ff]; fi;
 ff := map(f -> `if`(type(f,`^`),[op(f)],[f,1]),ff);
 ff := map(f -> three_u[op(op(1,f))]^op(2,f),ff);
 Q := {1};
 for f in ff do
  Q := {op(Q *~ f),op(Q /~ f)};
 od;
 Q := expand(rationalize(Q));
 return(Q);
end:

#@ three_circle 
three_circle := proc(max_denom)
 local NN,C;

 NN := {seq(n,n=1..max_denom)};
 NN := select(n -> mods({1,op(prime_factors(n))},3) = {1},NN);
 C := map(op,map(three_units_aux,NN));
 C := map(op,map(u -> expand(u *~ three_roots),C));
 C := map(u -> [Im(u)/sqrt(3),Re(u)],C);
 C := sort([op(C)]);
 return C;
end:

######################################################################

#@ elliptic_t
elliptic_t := (x) -> x[1]^2;

#@ elliptic_u
elliptic_u := (x) -> x[1]*(x[4]^2-5/sqrt(2)*x[3]*x[4]-1);

#@ elliptic_r
elliptic_r[0] := (x) -> x[1]^2+x[3]^2+x[4]^2-1;
elliptic_r[1] := (x) -> x[1]^2+x[4]^2+x[3]*x[4]/sqrt(2);

#@ elliptic_a
elliptic_a[0] := (x) -> -6*sqrt(2)*(x[3]+sqrt(2)*x[4])*x[4]^3;
elliptic_a[1] := (x) -> (1-x[3]^2-x[4]^2) * (x[3]^2 -10*x[4]^2 + 9 + x[3]*x[4]/sqrt(2)); 
elliptic_a[2] := (x) -> x[3]^2 + 2*x[4]^2 + x[3]*x[4]/sqrt(2) - x[1]^2 + 9;

#@ elliptic_rel
elliptic_rel := (u,t) -> u^2 - (t^3 - 10*t^2 + t);

######################################################################

#@ quasirational_proj 
quasirational_proj := (x) -> [x[3]/sqrt(2),x[2]^2 - x[1]^2 - 3*x[3]*x[4]/sqrt(2)];

#@ quasirational_lift 
quasirational_lift := proc(s)
 local t1,t2,t3,p1,p2,p3,p4;

 t1 := s[1]^2;
 t2 := 6*s[1]^2 - 2;
 t3 := -2*s[2]^2-4;
 p1 := 2+t1*t3;
 p2 := s[2]*t2;
 p3 := p1+p2;
 p4 := p1-p2;
 return([sqrt(p3)/2,sqrt(p4)/2,sqrt(2)*s[1],-s[1]*s[2]]);
end;

#@ is_quasirational 
is_quasirational := proc(x)
 type(x /~ [1,1,sqrt(2),1],[rational $ 4]);
end:

#@ inner_quasirational_projections 
inner_quasirational_projections := [
[103/154, 65/449],
[103/554, 2945/3041],
[104/1073, 105/233],
[1/10, 13/19],
[1/10, 16/17],
[1/10, 245/267],
[11/118, 819/1331],
[11/1190, 1664/2057],
[1127/2194, 581/731],
[1135/2114, 1088/1137],
[1147/2390, 97/961],
[1147/2830, 640/1369],
[1151/3722, 11/139],
[1223/2330, 16/17],
[12/29, 247/297],
[125/262, 16/59],
[1255/2146, 1547/1675],
[1260/3599, 559/1009],
[127/450, 416/417],
[1301/2270, 16/33],
[13/30, 145/1521],
[13/30, 16/33],
[1389/3038, 176/1049],
[1424/3913, 1571/2179],
[148/215, 29/323],
[151/338, 279/1129],
[1549/3038, 2768/3307],
[1603/2350, 19/147],
[1611/3878, 355/643],
[1648/2875, 2295/3113],
[1672/3329, 1257/2057],
[169/322, 475/507],
[17/106, 1760/2601],
[172/511, 15/17],
[175/298, 2464/2825],
[175/362, 112/163],
[1792/3635, 2623/3649],
[1809/3050, 32/41],
[1829/2750, 97/961],
[1864/2843, 145/2993],
[191/3490, 1211/1243],
[1939/3350, 19/147],
[196/335, 2719/3553],
[1964/3085, 31/97],
[19/70, 16/33],
[2093/3534, 115/147],
[209/338, 819/1331],
[211/350, 80/107],
[23/154, 895/993],
[23/34, 481/2881],
[23/50, 32/41],
[2435/3854, 16/59],
[248/401, 95/449],
[248/665, 29/323],
[256/377, 35/387],
[25/82, 237/275],
[2636/3887, 445/4003],
[280/457, 1243/2107],
[28/45, 559/1617],
[307/830, 267/779],
[31/202, 355/1507],
[313/466, 560/2651],
[3/14, 5/27],
[31/50, 355/1507],
[316/487, 835/2243],
[32/49, 767/2433],
[341/1070, 97/961],
[359/610, 463/1969],
[364/687, 409/441],
[36/77, 2575/2673],
[367/986, 2176/3401],
[37/190, 688/1369],
[373/630, 16/33],
[37/70, 5/27],
[401/650, 16/1067],
[403/590, 97/961],
[404/685, 31/97],
[415/706, 341/459],
[427/694, 128/803],
[43/166, 224/251],
[432/1681, 1055/3553],
[43/94, 480/1849],
[4/47, 65/193],
[455/778, 973/1075],
[463/1970, 16/17],
[468/775, 385/673],
[47/106, 80/97],
[4/7, 15/17],
[48/235, 107/459],
[501/742, 5/27],
[52/205, 137/169],
[555/974, 163/675],
[560/3163, 951/1225],
[56/107, 605/931],
[569/1042, 2735/2993],
[57/130, 32/41],
[577/1546, 1680/1873],
[600/913, 7/25],
[61/718, 581/731],
[65/122, 32/507],
[65/122, 973/1075],
[65/274, 2777/2873],
[700/1303, 931/3075],
[704/1163, 1435/2299],
[7/106, 245/267],
[71/130, 13/19],
[7/194, 1855/3777],
[72/115, 217/729],
[73/274, 1253/1947],
[735/1594, 1024/2401],
[739/1318, 249/601],
[7/466, 656/931],
[75/134, 341/459],
[75/134, 656/675],
[76/143, 327/473],
[767/1834, 235/779],
[777/2050, 224/513],
[7/90, 19/147],
[80/187, 799/2049],
[8/25, 1007/1041],
[8/25, 31/97],
[827/2590, 1963/3531],
[83/126, 5/27],
[839/2170, 736/857],
[881/1538, 160/209],
[907/2046, 1075/1203],
[92/381, 1127/2073],
[95/202, 1264/1411],
[95/202, 301/499],
[965/2086, 1072/1075],
NULL
]:

#@ inner_quasirational_points 
inner_quasirational_points :=
 map(quasirational_lift,inner_quasirational_projections);

#@ F16_C0_quasirational_points 
F16_C0_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(q -> c_rational[0](1/(1-q/2)),farey(12))):

#@ F16_C1_quasirational_points 
F16_C1_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(q -> c_quasirational[1](q-1/2),farey(12))):

#@ F16_C3_quasirational_points 
F16_C3_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(c_quasirational[3],three_circle(99))):

#@ F16_C5_quasirational_points 
F16_C5_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(c_quasirational[5],farey(20))):

#@ quasirational_points 
quasirational_points := [
 op(inner_quasirational_points),
 op(F16_C0_quasirational_points),
 op(F16_C1_quasirational_points),
 op(F16_C3_quasirational_points),
 op(F16_C5_quasirational_points)
];

#@ inner_quasirational_points_latex 
inner_quasirational_points_latex := proc() 
 local P,num_cols,S,T,L,i,j,s;
 P := inner_quasirational_projections;
 num_cols := 8;
 S := cat("\\[ \\renewcommand{\\arraystretch}{2} \n \\begin{array}{","c" $ num_cols,"}\n");
 j := 0;
 L := "\\\\ \n";
 for i from 1 to nops(P) do
  s := P[i];
  T := sprintf("\\left(\\frac{%4d}{%4d},\\frac{%4d}{%4d}\\right)",
               numer(s[1]),denom(s[1]),numer(s[2]),denom(s[2]));
  S := cat(S,T);
  j := j+1;
  if j = num_cols then
   if i < nops(P) then
    S := cat(S,L);
   fi;
   j := 0;
  else
   S := cat(S," &\n");
  fi;
 od;
 S := cat(S,"\n\\end{array} \\]\n");
 return(S);
end:

#@ inner_quasirational_points_tikz 
inner_quasirational_points_tikz := proc()
 local P,S,T,s,i;
 P := inner_quasirational_projections;

 S := "\\begin{center}\n \\begin{tikzpicture}[scale=8]\n";
 S := cat(S,"  \\draw[cyan   ] (0.000,0.000) -- (0.000,0.707);\n");
 S := cat(S,"  \\draw[green  ] (0.000,0.000) -- (1.000,0.000);\n");
 S := cat(S,"  \\draw[magenta] (0.000,0.707) -- (0.816,0.707);\n");
 T := "  \\draw[blue,smooth] ";
 for i from 0 to 11 do
  T := cat(T, sprintf("(%1.3f,%1.3f) -- ",op(evalf(y_proj0(c_E0[5](i*Pi/12))))));
 od; 
 T := cat(T, "(0.816,0.707);\n");
 S := cat(S,T);
 for i from 1 to nops(P) do
  s := evalf(P[i] *~ [sqrt(2),1/sqrt(2)]);
  S := cat(S,sprintf("  \\fill(%1.3f,%1.3f) circle(0.004);\n",s[1],s[2]));
 od;
 S := cat(S," \\end{tikzpicture}\n\\end{center}\n");
 return(S);
end:

######################################################################
# The following table gives a 7 x 5 grid of arithmetically simple
# points that lie in F16 and are quite evenly spaced.

#@ rational_grid_points 
rational_grid_points := [[
 [(1/2)*sqrt(2), (1/2)*sqrt(2), 0, 0],
 [23/34, 23/34, (7/34)*sqrt(2), 0],
 [79/130, 79/130, (47/130)*sqrt(2), 0],
 [1/2, 1/2, (1/2)*sqrt(2), 0],
 [79/202, 79/202, (119/202)*sqrt(2), 0],
 [7/34, 7/34, (23/34)*sqrt(2), 0],
 [0, 0, 1, 0]
],[
 [407/745, 624/745, 0, 0],
 [92124/152207, 232883/304414, (31/202)*sqrt(2), -11005/304414],
 [1287/2425, 344/485, (8/25)*sqrt(2), -248/2425],
 [7267/15458, 8565/15458, (125/262)*sqrt(2), -1000/7729],
 [92619/240218, 224276/600545, (359/610)*sqrt(2), -166217/1201090],
 [143/486, 298/1701, (83/126)*sqrt(2), -415/3402],
 [(1/60)*sqrt(195), 0, (1/4)*sqrt(15), -(1/60)*sqrt(30)]
],[
 [5/13, 12/13, 0, 0], [15/38, 86/95, (1/10)*sqrt(2), -13/190],
 [214099/533478, 217940/266739, (73/274)*sqrt(2), -91469/533478],
 [37323/100798, 29390/50399, (95/202)*sqrt(2), -28595/100798],
 [5389/14982, 27671/74910, (1301/2270)*sqrt(2), -10408/37455],
 [42732/137557, 20435/137557, (280/457)*sqrt(2), -49720/137557],
 [(3/70)*sqrt(55), 0, (2/7)*sqrt(10), -(9/70)*sqrt(5)]
],[
 [9/41, 40/41, 0, 0], [4793/21846, 72098/76461, (23/154)*sqrt(2), -20585/152922],
 [1121/4510, 1864/2255, (25/82)*sqrt(2), -237/902],
 [22828/111879, 1572959/2461338, (907/2046)*sqrt(2), -975025/2461338],
 [342221/1435238, 4932765/10046666, (1549/3038)*sqrt(2), -2143816/5023333],
 [23/119, 4/17, (4/7)*sqrt(2), -60/119],
 [(1/68)*sqrt(238), 0, (1/12)*sqrt(102), -(7/102)*sqrt(51)]
],[
 [0, 1, 0, 0],
 [0, 59/62, (11/62)*sqrt(2), -11/62],
 [0, 11/13, (4/13)*sqrt(2), -4/13],
 [0, 61/86, (35/86)*sqrt(2), -35/86],
 [0, 1/2, (1/2)*sqrt(2), -1/2],
 [0, 11/38, (21/38)*sqrt(2), -21/38],
 [0, 0, (1/3)*sqrt(6), -(1/3)*sqrt(3)]
]];

