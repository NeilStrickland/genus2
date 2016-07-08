check_disc_delta := proc()
 local i,T,sols,z,zz,err;

 printf("%a()\n",procname);

 _ASSERT(disc_delta_proj(v_E[0]) -~ [0, 1] = [0,0],"disc_delta_proj(v[0])");
 _ASSERT(disc_delta_proj(v_E[1]) -~ [0,-1] = [0,0],"disc_delta_proj(v[1])");
 _ASSERT(disc_delta_proj(v_E[2]) -~ [ 1/sqrt(2),0] = [0,0],"disc_delta_proj(v[2])");
 _ASSERT(disc_delta_proj(v_E[3]) -~ [-1/sqrt(2),0] = [0,0],"disc_delta_proj(v[3])");
 _ASSERT(disc_delta_proj(v_E[4]) -~ [-1/sqrt(2),0] = [0,0],"disc_delta_proj(v[4])");
 _ASSERT(disc_delta_proj(v_E[5]) -~ [ 1/sqrt(2),0] = [0,0],"disc_delta_proj(v[5])");
 _ASSERT(disc_delta_proj(v_E[6]) -~ [ 0,0] = [0,0],"disc_delta_proj(v[6])");
 _ASSERT(disc_delta_proj(v_E[7]) -~ [-1,0] = [0,0],"disc_delta_proj(v[7])");
 _ASSERT(disc_delta_proj(v_E[8]) -~ [ 0,0] = [0,0],"disc_delta_proj(v[8])");
 _ASSERT(disc_delta_proj(v_E[9]) -~ [ 1,0] = [0,0],"disc_delta_proj(v[9])");

 for i in {10,11,12,13,14,16,19,21} do
  _ASSERT(
   simplify(map(abs,disc_delta_proj(v_E[i])) - [0, sqrt(2)*a_E/sqrt(a_E^2+1)]) = [0,0],
   sprintf("disc_delta_proj(v[%d])",i)
  );
 od:

 for i in {15,17,18,20} do
  _ASSERT(
   simplify(map(abs,disc_delta_proj(v_E[i])) - [sqrt((1-a_E^2)/(a_E^2+1)), sqrt(2)*a_E/sqrt(a_E^2+1)]) = [0,0],
   sprintf("disc_delta_proj(v[%d])",i)
  );
 od:

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[0](t)) - [cos(t+Pi/4),0])) = [0,0],
  "disc_delta_proj(c[0](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[1](t)) - [0,cos(t)])) = [0,0],
  "disc_delta_proj(c[1](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[2](t)) - [-sin(t),cos(t)])) = [0,0],
  "disc_delta_proj(c[2](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[ 9](t)) - [0, sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
  "disc_delta_proj(c[ 9](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[12](t)) - [0,-sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
  "disc_delta_proj(c[12](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[10](t)) - [-sqrt(-a_E^4+1)*sin(t)/(a_E^2+1), sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
  "disc_delta_proj(c[10](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_E[11](t)) - [sqrt(-a_E^4+1)*sin(t)/(a_E^2+1), -sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
  "disc_delta_proj(c[11](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_cayley[1](t)) - 
     [(1/3)*sqrt(6)*sqrt((a_E^2+1)/(2*a_E^2+1))*cos(t-arctan((1/3)*sqrt(3)*sqrt((-2*a_E^2+1)/(2*a_E^2+1)))),
      -(2/3)*a_E*sqrt(6)*sin(t)/sqrt(2*a_E^2+1)])) = [0,0],
  "disc_delta_proj(c_cayley[1](t))"
 );

 _ASSERT(
  simplify(expand(disc_delta_proj(c_cayley[4](Pi-t)) - 
     [(1/3)*sqrt(6)*sqrt((a_E^2+1)/(2*a_E^2+1))*cos(t-arctan((1/3)*sqrt(3)*sqrt((-2*a_E^2+1)/(2*a_E^2+1)))),
      -(2/3)*a_E*sqrt(6)*sin(t)/sqrt(2*a_E^2+1)])) = [0,0],
  "disc_delta_proj(c_cayley[4](t))"
 );

 zz := disc_delta_proj(c_cayley[1](t));
 err := simplify(expand(2*zz[1]^2+sqrt(1-4*a_E^4)/(sqrt(1+2*a_E^2)*a_E)*zz[1]*zz[2]+(1+1/a_E^2)/2*zz[2]^2-1));

 _ASSERT(err = 0,"disc_delta_proj(c_cayley[1]) ellipse");

 unassign('zz');

 for T in K8 do
  _ASSERT(
   simplify(disc_delta_proj(act_R4[T](xx)) - act_disc_delta[T](disc_delta_proj(xx)))=[0,0],
   sprintf("disc_delta_proj is %a-equivariant",T)
  );
 od:

 sols :=
  map(s -> subs(s,xx),
   {solve({disc_delta_proj(xx)[1]=cos(t),
	   disc_delta_proj(xx)[2]=sin(t),
	   rho(xx)=1,
	   g0(xx)=0},
	  {x[1],x[2],x[3],x[4]})});
 _ASSERT(sols = {c_E[2](t-Pi/2)},"disc_delta proj over boundary");

 zz := [z[1],z[2]];

 _ASSERT(
  is_member_E(disc_delta_lift(zz)),
  "disc_delta_lift maps to X"
 );

 _ASSERT(
  simplify(disc_delta_proj(disc_delta_lift(zz)) - zz) = [0,0],
  "disc_delta_proj o disc_delta_lift"
 );

end:

add_check(check_disc_delta):

######################################################################

check_disc_pi := proc()
 local i,j,t,T;

 printf("%a()\n",procname);

 for i in {0,1,10,11,12,13} do
  _ASSERT(disc_pi_proj(v_E[i]) = [0,0],sprintf("disc_pi_proj(v[%d])",i));
 od;
 for i from 0 to 3 do
  j := i+2;
  t := i*Pi/2;
  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ [cos(t),sin(t)]) = [0,0],
	  sprintf("disc_pi_proj(v[%d])",j));
 od:
 for i from 0 to 3 do
  j := i+6;
  t := i*Pi/2 + Pi/4;
  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ [cos(t),sin(t)]) = [0,0],
	  sprintf("disc_pi_proj(v[%d])",j));
 od:
 for i from 0 to 3 do
  j := i+14;
  t := i*Pi/2 + Pi/4;
  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ sqrt((1-a_E^2)/(a_E^2+1)) *~ [cos(t),sin(t)]) = [0,0],
	  sprintf("disc_pi_proj(v[%d])",j));
 od:
 for i from 0 to 3 do
  j := i+18;
  t := i*Pi/2 - Pi/4;
  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ sqrt((1-a_E^2)/(a_E^2+1)) *~ [cos(t),sin(t)]) = [0,0],
	  sprintf("disc_pi_proj(v[%d])",j));
 od:

 for T in G16 do
  _ASSERT(
   simplify(disc_pi_proj(act_R4[T](xx)) - act_disc_pi[T](disc_pi_proj(xx)))=[0,0],
   sprintf("disc_pi_proj is %a-equivariant",T)
  );
 od:
end:

add_check(check_disc_pi):

######################################################################

check_disc_zeta := proc()
 local i,j,t,T,c0,c1;

 printf("%a()\n",procname);
 
 for i in {0,1} do
  _ASSERT(disc_zeta_proj(v_E[i]) = [(-1)^i/sqrt(2),0],sprintf("disc_zeta_proj(v[%d])",i));
 od;
 for i in {2,4} do
  _ASSERT(disc_zeta_proj(v_E[i]) = [0,0],sprintf("disc_zeta_proj(v[%d])",i));
 od;
 for i in {3,5} do
  _ASSERT(disc_zeta_proj(v_E[i]) = [0,(-1)^((i-3)/2)],sprintf("disc_zeta_proj(v[%d])",i));
 od;
 for i in {6,7,8,9} do
  _ASSERT(disc_zeta_proj(v_E[i]) = [0,(-1)^floor((i-6)/2)/sqrt(2)],sprintf("disc_zeta_proj(v[%d])",i));
 od;

 c0 :=  (1/2)*(sqrt(2)*a_E-sqrt(1-a_E^2))*sqrt(2)/sqrt(1+a_E^2);
 c1 :=  (1/2)*(sqrt(2)*a_E+sqrt(1-a_E^2))*sqrt(2)/sqrt(1+a_E^2);

 for i in {10,12} do
  _ASSERT(
   simplify_E(disc_zeta_proj(v_E[i]) -~ [(-1)^((i-10)/2)*c0,0]) = [0,0],
   sprintf("disc_zeta_proj(v[%d])",i)
  );
 od;

 for i in {11,13} do
  _ASSERT(
   simplify_E(disc_zeta_proj(v_E[i]) -~ [(-1)^((i-11)/2)*c1,0]) = [0,0],
   sprintf("disc_zeta_proj(v[%d])",i)
  );
 od;

 _ASSERT(
  disc_zeta_proj(c_E[0](t)) -~ [0,sin(t)] = [0,0],
  "disc_zeta_proj(c[0](t))"
 );

 _ASSERT(
  disc_zeta_proj(c_E[1](t)) -~ [cos(t),sin(t)] /~ sqrt(2) = [0,0],
  "disc_zeta_proj(c[1](t))"
 );

 _ASSERT(
  disc_zeta_proj(c_E[2](t)) -~ [cos(t),sin(t)] /~ sqrt(2) = [0,0],
  "disc_zeta_proj(c[2](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[3](t)) -~ [(sqrt(1/3)+sqrt(1/6)) * cos(t),sin(t)]) = [0,0],
  "disc_zeta_proj(c[3](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[4](t)) -~ [(sqrt(1/3)-sqrt(1/6)) * cos(t),0]) = [0,0],
  "disc_zeta_proj(c[4](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[5](t)) -~ [2+(1-cos(t))/sqrt(2),0] /~ sqrt(10 - 2*cos(t))) = [0,0],
  "disc_zeta_proj(c[5](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[6](t)) -~ [2-(1-cos(t))/sqrt(2), sin(t)] /~ sqrt(10 - 2*cos(t))) = [0,0],
  "disc_zeta_proj(c[6](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[7](t)) +~ [2+(1-cos(t))/sqrt(2),0] /~ sqrt(10 - 2*cos(t))) = [0,0],
  "disc_zeta_proj(c[7](t))"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[8](t)) +~ [2-(1-cos(t))/sqrt(2),-sin(t)] /~ sqrt(10 - 2*cos(t))) = [0,0],
  "disc_zeta_proj(c[8](t))"
 );

 for T in [1,LL,M,LLM,N,LLN,MN,LLMN] do
  _ASSERT(
   simplify(disc_zeta_proj(act_R4[T](xx)) - act_disc_zeta[T](disc_zeta_proj(xx)))=[0,0],
   sprintf("disc_zeta_proj is %a-equivariant",T)
  );
 od:
end:

add_check(check_disc_zeta):

