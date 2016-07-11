######################################################################

# These are some auxiliary functions that appear in Dunavant's
# analysis of symmetric triangle quadrature rules.  If we had 
# implemented static methods, these would be static methods of the
# triangle quadrature rule class.

#@ dunavant_v
dunavant_v := proc(j,k)
 option remember;
 int(cos(k*alpha)/cos(alpha)^(j+2),alpha=-Pi/3..Pi/3)/(sqrt(3)*(j+2)*2^j);
end:

#@ dunavant_jk_list
dunavant_jk_list := proc(deg)
 local L;
 L := [seq(seq([2*m-3*k,k],m=3*k..(3*k+deg)/2),k=0..6)]:
end:

#@ dunavant_m
dunavant_m := proc(deg)
 local alpha;
 
 alpha := [3,-4,-1,0,-1,-4][modp(deg,6)+1];
 return ((deg+3)^3 + alpha)/12;
end:

######################################################################

#@ CLASS: triangle_quadrature_rule

`Class/Declare`("triangle_quadrature_rule",
 "An instance of this class represents a rule for approximate integration of functions on the two-simplex.  Such a rule consists of a list of sample points and a list of weights; the approximate integral is obtained by evaluating at the sample points, multiplying by the corresponding weights, and taking the sum.  Following work of Dunavant, we consider only rules that are invariant under the evident action of the symmetric group $S_3$ on the simplex.  We choose one sample point from each $S_3$-orbit, and call these the base points for the rule.",

 ["Field","description"::string = ""],

 ["Field","degree"::integer,
  "The degree should be set to $d$ if the quadrature rule integrates all polynomials of degree at most $d$ exactly (up to the working precision)."
 ],

 ["Field","num_base_points"::integer,
  "The number of base points, or equivalently the number of $S_3$-orbits of sample points."
 ],

 ["Field","base_points"::list(RR_3),
  "The list of base points.  These should have the form @[p1,p2,p3]@ where either @p1 < p2 < p3@ or @p1 <> p2 = p3@ or @p1 = p2 = p3@."
 ],

 ["Field","base_weights"::list(RR),"The list of weights of base points"],
 ["Field","base_multiplicities"::list(posint),
  "The list of multiplicities of base points; the multiplicity is the size of the $S_3$-orbit"
 ],

 ["Field","base_r"::list(RR),
  "The list of $r$-values for base points.  Here the $r$-value of a point $u$ is the distance from the origin to the point @triangle_proj(u)@ in $\\mathbb{R}^2$."
 ],

 ["Field","base_theta"::list(RR),
  "The list of $\\theta$-values for base points.  Here the $\\theta$-value of a point $u$ is the standard polar coordinate for the point @triangle_proj(u)@ in $\\mathbb{R}^2$."
 ],

 ["Field","num_points"::integer,"The number of sample points."],
 ["Field","points"::list(RR_3),"The list of all sample points"],
 ["Field","weights"::list(RR),"The list of weights for all sample points"],

 ["Constructor","",
  proc(this,deg::integer,pts::list(RR_3),wgts::list(RR))
   this["set",deg,pts,wgts];
  end
 ],

 ["Method","set"::void,"",
  proc(this,deg::integer,pts::list([numeric,numeric,numeric]),wgts::list(numeric)) 
   local i,m,p,q,w;

   this["degree"] := deg;
   this["num_base_points"] := nops(pts);
   this["base_points"] := pts;
   this["base_weights"] := wgts;
   this["base_multiplicities"] := [];
   this["base_r"] := [];
   this["base_theta"] := [];
   this["points"] := [];
   this["weights"] := [];

   for i from 1 to nops(pts) do
    p := pts[i];
    w := wgts[i];
    q := triangle_proj(p);

    if p[2] = p[3] then
     if p[1] = p[2] then
      m := 1;
      this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
      this["points"] := [op(this["points"]),p];
      this["weights"] := [op(this["weights"]),w];
      this["base_r"] := [op(this["base_r"]),0];
      this["base_theta"] := [op(this["base_theta"]),0];
     else
      m := 3;
      this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
      this["points"] := [op(this["points"]),
		     [p[1],p[2],p[2]],
		     [p[2],p[1],p[2]],
		     [p[2],p[2],p[1]]];
      this["weights"] := [op(this["weights"]),w$3];
      this["base_r"] := [op(this["base_r"]),q[1]];
      this["base_theta"] := [op(this["base_theta"]),0];
     fi;
    else
     m := 6;
     this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
     this["points"] := [op(this["points"]),
		     [p[1],p[2],p[3]],
		     [p[1],p[3],p[2]],
		     [p[2],p[1],p[3]],
		     [p[2],p[3],p[1]],
		     [p[3],p[1],p[2]],
		     [p[3],p[2],p[1]]];
     this["weights"] := [op(this["weights"]),w$6];
     this["base_r"] := [op(this["base_r"]),evalf(sqrt(q[1]^2+q[2]^2))];
     this["base_theta"] := [op(this["base_theta"]),evalf(arctan(q[2],q[1]))];
    fi;
   od:

   this["num_points"] := nops(this["points"]);
   NULL;
  end
 ],

 ["Method","int",
  "This returns the approximate integral of @u@, which is expected to be an expression in the variables @t[1], t[2] and t[3]@",
  proc(this,u)
   local T,np,p,w,i;
   T := 0;
   np := this["num_points"];

   for i from 1 to np do
    p := this["points"][i];
    w := this["weights"][i];
    T := T + evalf(w * subs({t[1]=p[1],t[2]=p[2],t[3]=p[3]},u));
   od:

   return T;
  end
 ],

 ["Method","split_int",
  "This returns an approximate integral of @u@, obtained by dividing the triangle into $4^k$ smaller triangles, and applying the givenquadrature rule on each piece.",
  proc(this,u,k)
   if k = 0 then 
    return this["int",u];
   else 
    return  
     this["split_int",subs({t[1]=t[1]+t[2]/2+t[3]/2,t[2]=t[2]/2,t[3]=t[3]/2},u),k-1]/4 + 
     this["split_int",subs({t[1]=t[1]/2,t[2]=t[1]/2+t[2]+t[3]/2,t[3]=t[3]/2},u),k-1]/4 + 
     this["split_int",subs({t[1]=t[1]/2,t[2]=t[2]/2,t[3]=t[1]/2+t[2]/2+t[3]},u),k-1]/4 + 
     this["split_int",subs({t[1]=t[2]/2+t[3]/2,t[2]=t[1]/2+t[3]/2,t[3]=t[1]/2+t[2]/2},u),k-1]/4;
   fi:
  end
 ],

 ["Method","exact_int",
  "This calculates the integral of @u@ using Maple's adaptive algorithms.",
  proc(this,u)
   2 * evalf(int(int(subs(t[3]=1-t[1]-t[2],u),t[2]=0..1-t[1]),t[1]=0..1));
  end
 ],

 ["Method","base_plot","This generates a plot showing the base points",
  proc(this)
   return display(
    line(triangle_proj([1,0,0]),triangle_proj([0,1,0]),color=red),
    line(triangle_proj([0,1,0]),triangle_proj([0,0,1]),color=red),
    line(triangle_proj([0,0,1]),triangle_proj([1,0,0]),color=red),
    line(triangle_proj([0,0,1]),triangle_proj([1/2,1/2,0]),color=green),
    line(triangle_proj([0,1,0]),triangle_proj([1/2,0,1/2]),color=green),
    line(triangle_proj([1,0,0]),triangle_proj([0,1/2,1/2]),color=green),
    op(map(p -> point(evalf(triangle_proj(p))),this["base_points"])),
    axes = none, scaling = constrained
   ):
  end
 ],

 ["Method","plot","This generates a plot showing all the quadrature points",
  proc(this)
   return display(
    line(triangle_proj([1,0,0]),triangle_proj([0,1,0]),color=red),
    line(triangle_proj([0,1,0]),triangle_proj([0,0,1]),color=red),
    line(triangle_proj([0,0,1]),triangle_proj([1,0,0]),color=red),
    line(triangle_proj([0,0,1]),triangle_proj([1/2,1/2,0]),color=green),
    line(triangle_proj([0,1,0]),triangle_proj([1/2,0,1/2]),color=green),
    line(triangle_proj([1,0,0]),triangle_proj([0,1/2,1/2]),color=green),
    op(map(p -> point(evalf(triangle_proj(p))),this["points"])),
    axes = none, scaling = constrained
   ):
  end
 ],

 ["Method","moment_eq","This generates an equation depending on @j@ and @k@, which will be satisfied if the quadrature rule is exact.",
  proc(this,j,k)
   local E,i,r,theta,m,w;

   if j = 0 and k = 0 then
    return add(w, w in this["weights"]) - 1;
   fi;

   if j < 2 or j > this["degree"] or k < 0 or 3*k > j or modp(j+3*k,2) <> 0 then return FAIL; fi;

   E := (-1)^(k+1)*dunavant_v(j,3*k);
   for i from 2 to this["num_base_points"] do
    r := this["base_r"][i];
    theta := this["base_theta"][i];
    m := this["base_multiplicities"][i];
    w := this["base_weights"][i];
    if m = 3 then
     E := E + 3 * w * r^j;
    elif m = 6 then
     E := E + 6 * w * r^j * cos(3*k*theta);
    fi;
   od:

   return E;
  end
 ],

 ["Method","accuracy"::numeric,"",
  proc(this)
   local L;
   L := dunavant_jk_list(this["degree"]);
   return max(seq(abs(this["moment_eq",op(jk)]),jk in L));
  end
 ],

 ["Method","adjust","",
  proc(this)
   local Q1,np,m,r,theta,w,r1,theta1,w1,p1,i;

   np := this["num_base_points"];

   Q1 := `new/triangle_quadrature_rule`(this["degree"],[],[]);

   Q1["degree"]              := this["degree"];
   Q1["num_base_points"]     := this["num_base_points"];
   Q1["base_multiplicities"] := this["base_multiplicities"];

   Q1["base_points"] := [];
   Q1["base_weights"] := [];
   Q1["base_r"] := [];
   Q1["base_theta"] := [];
   Q1["points"] := [];
   Q1["weights"] := [];

   for i from 1 to np do 
    m := this["base_multiplicities"][i];
    r := this["base_r"][i];
    theta := this["base_theta"][i];
    w := this["base_weights"][i];

    w1 := w + e * dw[i];

    if m = 1 then
     r1 := 0;
     theta1 := 0;
    elif m = 3 then
     r1 := r + e * dr[i];
     theta1 := 0;
    elif m = 6 then
     r1 := r + e * dr[i];
     theta1 := theta + e * dt[i];
    fi;

    p1 := triangle_lift([r1*cos(theta1),r1*sin(theta1)]);
    p1 := subs(e = 0,p1) +~ e *~ subs(e = 0,map(diff,p1,e));
    p1 := evalf(expand(p1));

    Q1["base_points"]  := [op(Q1["base_points"]),p1];
    Q1["base_r"]       := [op(Q1["base_r"]),r1];
    Q1["base_theta"]   := [op(Q1["base_theta"]),theta1];
    Q1["base_weights"] := [op(Q1["base_weights"]),w1];

    if m = 1 then
     Q1["points"] := [op(Q1["points"]),p1];
    elif m = 3 then
     Q1["points"] := [op(Q1["points"]),
		     [p1[1],p1[2],p1[2]],
		     [p1[2],p1[1],p1[2]],
		     [p1[2],p1[2],p1[1]]];
    elif m = 6 then
     Q1["points"] := [op(Q1["points"]),
		     [p1[1],p1[2],p1[3]],
		     [p1[1],p1[3],p1[2]],
		     [p1[2],p1[1],p1[3]],
		     [p1[2],p1[3],p1[1]],
		     [p1[3],p1[1],p1[2]],
		     [p1[3],p1[2],p1[1]]];
    fi;

    Q1["weights"] := [op(Q1["weights"]),w1$m];
   od;

   return eval(Q1);
  end
 ],

 ["Method","improve","",
  proc(this)
   local Q1,JK,EE,sol,sol0,sol1,i,p,m,w;

   Q1 := eval(this["adjust"]):
   JK := dunavant_jk_list(this["degree"]):
   EE := map(jk -> Q1["moment_eq",op(jk)],JK):
   EE := evalf(map(E -> subs(e=0,E + diff(E,e)), EE)):

   sol := solve(EE):

   sol0 := map(u -> u=0,indets(map(rhs,sol)));
   sol1 := {e=1,op(map(u -> lhs(u) = subs(sol0,rhs(u)),sol))}:
   this["base_weights"] := evalf(subs(sol1,Q1["base_weights"])):
   this["base_r"] := evalf(subs(sol1,Q1["base_r"])):
   this["base_theta"] := evalf(subs(sol1,Q1["base_theta"])):
   this["base_points"] := [seq(evalf(triangle_lift(this["base_r"][i] *~ [cos(this["base_theta"][i]),sin(this["base_theta"][i])])),
			    i=1..this["num_base_points"])]:
   this["points"] := []:
   this["weights"] := []:
   for i from 1 to this["num_base_points"] do
    p := this["base_points"][i];
    m := this["base_multiplicities"][i];
    w := this["base_weights"][i];
    this["weights"] := [op(this["weights"]),w$m];
    if m = 1 then
     this["points"] := [op(this["points"]),p];
    elif m = 3 then
     this["points"] := [op(this["points"]),
		     [p[1],p[2],p[2]],
		     [p[2],p[1],p[2]],
		     [p[2],p[2],p[1]]];
    elif m = 6 then
     this["points"] := [op(this["points"]),
		     [p[1],p[2],p[3]],
		     [p[1],p[3],p[2]],
		     [p[2],p[1],p[3]],
		     [p[2],p[3],p[1]],
		     [p[3],p[1],p[2]],
		     [p[3],p[2],p[1]]];
    fi;
   od:

   NULL;
  end
 ]
);

