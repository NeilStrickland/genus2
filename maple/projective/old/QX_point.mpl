point_Q_set_a := proc(a0)
 global point_Q_a;

 point_Q_a := a0;
end:

new_point_Q := proc(c,x1,x2)
 local p;
 p := table();

 p["constraint"] := CONSTRAINT_FIXED;
 p["x"] := [0,0];
 p["u"] := [ 1/sqrt(2),1/sqrt(2)];
 p["v"] := [-1/sqrt(2),1/sqrt(2)];

 if nargs >= 3 then
  point_Q_set(p,args);
 fi;

 return(eval(p));
end:

point_Q_set := proc(p,c,x1,x2)
 p["constraint"] := c;
 p["x"] := [x1,x2];
 point_Q_fix(p);
end:

point_Q_set_C0 := proc(p,t)
 local theta;

 p["constraint"] := CONSTRAINT_C0;
 p["x"] := evalf([sin(t*Pi/2),cos(t*Pi/2)]);
 point_Q_fix(p);
end:

point_Q_set_C1 := proc(p,t)
 p["constraint"] := CONSTRAINT_C1;
 p["x"] := [0,t];
 point_Q_fix(p);
end:

point_Q_set_C3 := proc(p,t)
 local theta;

 p["constraint"] := CONSTRAINT_C3;
 theta := t * point_Q_alpha;
 p["x"] := [point_Q_a + t * (1 - point_Q_a),0];
 point_Q_fix(p);
end:

point_Q_set_C5 := proc(p,t)
 p["constraint"] := CONSTRAINT_C5;
 p["x"] := [t * point_Q_a,0];
 point_Q_fix(p);
end:

point_Q_set_v0 := proc(p)
 point_Q_set_C1(p,0);
 p["constraint"] := CONSTRAINT_FIXED;
end:

point_Q_set_v3 := proc(p)
 point_Q_set_C0(p,1);
 p["constraint"] := CONSTRAINT_FIXED;
end:

point_Q_set_v6 := proc(p)
 point_Q_set_C0(p,0);
 p["constraint"] := CONSTRAINT_FIXED;
end:

point_Q_set_v11 := proc(p)
 point_Q_set_C3(p,0);
 p["constraint"] := CONSTRAINT_FIXED;
end:

point_Q_is_inexact := proc(p)
 return hastype(p["x"],float);
end:

point_Q_fix := proc(p)
 local r;

 if point_Q_is_inexact(p) then
  p["x"] := evalf(p["x"]);
 else 
  p["x"] := simplify(p["x"]);
 fi;

 r := sqrt(p["x"][1]^2 + p["x"][2]^2);

 if ((type(p["x"],[numeric$2]) and r > 1.) or
       p["constraint"] = CONSTRAINT_C) then
    p["x"] := p["x"] /~ r;
 fi;

 if p["constraint"] = CONSTRAINT_C0 then
  p["u"] := [p["x"][2],-p["x"][1]];
  p["v"] := p["x"];
 elif p["constraint"] = CONSTRAINT_C0 then
  p["u"] := [0,1];
  p["v"] := [-1,0];
 else
  p["u"] := [1,0];
  p["v"] := [0,1];
 fi;
end:

point_Q_adjust := proc(p,s,t)
 if p["constraint"] <> CONSTRAINT_FIXED then
  if p["constraint"] = CONSTRAINT_FREE then
   p["x"] := p["x"] +~ (s *~ p["u"]) +~ (t *~ p["v"]);
  else
   p["x"] := p["x"] +~ (s *~ p["u"]);
  fi;

  point_Q_fix(p);
 fi;
end:

point_Q_midpoint := proc(p,q)
 local r;
 r := table():
 r["constraint"] := bitwise_and(p["constraint"],q["constraint"]);
 r["x"] := (p["x"] +~ q["x"]) /~ 2;
 point_Q_fix(r);
 return(eval(r));
end:

point_Q_midpoint4 := proc(p,q,r,s)
 local t;
 t := table():
 t["constraint"] := 
  bitwise_and(
   bitwise_and(p["constraint"],q["constraint"]),
   bitwise_and(r["constraint"],s["constraint"]));

 t["x"] := (p["x"] +~ q["x"] +~ r["x"] +~ s["x"]) /~ 4;
 point_Q_fix(t);
 return(eval(t));
end:

point_Q_orientation := proc(a,b,c)
 local m,n,d;

 m := b["x"] -~ a["x"];
 n := c["x"] -~ a["x"];

 d := m[1]*n[2] - m[2]*n[1];

 return(signum(d));
end:

######################################################################

point_Q_clone := proc(p)
 local p1;

 p1 := new_point_Q();
 p1["constraint"] := p["constraint"];

 p1["x"] := p["x"];
 p1["u"] := p["u"];
 p1["v"] := p["v"];

 return(eval(p1));
end:

point_Q_equal := proc(p,q)
 local err;
 err := p["x"] -~ q["x"];
 if hastype(err,float) then
  err := evalf(err);
  return(evalb(err = [0.,0.]));
 else
  err := simplify(err);
  return(evalb(err = [0,0]));
 fi;
end:

point_Q_print := proc(p)
 printf("  x = %A\n  u = %A\n  v = %A\n",
        p["x"],p["u"],p["v"]);
end: