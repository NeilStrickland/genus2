create_domain_T := proc()
 local X;

 X["type"] := "T";
 X["dim"] := 2;

 X["new_point"] := proc(c_::integer,x_::[numeric,numeric])
  local p;
  p := table();
  p["constraint"] := CONSTRAINT_FREE;
  p["x"] := [ 0,0];
  p["u"] := [ 1,0];
  p["v"] := [ 0,1];

  if nargs >= 2 then
   X["set_point"](p,c_,x_);
  fi;

  return(eval(p));  
 end:

 X["set_point"] := proc(p,c,x)
  p["constraint"] := c;
  p["x"] := x;
  X["fix_point"](p);
 end:

 X["set_C0"] := proc(p,t)
  p["constraint"] := CONSTRAINT_C0;
  p["x"] := [0,t];
  X["fix"](p);
 end:

 X["set_C1"] := proc(p,t)
  p["constraint"] := CONSTRAINT_C1;
  p["x"] := [t,0];
  X["fix"](p);
 end:

 X["set_C3"] := proc(p,t)
  p["constraint"] := CONSTRAINT_C3;
  p["x"] := [t,1];
  X["fix"](p);
 end:

 X["set_C5"] := proc(p,t)
  p["constraint"] := CONSTRAINT_C5;
  p["x"] := [1,t];
  X["fix"](p);
 end:

 X["set_v0"] := proc(p)
  X["set_C1"](p,0);
  p["constraint"] := CONSTRAINT_FIXED;
 end:

 X["set_v3"] := proc(p)
  X["set_C0"](p,1);
  p["constraint"] := CONSTRAINT_FIXED;
 end:

 X["set_v6"] := proc(p)
  X["set_C0"](p,0);
  p["constraint"] := CONSTRAINT_FIXED;
 end:

 X["set_v11"] := proc(p)
  X["set_C3"](p,0);
  p["constraint"] := CONSTRAINT_FIXED;
 end:

 X["is_inexact"] := proc(p)
  return hastype(p["x"],float);
 end:

 X["fix"] := proc(p)
  X["fix_x"](p);
  X["fix_uv"](p);
 end:

 X["fix_x"] := proc(p)
  local x1,x2,constraint;

  if X["is_inexact"](p) then
   p["x"] := evalf(p["x"]);
  else 
   p["x"] := simplify(p["x"]);
  fi;

  x1,x2 := op(p["x"]);
  constraint := p["constraint"];

  if constraint = CONSTRAINT_C0 then x1 := 0; fi;
  if constraint = CONSTRAINT_C1 then x2 := 0; fi;
  if constraint = CONSTRAINT_C3 then x2 := 1; fi;
  if constraint = CONSTRAINT_C5 then x1 := 1; fi;

  x1 := min(1,max(0,x1));
  x2 := min(1,max(0,x2));
  p["x"] := [x1,x2];
 end:

 X["fix_uv"] := proc(p)
  local r,dp,uu,vv,t,constraint,x,n,u,v,u1,u2,u3,u4;

  constraint := p["constraint"];
  u := [1,0];
  v := [0,1];

  if (constraint = CONSTRAINT_C0 or constraint = CONSTRAINT_C5) then
   u := [0,1]; v := [-1,0];
  fi;

  p["u"] := u;
  p["v"] := v;
 end:

 X["adjust"] := proc(p,s,t)
  if p["constraint"] <> CONSTRAINT_FIXED then
   if p["constraint"] = CONSTRAINT_FREE then
    p["x"] := p["x"] +~ (s *~ p["u"]) +~ (t *~ p["v"]);
   else
    p["x"] := p["x"] +~ (s *~ p["u"]);
   fi;

   X["fix"](p);
  fi;
 end:

 X["midpoint"] := proc(p,q)
  local r;
  r := table():
  r["constraint"] := bitwise_and(p["constraint"],q["constraint"]);
  r["x"] := (p["x"] +~ q["x"]) /~ 2;
  X["fix"](r);
  return(eval(r));
 end:

 X["midpoint4"] := proc(p,q,r,s)
  local t;
  t := table():
  t["constraint"] := 
   bitwise_and(
    bitwise_and(p["constraint"],q["constraint"]),
    bitwise_and(r["constraint"],s["constraint"]));

  t["x"] := (p["x"] +~ q["x"] +~ r["x"] +~ s["x"]) /~ 4;
  X["fix"](t);
  return(eval(t));
 end:

 X["orientation"] := proc(a,b,c)
  local ab,ac,d;

  ab := b["x"] -~ a["x"];
  ac := c["x"] -~ a["x"];

  d := LinearAlgebra[Determinant](Matrix([ab,ac]));

  return(signum(d));
 end:

 X["weighting"] := proc(a0,a1,a2)
  local u,v,uu,uv,vv;

  u := a1["x"] -~ a0["x"];
  v := a2["x"] -~ a0["x"];
  uu := dp2(u,u);
  uv := dp2(u,v);
  vv := dp2(v,v);
  return(uv/sqrt(uu * vv - uv * uv));
 end:

 ######################################################################

 X["clone"] := proc(p)
  local p1;

  p1 := X["new_point"]();
  p1["constraint"] := p["constraint"];

  p1["x"] := p["x"];
  p1["u"] := p["u"];
  p1["v"] := p["v"];

  return(eval(p1));
 end:

 X["equal"] := proc(p,q)
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

 X["print"] := proc(p)
  printf("  x = %A\n  u = %A\n  v = %A\n",
	 p["x"],p["u"],p["v"]);
 end:

 return(eval(X));
end:

