# This is a very crude check, which just verifies that brent_fsolve()
# does the right thing in a single instance.

check_brent := proc()
 local f,F,tol,ret;
 
 printf("%a()\n",procname);

 # This is an example of a function whose roots are moderately hard to find.
 
 f := (x) -> cos(5*Pi*x)+9/4-exp(x/2-x^2/4);
 
 F := (x) -> table(["err" = evalf(f(x))]);

 tol := 10.^(-90);
 ret := brent_fsolve(F,0,1,false,false,tol);

 _ASSERT(evalf(abs(f(ret[1]))) < tol and
         abs(ret[2]["err"]) < tol,
	 "An instance of brent_fsolve()");

end:

add_check(check_brent):