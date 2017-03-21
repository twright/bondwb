init = [10.0;0.0;100.0;0.0;0.0];
t = linspace(0.0,10.0,100);
function xdot = f(x,t) 

xdot(1) = 5.0.*x(5) .+ 5.0.*x(4);
xdot(2) = 5.0.*x(4);
xdot(3) = 5.0.*x(5);
xdot(4) = -5.0.*x(4);
xdot(5) = -5.0.*x(5) .+ 10.0.*x(5) .+ -10.0.*x(5);
endfunction;

function jac = jj(x,t)

jac(1,1) = 0.0;
jac(1,2) = 0.0;
jac(1,3) = 0.0;
jac(1,4) = 5.0;
jac(1,5) = 5.0;
jac(2,1) = 0.0;
jac(2,2) = 0.0;
jac(2,3) = 0.0;
jac(2,4) = 5.0;
jac(2,5) = 0.0;
jac(3,1) = 0.0;
jac(3,2) = 0.0;
jac(3,3) = 0.0;
jac(3,4) = 0.0;
jac(3,5) = 5.0;
jac(4,1) = 0.0;
jac(4,2) = 0.0;
jac(4,3) = 0.0;
jac(4,4) = -5.0;
jac(4,5) = 0.0;
jac(5,1) = 0.0;
jac(5,2) = 0.0;
jac(5,3) = 0.0;
jac(5,4) = 0.0;
jac(5,5) = -5.0;
endfunction;

x = lsode({@f,@jj}, init, t);
save ("-ascii", "-", "x");
plot(t,x);