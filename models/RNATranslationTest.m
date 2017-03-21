init = [0.0;1.0;1.0;1.0];
t = linspace(0.0,10.0,100);
function xdot = f(x,t) 

xdot(1) = x(3).*x(4);
xdot(2) = -1.0.*x(2).*x(4);
xdot(3) = x(3).*x(4) .+ -1.0.*x(3).*x(4) .+ x(2).*x(4);
xdot(4) = -1.0.*x(3).*x(4) .+ -1.0.*x(2).*x(4);
endfunction;

function jac = jj(x,t)

jac(1,1) = 0.0;
jac(1,2) = 0.0;
jac(1,3) = x(4);
jac(1,4) = x(3);
jac(2,1) = 0.0;
jac(2,2) = -1.0.*x(4);
jac(2,3) = 0.0;
jac(2,4) = -1.0.*x(2);
jac(3,1) = 0.0;
jac(3,2) = x(4);
jac(3,3) = x(4) .+ -1.0.*x(4);
jac(3,4) = x(3) .+ -1.0.*x(3) .+ x(2);
jac(4,1) = 0.0;
jac(4,2) = -1.0.*x(4);
jac(4,3) = -1.0.*x(4);
jac(4,4) = -1.0.*x(3) .+ -1.0.*x(2);
endfunction;

x = lsode({@f,@jj}, init, t);
save ("-ascii", "-", "x");
plot(t,x);
