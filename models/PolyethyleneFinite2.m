init = [0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]+1;
t = linspace(0.0,100.0,1000);
function xdot = f(x,t) 

xdot(1) = 0.1.*x(10) .+ -1.0.*x(1).*x(2) .+ 0.1.*x(9) .+ 0.1.*x(11);
xdot(2) = 0.1.*x(10) .+ -1.0.*x(1).*x(2) .+ 0.1.*x(12) .+ 0.1.*x(13);
xdot(3) = 0.1.*x(16) .+ -1.0.*x(3).*x(4) .+ 0.1.*x(15) .+ 0.1.*x(17);
xdot(4) = 0.1.*x(16) .+ -1.0.*x(3).*x(4) .+ 0.1.*x(18) .+ 0.1.*x(19);
xdot(5) = 0.1.*x(9) .+ 0.1.*x(11);
xdot(6) = 0.1.*x(14) .+ -1.0.*x(6).*x(7) .+ 0.1.*x(12) .+ 0.1.*x(13);
xdot(7) = 0.1.*x(14) .+ -1.0.*x(6).*x(7) .+ 0.1.*x(15) .+ 0.1.*x(17);
xdot(8) = 0.1.*x(18) .+ 0.1.*x(19);
xdot(9) = -0.1.*x(9);
xdot(10) = -0.1.*x(10) .+ x(1).*x(2);
xdot(11) = -0.1.*x(11);
xdot(12) = -0.1.*x(12);
xdot(13) = -0.1.*x(13);
xdot(14) = -0.1.*x(14) .+ x(6).*x(7);
xdot(15) = -0.1.*x(15);
xdot(16) = -0.1.*x(16) .+ x(3).*x(4);
xdot(17) = -0.1.*x(17);
xdot(18) = -0.1.*x(18);
xdot(19) = -0.1.*x(19);
endfunction;

function jac = jj(x,t)

jac(1,1) = -1.0.*x(2);
jac(1,2) = -1.0.*x(1);
jac(1,3) = 0.0;
jac(1,4) = 0.0;
jac(1,5) = 0.0;
jac(1,6) = 0.0;
jac(1,7) = 0.0;
jac(1,8) = 0.0;
jac(1,9) = 0.1;
jac(1,10) = 0.1;
jac(1,11) = 0.1;
jac(1,12) = 0.0;
jac(1,13) = 0.0;
jac(1,14) = 0.0;
jac(1,15) = 0.0;
jac(1,16) = 0.0;
jac(1,17) = 0.0;
jac(1,18) = 0.0;
jac(1,19) = 0.0;
jac(2,1) = -1.0.*x(2);
jac(2,2) = -1.0.*x(1);
jac(2,3) = 0.0;
jac(2,4) = 0.0;
jac(2,5) = 0.0;
jac(2,6) = 0.0;
jac(2,7) = 0.0;
jac(2,8) = 0.0;
jac(2,9) = 0.0;
jac(2,10) = 0.1;
jac(2,11) = 0.0;
jac(2,12) = 0.1;
jac(2,13) = 0.1;
jac(2,14) = 0.0;
jac(2,15) = 0.0;
jac(2,16) = 0.0;
jac(2,17) = 0.0;
jac(2,18) = 0.0;
jac(2,19) = 0.0;
jac(3,1) = 0.0;
jac(3,2) = 0.0;
jac(3,3) = -1.0.*x(4);
jac(3,4) = -1.0.*x(3);
jac(3,5) = 0.0;
jac(3,6) = 0.0;
jac(3,7) = 0.0;
jac(3,8) = 0.0;
jac(3,9) = 0.0;
jac(3,10) = 0.0;
jac(3,11) = 0.0;
jac(3,12) = 0.0;
jac(3,13) = 0.0;
jac(3,14) = 0.0;
jac(3,15) = 0.1;
jac(3,16) = 0.1;
jac(3,17) = 0.1;
jac(3,18) = 0.0;
jac(3,19) = 0.0;
jac(4,1) = 0.0;
jac(4,2) = 0.0;
jac(4,3) = -1.0.*x(4);
jac(4,4) = -1.0.*x(3);
jac(4,5) = 0.0;
jac(4,6) = 0.0;
jac(4,7) = 0.0;
jac(4,8) = 0.0;
jac(4,9) = 0.0;
jac(4,10) = 0.0;
jac(4,11) = 0.0;
jac(4,12) = 0.0;
jac(4,13) = 0.0;
jac(4,14) = 0.0;
jac(4,15) = 0.0;
jac(4,16) = 0.1;
jac(4,17) = 0.0;
jac(4,18) = 0.1;
jac(4,19) = 0.1;
jac(5,1) = 0.0;
jac(5,2) = 0.0;
jac(5,3) = 0.0;
jac(5,4) = 0.0;
jac(5,5) = 0.0;
jac(5,6) = 0.0;
jac(5,7) = 0.0;
jac(5,8) = 0.0;
jac(5,9) = 0.1;
jac(5,10) = 0.0;
jac(5,11) = 0.1;
jac(5,12) = 0.0;
jac(5,13) = 0.0;
jac(5,14) = 0.0;
jac(5,15) = 0.0;
jac(5,16) = 0.0;
jac(5,17) = 0.0;
jac(5,18) = 0.0;
jac(5,19) = 0.0;
jac(6,1) = 0.0;
jac(6,2) = 0.0;
jac(6,3) = 0.0;
jac(6,4) = 0.0;
jac(6,5) = 0.0;
jac(6,6) = -1.0.*x(7);
jac(6,7) = -1.0.*x(6);
jac(6,8) = 0.0;
jac(6,9) = 0.0;
jac(6,10) = 0.0;
jac(6,11) = 0.0;
jac(6,12) = 0.1;
jac(6,13) = 0.1;
jac(6,14) = 0.1;
jac(6,15) = 0.0;
jac(6,16) = 0.0;
jac(6,17) = 0.0;
jac(6,18) = 0.0;
jac(6,19) = 0.0;
jac(7,1) = 0.0;
jac(7,2) = 0.0;
jac(7,3) = 0.0;
jac(7,4) = 0.0;
jac(7,5) = 0.0;
jac(7,6) = -1.0.*x(7);
jac(7,7) = -1.0.*x(6);
jac(7,8) = 0.0;
jac(7,9) = 0.0;
jac(7,10) = 0.0;
jac(7,11) = 0.0;
jac(7,12) = 0.0;
jac(7,13) = 0.0;
jac(7,14) = 0.1;
jac(7,15) = 0.1;
jac(7,16) = 0.0;
jac(7,17) = 0.1;
jac(7,18) = 0.0;
jac(7,19) = 0.0;
jac(8,1) = 0.0;
jac(8,2) = 0.0;
jac(8,3) = 0.0;
jac(8,4) = 0.0;
jac(8,5) = 0.0;
jac(8,6) = 0.0;
jac(8,7) = 0.0;
jac(8,8) = 0.0;
jac(8,9) = 0.0;
jac(8,10) = 0.0;
jac(8,11) = 0.0;
jac(8,12) = 0.0;
jac(8,13) = 0.0;
jac(8,14) = 0.0;
jac(8,15) = 0.0;
jac(8,16) = 0.0;
jac(8,17) = 0.0;
jac(8,18) = 0.1;
jac(8,19) = 0.1;
jac(9,1) = 0.0;
jac(9,2) = 0.0;
jac(9,3) = 0.0;
jac(9,4) = 0.0;
jac(9,5) = 0.0;
jac(9,6) = 0.0;
jac(9,7) = 0.0;
jac(9,8) = 0.0;
jac(9,9) = -0.1;
jac(9,10) = 0.0;
jac(9,11) = 0.0;
jac(9,12) = 0.0;
jac(9,13) = 0.0;
jac(9,14) = 0.0;
jac(9,15) = 0.0;
jac(9,16) = 0.0;
jac(9,17) = 0.0;
jac(9,18) = 0.0;
jac(9,19) = 0.0;
jac(10,1) = x(2);
jac(10,2) = x(1);
jac(10,3) = 0.0;
jac(10,4) = 0.0;
jac(10,5) = 0.0;
jac(10,6) = 0.0;
jac(10,7) = 0.0;
jac(10,8) = 0.0;
jac(10,9) = 0.0;
jac(10,10) = -0.1;
jac(10,11) = 0.0;
jac(10,12) = 0.0;
jac(10,13) = 0.0;
jac(10,14) = 0.0;
jac(10,15) = 0.0;
jac(10,16) = 0.0;
jac(10,17) = 0.0;
jac(10,18) = 0.0;
jac(10,19) = 0.0;
jac(11,1) = 0.0;
jac(11,2) = 0.0;
jac(11,3) = 0.0;
jac(11,4) = 0.0;
jac(11,5) = 0.0;
jac(11,6) = 0.0;
jac(11,7) = 0.0;
jac(11,8) = 0.0;
jac(11,9) = 0.0;
jac(11,10) = 0.0;
jac(11,11) = -0.1;
jac(11,12) = 0.0;
jac(11,13) = 0.0;
jac(11,14) = 0.0;
jac(11,15) = 0.0;
jac(11,16) = 0.0;
jac(11,17) = 0.0;
jac(11,18) = 0.0;
jac(11,19) = 0.0;
jac(12,1) = 0.0;
jac(12,2) = 0.0;
jac(12,3) = 0.0;
jac(12,4) = 0.0;
jac(12,5) = 0.0;
jac(12,6) = 0.0;
jac(12,7) = 0.0;
jac(12,8) = 0.0;
jac(12,9) = 0.0;
jac(12,10) = 0.0;
jac(12,11) = 0.0;
jac(12,12) = -0.1;
jac(12,13) = 0.0;
jac(12,14) = 0.0;
jac(12,15) = 0.0;
jac(12,16) = 0.0;
jac(12,17) = 0.0;
jac(12,18) = 0.0;
jac(12,19) = 0.0;
jac(13,1) = 0.0;
jac(13,2) = 0.0;
jac(13,3) = 0.0;
jac(13,4) = 0.0;
jac(13,5) = 0.0;
jac(13,6) = 0.0;
jac(13,7) = 0.0;
jac(13,8) = 0.0;
jac(13,9) = 0.0;
jac(13,10) = 0.0;
jac(13,11) = 0.0;
jac(13,12) = 0.0;
jac(13,13) = -0.1;
jac(13,14) = 0.0;
jac(13,15) = 0.0;
jac(13,16) = 0.0;
jac(13,17) = 0.0;
jac(13,18) = 0.0;
jac(13,19) = 0.0;
jac(14,1) = 0.0;
jac(14,2) = 0.0;
jac(14,3) = 0.0;
jac(14,4) = 0.0;
jac(14,5) = 0.0;
jac(14,6) = x(7);
jac(14,7) = x(6);
jac(14,8) = 0.0;
jac(14,9) = 0.0;
jac(14,10) = 0.0;
jac(14,11) = 0.0;
jac(14,12) = 0.0;
jac(14,13) = 0.0;
jac(14,14) = -0.1;
jac(14,15) = 0.0;
jac(14,16) = 0.0;
jac(14,17) = 0.0;
jac(14,18) = 0.0;
jac(14,19) = 0.0;
jac(15,1) = 0.0;
jac(15,2) = 0.0;
jac(15,3) = 0.0;
jac(15,4) = 0.0;
jac(15,5) = 0.0;
jac(15,6) = 0.0;
jac(15,7) = 0.0;
jac(15,8) = 0.0;
jac(15,9) = 0.0;
jac(15,10) = 0.0;
jac(15,11) = 0.0;
jac(15,12) = 0.0;
jac(15,13) = 0.0;
jac(15,14) = 0.0;
jac(15,15) = -0.1;
jac(15,16) = 0.0;
jac(15,17) = 0.0;
jac(15,18) = 0.0;
jac(15,19) = 0.0;
jac(16,1) = 0.0;
jac(16,2) = 0.0;
jac(16,3) = x(4);
jac(16,4) = x(3);
jac(16,5) = 0.0;
jac(16,6) = 0.0;
jac(16,7) = 0.0;
jac(16,8) = 0.0;
jac(16,9) = 0.0;
jac(16,10) = 0.0;
jac(16,11) = 0.0;
jac(16,12) = 0.0;
jac(16,13) = 0.0;
jac(16,14) = 0.0;
jac(16,15) = 0.0;
jac(16,16) = -0.1;
jac(16,17) = 0.0;
jac(16,18) = 0.0;
jac(16,19) = 0.0;
jac(17,1) = 0.0;
jac(17,2) = 0.0;
jac(17,3) = 0.0;
jac(17,4) = 0.0;
jac(17,5) = 0.0;
jac(17,6) = 0.0;
jac(17,7) = 0.0;
jac(17,8) = 0.0;
jac(17,9) = 0.0;
jac(17,10) = 0.0;
jac(17,11) = 0.0;
jac(17,12) = 0.0;
jac(17,13) = 0.0;
jac(17,14) = 0.0;
jac(17,15) = 0.0;
jac(17,16) = 0.0;
jac(17,17) = -0.1;
jac(17,18) = 0.0;
jac(17,19) = 0.0;
jac(18,1) = 0.0;
jac(18,2) = 0.0;
jac(18,3) = 0.0;
jac(18,4) = 0.0;
jac(18,5) = 0.0;
jac(18,6) = 0.0;
jac(18,7) = 0.0;
jac(18,8) = 0.0;
jac(18,9) = 0.0;
jac(18,10) = 0.0;
jac(18,11) = 0.0;
jac(18,12) = 0.0;
jac(18,13) = 0.0;
jac(18,14) = 0.0;
jac(18,15) = 0.0;
jac(18,16) = 0.0;
jac(18,17) = 0.0;
jac(18,18) = -0.1;
jac(18,19) = 0.0;
jac(19,1) = 0.0;
jac(19,2) = 0.0;
jac(19,3) = 0.0;
jac(19,4) = 0.0;
jac(19,5) = 0.0;
jac(19,6) = 0.0;
jac(19,7) = 0.0;
jac(19,8) = 0.0;
jac(19,9) = 0.0;
jac(19,10) = 0.0;
jac(19,11) = 0.0;
jac(19,12) = 0.0;
jac(19,13) = 0.0;
jac(19,14) = 0.0;
jac(19,15) = 0.0;
jac(19,16) = 0.0;
jac(19,17) = 0.0;
jac(19,18) = 0.0;
jac(19,19) = -0.1;
endfunction;

x = lsode({@f,@jj}, init, t);
save ("-ascii", "-", "x");
plot(t,x);