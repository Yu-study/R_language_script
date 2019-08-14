mousemove<-function(buttons, x, y) {
  r<-0.2;
  idx=(xx-x)^2+(yy-y)^2<r^2
  plot(xx, yy, type="n")
  rect(-1, -1, 2, 2, col="gray")
  points(xx[idx], yy[idx], col="green", cex = 2)
  points(xx[!idx], yy[!idx], col="green")
  t<-seq(0,2*pi,0.01);
  lines(r*sin(t)+x,r*cos(t)+y,col="blue",lwd=5);
  lines(1.1*r*sin(t)+x,1.1*r*cos(t)+y,col="blue",lwd=5);
}

library(fun)
if(.Platform$OS.type=="windows") x11() else x11(type="Xlib")
mine_sweeper()
