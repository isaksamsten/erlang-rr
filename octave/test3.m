function test3(X, Xval, y, yval, ...
              si = 0.99, ub = 0, nl=0, opts = "-n 100 --progress none", ...
               ttl = 'Random Forest, E_{in}=%g, E_{out}=%g, F1_{out}=%g')
  model = rflearn(X, y, opts);
  p = rfpredict(X, model);
  Ein = 1-mean(double(y == p));

  p = rfpredict(Xval, model);
  Eout = 1-mean(double(yval == p));

  [c, nu, prec, recall, F] = getcm(yval, p, [-1, 1]);

  clf;
  plotData(X, y);
  visualizeBoundaryRf(X, y, @(x) rfpredict(x, model));
  plotTarget(X, si, ub);
  hold on;
  title(sprintf(ttl, Ein, Eout, (F(1)+F(2))/2));
  xlabel('x_1'); ylabel('x_2');
  legend('target function');
  legend
  hold off;
end
