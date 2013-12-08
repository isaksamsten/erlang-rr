function test2(si = 0.99, ub = 0, nl=0)
  X = unifrnd(-1, 1, 1000, 2);
  Xval = unifrnd(-1, 1, 1000, 2);
  y = sintarget(X, si, ub);
  yval = sintarget(Xval, si, ub);

  y = flip(y, nl);
  yval = flip(yval, nl);

  figure(1);
  clf;
  test(X, Xval, y, yval, si, ub, nl, '-n100 --score info', ...
       'Random Forest (with RBF), E_{in}=%g, E_{out}=%g, F1_{out}=%g');
  figure(2);
  clf;
  test3(X, Xval, y, yval, si, ub, nl, '-n100 --score info', ...
       'Random Forest (w/o RBF), E_{in}=%g, E_{out}=%g, F1_{out}=%g');
end

function r = flip(y, nl)
  r = y;
  r(rand(length(y),1) < nl) *= -1;
end
