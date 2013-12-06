function [y,p] = rfpredict(X, model, quiet = 1)
  %
  % -- Function File:  rfpredict(X, model)
  %
  %    Predicts all examples in X using a model
  %        y = rfpredict(X, model)
  %    X is a M x N matrix and the model is a random forest model built using rflearn
  %
  %    Isak Karlsson <isak-kar@dsv.su.se> (December 2013)
  %
  %    See also: rflearn
  if quiet
    q = ' 2> /dev/null';
  else
    q = '';
  end
  m = size(X,1);
  tmpfile = rfdataset(X, zeros(m,1));
  [status, yval] = system(sprintf('./rr predict-all -i %s -m "%s" %s', ...
                                  tmpfile, model.tmpnam, q), 1);
  yval = strread(yval, '%s', 'delimiter', sprintf('\n'));
  y = zeros(m,1);
  p = zeros(m,1);
  for i=1:m
    [s, e, te, m, t] = regexp(yval(i), '(.*):(.*)');
    y(i) = str2double(t{1}{1}{1,1});
    p(i) = str2double(t{1}{1}{1,2});
  end
  unlink(tmpfile);
end
