function tmpfile = rfdataset(X, y)
  %
  % -- Function File: rfdataset(X, y)
  %
  %    write a matrix X and a vector y to a tmpfile which can be used by
  %    rflearn and rfpredict
  %
  %    Isak Karlsson <isak-kar@dsv.su.se> (last edit: dec 2013)
  %
  %    See also: rfpredict, rflearn
  header = repeat('numeric,', size(X,2));
  tmpfile = tmpnam('./tmp');
  fid = fopen(tmpfile, 'w');
  fprintf(fid, '%sclass\n', header);
  fprintf(fid, '%sclass\n', header);
  fclose(fid);
  dlmwrite(tmpfile, [X, y], '-append', 'precision', '%.6f', 'delimiter', ',');
end

function S = repeat(s , n)
  S = repmat(s , [1,n]);
end
