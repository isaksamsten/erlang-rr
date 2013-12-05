function model = rflearn(X, y, opts = '-n 100')
% 
% -- Function File:  rflearn(X, y, opts) 
% 
%    Learn a random forest model from the examples in X with the targets in y
%    multiple targets are allowed (i.e. |y| >= 2)
%
%    Isak Karlsson <isak-kar@dsv.su.se> (December 2013)
%    
%    See also: rfpredict
	tmpfile = rfdataset(X, y);
	model = tmpnam('./tmp/', 'model');
	system(sprintf('./rr deploy -i %s -c "rf %s" -o %s', tmpfile, opts, model));
end