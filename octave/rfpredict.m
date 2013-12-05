function y = rfpredict(X, model)
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
	m = size(X,1);
	tmpfile = rfdataset(X, zeros(m,1));
	[status, yval] = system(sprintf('./rr predict-all -i %s -m "%s"', tmpfile, model), 1);
	yval = strread(yval, '%s', 'delimiter', sprintf('\n'));
	y = zeros(m,1);
	for i=1:m
		y(i) = str2double(yval(i));
	end
	unlink(tmpfile);
end

function S = repeat(s , n)
    S = repmat(s , [1,n]);
end