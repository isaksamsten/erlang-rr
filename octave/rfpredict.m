function y = rfpredict(X, model)
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