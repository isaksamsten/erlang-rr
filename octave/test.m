function test(X, Xval, y, yval, ...
			  si = 0.99, ub = 0, nl=0, opts = "-n 100 --progress none", ttl = 'Random Forest, E_{in}=%g, E_{out}=%g, F1_{out}=%g')
	% y = sintarget(X, si, ub);
	% yval = sintarget(Xval, si, ub);

	% y = add_noise(y, nl);
	% yval = add_noise(yval, nl);

	model = rflearn(X, y, opts);
	p = rfpredict(X, model);
	Ein = 1-mean(double(y == p));

	p = rfpredict(Xval, model);
	Eout = 1-mean(double(yval == p));

	[c, nu, prec, recall, F] = getcm(yval, p, [-1, 1]);

	clf;
	plotData(X, y);
	visualizeBoundaryRf(X, y, model);
	plotTarget(X, si, ub);
	hold on;
	title(sprintf(ttl, Ein, Eout, (F(1)+F(2))/2));
	xlabel('x_1'); ylabel('x_2');
	legend('target function');
	legend
	hold off;
end

function ny = add_noise(y, l)
	ny = y;
	r = rand(length(y), 1);
	ny(r < l) *= -1;
end	