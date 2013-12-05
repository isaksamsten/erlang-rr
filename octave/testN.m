function testN(N=[10, 10, 100], si = 0.99, ub = 0, nl=0)
	d = N(1):N(2):N(3);
	X = unifrnd(-1, 1, 100, 2);
	Xval = unifrnd(-1, 1, 1000, 2);
	y = sintarget(X, si, ub);
	yval = sintarget(Xval, si, ub);
	for i = 1:length(d);
		printf('running %i\n', d(i));
		figure(1, 'visible', 'off');
		clf;
		test(X, Xval, y, yval, si, ub, nl, sprintf('-n%i --no-features 1 --score info', d(i)), ...
		    strcat(sprintf('Random Forest (N=%i)', d(i)), 'E_{in}=%g, E_{out}=%g, F1_{out}=%g'));
		print(sprintf('rf_%i.png', i), '-dpng');
	end

end