function test2(si = 0.99, ub = 0, nl=0)
	X = unifrnd(-1, 1, 100, 2);
	Xval = unifrnd(-1, 1, 1000, 2);
	y = sintarget(X, si, ub);
	yval = sintarget(Xval, si, ub);

	y = add_noise(y, nl);
	yval = add_noise(yval, nl);

	figure(1);
	clf;
	test(X, Xval, y, yval, si, ub, nl, '-n100 --score info', 'Random Forest (info-gain), E_{in}=%g, E_{out}=%g, F1_{out}=%g');
	figure(2);
	clf;
	test(X, Xval, y, yval, si, ub, nl, '-n100 --score hellinger', 'Random Forest (hellinger), E_{in}=%g, E_{out}=%g, F1_{out}=%g');

end