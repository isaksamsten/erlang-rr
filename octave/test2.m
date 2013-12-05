function test2(si = 0.99, ub = 0, nl=0)
	X = unifrnd(-1, 1, 1000, 2);
	Xval = unifrnd(-1, 1, 2000, 2);
	y = sintarget(X, si, ub);
	yval = sintarget(Xval, si, ub);

	y = add_noise(y, nl);
	yval = add_noise(yval, nl);

	figure(1);
	test(X, Xval, y, yval, si, ub, nl, '-n1000 --score info', 'Random Forest (info-gain), E_{in}=%f, E_{out}=%f');
	figure(2);
	test(X, Xval, y, yval, si, ub, nl, '-n1000 --score jensen-difference', 'Random Forest (jensen), E_{in}=%f, E_{out}=%f');

end