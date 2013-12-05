function model = rflearn(X, y, opts = '-n 100')
	tmpfile = rfdataset(X, y);
	model = tmpnam('./tmp/', 'model');
	system(sprintf('./rr deploy -i %s -c "rf %s" -o %s', tmpfile, opts, model));
end