function tmpfile = rfdataset(X, y)
	header = repeat('numeric,', size(X,2));
	tmpfile = tmpnam('./tmp');
	fid = fopen(tmpfile, 'w');
	fprintf(fid, '%sclass\n', header);
	fprintf(fid, '%sclass\n', header);
	fclose(fid);
	dlmwrite(tmpfile, [X, y], '-append', 'precision', '%.6f', 'delimiter', ',');
end