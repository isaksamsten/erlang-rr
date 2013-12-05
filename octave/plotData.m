function plotData(X, y, mu)
	hold on;
	pos = find(y == 1); neg = find(y == -1);
	plot(X(pos,1), X(pos,2), 'b+');
	plot(X(neg,1), X(neg,2), 'r+');
	hold off;
end