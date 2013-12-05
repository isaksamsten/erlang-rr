function plotTarget(X, l)
	xp = linspace(min(X(:,1)), max(X(:,1)), 100);
	for i=1:length(xp)
		yp(i) = xp(i) - l * sin(pi*xp(i));
	end
	plot(xp, yp, 'k');

end	