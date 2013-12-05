function plotTarget(X, l=0.25, ub=0)
	hold on;
	xp = linspace(min(X(:,1)), max(X(:,1)), 100);
	for i=1:length(xp)
		yp(i) = ub+xp(i) - l * sin(pi*xp(i));
	end
	plot(xp, yp, 'k');
	axis([-1 1 -1 1])
	hold off;
end	