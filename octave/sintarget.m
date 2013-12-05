
function y = sintarget(X, l = 0.25, ub = 0)
	y = sign(X(:,2) - (ub+X(:,1)) + l * sin(pi*X(:,1)));
end