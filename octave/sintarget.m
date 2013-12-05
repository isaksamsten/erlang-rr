function y = sintarget(X, l = 0.25)
	y = sign(X(:,2) - X(:,1) + l * sin(pi*X(:,1)));
end