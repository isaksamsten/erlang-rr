function test(X, Xval, y, yval, si = 0.99, ub = 0, nl=0,  ...
              opts = "-n 100 --progress none",
              ttl = 'Random Forest, E_{in}=%g, E_{out}=%g, F1_{out}=%g')

  [mu, ~] = lloyd(X, 9);
  rbf_x = RBF(X, mu, 1.5);

  model = rflearn(rbf_x, y, opts);
  p = rfpredict(rbf_x, model);
  Ein = 1-mean(double(y == p));

  p = rfpredict(RBF(Xval, mu, 1.5), model);
  Eout = 1-mean(double(yval == p));

  [c, nu, prec, recall, F] = getcm(yval, p, [-1, 1]);

  clf;
  plotData(X, y);
  visualizeBoundaryRf(X, y, @(x) rfpredict(RBF(x, mu, 1.5), model));
  plotTarget(X, si, ub);
  hold on;
  title(sprintf(ttl, Ein, Eout, (F(1)+F(2))/2));
  xlabel('x_1'); ylabel('x_2');
  legend('target function');
  legend
  hold off;
end

function k = rbf(x, xn, g)
  k = exp(-g * norm(x - xn)^2);
end

function Phi = RBF(X, mu, g)
  for i=1:size(X,1)
    for k=1:size(mu,1)
      Phi(i,k) = rbf(X(i,:), mu(k,:), g);
    end
  end
end

function [centroids, iterations] = lloyd(X, K, plt=0)
  iterations = 0; m = size(X, 1); n = size(X, 2);
  [centroids, last_assign, assign] = initialize(K, n, m);
  %X = [ones(m, 1), X];
  while(!all(last_assign == assign))
    last_assign = assign;
    for i=1:m
      for k=1:K
        dist(k) = norm(X(i,:) - centroids(k,:))^2;
      end
      [d, closest] = min(dist); assign(i) = closest;
    end
    count = histc(assign, unique(assign));
    if length(count) < K
      [centroids, last_assign, assign] = initialize(K, n, m);
      continue;
    end
    centroids = zeros(K, n);
    for a=1:length(assign)
      k = assign(a); centroids(k,:) += 1/count(k) * X(a,:);
    end
    iterations += 1;
    if plt
      figure(1, 'visible', 'off')
      hold on;
      clr = jet(K);
      for k=1:K
        cent = centroids(k,:);
        NewX = X(assign == k,:);
        plot(NewX(:,2), NewX(:,3), 'k+', 'Color', clr(k,:));
        plot(cent(2), cent(3), 'ko', 'Color', clr(k,:))
      end
      title(sprintf('Iteration %i', iterations))
      print(sprintf('%i.png', iterations), '-dpng');
      hold off;
      clf('reset');
    end
  end
end

function [centroids, last_assign, assign] = initialize(K, n, m)
  centroids = unifrnd(-1, 1, K, n);
  last_assign = randi([1, K], [m, 1]);
  assign = zeros(m, 1);
end
