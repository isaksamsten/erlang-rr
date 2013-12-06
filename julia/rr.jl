module RR
	using DataFrames

	export rflearn, rfpredict, RandomForest;


	immutable RandomForest
		opts :: String
		nfeatures :: Int64
		nexamples :: Int64
		oob_error :: Int64
		vi :: Array{Float64}
	end

	function rflearn(X, y, opts, quiet = true)
		RandomForest(opts, size(X,2), size(X,1), 0, zeros(size(X,2), 1));
		#system(sprintf('./rr deploy -i %s -c "rf %s" -o %s %s', tmpfile, opts, model.tmpnam, q));
	end

	function rfpredict(X, model, quiet = true)
		#system(sprintf('./rr predict-all -i %s -m "%s" %s', tmpfile, model.tmpnam, q), 1);

	end

end