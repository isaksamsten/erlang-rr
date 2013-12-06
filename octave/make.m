function make()
%
%   -- make() build the rr environment
%
%   Isak Karlsson <isak-kar@dsv.su.se> (last edit: dec 2013)

	[escript, ~] = system('command -v escript');
	[rebar, ~] = system('ls ../rebar 2>&1 /dev/null',1);
	[rr, ~] = system('ls ../rr 2>&1 /dev/null',1);
	[rrconf, ~] = system('ls ../rr.config 2>&1 /dev/null',1);
	escript = !escript; rebar = !rebar; rr = !rr; rrconf = !rrconf;
	if !escript
		fprintf('error: could not find escript (is erlang installed and on PATH?)\n')
		return;
	elseif !rebar
		fprintf('warn: could not find rebar.config in ../)\n')
	end

	if rebar
		fprintf(' *** compiling rr from source *** \n')
        fflush(stdout);
		system('cd ../ && ./rebar compile escriptize');
		system('cp ../rr .');
		system('cp ../rr.config .');
	else
		fprintf(' *** rebar not found, trying for binaries **** \n')
		if rr && rrconf
			system('cp ../rr .');
			system('cp ../rr.config .');
		else
			fprintf(' *** completed with errors (check if rr and rr.config exist in ../) *** \n')
		end
	end

	fprintf(' *** completed without errors *** \n')	
end