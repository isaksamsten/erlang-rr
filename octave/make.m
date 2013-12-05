function make()
	[escript, ~] = system('command -v escript');
	[rebar, ~] = system('ls ../rebar 2>&1 /dev/null',1);
	if escript
		fprintf('could not find escript (is erlang installed and on PATH?)\n')
		return;
	elseif rebar
		fprintf('could not find rebar (is "rr" in ../?)\n')
		return;
	end
	system('cd ../ && ./rebar compile escriptize');
	system('cp ../rr .');
	system('cp ../rr.config .');

	fprintf('completed')	
end