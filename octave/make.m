function make()
	system('cd ../ && ./rebar compile escriptize');
	system('cp ../rr .');
	system('cp ../rr.config .');
end