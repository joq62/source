all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	erl -pa ebin -s boot start localhost 20 dir /home/pi/erlang/d/source -sname boot
