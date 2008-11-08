all: compile

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam

run:
	erl -sname 'b-yuled' -pa ebin -eval 'gui:start().'
