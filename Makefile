
ERL=erl

all: src

src: true
	(cd src; erl -make all)

true:

clean:
	(cd ebin; rm -f *.beam)


doc: doc/yatsy.html

doc/%.html: src/%.erl
	(${ERL} -noshell -run edoc file $< -run init stop; \
	 mv src/$*.html doc)

