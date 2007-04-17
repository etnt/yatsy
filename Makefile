
ERL=erl

all: conf src

src: true
	(cd src; erl -make all)

conf: true
	(cd conf; $(MAKE))

conf_clean:
	(cd conf; $(MAKE) clean)

true:

clean:
	(cd ebin; rm -f *.beam)


doc: doc/yatsy.html

doc/%.html: src/%.erl
	(${ERL} -noshell -run edoc file $< -run init stop; \
	 mv src/$*.html doc)

