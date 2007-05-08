
ERL=erl

.PHONY: all src conf conf_clean clean
all: conf src

src:
	cd src && erl -make all

conf:
	cd conf && $(MAKE)

conf_clean:
	cd conf && $(MAKE) clean

clean:
	cd ebin && rm -f *.beam


doc: doc/yatsy.html

doc/%.html: src/%.erl
	${ERL} -noshell -run edoc file $< -run init stop && \
	mv src/$*.html doc

