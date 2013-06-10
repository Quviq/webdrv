VERSION=1.0
RELDIR=./rel/webdrv-$(VERSION)

all: compile doc
.PHONY: test doc

compile: 
	@(cd src; erl -make)

test: 
	@(cd test; erl -make)

doc:
	erl -eval "edoc:application(webdrv,\".\",[{dir, \"./doc\"}, {preprocess, true}])" \
-s init stop -noshell

release: compile
	rm -rf $(RELDIR);\
mkdir -p $(RELDIR);\
cp -r ebin doc include $(RELDIR)

clean:
	rm -f ebin/*.beam; \
rm -f doc/*.html; \
rm -f doc/*.png doc/*info doc/*.css; \
rm -rf rel