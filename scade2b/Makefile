OCAMLBUILD= ocamlbuild -no-links -classic-display \
		-libs unix,nums \
		-tags debug,annot

TARGET=native
MAIN=scade2b

all: $(MAIN)

native: TARGET := native
native: all
opt: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := byte
byte: all
$(MAIN).byte: byte


$(MAIN): $(MAIN).target
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) $(MAIN).$(TARGET)


clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) *~

cleanall: realclean

# +----------+              +-----------------+
# | dir/KCG/ |   scade2b    | dir/Machines_B/ |
# |          |              |                 |
# | Source   |  --------->  |     Actual      |
# |  code    |              |     output      |
# +----------+              +-----------------+
#                               |
# +-----------+                 |            *.ok all exist
# | dir/spec/ |                 |                  ==
# |           |                 |              tests pass
# | Expected  | -------------+  |
# |  output   |              |  |                  /\
# +-----------+              |  |                  ||
#                            v  v                  ||
#  \___  ___/            +-----------+       +-------------+
#      \/                | dir.diff  |       |    dir.ok   |
#                        |           |       |             |
#    in git              |  Unified  | ----> |  Exists if  |
#                        |   diff    |       | out == spec |
#                        +-----------+       +-------------+

CHECK_EXEC?=$(MAIN)

check_cov:
	$(MAKE) check CHECK_EXEC=$(MAIN).cov

check: $(patsubst %.test, %.ok, $(wildcard tests/*.test))

%/Machines_B/: % $(CHECK_EXEC)
	./$(CHECK_EXEC) $</

%.diff: %.test/spec %.test/Machines_B
	diff -Nru $+ | tee $@

# -e : file exists
# -s : file exists and is not empty
%.ok: %.diff
	[ -e $< -a ! -s $< ] && touch $@

check_clean:
	rm -r tests/*/Machines_B/ tests/*.ok tests/*.diff
