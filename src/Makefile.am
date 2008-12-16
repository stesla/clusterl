app = clusterl
erlfiles = $(wildcard *.erl)
beamdir = $(ERLANG_INSTALL_LIB_DIR)/$(app)-$(VERSION)/ebin
beam_DATA =	$(erlfiles:%.erl=%.beam)
app_DATA = $(app).app
appdir = $(beamdir)
CLEANFILES = $(beam_DATA) $(app_DATA)

ERLCFLAGS = @ERLCFLAGS@ +debug_info

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<

clusterl.app: clusterl.app.in
	$(SED) -e 's|[@]VERSION[@]|$(VERSION)|g' \
		   -e 's|[@]MODULES[@]|$(beam_DATA:%.beam=%,)|g' -e 's/,]}/]}/' \
		   < $< > $@
