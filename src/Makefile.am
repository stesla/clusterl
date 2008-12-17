app = clusterl

appdir = $(ERLANG_INSTALL_LIB_DIR)/$(app)-$(VERSION)
ebindir = $(appdir)/ebin

erl_files = $(wildcard *.erl)

beam_files = $(erl_files:%.erl=../ebin/%.beam)
app_files = ../ebin/$(app).app
ebin_DATA = $(beam_files) $(app_files)

CLEANFILES = $(ebin_DATA)

ERLCFLAGS = @ERLCFLAGS@ +debug_info

do_subst = $(SED) -e 's|[@]VERSION[@]|$(VERSION)|g'

../ebin/%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam -o ../ebin $<

../ebin/$(app).app: modules = $(erl_files:%.erl=%,)
../ebin/$(app).app: $(app).app.in Makefile
	$(do_subst) -e 's|[@]modules[@]|$(modules)|g' -e 's/,]}/]}/' < $< > $@
