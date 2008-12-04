beamdir = $(ERLANG_INSTALL_LIB_DIR)/clusterl-$(VERSION)/ebin
beam_DATA =	clusterl.app \
			clusterl.beam \
			clusterl_network.beam \
			clusterl_radio.beam \
			clusterl_sup.beam
CLEANFILES = $(beam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<

clusterl.app: clusterl.app.in
	$(SED) -e 's|[@]VERSION[@]|$(VERSION)|g' \
		< $< > $@
