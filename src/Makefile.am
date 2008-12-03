beamdir = $(ERLANG_INSTALL_LIB_DIR)/clusterl-$(VERSION)/ebin
beam_DATA = clusterl.beam
CLEANFILES = $(beam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
