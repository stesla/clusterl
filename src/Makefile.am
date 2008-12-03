beamdir = $(ERLANG_INSTALL_LIB_DIR_clusterl)/ebin
beam_DATA = clusterl.beam
CLEANFILES = $(beam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
