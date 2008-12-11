SUBDIRS = src

otp.plt: BEAMS = $(wildcard $(ERLANG_LIB_DIR_kernel)/ebin/*.beam) \
				 $(wildcard $(ERLANG_LIB_DIR_stdlib)/ebin/*.beam) \
				 $(wildcard $(ERLANG_LIB_DIR_mnesia)/ebin/*.beam)
otp.plt: $(BEAMS)
	$(DIALYZER) --build_plt --output_plt $@ -r -c $(BEAMS)

dialyzer: OPTS = --add_to_plt --plt otp.plt --output_plt clusterl.plt
dialyzer: WARNINGS = -Wunmatched_returns -Werror_handling
dialyzer: BEAMS = $(wildcard src/*.beam)
dialyzer: all otp.plt
	$(DIALYZER) $(OPTS) $(WARNINGS) -c $(BEAMS)

.PHONY: dialyzer
