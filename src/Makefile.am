erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_escobar)/src
erlsrc_DATA = escobar.erl escobar_file.erl \
escobar_xref.erl escobar_mk_xref.erl escobar_tree.erl
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_escobar)/ebin
erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))
CLEANFILES = $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS)  $<
