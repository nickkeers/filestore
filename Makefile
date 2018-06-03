.PHONY: rel
num ?= 3
rel:
	for num in `seq 1 $(num)`; do \
		rebar3 release -n filestore$$num ; \
	done
