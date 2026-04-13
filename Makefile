all: test lib

DEPLOY_DEST ?= vps:/var/www/naproche.org/
RSYNC_FLAGS ?= -az --delete-delay --itemize-changes --exclude=".DS_Store"

.PHONY: lib
lib:
	stack build
	time stack exec zf -- --log library/everything.tex -t 17 --uncached

.PHONY: dump
dump:
	stack build
	time stack exec zf -- --log library/everything.tex -t 17 --uncached --dump dump

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test --test-arguments "--accept"

.PHONY: publish
publish:
	rsync $(RSYNC_FLAGS) html/ $(DEPLOY_DEST)

.PHONE: profile
profile:
	stack --work-dir .stack-work-profile build --profile --ghc-options "-fprof-auto -fprof-cafs"
	stack --work-dir .stack-work-profile exec --profile zf -- debug/formalizations_with_section_20_up_to_euclidean_metric_theorem.tex --uncached +RTS -s -p
	ghc-prof-flamegraph zf.prof

.PHONE: profilenominal
profilenominal:
	stack --work-dir .stack-work-profile build --profile --ghc-options "-fprof-auto -fprof-cafs"
	stack --work-dir .stack-work-profile exec --profile zf -- debug/formalizations_with_section_20_up_to_euclidean_metric_theorem.tex --nominal --uncached +RTS -s -p
	ghc-prof-flamegraph zf.prof

.PHONE: profilemem
profilemem:
	stack --work-dir .stack-work-profile build --profile --ghc-options "-fprof-auto -fprof-cafs"
	stack --work-dir .stack-work-profile exec --profile zf -- debug/formalizations_with_section_20_up_to_euclidean_metric_theorem.tex --uncached +RTS -hc -RTS
	hp2ps zf.hp
	gs -q -dNOPAUSE -dBATCH -sOutputFile=temp.ps -sDEVICE=ps2write -c "<</Orientation 1>> setpagedevice" -- zf.ps && mv temp.ps zf.ps

.PHONE: profileparser
profileparser:
	stack --work-dir .stack-work-profile build --profile --ghc-options "-fprof-auto -fprof-cafs"
	stack --work-dir .stack-work-profile exec --profile zf -- debug/formalizations_with_section_20_up_to_euclidean_metric_theorem.tex --parseonly --uncached +RTS -p
	ghc-prof-flamegraph zf.prof
