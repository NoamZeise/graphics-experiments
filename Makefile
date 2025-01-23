LISP := "sbcl"
exec-name := experiments
build-dir := bin

framework-code := $(wildcard framework/**.lisp)
experiment-code := $(wildcard experiments/**.lisp)
gficl-code := $(wildcard gficl/src/**.lisp)
build-deps := $(framework-code) $(experiment-code) framework.asd experiments.asd $(gficl-code) gficl/gficl.asd

assets-src := $(wildcard assets/*)
assets := $(assets-src:%=$(build-dir)/%)

shaders-src := $(wildcard shaders/*)
shaders := $(shaders-src:%=$(build-dir)/%)

.PHONY: build
build: build-setup $(build-dir)/$(exec-name)

$(build-dir)/$(exec-name): $(build-deps)
	$(LISP)	--eval "(ql:quickload :deploy)" \
		--load experiments.asd \
		--eval "(ql:quickload :experiments)" \
		--eval "(asdf:make :experiments)"

# build without quicklisp
.PHONY: asdf
asdf: build-setup $(build-deps)
	$(LISP)	--load experiments.asd \
                --eval "(asdf:load-system :experiments)" \
                --eval "(asdf:make :experiments)"

.PHONY: build-setup
build-setup: gficl $(assets) $(shaders)

gficl:
	git clone https://github.com/NoamZeise/gficl.git

$(build-dir)/assets/%: assets/% | $(build-dir)/assets
	cp -r $< $@

$(build-dir)/shaders/%: shaders/% | $(build-dir)/shaders
	cp -r $< $@

$(build-dir)/assets: | $(build-dir)
	mkdir -p $(build-dir)/assets
$(build-dir)/shaders: | $(build-dir)
	mkdir -p $(build-dir)/shaders
$(build-dir):
	mkdir -p $(build-dir)

.PHONY: repl
repl: gficl
	$(LISP) --load repl-setup.lisp \
	     	--eval "(experiments:run)"

.PHONY: clean
clean:
	rm -rf $(build-dir)

.PHONY: cleanall
cleanall: clean
	rm -rf gficl/

.PHONY: pull
pull: gficl
	git pull
	cd gficl
	git pull

.PHONY: docker
docker:
	docker build . -t experiments
	docker run -it --rm -v=.:/experiments experiments
