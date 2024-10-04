LISP="sbcl"

.PHONY: build
build: build-setup
	$(LISP)	--eval "(ql:quickload :deploy)" \
		--load project.asd \
		--eval "(ql:quickload :project)" \
		--eval "(asdf:make :project)"

# build without quicklisp
.PHONY: asdf
asdf: build-setup
	$(LISP)	--load project.asd \
                --eval "(asdf:load-system :project)" \
                --eval "(asdf:make :project)"

.PHONY: build-setup
build-setup: gficl copy-assets

.PHONY: copy-assets
copy-assets:
	mkdir -p bin
	cp -r assets/ bin/assets/
	cp -r shaders/ bin/shaders/

gficl:
	git clone https://github.com/NoamZeise/gficl.git

.PHONY: clean
clean:
	rm -rf bin/

.PHONY: clean
cleanall: clean
	rm -rf gficl
