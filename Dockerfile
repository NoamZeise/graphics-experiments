FROM debian:bookworm

WORKDIR /
RUN apt update
RUN apt -y install git curl make sbcl libglfw3 libglx0 libzstd-dev
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" \
         --quit
RUN echo '(load "~/quicklisp/setup.lisp")' >> ~/.sbclrc

WORKDIR /experiments