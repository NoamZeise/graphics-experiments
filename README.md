# Project

## Requirements

- [sbcl](https://www.sbcl.org/) or another common lisp implementation
- [quicklisp](https://www.quicklisp.org/beta/) or manually install the nessecary lisp libraries
- [glfw3](https://www.glfw.org/) for window management _(installed by quicklisp on windows)_
- opengl drivers supporting version 4.6
- git
- make
- [gficl](https://github.com/NoamZeise/gficl) opengl helper library _(downloaded by Makefile)_

## Usage

### Build

To create an executable run `make`. 
It builds a binary called `bin/project`.
If it is the first time running this command,
it also clones the `gficl` library from github into the project root.

- Do `make clean` to remove the `bin` directory
- Do `make cleanall` to remove all extra files


### Build With Docker

You can also build for linux with docker. 
If you have docker installed run the following in the project root.

```
docker build . -t project
docker run -it --rm -v=.:/project project
make
exit
```
Then the built executable is `bin/project`.

### Interactive Development

Do `make gficl` to download the gficl library into the project root.
load `repl-setup.lisp` with your common lisp implementation, 
then run `(project:run)` if you want to run the project.
