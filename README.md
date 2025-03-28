# Graphics Experiments 

This project aims to provide tools for building graphics pipelines and collecting data on their performance.

## Running

You can build the project as a binary and run it, but the best way is through an interactive repl.
This means the program is loaded into a common lisp shell and run.
There is repl setup code in the makefile.
The program can then be interacted with and modified through the repl while it is running.

### Controls

On running the program, the camera will show a spinning environment.
Press `M` to switch pipelines.
By default there are a list of many different pipelines you can switch between.
Here are the controls for the default scene:

```
Camera Controls:
  R - toggle rotation
  W,A,S,D - move direction camera is facing
  Arrow Keys - move the position of the camera
  Shift, Space - move the camera up and down

Scene Controls:
  M - switch active pipeline
  P - start performance analyser 
      saves results in performance.csv
  T,Y - change angle of the sun
  H,J,K,L - move bunny model around
```

### Running Experiments

To run the program from a repl do
```
(experiments:run)
```
An argument can be supplied to change which graphics pipelines are used. 
For example
```
(experiments:run :pipelines :levels)
```
Uses the pipelines for comparing different levels of radiance cascades.
The options for this argument are:
```
:medley     <- default, most of the implemented pipelines
:methods    <- shadow mapping vs cascades vs ssao
:levels     <- cascade levels comparison
:resolution <- base cascade level comparison
:samples    <- number of base rays cast comparison
:steps      <- number of ray steps comparison
```
Then press `P` once the program is loaded, and the performance analyser will collect performance data for each pipeline and output it to the file `performance.csv`.

## Project Layout

The project is broken into two libraries.

The library described by `framework.asd` is in the `framework/` folder. 
This library defines the classes used to implement the various pipelines being investigated.
It handles the GPU resources and ensures they are properly deleted when no longer used.
It also handles assets such as the shader reload code and model/texture resources.

The library described by `experiments.asd` is in the `experiments/` folder.
This library contains the entry point of the built application (`main.lisp`), and uses the `framework` library to implement many rendering techniques, such as radiance cascade (`cascades.lisp`), ssao (`ssao.lisp`), shadow mapping (`shadow.lisp`), etc.
It also defines the performance analyser used to collect data on differnt pipelines.

The `shader/` folder holds the glsl shader code used by the `experiments` library.
These files are loaded by the program when it runs, and the shaders used are watched for changes by the `framework/hot-reload.lisp` code.

The `assets/` folder holds any texture and 3D models loaded by `experiments/main.lisp` in the `load-assets` function.

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
- Do `make repl` to open a repl with the project loaded

### Build With Docker

You can also build for linux with docker. 
If you have docker installed run the following in the project root.

```
docker build . -t project
docker run -it --rm -v=.:/project project
make
exit
```
Then the built executable is `bin/experiments`.

### Interactive Development

Do `make gficl` to download the gficl library into the project root.
load `repl-setup.lisp` with your common lisp implementation, 
then run `(experiments:run)` if you want to run the project.
