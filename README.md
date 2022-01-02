# Project - ICANSQUAD

Welcome to Image Compression and N-Body Simulation with QuadTrees!

## Module organization

Modules should be read in the order detailed with the library first, then the two applications, then any tests.

The Main library functions are in `src/Quad.hs` with its tests in `test/QuadTest.hs.`.

Next are the two applications based on this library which are under the source directory.

### Image:

- `src/img/Image.hs` - contains all the functions for running the image application. Running `stack ghci src/img/Image.hs` then `main` in the repl will let you get started on that.
- `test/ImageTest.hs` - contains all the tests for image specific functionality such as building, compressing, and overlaying plus monoid tests.

### N-Body simulation:

- `src/Vectors.hs` - build a small Vector library to support functions in `Quad.hs` `NBody.hs` and `NBodyQuad.hs`
- `src/nbody/NBody.hs` - contains all the necessary functions and data types for performing n-body simulation using Lists.
- `src/nbody/NBodyQuad.hs`- builds upon NBody.hs to support functions for performing n-body simulation using QuadTrees.
- `src/nbody/Simulator.hs` - contains function to draw bodies and simulate n-body problem using Lists.
- `src/nbody/QuadTreeSim.hs` - contains function to draw boxes and simulate n-body problem using QuadTrees.
- `test/NBodyTest.hs` - contains all unit tests and quickCheck tests to verify functions used in `NBody.hs` and `NBodyQuad.hs`
- `test/VectorsTest.hs` - contains unit tests and quickCheck tests to verify functions used in `Vectors.hs`

### Dependencies:

- [Juicy Pixels](https://www.stackage.org/lts-18.18/package/JuicyPixels-3.3.6) - This library is used to read and write images to disk for the image application
- [vector](https://www.stackage.org/lts-18.18/package/vector-0.12.3.1) - Used by Juicy Pixels to store the image data, so the image application accesses the same data structure
- [ieee754](https://www.stackage.org/lts-18.18/package/ieee754-0.8.0) - Since the quadtree uses doubles, this library allows comparisons between doubles with an epsilon difference through `Data.AEq`.
- [Gloss](https://www.stackage.org/lts-18.18/package/gloss) - 2D vector graphics library used for simulation in Simulator.hs and QuadTreeSim.hs

## Building, running, and testing

This project compiles with `stack build`.

You can run the main executable with `stack run`. Everything is from the commandline. The prompts should guide you through the different features.

You can run the tests with `stack test`.

Lastly, you can start a REPL with `stack ghci`.
