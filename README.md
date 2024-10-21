### Description
Perlin noise for an arbitrary number of dimensions.

Stateless interface:
* `(noise-seed seed)` to set the seed.
* `(noise &rest coords)` to generate a noise value in the range 0-1. You will have to experiment with scaling the inputs and output to get the results you want. Note that there will be higher latency the first time it's called for a given seed & number of dimensions, as initialisation must be performed. For more consistent latency, use the stateful interface.
* `(noise-detail &key lod falloff)` determines the 'character' of the noise, based on the `noiseDetail()` function from [p5js](https://p5js.org/reference/#/p5/noiseDetail). LOD stands for level of detail, default value is 4. It should be a positive integer value, and determines how many noise values are generated and summed together to produce the final value. Low values -> smoother and more efficient, high values -> greater level of detail. FALLOFF should be between 0 and 1, each successive noise value in the sum is weighted by increasing powers of FALLOFF. A falloff of greater than 0.5 will possibly result in noise outputs of greater than 1.

Stateful interface, use where multithreading is expected:
* `(make-noise dimensions &key seed lod falloff)`
* `(noise-gen N &rest coords)`
* `(noise-set-detail N &key lod falloff)`


### Dependencies
* random-state

### Installation
Using quicklisp: `(ql:quickload 'noisy)`.

Also available via the [UltraLisp distribution](https://ultralisp.org/projects/Kevinpgalligan/noisy), the [ocicl](https://github.com/ocicl/ocicl) package manager, and probably others.

### Example output
![visualisation of different noise configurations in 1 dimension](https://github.com/Kevinpgalligan/noisy/blob/master/noise.png)
![visualisation of different noise configurations in 2 dimensions](https://github.com/Kevinpgalligan/noisy/blob/master/2d-noise.png)

### Example usage
```lisp
CL-USER> (ql:quickload 'noisy)
CL-USER> (use-package 'noisy)
CL-USER> (noise-seed 7)
CL-USER> (noise-detail :lod 2 :falloff 0.4)
CL-USER> (noise 0.02 0.3)
CL-USER> (defparameter *N* (make-noise 2 :seed 7 :lod 1 :falloff 1.0))
CL-USER> (set-noise-detail *N* :lod 2 :falloff 0.3)
CL-USER> (noise-gen *N* 0.04 0.09)
```

### License
MIT
