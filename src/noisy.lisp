(defpackage :noisy
  (:use :cl)
  (:export
   :noise
   :noise-seed
   :noise-detail
   :make-noise
   :set-noise-detail
   ))

(in-package noisy)

(defparameter *noise-size* 256)
(defparameter *permutation-table*
  (make-array (list *noise-size*)
              :initial-contents
              '(151 160 137 91 90 15  
                131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23  
                190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33  
                88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166  
                77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244  
                102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196  
                135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123  
                5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42  
                223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9  
                129 22 39 253 9 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228  
                251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239 107  
                49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254  
                138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180)))


;;;; Some utilities.

(declaim (inline lerp))
(defun lerp (x1 x2 t0)
  "Linear interpolation between x1 and x2 given weight t0, 0<=t0<=1."
  (+ (* (- 1 t0) x1) (* t0 x2)))

(declaim (inline smoothstep))
(defun smoothstep (t0)
  (* t0 t0 (- 3 (* 2 t0))))

(declaim (inline remap))
(defun remap (x la ha lb hb)
  "Takes x from the interval [la, ha] and remaps it to the
interval [lb, hb]. If x is outside the expected interval, then
it gets clamped."
  (min hb
       (max lb
            (+ lb (* (- hb lb) (/ (- x la) (- ha la)))))))


;;; The "stateless" interface.

(defparameter *noise-cache* (make-hash-table))
(defparameter *seed* nil)
(defparameter *lod* 4)
(defparameter *falloff* 0.5)

(defun noise (&rest coords)
  "Generates correlated noise for coordinates COORDS.
Use the stateful MAKE-NOISE and NOISE-GEN interface if multiple threads will be used."
  (let ((num-dimensions (length coords)))
    (when (not (gethash num-dimensions *noise-cache*))
      (setf (gethash num-dimensions *noise-cache*)
            (make-noise num-dimensions :seed *seed*)))
    (let ((N (gethash num-dimensions *noise-cache*)))
      (apply #'noise-gen N coords))))

(defun noise-seed (seed)
  "Sets the seed for noise generation."
  (setf *seed* seed)
  ;; Ensure that any new noise values are based on the new seed.
  (setf *noise-cache* (make-hash-table)))

(defun noise-detail (&key lod falloff)
  "Configure the 'character' of the noise.
LOD stands for level of detail. It should be an integer value, determining how many noise values are
generated and summed together. Low values -> smoother and more efficient, high values -> greater
level of detail.
The noise values are weighted by the order they are generated, so the first ones have more weight. FALLOFF
should have a value between 0 and 1, it determines the weights. The i-th noise value will have a
weight of FALLOFF^i. A falloff of greater than 0.5 will possibly result in noise outputs of greater than 1.
Based on: https://p5js.org/reference/#/p5/noiseDetail"
  (when lod (setf *lod* lod))
  (when falloff (setf *falloff* falloff))
  ;; Update the already-created noise instances.
  (loop for N being the hash-values of *noise-cache*
        do (setf (slot-value N 'lod) *lod*
                 (slot-value N 'falloff) *falloff*)))

(defun permute (i)
  (aref *permutation-table* (mod i *noise-size*)))


;;;; The stateful interface.

;; The user is not supposed to interact with this class except through
;; the provided functions.
(defclass noise-state ()
  ((noisefun :initarg :noisefun)
   (lod :initarg :lod)
   (falloff :initarg :falloff)
   (input :initarg :input)))

(defun noise-gen (N &rest coords)
  (with-slots (noisefun lod falloff input) N
    (if (= lod 1)
        ;; Don't apply falloff to the noise if there's only one 'frequency'.
        (apply noisefun coords)
        (loop repeat lod
              for freq = 1 then (* 2 freq)
              ;; To avoid cons-ing every time this function is called, we
              ;; use the list INPUT to store the coordinates that
              ;; will be passed to NOISEFUN.
              for amplitude = falloff then (* falloff amplitude)
              do (loop for c in coords
                       for x on input
                       do (setf (car x) (* freq c)))
              sum (* amplitude (apply noisefun input))))))

(defun make-noise (dimensions &key seed (lod 4) (falloff 0.5))
  (make-instance 'noise-state
                 :noisefun (make-noise-function dimensions
                                                (or seed (1+ (random 1000000))))
                 :lod lod
                 :falloff falloff
                 :input (loop repeat dimensions collect 0)))

(defun set-noise-detail (N &key lod falloff)
  (when lod (setf (slot-value N 'lod) lod))
  (when falloff (setf (slot-value N 'falloff) falloff)))

(defun make-noise-function (dimensions seed)
  (let* ((r (make-array (list *noise-size*)))
         ;; We will reuse these so that we don't need to allocate
         ;; memory / compute stuff every time the noise function is
         ;; called.
         (base-corner (loop repeat dimensions collect nil))
         (offsets (generate-corner-offsets dimensions))
         (num-corners (expt 2 dimensions))
         (vals (loop repeat num-corners collect nil)))
    ;; Create gradient vectors.
    (let ((rng (random-state:make-generator 'random-state:mersenne-twister-64)))
      (random-state:reseed rng seed)
      (dotimes (i *noise-size*)
        (setf (aref r i)
              (if (= dimensions 1)
                  ;; Edge case, in 1 dimension you use a scalar between -1 and 1
                  ;; instead of a direction vector, since direction doesn't mean
                  ;; much in 1 dimension.
                  (list (random-state:random-float rng -1 1))
                  (generate-point-on-unit-sphere dimensions rng)))))
    ;; This is the interface to creating noise: a function that takes a
    ;; point and returns a noise value.
    (lambda (&rest point)
      ;; Set the base corner in the grid.
      (overwrite-list! base-corner point #'floor)
      ;; Compute the dot product between the gradient at each corner surrounding the point, and
      ;; the vector between the given corner and the point.
      (loop for offset in offsets
            for gradient = (fetch-gradient r base-corner offset)
            for vals-cell on vals
            do (rplaca vals-cell
                       (if (= dimensions 1)
                           ;; Edge case again!
                           (let ((x (car point))
                                 (x1 (+ (car base-corner) (car offset)))
                                 (m (car gradient)))
                             (* m (- x x1)))
                           (dot-gradient-with-direction gradient base-corner offset point))))
      ;; Now interpolate between the values.
      (loop for x in point
            for f = (multiple-value-bind (i fractional-part)
                        (floor x)
                      (declare (ignore i))
                      fractional-part)
            for remaining-values = num-corners then (/ remaining-values 2)
            while (> remaining-values 1)
            do (loop for (v1 . (v2 . rest)) on vals by #'cddr
                     for vals-cell on vals
                     for i = 0 then (+ 2 i)
                     while (< i remaining-values)
                     do (rplaca vals-cell (lerp v1 v2 (smoothstep f)))))
      (remap (car vals) -1 1 0 1))))

(defun overwrite-list! (xs ys f)
  "Takes 2 lists XS and YS, of the same length. Overwrites the
values of XS by calling the function F on each of the values of YS."
  (loop for x-cell on xs
        for y in ys
        do (rplaca x-cell (funcall f y))))

(defun fetch-gradient (r base-corner offset)
  (aref r (get-gradient-index base-corner offset)))

(defun get-gradient-index (base-corner offset)
  (let ((i 0))
    (loop for b in base-corner
          for o in offset
          ;; Adding b & o gives a coordinate of the corner.
          do (setf i (permute (+ i b o))))
    i))

(defun dot-gradient-with-direction (gradient base-corner offset point)
  ;; Take the vector that points from the corner (given by adding base-corner & offset) to
  ;; the point, and dot it with the gradient.
  (loop for g in gradient
        for x in point
        for b in base-corner
        for o in offset
        sum (* g (- x (+ b o)))))

(defun generate-point-on-unit-sphere (dimensions rng)
  ;; Rejection sampling: generate random points with coordinates in the
  ;; range [-1, 1]. Keep rejecting until one of the points falls within the
  ;; unit sphere. In higher dimensions this will take a long time because the
  ;; volume of the sphere is relatively small compared to the cube/box. Alternatively,
  ;; you can sample coordinates from the Gaussian distribution and the resulting
  ;; point will be uniformly distributed over the sphere. For our target dimensions,
  ;; this should be fine (~3 attempts on average to generate a point in 4 dimensions).
  (let ((point (loop repeat dimensions collect nil))
        (length 0))
    (loop do (loop for coord on point
                   do (setf (car coord) (random-state:random-float rng -1 1)))
          do (setf length (list-euclidean-length point))
          when (<= length 1)
            return (list-normalise! point length))))

(defun list-euclidean-length (list)
  (sqrt (loop for x in list sum (* x x))))

(defun list-normalise! (list length)
  (loop for remaining on list
        do (setf (car remaining) (/ (car remaining) length)))
  list)

(defun combine-pairs! (binary-function list)
  (loop for pair on list
        while pair
        do (progn
             (setf (car pair) (funcall binary-function (car pair) (cadr pair)))
             (setf (cdr pair) (cddr pair)))))

(defun generate-corner-offsets (n)
  (labels ((rec (n sequences)
             (if (<= n 0)
                 sequences
                 (rec (1- n)
                      (nconc (mapcar (lambda (sequence)
                                       (reverse (cons 0 (reverse sequence))))
                                     sequences)
                             (mapcar (lambda (sequence)
                                       (reverse (cons 1 (reverse sequence))))
                                     sequences))))))
    (rec n (list (list)))))
