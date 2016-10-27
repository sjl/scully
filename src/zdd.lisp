(in-package :scully.zdd)


(defparameter *cudd-manager*
  (scully.cudd:cudd-init 0
                         0
                         scully.cudd:+cudd-unique-slots+
                         scully.cudd:+cudd-cache-slots+
                         0))

