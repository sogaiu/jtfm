(defn log
  [& args]
  (print ;args))

(defn logf
  [& args]
  (if (empty? args)
    (print)
    (printf ;args)))

(defn lof
  [& args]
  (prinf ;args))

(defn elog
  [& args]
  (eprint ;args))

(defn elogf
  [& args]
  (if (empty? args)
    (eprint)
    (eprintf ;args)))

(defn elof
  [& args]
  (eprinf ;args))

