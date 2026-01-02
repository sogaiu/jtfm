(defn l/log
  [& args]
  (print ;args))

(defn l/logf
  [& args]
  (if (empty? args)
    (print)
    (printf ;args)))

(defn l/lof
  [& args]
  (prinf ;args))

(defn l/elog
  [& args]
  (eprint ;args))

(defn l/elogf
  [& args]
  (if (empty? args)
    (eprint)
    (eprintf ;args)))

(defn l/elof
  [& args]
  (eprinf ;args))

