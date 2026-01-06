# :w - warn
# :e - error
# :i - info
# :o - output

(def d-table
  {:w eprin
   :e eprin
   :i eprin
   :o prin})

(defn note
  [flavor & args]
  (def disp-table (dyn :d-table d-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def df-table
  {:w eprinf
   :e eprinf
   :i eprinf
   :o prinf})

(defn notef
  [flavor & args]
  (def disp-table (dyn :df-table df-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def dn-table
  {:w eprint
   :e eprint
   :i eprint
   :o print})

(defn noten
  [flavor & args]
  (def disp-table (dyn :dn-table dn-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def dnf-table
  {:w eprintf
   :e eprintf
   :i eprintf
   :o printf})

(defn notenf
  [flavor & args]
  (def disp-table (dyn :dnf-table dnf-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

