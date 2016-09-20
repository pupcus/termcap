(ns termcap.utils
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [utils.common.parse :refer [parse-number]]))

(defn get-term []
  (System/getenv "TERM"))

;; TODO: pull more info out of stty as we need to??
(defn terminal-settings []
  (:out (shell/sh "/bin/bash" "-c" "stty -e </dev/tty")))

(defn terminal-dimensions []
  (let [[_ _ rows columns] (re-find #"(?smd)^speed\s+(\d+)\s+baud;\s+(\d+)\s+rows;\s+(\d+)\s+columns;.*" (terminal-settings))]
    [(parse-number rows) (parse-number columns)]))

(defn get-path [term]
  (let [os-name (System/getProperty "os.name")
        c       (first term)]
    (cond
      (= os-name "Mac OS X")   (format "/usr/share/terminfo/%x/%s" (int c) term)
      :otherwise               (format "/usr/share/terminfo/%c/%s" c term))))

(defn read-binary-entry [term]
  (let [file (io/as-file (str (get-path term)))]
    (if (.exists file)
      (with-open [is (java.io.FileInputStream. file)]
        (let [size (.available is)
              buffer (byte-array size)]
          (.read is buffer 0 size)
          buffer))
      (throw (ex-info "termcap file cannot be found!" {:file file})))))

