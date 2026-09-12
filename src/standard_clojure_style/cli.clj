(ns standard-clojure-style.cli
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [jolt.fibers :as fibers]
   [standard-clojure-style.core :as core]))

(def ^:dynamic *version* "0.29.0")

(def default-file-extensions #{".clj" ".cljs" ".cljc" ".jank" ".edn"})

;; -----------------------------------------------------------------------------
;; ANSI color formatting
;; -----------------------------------------------------------------------------

(defn use-colors? []
  (let [no-color (System/getenv "NO_COLOR")
        force-color (System/getenv "FORCE_COLOR")]
    (and (or (nil? no-color) (= no-color ""))
         (not= force-color "0"))))

(defn color [code s]
  (if (use-colors?)
    (str "\u001b[" code "m" s "\u001b[0m")
    (str s)))

(defn bold [s] (color "1" s))
(defn dim [s] (color "2" s))
(defn red [s] (color "31" s))
(defn green [s] (color "32" s))
(defn yellow [s] (color "33" s))

(defn format-duration [duration-ms]
  (let [rounded (/ (Math/round (* duration-ms 100.0)) 100.0)]
    (dim (str "[" rounded "ms]"))))

(defn file-str [n]
  (if (= n 1) "file" "files"))

(defn relative-filename [filename root-dir]
  (let [root-path (.getCanonicalPath (io/file root-dir))
        root-prefix (if (str/ends-with? root-path "/") root-path (str root-path "/"))]
    (if (str/starts-with? filename root-prefix)
      (subs filename (count root-prefix))
      (if (= filename root-path)
        ""
        filename))))

(defn add-period-prefix [ext]
  (if (str/starts-with? ext ".") ext (str "." ext)))

(defn normalize-log-level [level]
  (let [s (str level)]
    (cond
      (or (= s "ignore-already-formatted") (= s "1")) "ignore-already-formatted"
      (or (= s "quiet") (= s "5")) "quiet"
      :else "everything")))

;; -----------------------------------------------------------------------------
;; Glob & File Discovery
;; -----------------------------------------------------------------------------

(defn contains-glob-syntax? [pattern]
  (boolean (re-find #"[*?{}\[\]]" pattern)))

(defn glob->regex [pattern]
  (let [s (str/replace pattern #"\{([^{}]+)\}"
                       (fn [[_ inner]]
                         (str "(?:" (str/replace inner #"," "|") ")")))
        len (count s)
        sb (StringBuilder. "^")]
    (loop [i 0]
      (if (>= i len)
        (do (.append sb "$") (re-pattern (.toString sb)))
        (let [c (.charAt s i)]
          (cond
            (and (= c \*) (< (inc i) len) (= (.charAt s (inc i)) \*))
            (if (and (< (+ i 2) len) (= (.charAt s (+ i 2)) \/))
              (do (.append sb "(?:.*/)?")
                  (recur (+ i 3)))
              (do (.append sb ".*")
                  (recur (+ i 2))))

            (= c \*)
            (do (.append sb "[^/]*")
                (recur (inc i)))

            (= c \?)
            (do (.append sb "[^/]")
                (recur (inc i)))

            (and (= c \() (< (+ i 2) len) (= (.substring s i (+ i 3)) "(?:"))
            (let [end-idx (.indexOf s ")" i)]
              (if (>= end-idx 0)
                (do (.append sb (.substring s i (inc end-idx)))
                    (recur (inc end-idx)))
                (do (.append sb "\\(")
                    (recur (inc i)))))

            (#{\. \$ \^ \+ \[ \] \\} c)
            (do (.append sb (str "\\" c))
                (recur (inc i)))

            :else
            (do (.append sb (str c))
                (recur (inc i)))))))))

(defn absolute-path [root-dir filename]
  (let [f (io/file filename)]
    (if (.isAbsolute f)
      (.getCanonicalPath f)
      (.getCanonicalPath (io/file root-dir filename)))))

(defn file-ext [filename]
  (let [idx (str/last-index-of filename ".")]
    (if (and idx (>= idx 0))
      (subs filename idx)
      "")))

(defn has-allowed-extension? [filename file-extensions]
  (contains? file-extensions (file-ext filename)))

(defn files-from-directory [directory should-include-fn]
  (let [d (io/file directory)]
    (if (.isDirectory d)
      (->> (file-seq d)
           (filter #(.isFile %))
           (map #(.getCanonicalPath %))
           (filter should-include-fn)
           set)
      #{})))

(defn files-from-glob [root-dir pattern]
  (let [root-file (io/file root-dir)
        root-path (.getCanonicalPath root-file)
        root-prefix (str root-path "/")
        is-abs (str/starts-with? pattern "/")
        re (glob->regex pattern)]
    (->> (file-seq (if is-abs (io/file "/") root-file))
         (filter #(.isFile %))
         (keep (fn [f]
                 (let [abs (.getCanonicalPath f)
                       rel (if (str/starts-with? abs root-prefix)
                             (subs abs (count root-prefix))
                             abs)
                       test-path (if is-abs abs rel)]
                   (when (re-matches re test-path)
                     abs))))
         set)))

(defn add-direct-argument [files root-dir direct-arg file-extensions on-missing-path]
  (let [p (absolute-path root-dir direct-arg)
        f (io/file p)]
    (cond
      (.isFile f) (conj files p)
      (.isDirectory f) (into files (files-from-directory p #(has-allowed-extension? % file-extensions)))
      :else (do (on-missing-path "include" direct-arg) files))))

(defn add-include-pattern [files root-dir pattern file-extensions on-missing-path]
  (let [p (absolute-path root-dir pattern)
        f (io/file p)]
    (cond
      (.isFile f)
      (if (has-allowed-extension? p file-extensions)
        (conj files p)
        files)

      (.isDirectory f)
      (into files (files-from-directory p #(has-allowed-extension? % file-extensions)))

      :else
      (let [matches (files-from-glob root-dir pattern)
            filtered (filter #(has-allowed-extension? % file-extensions) matches)]
        (into files filtered)))))

(defn add-ignore-pattern [files root-dir pattern on-missing-path]
  (let [p (absolute-path root-dir pattern)
        f (io/file p)]
    (cond
      (.isFile f)
      (conj files p)

      (.isDirectory f)
      (into files (files-from-directory p (constantly true)))

      :else
      (let [matches (files-from-glob root-dir pattern)]
        (if (empty? matches)
          (do
            (when-not (contains-glob-syntax? pattern)
              (on-missing-path "ignore" pattern))
            files)
          (into files matches))))))

(defn discover-files [{:keys [root-dir direct-args include-patterns ignore-patterns
                              file-extensions on-missing-path]}]
  (let [file-exts (or file-extensions default-file-extensions)
        on-missing (or on-missing-path (fn [_ _]))
        included (reduce (fn [acc arg] (add-direct-argument acc root-dir arg file-exts on-missing))
                         #{}
                         direct-args)
        included (reduce (fn [acc pat] (add-include-pattern acc root-dir pat file-exts on-missing))
                         included
                         include-patterns)
        ignored (reduce (fn [acc pat] (add-ignore-pattern acc root-dir pat on-missing))
                        #{}
                        ignore-patterns)]
    (set/difference included ignored)))

;; -----------------------------------------------------------------------------
;; Simple JSON / EDN parsing for config files
;; -----------------------------------------------------------------------------

(defn parse-json-config [s]
  ;; Simple regex-based config extractor for include, ignore, log-level
  (let [extract-strings (fn [key-name text]
                          (let [re (re-pattern (str "\"" key-name "\"\\s*:\\s*\\[([^\\]]*)\\]"))
                                m (re-find re text)]
                            (if m
                              (->> (re-seq #"\"([^\"]+)\"" (second m))
                                   (map second)
                                   vec)
                              (let [single-re (re-pattern (str "\"" key-name "\"\\s*:\\s*\"([^\"]+)\""))
                                    sm (re-find single-re text)]
                                (when sm [(second sm)])))))
        extract-string (fn [key-name text]
                         (let [re (re-pattern (str "\"" key-name "\"\\s*:\\s*\"([^\"]+)\""))
                               m (re-find re text)]
                           (when m (second m))))]
    {:include (extract-strings "include" s)
     :ignore (extract-strings "ignore" s)
     :log-level (extract-string "log-level" s)}))

(defn load-config [config-arg root-dir]
  (if (and (string? config-arg) (not= config-arg ""))
    (let [f (io/file config-arg)
          is-json (str/ends-with? config-arg ".json")
          is-edn (str/ends-with? config-arg ".edn")]
      (if-not (.exists f)
        {:error (str "Unable to load config file: " config-arg "\nFile does not exist.")}
        (try
          (let [content (slurp f)]
            (cond
              is-json (parse-json-config content)
              is-edn (let [edn-map (edn/read-string content)]
                       {:include (let [inc (:include edn-map)]
                                   (cond (vector? inc) inc (string? inc) [inc] :else nil))
                        :ignore (let [ig (:ignore edn-map)]
                                  (cond (vector? ig) ig (string? ig) [ig] :else nil))
                        :log-level (some-> (get edn-map :log-level) str)})
              :else {:error "The filename does not end in .json or .edn. That is probably wrong."}))
          (catch Exception e
            {:error (str "Unable to load config file: " config-arg
                         (if is-json "\nMaybe the file is invalid JSON?" "\nMaybe the file is invalid EDN?"))}))))
    ;; Try default config files
    (let [edn-f (io/file root-dir ".standard-clj.edn")
          json-f (io/file root-dir ".standard-clj.json")]
      (cond
        (.exists edn-f)
        (try
          (let [edn-map (edn/read-string (slurp edn-f))]
            {:include (let [inc (:include edn-map)]
                        (cond (vector? inc) inc (string? inc) [inc] :else nil))
             :ignore (let [ig (:ignore edn-map)]
                       (cond (vector? ig) ig (string? ig) [ig] :else nil))
             :log-level (some-> (get edn-map :log-level) str)})
          (catch Exception _ nil))

        (.exists json-f)
        (try
          (parse-json-config (slurp json-f))
          (catch Exception _ nil))

        :else nil))))

;; -----------------------------------------------------------------------------
;; Classify Format Result
;; -----------------------------------------------------------------------------

(defn classify-format-result [original-text format-result]
  (if (and format-result (= (:status format-result) "success"))
    (let [output-text (str (:out format-result) "\n")]
      (if (and (string? original-text) (= output-text original-text))
        {:action "already-formatted" :outputText output-text}
        {:action "formatted" :outputText output-text}))
    (if (and format-result (= (:status format-result) "error") (string? (:reason format-result)))
      {:action "error" :errorMessage (:reason format-result)}
      {:action "error" :errorMessage "Unknown error! Please help the standard-clj project by opening an issue to report this 🙏"})))

;; -----------------------------------------------------------------------------
;; CLI Argument Parsing
;; -----------------------------------------------------------------------------

(defn parse-args [args]
  (loop [args args
         opts {:command nil
               :direct-args []
               :include []
               :ignore []
               :file-ext default-file-extensions
               :config nil
               :output nil
               :log-level "everything"
               :help false
               :version false}]
    (if (empty? args)
      opts
      (let [a (first args)]
        (cond
          (contains? #{"-h" "--help" "help"} a)
          (recur (rest args) (assoc opts :help true))

          (contains? #{"-v" "--version" "version"} a)
          (recur (rest args) (assoc opts :version true))

          (and (nil? (:command opts)) (contains? #{"check" "fix" "list"} a))
          (recur (rest args) (assoc opts :command a))

          (contains? #{"-c" "--config"} a)
          (recur (drop 2 args) (assoc opts :config (second args)))

          (contains? #{"-in" "--include"} a)
          (recur (drop 2 args) (update opts :include conj (second args)))

          (contains? #{"-ig" "--ignore"} a)
          (recur (drop 2 args) (update opts :ignore conj (second args)))

          (contains? #{"-l" "--log-level"} a)
          (recur (drop 2 args) (assoc opts :log-level (normalize-log-level (second args))))

          (= a "--output")
          (recur (drop 2 args) (assoc opts :output (second args)))

          (= a "--file-ext")
          (let [exts (->> (str/split (or (second args) "") #",")
                          (map add-period-prefix)
                          set)]
            (recur (drop 2 args) (assoc opts :file-ext exts)))

          (str/starts-with? a "-")
          (if (= a "-")
            (recur (rest args) (update opts :direct-args conj a))
            (recur (rest args) opts))

          :else
          (recur (rest args) (update opts :direct-args conj a)))))))

;; -----------------------------------------------------------------------------
;; Help & Info
;; -----------------------------------------------------------------------------

(defn print-help []
  (println "standard-clj <cmd> [args]")
  (println "")
  (println "Commands:")
  (println "  check  Checks if files are formatted according to Standard Clojure Style.")
  (println "         This command does not modify files.")
  (println "         Returns exit code 0 if all files are formatted, 1 otherwise.")
  (println "  fix    Formats files according to Standard Clojure Style.")
  (println "         This command will modify your files on disk.")
  (println "         Returns exit code 0 if all files are formatted, 1 otherwise.")
  (println "  list   Prints a list of files that will be used by the \"check\" or \"fix\" commands.")
  (println "         Useful for debugging your .standard-clj.edn file or glob patterns.")
  (println "")
  (println "Options:")
  (println "  -c, --config    Path to config file (.standard-clj.edn or .standard-clj.json)")
  (println "  -in, --include  Include a file, directory, or glob pattern (can be repeated)")
  (println "  -ig, --ignore   Ignore a file, directory, or glob pattern (can be repeated)")
  (println "  --file-ext      Comma-separated list of file extensions (default: clj,cljs,cljc,jank,edn)")
  (println "  -l, --log-level Set the logging level: \"everything\", \"ignore-already-formatted\", \"quiet\"")
  (println "  --output        Output format for list: json, json-pretty, edn, edn-pretty")
  (println "  -v, --version   Show version number")
  (println "  -h, --help      Show help")
  (println "")
  (println "Examples:")
  (println "  standard-clj list src/")
  (println "  standard-clj check src/ test/")
  (println "  standard-clj fix src/")
  (println "  standard-clj fix -"))

(defn print-version []
  (println *version*))

;; -----------------------------------------------------------------------------
;; Commands execution
;; -----------------------------------------------------------------------------

(defn run-cli [args]
  (let [opts (parse-args args)]
    (cond
      (:help opts) (do (print-help) 0)
      (:version opts) (do (print-version) 0)
      (nil? (:command opts)) (do (print-help) 1)

      :else
      (let [root-dir (System/getProperty "user.dir")
            cfg (load-config (:config opts) root-dir)]
        (if (:error cfg)
          (do
            (binding [*out* *err*]
              (println (:error cfg)))
            1)
          (let [log-lvl (or (:log-level opts) (:log-level cfg) "everything")
                include-patterns (if (seq (:include opts))
                                   (:include opts)
                                   (or (:include cfg) []))
                ignore-patterns (if (seq (:ignore opts))
                                  (:ignore opts)
                                  (or (:ignore cfg) []))
                has-file-selection (or (seq (:direct-args opts)) (seq (:include opts)))
                final-include-patterns (if has-file-selection (:include opts) include-patterns)
                on-missing-path (fn [kind p]
                                  (when-not (= log-lvl "quiet")
                                    (binding [*out* *err*]
                                      (let [ign (if (= kind "ignore") " to ignore" "")]
                                        (println (str (bold (yellow "WARN")) " Could not find a file or directory" ign " at \"" p "\""))))))
                cmd (:command opts)]

            (cond
              ;; LIST COMMAND
              (= cmd "list")
              (let [files (discover-files {:root-dir root-dir
                                           :direct-args (:direct-args opts)
                                           :include-patterns final-include-patterns
                                           :ignore-patterns ignore-patterns
                                           :file-extensions (:file-ext opts)
                                           :on-missing-path on-missing-path})
                    sorted-files (sort files)]
                (case (:output opts)
                  "json"
                  (println (str "[" (str/join "," (map pr-str sorted-files)) "]"))

                  "json-pretty"
                  (if (empty? sorted-files)
                    (println "[]")
                    (println (str "[\n" (str/join ",\n" (map #(str "  " (pr-str %)) sorted-files)) "\n]")))

                  "edn"
                  (println (pr-str (vec sorted-files)))

                  "edn-pretty"
                  (if (empty? sorted-files)
                    (println "[]")
                    (println (str "[\n " (str/join "\n " (map pr-str sorted-files)) "\n]")))

                  ;; default
                  (doseq [f sorted-files]
                    (println f)))
                0)

              ;; FIX STDIN
              (and (= cmd "fix") (= (last (:direct-args opts)) "-"))
              (let [stdin-str (slurp *in*)]
                (if (or (nil? stdin-str) (= stdin-str ""))
                  (do
                    (binding [*out* *err*]
                      (println "Nothing found on stdin. Please pipe some Clojure code to stdin when using \"standard-clj fix -\""))
                    1)
                  (let [res (core/format stdin-str)]
                    (if (= (:status res) "success")
                      (do
                        (print (:out res))
                        (when-not (str/ends-with? (:out res) "\n")
                          (println ""))
                        0)
                      (do
                        (binding [*out* *err*]
                          (println (str "Failed to format code: " (:reason res))))
                        1)))))

              ;; CHECK or FIX FILES
              (or (= cmd "check") (= cmd "fix"))
              (let [start-time (System/currentTimeMillis)
                    files (discover-files {:root-dir root-dir
                                           :direct-args (remove #{"-"} (:direct-args opts))
                                           :include-patterns final-include-patterns
                                           :ignore-patterns ignore-patterns
                                           :file-extensions (:file-ext opts)
                                           :on-missing-path on-missing-path})]
                (if (empty? files)
                  (do
                    (binding [*out* *err*]
                      (println (str "No files were passed to the \"" cmd "\" command. Please pass a filename, directory, or --include glob pattern.")))
                    1)
                  (let [sorted-files (sort files)
                        total (count sorted-files)
                        already-fmt (atom 0)
                        need-fmt (atom 0)
                        errors (atom 0)
                        at-least-one-printed (atom false)]
                    (when-not (= log-lvl "quiet")
                      (println (str (bold (str "standard-clj " cmd)) " " (dim (str "[" *version* "]"))))
                      (println ""))

                    (let [spawned (mapv (fn [f-path]
                                          (fibers/spawn
                                            (fn []
                                              (let [f-start (System/currentTimeMillis)
                                                    rel-p (relative-filename f-path root-dir)
                                                    content (try (slurp f-path) (catch Exception _ nil))]
                                                (if (nil? content)
                                                  {:type :read-error :f-path f-path :rel-p rel-p :dur (- (System/currentTimeMillis) f-start)}
                                                  (let [res (core/format content)
                                                        cls (classify-format-result content res)
                                                        dur (- (System/currentTimeMillis) f-start)]
                                                    {:type :ok :f-path f-path :rel-p rel-p :dur dur :cls cls}))))))
                                        sorted-files)
                          file-results (mapv fibers/join spawned)]

                      (doseq [item file-results]
                        (let [{:keys [type f-path rel-p dur cls]} item]
                          (if (= type :read-error)
                            (do
                              (swap! errors inc)
                              (reset! at-least-one-printed true)
                              (when-not (= log-lvl "quiet")
                                (binding [*out* *err*]
                                  (println (str (red "E") " " (bold (red rel-p)) " - Unable to read file "
                                                (format-duration dur))))))
                            (case (:action cls)
                              "already-formatted"
                              (do
                                (swap! already-fmt inc)
                                (when (and (not= log-lvl "quiet") (not= log-lvl "ignore-already-formatted"))
                                  (reset! at-least-one-printed true)
                                  (println (str (green "✓") " " (bold rel-p) " " (format-duration dur)))))

                              "formatted"
                              (do
                                (reset! at-least-one-printed true)
                                (if (= cmd "fix")
                                  (do
                                    (spit f-path (:outputText cls))
                                    (swap! need-fmt inc)
                                    (when-not (= log-lvl "quiet")
                                      (println (str (green "F") " " (bold rel-p) " " (format-duration dur)))))
                                  (do
                                    (swap! need-fmt inc)
                                    (when-not (= log-lvl "quiet")
                                      (binding [*out* *err*]
                                        (println (str (red "✗") " " (bold rel-p) " " (format-duration dur))))))))

                              "error"
                              (do
                                (swap! errors inc)
                                (reset! at-least-one-printed true)
                                (when-not (= log-lvl "quiet")
                                  (binding [*out* *err*]
                                    (println (str (red "E") " " (bold (red rel-p)) " - " (:errorMessage cls) " "
                                                  (format-duration dur)))))))))))

                    (let [total-dur (- (System/currentTimeMillis) start-time)]
                      (when (and @at-least-one-printed (not= log-lvl "quiet"))
                        (println ""))

                      (if (= cmd "check")
                        (if (and (= @already-fmt total) (= @errors 0))
                          (do
                            (when-not (= log-lvl "quiet")
                              (println (str (green (if (= total 1)
                                                     "1 file formatted with Standard Clojure Style 👍"
                                                     (str "All " total " files formatted with Standard Clojure Style 👍")))
                                            " " (format-duration total-dur))))
                            0)
                          (do
                            (when-not (= log-lvl "quiet")
                              (println (green (str @already-fmt " " (file-str @already-fmt) " formatted with Standard Clojure Style")))
                              (println (red (str @need-fmt " " (file-str @need-fmt) " require formatting")))
                              (when (> @errors 0)
                                (println (red (str @errors " " (file-str @errors) " with errors"))))
                              (println (str "Checked " total " " (file-str total) ". " (format-duration total-dur))))
                            1))

                        ;; fix summary
                        (if (= @errors 0)
                          (do
                            (when-not (= log-lvl "quiet")
                              (println (str (green (if (= total 1)
                                                     "1 file formatted with Standard Clojure Style 👍"
                                                     (str "All " total " files formatted with Standard Clojure Style 👍")))
                                            " " (format-duration total-dur))))
                            0)
                          (do
                            (when-not (= log-lvl "quiet")
                              (println (green (str (+ @already-fmt @need-fmt) " files formatted with Standard Clojure Style")))
                              (println (red (str @errors " files with errors")))
                              (println (str "Checked " total " files. " (format-duration total-dur))))
                            1))))))))))))))
