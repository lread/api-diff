(ns borkdude.api-diff
  (:require
   [clj-kondo.core :as clj-kondo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.deps.alpha :as tda]
   [clojure.tools.deps.alpha.util.maven :as mvn]))

(defn path [lib v]
  (let [deps `{:deps {~lib {:mvn/version ~v}} :mvn/repos ~mvn/standard-repos}]
    (-> (tda/resolve-deps deps {})
        (get lib)
        :paths first)))

(defn index-by
  [f coll]
  (persistent! (reduce #(assoc! %1 (f %2) %2) (transient {}) coll)))

(defn group [vars]
  (->> vars
       (sort-by (juxt :ns :row :col))
       (map (fn [v]
              [((juxt :ns :name) v)
               (select-keys v [:ns :name :row :col :filename
                               :fixed-arities :varargs-min-arity
                               :deprecated :private ::excluded-meta])]))))

(defn vars [lib exclude-meta]
  (->> (if exclude-meta
         (let [{:keys [namespace-definitions var-definitions]}
               (-> (clj-kondo/run! {:lint   [lib]
                                    :config {:output
                                             {:analysis {:var-definitions       {:meta true}
                                                         :namespace-definitions {:meta true}}
                                              :format   :edn}}})
                   :analysis)
               ns-meta-excludes (reduce #(let [m (select-keys (:meta %2) exclude-meta)]
                                           (if (seq m)
                                             (assoc %1 (:name %2) m)
                                             %1))
                                        {}
                                        namespace-definitions)]
           (->> var-definitions
                (map #(if-let [excluded-by (some-> (merge (:meta %) (get ns-meta-excludes (:ns %)))
                                                   (select-keys exclude-meta)
                                                   seq)]
                        (assoc % ::excluded-meta (-> excluded-by keys sort vec))
                        %))))
         (->> (clj-kondo/run! {:lint   [lib]
                               :config {:output {:analysis true :format :edn}}})
              :analysis :var-definitions))
       (remove #(some #{(:defined-by %)} ['cljs.core/declare 'clojure.core/declare]))))

(defn reg-finding! [findings m v1 v2]
  (swap! findings conj (cond-> (assoc m
                                      :v1 (select-keys v1 [:row :col :filename])
                                      :ns (:ns v1)
                                      :name (:name v1))
                         v2 (assoc :v2 (select-keys v2 [:row :col :filename])))))

(defn- force-os-path-syntax
  "see https://github.com/clj-kondo/clj-kondo/issues/1438
  can turf if/when this issue is fixed"
  [path]
  (some-> path io/file str))

(defn- report-text [result]
  (doseq [f (:findings result)
          :let [v1 (:v1 f)]]
    (println (str (:filename v1) ":"
                  (:row v1) ":"
                  (:col v1) ": "
                  (name (:level f)) ": "
                  (:ns f) "/" (:name f) " "
                  (case (:type f)
                    :var "was removed"
                    :now-private "has become private"
                    :deprecated "was deprecated"
                    :arity (format "arity %s was removed" (:data f))
                    :meta (format "now has meta %s" (:data f)))
                  "."))))

(defn api-diff [{:keys [lib v1 v2
                        path1 path2
                        exclude-meta]}]
  (let [path1 (or (force-os-path-syntax path1) (path lib v1))
        path2 (or (force-os-path-syntax path2) (path lib v2))
        vars-1 (vars path1 exclude-meta)
        vars-2 (vars path2 exclude-meta)
        compare-group-1 (group vars-1)
        compare-group-2-lookup (into {} (group vars-2))
        findings (atom [])]
    (doseq [[k var-1] compare-group-1]
      (when (and (not (:private var-1)) (not (::excluded-meta var-1)))
        (if-let [var-2 (get compare-group-2-lookup k)]
          (do
            (when (:private var-2)
              (reg-finding! findings {:level :error :diff :removal :type :now-private} var-1 var-2))
            (let [fixed-arities-v1  (:fixed-arities var-1)
                  fixed-arities-v2  (:fixed-arities var-2)
                  varargs-min-arity (:varargs-min-arity var-2)]
              (doseq [arity fixed-arities-v1]
                (when-not (or (contains? fixed-arities-v2 arity)
                              (and varargs-min-arity (>= arity varargs-min-arity)))
                  (reg-finding! findings {:level :error :diff :removal :type :arity :data arity} var-1 var-2))))
            (when (and (:deprecated var-2) (not (:deprecated var-1)))
              (reg-finding! findings {:level :warning :diff :removal :type :deprecated} var-1 var-2))
            (when (::excluded-meta var-2)
              (reg-finding! findings {:level :warning :diff :removal :type :meta :data (::excluded-meta var-2)} var-1 var-2)))
          (reg-finding! findings {:level :error :diff :removal :type :var} var-1 nil))))
    @findings))

(defn- to-keyword [s]
  (keyword
    (if (str/starts-with? s ":")
      (subs s 1)
      s)))

(defn parse-opts [opts opts-def]
  (let [[cmds opts] (split-with #(not (str/starts-with? % ":")) opts)]
    (reduce
     (fn [opts [arg-name arg-val]]
       (let [k (keyword (subs arg-name 1))
             od (k opts-def)
             v ((or (:parse-fn od) identity) arg-val)]
         (if-let [c (:collect-fn od)]
           (update opts k c v)
           (assoc opts k v))))
     {:cmds cmds}
     (partition 2 opts))))

(defn -main [& args]
  (let [{:keys [lib v1 v2 path1 path2] :as opts}
        (parse-opts args {:exclude-meta {:parse-fn to-keyword
                                         :collect-fn (fnil conj #{})}
                          :lib {:parse-fn symbol}
                          :format {:parse-fn to-keyword}})]
    (when-not (or (and lib v1 v2) (and path1 path2))
      (throw (ex-info "must specify either :lib, :v1 and :v2 OR :path1 and :path2" {})))
    (let [result {:args (vec args)
                  :opts opts
                  :findings (api-diff opts)}]
      (if (= :edn (:format opts))
        (prn result)
        (report-text result)))))
