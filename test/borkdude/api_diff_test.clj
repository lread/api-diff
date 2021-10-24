(ns borkdude.api-diff-test
  (:require [borkdude.api-diff :as api-diff]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest testing is]]
            [clojure.walk :as walk]))

(defn- osify-path [p]
  (str/replace p "/" (System/getProperty "file.separator")))

(defn- osify [lines]
  (mapv (fn [l]
          (if (str/starts-with? l "test-resources")
            (let [[p r] (str/split l #" " 2)]
              (str (osify-path p) " " r))
            l))
        lines))

(defn- osify-findings [findings]
  (walk/postwalk #(if-let [f (:filename %)]
                    (assoc % :filename (osify-path f))
                    %)
                 findings))

(deftest diff-test
  (testing "libs"
    (let [actual-lines (-> (api-diff/-main ":lib" "clj-kondo/clj-kondo"
                                           ":v1" "2021.09.25"
                                           ":v2" "2021.09.15")
                           with-out-str
                           str/split-lines)]
      (is (= (osify ["clj_kondo/core.clj:205:1: error: clj-kondo.core/resolve-config was removed."
                     "clj_kondo/core.clj:213:1: error: clj-kondo.core/config-hash was removed."
                     "clj_kondo/impl/analyzer.clj:1473:1: error: clj-kondo.impl.analyzer/analyze-ns-unmap was removed."])
             actual-lines))))
  (testing "paths"
    (let [actual-lines (-> (api-diff/-main ":path1" "test-resources/older"
                                           ":path2" "test-resources/newer")
                           with-out-str
                           str/split-lines)]
      (is (= (osify ["test-resources/older/example.clj:6:1: error: example/becomes-private has become private."
                     "test-resources/older/example.clj:11:1: error: example/y was removed."
                     "test-resources/older/example.clj:12:1: warning: example/z was deprecated."
                     "test-resources/older/example.clj:14:1: error: example/nodoc-changes arity 1 was removed."
                     "test-resources/older/example.clj:16:1: error: example/skip-wiki was removed."
                     "test-resources/older/example.clj:18:1: error: example/arity-change-and-becomes-skip-wiki arity 2 was removed."
                     "test-resources/older/other.clj:3:1: error: other/other-x was removed."])
             actual-lines)))
    (testing "files"
      (let [actual-lines (-> (api-diff/-main ":path1" "test-resources/older/example.clj"
                                             ":path2" "test-resources/newer/example.clj")
                             with-out-str
                             str/split-lines)]
        (is (= (osify ["test-resources/older/example.clj:6:1: error: example/becomes-private has become private."
                       "test-resources/older/example.clj:11:1: error: example/y was removed."
                       "test-resources/older/example.clj:12:1: warning: example/z was deprecated."
                       "test-resources/older/example.clj:14:1: error: example/nodoc-changes arity 1 was removed."
                       "test-resources/older/example.clj:16:1: error: example/skip-wiki was removed."
                       "test-resources/older/example.clj:18:1: error: example/arity-change-and-becomes-skip-wiki arity 2 was removed."])
               actual-lines))))
    (testing "exclude-meta single"
      (let [actual-lines (-> (api-diff/-main ":path1" "test-resources/older"
                                             ":path2" "test-resources/newer"
                                             ":exclude-meta" ":no-doc")
                             with-out-str
                             str/split-lines)]
        (is (= (osify ["test-resources/older/example.clj:6:1: error: example/becomes-private has become private."
                       "test-resources/older/example.clj:8:1: warning: example/becomes-nodoc now has meta [:no-doc]."
                       "test-resources/older/example.clj:11:1: error: example/y was removed."
                       "test-resources/older/example.clj:12:1: warning: example/z was deprecated."
                       "test-resources/older/example.clj:16:1: error: example/skip-wiki was removed."
                       "test-resources/older/example.clj:18:1: error: example/arity-change-and-becomes-skip-wiki arity 2 was removed."])
               actual-lines))))
    (testing "exclude-meta multiple"

      (let [actual-lines (-> (api-diff/-main ":path1" "test-resources/older"
                                             ":path2" "test-resources/newer"
                                             ":exclude-meta" ":no-doc"
                                             ":exclude-meta" ":skip-wiki")
                             with-out-str
                             str/split-lines)]
        (is (= (osify ["test-resources/older/example.clj:6:1: error: example/becomes-private has become private."
                       "test-resources/older/example.clj:8:1: warning: example/becomes-nodoc now has meta [:no-doc]."
                       "test-resources/older/example.clj:11:1: error: example/y was removed."
                       "test-resources/older/example.clj:12:1: warning: example/z was deprecated."
                       "test-resources/older/example.clj:18:1: error: example/arity-change-and-becomes-skip-wiki arity 2 was removed."
                       "test-resources/older/example.clj:18:1: warning: example/arity-change-and-becomes-skip-wiki now has meta [:skip-wiki]."])
               actual-lines)))
      (testing "structured output"
        (let [actual-edn (-> (api-diff/-main ":path1" "test-resources/older"
                                             ":path2" "test-resources/newer"
                                             ":exclude-meta" ":no-doc"
                                             ":exclude-meta" ":skip-wiki"
                                             ":format" ":edn")
                             with-out-str
                             edn/read-string)]
          (is (= {:args [":path1" "test-resources/older" ":path2" "test-resources/newer"
                         ":exclude-meta" ":no-doc" ":exclude-meta" ":skip-wiki" ":format" ":edn"]
                  :opts {:cmds [] :path1 "test-resources/older" :path2 "test-resources/newer"
                         :exclude-meta #{:no-doc :skip-wiki} :format :edn}
                  :findings (osify-findings '[{:level :error,
                                               :diff :removal,
                                               :type :now-private,
                                               :v1 {:row 6, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name becomes-private,
                                               :v2 {:row 8, :col 1, :filename "test-resources/newer/example.clj"}}
                                              {:level :warning,
                                               :diff :removal,
                                               :type :meta,
                                               :data [:no-doc],
                                               :v1 {:row 8, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name becomes-nodoc,
                                               :v2 {:row 11, :col 1, :filename "test-resources/newer/example.clj"}}
                                              {:level :error,
                                               :diff :removal,
                                               :type :var,
                                               :v1 {:row 11, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name y}
                                              {:level :warning,
                                               :diff :removal,
                                               :type :deprecated,
                                               :v1 {:row 12, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name z,
                                               :v2 {:row 15, :col 1, :filename "test-resources/newer/example.clj"}}
                                              {:level :error,
                                               :diff :removal,
                                               :type :arity,
                                               :data 2,
                                               :v1 {:row 18, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name arity-change-and-becomes-skip-wiki,
                                               :v2 {:row 21, :col 1, :filename "test-resources/newer/example.clj"}}
                                              {:level :warning,
                                               :diff :removal,
                                               :type :meta,
                                               :data [:skip-wiki],
                                               :v1 {:row 18, :col 1, :filename "test-resources/older/example.clj"},
                                               :ns example, :name arity-change-and-becomes-skip-wiki,
                                               :v2 {:row 21, :col 1, :filename "test-resources/newer/example.clj"}}])}
                 actual-edn)))))))
