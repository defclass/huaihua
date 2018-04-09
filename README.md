# huaihua

Simple Template Engine for Clojure.


## Usage

Add huaihua dependence:

`[defclass/huaihua "0.1.0-SNAPSHOT"]`


```clojure

(require '[huaihua.core :refer :all])
;;=> nil

(def tpl "欢迎！{{description}}我白天是个{{job}}邮递员{{/job}}{{/description}}，晚上就是个有抱负的演员。")
;;=> #'user/tpl
;; 
(def snippet (get-snippet tpl))
;;=> #'user/snippet

(transform snippet
  :job "程序员"
  :description (fn [x] (str x ", 在倍洽开心地写bug")))
;;=> "欢迎！我白天是个程序员, 在倍洽开心的写bug，晚上就是个有抱负的演员。"


```

## License

Copyright © 2018 Michael Wong

Distributed under the Eclipse Public License .
