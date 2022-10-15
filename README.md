# Spreadsheet

Alpha software.

## Dependency

Leiningen dependency:

```clojure
[com.phronemophobic/spreadsheet "0.1.1"]
```

deps.edn dependency:

```clojure
com.phronemophobic/spreadsheet {:mvn/version "0.1.1"}
```

## Usage

### Try it!

```sh
clj -Sdeps {:deps\ {com.phronemophobic/spreadsheet\ {:mvn/version\ \"0.1.1\"}}} -M -m com.phronemophobic.membrane.spreadsheet
```

### Programmatic usage

Open a spreadsheet window. Expressions will be evaluated in the caller's namespace.

```clojure
(require '[com.phronemophobic.membrane.spreadsheet :as ss])

(ss/run)
```

## License

Copyright © 2021 Adrian

Distributed under the Eclipse Public License version 1.0.
